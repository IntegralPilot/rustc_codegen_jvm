use super::*;

#[derive(Debug, Clone)]
pub(crate) enum FnPointerTarget {
    Static(crate::lower1::naming::FnNameData),
    ImportedStatic(crate::lower1::naming::FnNameData),
    Virtual {
        class_name: String,
        method_name: String,
    },
    Interface {
        class_name: String,
        method_name: String,
    },
    InterfacePointer {
        class_name: String,
        method_name: String,
    },
    ImportedField {
        class_name: String,
        field_name: String,
        access: crate::lower1::naming::JvmFieldAccess,
    },
    ImportedStaticField {
        class_name: String,
        field_name: String,
        access: crate::lower1::naming::JvmFieldAccess,
    },
    ImportedConstructor {
        class_name: String,
    },
}

impl FnPointerTarget {
    fn display_name(&self) -> String {
        match self {
            Self::Static(target) | Self::ImportedStatic(target) => {
                target.class_to_call_on.as_deref().map_or_else(
                    || target.method_name.clone(),
                    |class| format!("{class}::{}", target.method_name),
                )
            }
            Self::Virtual {
                class_name,
                method_name,
            }
            | Self::Interface {
                class_name,
                method_name,
            }
            | Self::InterfacePointer {
                class_name,
                method_name,
            } => format!("{class_name}::{method_name}"),
            Self::ImportedField {
                class_name,
                field_name,
                ..
            }
            | Self::ImportedStaticField {
                class_name,
                field_name,
                ..
            } => format!("{class_name}::{field_name}"),
            Self::ImportedConstructor { class_name } => format!("{class_name}::<init>"),
        }
    }
}

/// Finds a callable JVM target for a Rust function pointer. Concrete Rust
/// instances use their distinct static entry points; only actual virtual Rust
/// instances dispatch through a generated JVM class or interface method.
pub(crate) fn fn_pointer_target<'tcx>(
    tcx: TyCtxt<'tcx>,
    target_instance: Instance<'tcx>,
    signature: &oomir::Signature,
) -> Option<FnPointerTarget> {
    let jvm_import = crate::lower1::naming::jvm_import_from_instance(tcx, target_instance)
        .unwrap_or_else(|message| tcx.dcx().fatal(message));
    if let Some(import) = jvm_import {
        match import {
            crate::lower1::naming::JvmImport::Static(import) => {
                let rust_descriptor = signature.to_jvm_descriptor_with_explicit_params();
                if let Some(explicit_descriptor) = import.descriptor
                    && rust_descriptor != explicit_descriptor
                {
                    tcx.dcx().fatal(format!(
                        "JVM import descriptor `{explicit_descriptor}` does not match the lowered Rust signature `{rust_descriptor}`"
                    ));
                }
                return Some(FnPointerTarget::ImportedStatic(
                    crate::lower1::naming::FnNameData {
                        class_to_call_on: Some(import.class_name),
                        method_name: import.method_name,
                    },
                ));
            }
            crate::lower1::naming::JvmImport::Virtual(import) => {
                let class_name = crate::lower1::naming::jvm_virtual_receiver_class_from_instance(
                    tcx,
                    target_instance,
                )
                .unwrap_or_else(|message| tcx.dcx().fatal(message));
                let mut method_signature = signature.clone();
                method_signature.is_static = false;
                let rust_descriptor = method_signature.to_string();
                if let Some(explicit_descriptor) = import.descriptor
                    && rust_descriptor != explicit_descriptor
                {
                    tcx.dcx().fatal(format!(
                        "JVM import descriptor `{explicit_descriptor}` does not match the lowered Rust signature `{rust_descriptor}`"
                    ));
                }
                return Some(FnPointerTarget::Virtual {
                    class_name,
                    method_name: import.method_name,
                });
            }
            crate::lower1::naming::JvmImport::Field(import) => {
                let class_name = crate::lower1::naming::jvm_field_receiver_class_from_instance(
                    tcx,
                    target_instance,
                )
                .unwrap_or_else(|message| tcx.dcx().fatal(message));
                let access = crate::lower1::naming::classify_jvm_field_access(signature, false)
                    .unwrap_or_else(|message| tcx.dcx().fatal(message));
                return Some(FnPointerTarget::ImportedField {
                    class_name,
                    field_name: import.field_name,
                    access,
                });
            }
            crate::lower1::naming::JvmImport::StaticField(import) => {
                let access = crate::lower1::naming::classify_jvm_field_access(signature, true)
                    .unwrap_or_else(|message| tcx.dcx().fatal(message));
                return Some(FnPointerTarget::ImportedStaticField {
                    class_name: import.class_name,
                    field_name: import.field_name,
                    access,
                });
            }
            crate::lower1::naming::JvmImport::Constructor(import) => {
                return Some(FnPointerTarget::ImportedConstructor {
                    class_name: import.class_name,
                });
            }
        }
    }
    let static_name = crate::lower1::naming::mono_fn_name_from_instance(tcx, target_instance);
    let associated_item = tcx.opt_associated_item(target_instance.def_id());
    let dynamic_trait_impl = associated_item.as_ref().is_some_and(|item| {
        let Some(impl_def_id) = item.impl_container(tcx) else {
            return false;
        };
        let Some(trait_ref) = tcx.impl_opt_trait_ref(impl_def_id) else {
            return false;
        };
        let trait_ref = trait_ref
            .instantiate(tcx, target_instance.args)
            .skip_norm_wip();
        let mut self_ty = trait_ref.self_ty();
        while let TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) = self_ty.kind() {
            self_ty = *pointee;
        }
        matches!(self_ty.kind(), TyKind::Dynamic(predicates, _)
        if predicates.principal().is_some_and(|principal| {
            principal.skip_binder().def_id == trait_ref.def_id
        }))
    });
    if !matches!(target_instance.def, InstanceKind::Virtual(..)) && !dynamic_trait_impl {
        return Some(FnPointerTarget::Static(static_name));
    }

    let Some(associated_item) = associated_item else {
        return Some(FnPointerTarget::Static(static_name));
    };
    if !associated_item.is_method() {
        return Some(FnPointerTarget::Static(static_name));
    }
    let (_, receiver_ty) = signature.params.first()?;
    let (receiver_ty, indirect_receiver) = match receiver_ty {
        oomir::Type::Pointer(inner) | oomir::Type::Reference(inner) => (inner.as_ref(), true),
        other => (other, false),
    };
    let mut method_signature = signature.clone();
    method_signature.is_static = false;
    let method_name = crate::lower1::naming::associated_method_name_from_instance(
        tcx,
        target_instance,
        &method_signature,
    );
    match receiver_ty {
        oomir::Type::Class(class_name) => Some(FnPointerTarget::Virtual {
            class_name: class_name.clone(),
            method_name,
        }),
        oomir::Type::Interface(class_name) => Some(if indirect_receiver {
            FnPointerTarget::InterfacePointer {
                class_name: class_name.clone(),
                method_name,
            }
        } else {
            FnPointerTarget::Interface {
                class_name: class_name.clone(),
                method_name,
            }
        }),
        // Primitive and slice-like receivers have no JVM object on which to
        // dispatch. Their compiled-core implementation is emitted as a static
        // method with the Rust function-pointer ABI.
        _ => Some(FnPointerTarget::Static(static_name)),
    }
}

pub(crate) fn ensure_fn_pointer_adapter_class<'tcx>(
    data_types: &mut Definitions<'tcx>,
    target_function: Option<&FnPointerTarget>,
    signature: &oomir::Signature,
    interface_name: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> String {
    let descriptor = signature.to_jvm_descriptor_with_explicit_params();
    let target_name = target_function
        .map(FnPointerTarget::display_name)
        .unwrap_or_else(|| "unsupported".to_string());
    let identity = format!("{target_name}:{descriptor}");
    let base_name = jvm_names::path_segment(&target_name);
    let interface_token = interface_name.rsplit('/').next().unwrap_or(interface_name);
    let local_name = crate::stable_hash::readable_or_hashed_name(
        "FnPtrImpl",
        &format!("{base_name}_{interface_token}"),
        &identity,
        180,
    );
    let class_name = jvm_names::synthetic_class_for_instance(tcx, instance, local_name);

    let mut method_params = Vec::with_capacity(signature.params.len() + 1);
    method_params.push(("self".to_string(), oomir::Type::Class(class_name.clone())));
    method_params.extend(signature.params.iter().cloned());

    let instructions = if let Some(target_function) = target_function {
        let call_dest = if !signature.ret.has_jvm_value() {
            None
        } else {
            Some("_ret".to_string())
        };
        let mut call_args: Vec<_> = signature
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, ty))| oomir::Operand::Variable {
                name: format!("_{}", i + 2),
                ty: ty.clone(),
            })
            .collect();

        let mut prefix = Vec::new();
        let call = match target_function {
            FnPointerTarget::Static(target) => oomir::Instruction::InvokeRustStatic {
                dest: call_dest.clone(),
                class_name: target
                    .class_to_call_on
                    .clone()
                    .expect("function pointer targets have JVM owners"),
                method_name: target.method_name.clone(),
                method_ty: signature.clone(),
                args: call_args,
            },
            FnPointerTarget::ImportedStatic(target) => oomir::Instruction::InvokeStatic {
                dest: call_dest.clone(),
                class_name: target
                    .class_to_call_on
                    .clone()
                    .expect("function pointer targets have JVM owners"),
                method_name: target.method_name.clone(),
                method_ty: signature.clone(),
                args: call_args,
            },
            FnPointerTarget::ImportedField {
                class_name,
                field_name,
                access,
            } => {
                let receiver = call_args.remove(0);
                match access {
                    crate::lower1::naming::JvmFieldAccess::Getter { field_ty } => {
                        oomir::Instruction::GetJvmField {
                            dest: call_dest
                                .clone()
                                .expect("a field getter function pointer returns a value"),
                            object: receiver,
                            field_name: field_name.clone(),
                            field_ty: field_ty.clone(),
                            class_name: class_name.clone(),
                        }
                    }
                    crate::lower1::naming::JvmFieldAccess::Setter { field_ty } => {
                        oomir::Instruction::SetJvmField {
                            object: receiver,
                            field_name: field_name.clone(),
                            value: call_args.remove(0),
                            field_ty: field_ty.clone(),
                            class_name: class_name.clone(),
                        }
                    }
                }
            }
            FnPointerTarget::ImportedStaticField {
                class_name,
                field_name,
                access,
            } => match access {
                crate::lower1::naming::JvmFieldAccess::Getter { field_ty } => {
                    oomir::Instruction::GetStaticField {
                        dest: call_dest
                            .clone()
                            .expect("a static field getter function pointer returns a value"),
                        class_name: class_name.clone(),
                        field_name: field_name.clone(),
                        field_ty: field_ty.clone(),
                    }
                }
                crate::lower1::naming::JvmFieldAccess::Setter { field_ty } => {
                    oomir::Instruction::SetStaticField {
                        class_name: class_name.clone(),
                        field_name: field_name.clone(),
                        value: call_args.remove(0),
                        field_ty: field_ty.clone(),
                    }
                }
            },
            FnPointerTarget::ImportedConstructor { class_name } => {
                oomir::Instruction::ConstructObject {
                    dest: call_dest
                        .clone()
                        .expect("a constructor function pointer returns an object"),
                    class_name: class_name.clone(),
                    args: call_args
                        .into_iter()
                        .zip(signature.params.iter().map(|(_, ty)| ty.clone()))
                        .collect(),
                }
            }
            FnPointerTarget::Virtual {
                class_name,
                method_name,
            } => {
                let receiver = call_args.remove(0);
                let mut method_ty = signature.clone();
                method_ty.is_static = false;
                oomir::Instruction::InvokeVirtual {
                    dest: call_dest.clone(),
                    class_name: class_name.clone(),
                    method_name: method_name.clone(),
                    method_ty,
                    args: call_args,
                    operand: receiver,
                }
            }
            FnPointerTarget::Interface {
                class_name,
                method_name,
            } => {
                let receiver = call_args.remove(0);
                let mut method_ty = signature.clone();
                method_ty.is_static = false;
                oomir::Instruction::InvokeInterface {
                    dest: call_dest.clone(),
                    class_name: class_name.clone(),
                    method_name: method_name.clone(),
                    method_ty,
                    args: call_args,
                    operand: receiver,
                }
            }
            FnPointerTarget::InterfacePointer {
                class_name,
                method_name,
            } => {
                let receiver_pointer = call_args.remove(0);
                let receiver_pointer_ty = receiver_pointer
                    .get_type()
                    .expect("trait-object function pointer receiver is typed");
                let receiver_object = "_trait_object_receiver_object".to_string();
                prefix.push(oomir::Instruction::InvokeVirtual {
                    dest: Some(receiver_object.clone()),
                    class_name: oomir::POINTER_CLASS.to_string(),
                    method_name: "getObject".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), receiver_pointer_ty)],
                        ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                        is_static: false,
                    },
                    args: Vec::new(),
                    operand: receiver_pointer,
                });
                let receiver = "_trait_object_receiver".to_string();
                prefix.push(oomir::Instruction::Cast {
                    dest: receiver.clone(),
                    op: oomir::Operand::Variable {
                        name: receiver_object,
                        ty: oomir::Type::Class("java/lang/Object".to_string()),
                    },
                    ty: oomir::Type::Interface(class_name.clone()),
                });
                let mut method_ty = signature.clone();
                method_ty.is_static = false;
                oomir::Instruction::InvokeInterface {
                    dest: call_dest.clone(),
                    class_name: class_name.clone(),
                    method_name: method_name.clone(),
                    method_ty,
                    args: call_args,
                    operand: oomir::Operand::Variable {
                        name: receiver,
                        ty: oomir::Type::Interface(class_name.clone()),
                    },
                }
            }
        };
        prefix.push(call);
        let mut instructions = prefix;

        instructions.push(oomir::Instruction::Return {
            operand: call_dest.map(|name| oomir::Operand::Variable {
                name,
                ty: signature.ret.as_ref().clone(),
            }),
        });
        instructions
    } else {
        vec![oomir::Instruction::ThrowNewWithMessage {
            exception_class: "java/lang/UnsupportedOperationException".to_string(),
            message: format!("Unsupported non-local function pointer: {target_name}"),
        }]
    };

    let call_method = oomir::DataTypeMethod::Function(oomir::Function {
        name: "call".to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: method_params,
            ret: signature.ret.clone(),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "bb0".to_string(),
            basic_blocks: HashMap::from_iter([(
                "bb0".to_string(),
                oomir::BasicBlock {
                    label: "bb0".to_string(),
                    instructions,
                },
            )]),
        }
        .into(),
    });
    let relative_method_name = format!("call{}", oomir::RELATIVE_POINTER_METHOD_SUFFIX);
    let relative_call_method = signature.supports_relative_pointer_abi().then(|| {
        let oomir::DataTypeMethod::Function(mut function) = call_method.clone() else {
            unreachable!("function-pointer adapters are OOMIR functions");
        };
        function.name = relative_method_name.clone();
        oomir::DataTypeMethod::Function(function)
    });

    match data_types.get_mut(&class_name) {
        Some(oomir::DataType::Class {
            methods,
            interfaces,
            ..
        }) => {
            methods.entry("call".to_string()).or_insert(call_method);
            if let Some(relative_call_method) = relative_call_method {
                methods
                    .entry(relative_method_name)
                    .or_insert(relative_call_method);
            }
            if !interfaces
                .iter()
                .any(|interface| interface == interface_name)
            {
                interfaces.push(interface_name.to_string());
            }
        }
        Some(oomir::DataType::Interface { .. }) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "mir-lowering",
                format!(
                    "Function pointer adapter name '{}' already exists as an interface",
                    class_name
                )
            );
        }
        None => {
            let mut methods = HashMap::from_iter([("call".to_string(), call_method)]);
            if let Some(relative_call_method) = relative_call_method {
                methods.insert(relative_method_name, relative_call_method);
            }
            data_types.insert(
                class_name.clone(),
                oomir::DataType::Class {
                    fields: vec![],
                    is_abstract: false,
                    methods,
                    super_class: Some("java/lang/Object".to_string()),
                    interfaces: vec![interface_name.to_string()],
                },
            );
        }
    }

    class_name
}
