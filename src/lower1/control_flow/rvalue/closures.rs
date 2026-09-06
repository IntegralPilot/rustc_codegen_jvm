use super::*;

pub(crate) fn ensure_closure_fn_pointer_adapter_class<'tcx>(
    data_types: &mut Definitions<'tcx>,
    closure_instance: Instance<'tcx>,
    signature: &oomir::Signature,
    interface_name: &str,
    tcx: TyCtxt<'tcx>,
    instance_context: Instance<'tcx>,
) -> String {
    let typing_env = TypingEnv::fully_monomorphized();
    let closure_ty = closure_instance.ty(tcx, typing_env);
    let TyKind::Closure(_, closure_args) = closure_ty.kind() else {
        panic!(
            "closure function-pointer adapter received non-closure instance {:?}",
            closure_instance
        );
    };
    assert!(
        closure_args.as_closure().upvar_tys().is_empty(),
        "only non-capturing closures can be coerced to function pointers"
    );

    let closure_sig = closure_args.as_closure().sig();
    let closure_inputs = closure_sig.inputs().skip_binder();
    let tuple_ty = *closure_inputs
        .first()
        .expect("closure call ABI always has a tuple argument");
    let tuple_oomir_ty = ty_to_oomir_type(tuple_ty, tcx, data_types, instance_context);
    let target_owner = crate::lower1::naming::mono_owner_class(tcx, closure_instance);
    let target_method = data_types.closure_method_name(tcx, closure_instance);
    let descriptor = signature.to_jvm_descriptor_with_explicit_params();
    let target_name = format!("{target_owner}::{target_method}");
    let identity = format!("{target_name}:{descriptor}");
    let base_name = jvm_names::path_segment(&target_method);
    let interface_token = interface_name.rsplit('/').next().unwrap_or(interface_name);
    let local_name = crate::stable_hash::readable_or_hashed_name(
        "ClosureFnPtrImpl",
        &format!("{base_name}_{interface_token}"),
        &identity,
        180,
    );
    let class_name = jvm_names::synthetic_class_for_instance(tcx, instance_context, local_name);

    let mut method_params = Vec::with_capacity(signature.params.len() + 1);
    method_params.push(("self".to_string(), oomir::Type::Class(class_name.clone())));
    method_params.extend(signature.params.iter().cloned());

    let tuple_dest = "_closure_args".to_string();
    let tuple_args = signature
        .params
        .iter()
        .enumerate()
        .filter(|(_, (_, ty))| ty.has_jvm_value())
        .map(|(index, (_, ty))| {
            (
                oomir::Operand::Variable {
                    name: format!("_{}", index + 2),
                    ty: ty.clone(),
                },
                ty.clone(),
            )
        })
        .collect::<Vec<_>>();
    let mut instructions = Vec::new();
    let closure_call_args = if tuple_oomir_ty.has_jvm_value() {
        let tuple_class = tuple_oomir_ty
            .get_class_name()
            .expect("non-unit closure argument tuple is represented by a JVM class")
            .to_string();
        instructions.push(oomir::Instruction::ConstructObject {
            dest: tuple_dest.clone(),
            class_name: tuple_class,
            args: tuple_args,
        });
        vec![oomir::Operand::Variable {
            name: tuple_dest,
            ty: tuple_oomir_ty.clone(),
        }]
    } else {
        Vec::new()
    };

    let closure_method_params = tuple_oomir_ty
        .has_jvm_value()
        .then(|| ("args".to_string(), tuple_oomir_ty))
        .into_iter()
        .collect();
    let call_dest = signature.ret.has_jvm_value().then(|| "_ret".to_string());
    instructions.push(oomir::Instruction::InvokeRustStatic {
        dest: call_dest.clone(),
        class_name: target_owner,
        method_name: target_method,
        method_ty: oomir::Signature {
            params: closure_method_params,
            ret: signature.ret.clone(),
            is_static: true,
        },
        args: closure_call_args,
    });
    instructions.push(oomir::Instruction::Return {
        operand: call_dest.map(|name| oomir::Operand::Variable {
            name,
            ty: signature.ret.as_ref().clone(),
        }),
    });

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
            unreachable!("closure function-pointer adapters are OOMIR functions");
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
            if !interfaces.iter().any(|name| name == interface_name) {
                interfaces.push(interface_name.to_string());
            }
        }
        Some(oomir::DataType::Interface { .. }) => {
            panic!("closure function-pointer adapter name collided with an interface")
        }
        None => {
            let mut methods = HashMap::from_iter([("call".to_string(), call_method)]);
            if let Some(relative_call_method) = relative_call_method {
                methods.insert(relative_method_name, relative_call_method);
            }
            data_types.insert(
                class_name.clone(),
                oomir::DataType::Class {
                    fields: Vec::new(),
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

pub(super) fn ensure_non_capturing_closure_fn_pointer_bridge<'tcx>(
    data_types: &mut Definitions<'tcx>,
    external_interfaces: &mut HashSet<String>,
    closure_instance: Instance<'tcx>,
    signature: &oomir::Signature,
    tcx: TyCtxt<'tcx>,
    instance_context: Instance<'tcx>,
) -> (String, String) {
    let closure_ty = closure_instance.ty(tcx, TypingEnv::fully_monomorphized());
    let TyKind::Closure(_, closure_args) = closure_ty.kind() else {
        panic!("function-pointer bridge received non-closure {closure_instance:?}");
    };
    assert!(
        closure_args.as_closure().upvar_tys().is_empty(),
        "only non-capturing closures can become function pointers"
    );
    let oomir::Type::Class(closure_class) =
        ty_to_oomir_type(closure_ty, tcx, data_types, instance_context)
    else {
        panic!("closure type did not lower to a JVM class");
    };
    let method_name = "_fn_ptr_call".to_string();
    let already_defined = matches!(
        data_types.get(&closure_class),
        Some(oomir::DataType::Class { methods, .. }) if methods.contains_key(&method_name)
    );
    if already_defined {
        return (closure_class, method_name);
    }

    // Optimised MIR can remove the standalone closure mono item when its only
    // use is this coercion. Keep the implementation alongside the bridge in
    // the closure's existing class so the LambdaMetafactory target never
    // depends on that separate reachability decision.
    let implementation_method_name = "_fn_ptr_impl".to_string();
    let implementation_already_defined = matches!(
        data_types.get(&closure_class),
        Some(oomir::DataType::Class { methods, .. })
            if methods.contains_key(&implementation_method_name)
    );
    if !implementation_already_defined {
        let closure_mir = tcx.instance_mir(closure_instance.def);
        let implementation = crate::lower1::mir_to_oomir(
            tcx,
            closure_instance,
            closure_mir,
            Some(crate::lower1::naming::FnNameData {
                class_to_call_on: Some(closure_class.clone()),
                method_name: implementation_method_name.clone(),
            }),
            true,
            data_types,
            external_interfaces,
        );
        let Some(oomir::DataType::Class { methods, .. }) = data_types.get_mut(&closure_class)
        else {
            panic!("closure class disappeared while adding its function-pointer implementation");
        };
        methods.insert(
            implementation_method_name.clone(),
            DataTypeMethod::Function(implementation),
        );
    }

    let closure_sig = closure_args.as_closure().sig();
    let tuple_ty = *closure_sig
        .inputs()
        .skip_binder()
        .first()
        .expect("closure call ABI always has a tuple argument");
    let tuple_oomir_ty = ty_to_oomir_type(tuple_ty, tcx, data_types, instance_context);
    let mut instructions = Vec::new();
    let closure_args = if tuple_oomir_ty.has_jvm_value() {
        let tuple_dest = "_closure_args".to_string();
        instructions.push(oomir::Instruction::ConstructObject {
            dest: tuple_dest.clone(),
            class_name: tuple_oomir_ty
                .get_class_name()
                .expect("non-unit closure tuple is represented by a JVM class")
                .to_string(),
            args: signature
                .params
                .iter()
                .enumerate()
                .map(|(index, (_, ty))| {
                    (
                        oomir::Operand::Variable {
                            name: format!("_{}", index + 1),
                            ty: ty.clone(),
                        },
                        ty.clone(),
                    )
                })
                .collect(),
        });
        vec![oomir::Operand::Variable {
            name: tuple_dest,
            ty: tuple_oomir_ty.clone(),
        }]
    } else {
        Vec::new()
    };
    let call_dest = signature.ret.has_jvm_value().then(|| "_ret".to_string());
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: call_dest.clone(),
        class_name: closure_class.clone(),
        method_name: implementation_method_name,
        method_ty: oomir::Signature {
            params: tuple_oomir_ty
                .has_jvm_value()
                .then(|| ("args".to_string(), tuple_oomir_ty))
                .into_iter()
                .collect(),
            ret: signature.ret.clone(),
            is_static: true,
        },
        args: closure_args,
    });
    instructions.push(oomir::Instruction::Return {
        operand: call_dest.map(|name| oomir::Operand::Variable {
            name,
            ty: signature.ret.as_ref().clone(),
        }),
    });
    let bridge = DataTypeMethod::Function(oomir::Function {
        name: method_name.clone(),
        owner_class: None,
        signature: signature.clone(),
        debug_variables: Vec::new(),
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
    let Some(oomir::DataType::Class { methods, .. }) = data_types.get_mut(&closure_class) else {
        panic!("closure class disappeared while adding its function-pointer bridge");
    };
    methods.insert(method_name.clone(), bridge);
    (closure_class, method_name)
}

/// Makes a concrete Rust closure directly implement the primitive-specialized
/// JVM SAM used by `dyn Fn*`.  The bridge rebuilds Rust's tuple call argument
/// and, for a capturing closure, supplies the address-like environment carrier
/// expected by the lowered closure body.
pub(super) fn ensure_closure_callable_bridge<'tcx>(
    closure_ty: rustc_middle::ty::Ty<'tcx>,
    callable_abi: &crate::lower1::types::CallableTraitObjectAbi<'tcx>,
    data_types: &mut Definitions<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance_context: Instance<'tcx>,
) -> bool {
    let instantiated = EarlyBinder::bind(tcx, closure_ty)
        .instantiate(tcx, instance_context.args)
        .skip_norm_wip();
    let closure_ty = tcx
        .try_normalize_erasing_regions(
            TypingEnv::fully_monomorphized(),
            rustc_middle::ty::Unnormalized::new_wip(instantiated),
        )
        .unwrap_or(instantiated);
    let closure_ty = match closure_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        _ => closure_ty,
    };
    let TyKind::Closure(def_id, closure_args) = closure_ty.kind() else {
        return false;
    };
    let closure_instance = Instance::new_raw(*def_id, closure_args);
    let oomir::Type::Class(closure_class) =
        ty_to_oomir_type(closure_ty, tcx, data_types, instance_context)
    else {
        return false;
    };

    let tuple_oomir_ty = ty_to_oomir_type(callable_abi.tuple_ty, tcx, data_types, instance_context);
    let mut instructions = Vec::new();
    let tuple_dest = "_closure_args".to_string();
    let tuple_call_arg = if tuple_oomir_ty.has_jvm_value() {
        let tuple_class = tuple_oomir_ty
            .get_class_name()
            .expect("non-unit closure argument tuple is represented by a JVM class")
            .to_string();
        let tuple_args = callable_abi
            .signature
            .params
            .iter()
            .enumerate()
            .map(|(index, (_, ty))| {
                (
                    oomir::Operand::Variable {
                        name: format!("_{}", index + 2),
                        ty: ty.clone(),
                    },
                    ty.clone(),
                )
            })
            .collect();
        instructions.push(oomir::Instruction::ConstructObject {
            dest: tuple_dest.clone(),
            class_name: tuple_class,
            args: tuple_args,
        });
        Some(oomir::Operand::Variable {
            name: tuple_dest,
            ty: tuple_oomir_ty.clone(),
        })
    } else {
        None
    };

    let has_captures = closure_args
        .as_closure()
        .upvar_tys()
        .iter()
        .next()
        .is_some();
    let mut closure_params = Vec::new();
    let mut closure_call_args = Vec::new();
    if has_captures {
        let closure_mir = tcx.instance_mir(closure_instance.def);
        let environment_rust_ty = EarlyBinder::bind(
            tcx,
            closure_mir.local_decls[rustc_middle::mir::Local::from_usize(1)].ty,
        )
        .instantiate(tcx, closure_instance.args)
        .skip_norm_wip();
        let environment_ty =
            ty_to_oomir_type(environment_rust_ty, tcx, data_types, instance_context);
        closure_params.push(("closure_env".to_string(), environment_ty.clone()));
        let receiver = oomir::Operand::Variable {
            name: "_1".to_string(),
            ty: oomir::Type::Class(closure_class.clone()),
        };
        if matches!(environment_ty, oomir::Type::Pointer(_)) {
            let environment_name = "_closure_env".to_string();
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(environment_name.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "cell".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        (
                            "value".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("size".to_string(), oomir::Type::U64),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(environment_ty.clone()),
                    is_static: true,
                },
                args: vec![
                    receiver,
                    oomir::Operand::Constant(oomir::Constant::U64(
                        u64::try_from(
                            crate::lower1::types::layout_size_bytes(tcx, closure_ty)
                                .expect("closure layout is available"),
                        )
                        .expect("closure layout exceeds u64"),
                    )),
                    crate::lower1::types::pointer_view_codec_operand(
                        closure_ty,
                        tcx,
                        data_types,
                        instance_context,
                    ),
                ],
            });
            closure_call_args.push(oomir::Operand::Variable {
                name: environment_name,
                ty: environment_ty,
            });
        } else {
            closure_call_args.push(receiver);
        }
    }
    if let Some(tuple_call_arg) = tuple_call_arg {
        closure_params.push(("args".to_string(), tuple_oomir_ty));
        closure_call_args.push(tuple_call_arg);
    }

    let call_dest = callable_abi
        .signature
        .ret
        .has_jvm_value()
        .then(|| "_ret".to_string());
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: call_dest.clone(),
        class_name: crate::lower1::naming::mono_owner_class(tcx, closure_instance),
        method_name: data_types.closure_method_name(tcx, closure_instance),
        method_ty: oomir::Signature {
            params: closure_params,
            ret: callable_abi.signature.ret.clone(),
            is_static: true,
        },
        args: closure_call_args,
    });
    instructions.push(oomir::Instruction::Return {
        operand: call_dest.map(|name| oomir::Operand::Variable {
            name,
            ty: callable_abi.signature.ret.as_ref().clone(),
        }),
    });

    let mut method_params = vec![(
        "self".to_string(),
        oomir::Type::Class(closure_class.clone()),
    )];
    method_params.extend(callable_abi.signature.params.iter().cloned());
    let call_method = DataTypeMethod::Function(oomir::Function {
        name: "call".to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: method_params,
            ret: callable_abi.signature.ret.clone(),
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

    let Some(oomir::DataType::Class {
        methods,
        interfaces,
        ..
    }) = data_types.get_mut(&closure_class)
    else {
        return false;
    };
    methods.entry("call".to_string()).or_insert(call_method);
    if !interfaces
        .iter()
        .any(|name| name == &callable_abi.interface_name)
    {
        interfaces.push(callable_abi.interface_name.clone());
    }
    true
}

pub(super) fn closure_callable_abi<'tcx>(
    closure_ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Option<crate::lower1::types::CallableTraitObjectAbi<'tcx>> {
    let closure_ty = normalize_unsize_ty(closure_ty, tcx, instance);
    let TyKind::Closure(_, closure_args) = closure_ty.kind() else {
        return None;
    };
    let closure_signature = closure_args.as_closure().sig();
    let tuple_ty = *closure_signature.inputs().skip_binder().first()?;
    let TyKind::Tuple(tuple_elements) = tuple_ty.kind() else {
        return None;
    };
    let params = tuple_elements
        .iter()
        .enumerate()
        .filter_map(|(index, element_ty)| {
            let ty = ty_to_oomir_type(element_ty, tcx, data_types, instance);
            ty.has_jvm_value().then(|| (format!("arg{index}"), ty))
        })
        .collect();
    let output_ty = closure_signature.output().skip_binder();
    let signature = oomir::Signature {
        params,
        ret: Box::new(ty_to_oomir_type(output_ty, tcx, data_types, instance)),
        is_static: true,
    };
    let interface_name = ensure_fn_ptr_interface(&signature, data_types, tcx, instance);
    Some(crate::lower1::types::CallableTraitObjectAbi {
        tuple_ty,
        signature,
        interface_name,
    })
}

pub(super) fn ensure_erased_receiver_fn_pointer_bridge<'tcx>(
    data_types: &mut Definitions<'tcx>,
    source_signature: &oomir::Signature,
    source_interface: &str,
    target_signature: &oomir::Signature,
    target_interface: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Result<String, String> {
    if source_signature.params.len() != target_signature.params.len() {
        return Err("source and target arities differ".to_string());
    }
    if source_signature.ret.to_jvm_return_descriptor()
        != target_signature.ret.to_jvm_return_descriptor()
    {
        return Err("source and target return ABIs differ".to_string());
    }

    let Some((_, source_receiver_ty)) = source_signature.params.first() else {
        return Err("function has no receiver parameter to erase".to_string());
    };
    let Some((_, target_receiver_ty)) = target_signature.params.first() else {
        return Err("function has no receiver carrier parameter".to_string());
    };
    if source_receiver_ty == target_receiver_ty {
        return Err("function receiver representation is unchanged".to_string());
    }
    if !source_receiver_ty.has_jvm_value() {
        return Err("zero-sized erased receivers need no bridge".to_string());
    }

    for ((_, source_ty), (_, target_ty)) in source_signature
        .params
        .iter()
        .skip(1)
        .zip(target_signature.params.iter().skip(1))
    {
        if !source_ty.same_jvm_type(target_ty) {
            return Err("a non-receiver parameter changes JVM ABI".to_string());
        }
    }

    let direct_pointer_carrier = matches!(target_receiver_ty, oomir::Type::Pointer(_));
    let (carrier_class, carrier_field_name, carrier_field_ty) =
        if let oomir::Type::Class(carrier_class) = target_receiver_ty {
            if !oomir::is_non_null_class_name(carrier_class) {
                return Err("target receiver is not a NonNull carrier".to_string());
            }
            let Some(oomir::DataType::Class { fields, .. }) = data_types.get(carrier_class) else {
                return Err(format!(
                    "receiver carrier class {carrier_class} is undefined"
                ));
            };
            let Some((field_name, field_ty)) = fields.first().cloned() else {
                return Err(format!(
                    "receiver carrier class {carrier_class} has no payload"
                ));
            };
            (Some(carrier_class.clone()), Some(field_name), field_ty)
        } else if direct_pointer_carrier {
            (None, None, target_receiver_ty.clone())
        } else {
            return Err("target receiver is not a NonNull carrier".to_string());
        };
    let erased_pointer_to_slice = matches!(carrier_field_ty, oomir::Type::Pointer(_))
        && matches!(source_receiver_ty, oomir::Type::Slice(_) | oomir::Type::Str);
    if carrier_field_ty.to_jvm_descriptor() != "Ljava/lang/Object;"
        && !carrier_field_ty.same_jvm_type(source_receiver_ty)
        && !erased_pointer_to_slice
    {
        return Err(format!(
            "receiver carrier payload has unexpected JVM type {}",
            carrier_field_ty.to_jvm_descriptor()
        ));
    }
    let erased_payload_ty = carrier_field_ty;

    let source_descriptor = source_signature.to_jvm_descriptor_with_explicit_params();
    let target_descriptor = target_signature.to_jvm_descriptor_with_explicit_params();
    let identity = format!(
        "{source_signature:?}:{source_descriptor}->{target_signature:?}:{target_descriptor}"
    );
    let local_name = crate::stable_hash::readable_or_hashed_name(
        "FnPtrErasedReceiverBridge",
        &format!(
            "{}_to_{}",
            source_signature.fn_ptr_interface_name(),
            target_signature.fn_ptr_interface_name()
        ),
        &identity,
        180,
    );
    let class_name = jvm_names::synthetic_class_for_instance(tcx, instance, local_name);
    if data_types.contains_key(&class_name) {
        return Ok(class_name);
    }

    let self_operand = oomir::Operand::Variable {
        name: "_1".to_string(),
        ty: oomir::Type::Class(class_name.clone()),
    };
    let erased_receiver = oomir::Operand::Variable {
        name: "_2".to_string(),
        ty: target_receiver_ty.clone(),
    };
    let source_pointer_name = "_source_function".to_string();
    let erased_payload_name = "_erased_receiver_payload".to_string();
    let typed_receiver_name = "_typed_receiver".to_string();

    let mut instructions = vec![oomir::Instruction::GetField {
        dest: source_pointer_name.clone(),
        object: self_operand,
        field_name: "function".to_string(),
        field_ty: oomir::Type::Interface(source_interface.to_string()),
        owner_class: class_name.clone(),
    }];
    let erased_payload = if let (Some(carrier_class), Some(carrier_field_name)) =
        (carrier_class, carrier_field_name)
    {
        instructions.push(oomir::Instruction::GetField {
            dest: erased_payload_name.clone(),
            object: erased_receiver,
            field_name: carrier_field_name,
            field_ty: erased_payload_ty.clone(),
            owner_class: carrier_class,
        });
        oomir::Operand::Variable {
            name: erased_payload_name,
            ty: erased_payload_ty.clone(),
        }
    } else {
        erased_receiver
    };
    if erased_pointer_to_slice {
        let restored_object_name = "_restored_slice_object".to_string();
        let view_class_name = match source_receiver_ty {
            oomir::Type::Slice(_) => oomir::SLICE_VIEW_CLASS,
            oomir::Type::Str => oomir::UTF8_VIEW_CLASS,
            _ => unreachable!("erased pointer-to-slice bridge has a slice-like receiver"),
        };
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(restored_object_name.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "restoreErasedSliceView".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), erased_payload_ty.clone()),
                    ("view_class".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                is_static: true,
            },
            args: vec![
                erased_payload,
                oomir::Operand::Constant(oomir::Constant::String(view_class_name.to_string())),
            ],
        });
        instructions.push(oomir::Instruction::Cast {
            op: oomir::Operand::Variable {
                name: restored_object_name,
                ty: oomir::Type::Class("java/lang/Object".to_string()),
            },
            ty: source_receiver_ty.clone(),
            dest: typed_receiver_name.clone(),
        });
    } else if matches!(source_receiver_ty, oomir::Type::Pointer(_))
        && matches!(erased_payload_ty, oomir::Type::Pointer(_))
    {
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(typed_receiver_name.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "restoreErasedView".to_string(),
            method_ty: oomir::Signature {
                params: vec![("pointer".to_string(), erased_payload_ty)],
                ret: Box::new(source_receiver_ty.clone()),
                is_static: true,
            },
            args: vec![erased_payload],
        });
    } else {
        instructions.push(oomir::Instruction::Cast {
            op: erased_payload,
            ty: source_receiver_ty.clone(),
            dest: typed_receiver_name.clone(),
        });
    }

    let mut call_args = vec![oomir::Operand::Variable {
        name: typed_receiver_name,
        ty: source_receiver_ty.clone(),
    }];
    call_args.extend(
        target_signature
            .params
            .iter()
            .enumerate()
            .skip(1)
            .map(|(index, (_, ty))| oomir::Operand::Variable {
                name: format!("_{}", index + 2),
                ty: ty.clone(),
            }),
    );

    let call_dest = source_signature
        .ret
        .has_jvm_value()
        .then(|| "_ret".to_string());
    instructions.push(oomir::Instruction::CallIndirect {
        dest: call_dest.clone(),
        function_ptr: Box::new(oomir::Operand::Variable {
            name: source_pointer_name,
            ty: oomir::Type::Interface(source_interface.to_string()),
        }),
        args: call_args,
        signature: source_signature.clone(),
    });
    instructions.push(oomir::Instruction::Return {
        operand: call_dest.map(|name| oomir::Operand::Variable {
            name,
            ty: source_signature.ret.as_ref().clone(),
        }),
    });

    let mut method_params = Vec::with_capacity(target_signature.params.len() + 1);
    method_params.push(("self".to_string(), oomir::Type::Class(class_name.clone())));
    method_params.extend(target_signature.params.iter().cloned());
    let call_method = DataTypeMethod::Function(oomir::Function {
        name: "call".to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: method_params,
            ret: target_signature.ret.clone(),
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

    let identity = DataTypeMethod::Function(oomir::Function {
        name: "functionPointerTarget".into(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![("self".into(), oomir::Type::Class(class_name.clone()))],
            ret: Box::new(oomir::Type::Class("java/lang/Object".into())),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "bb0".into(),
            basic_blocks: HashMap::from_iter([(
                "bb0".into(),
                oomir::BasicBlock {
                    label: "bb0".into(),
                    instructions: vec![
                        oomir::Instruction::GetField {
                            dest: "target".into(),
                            object: oomir::Operand::Variable {
                                name: "_1".into(),
                                ty: oomir::Type::Class(class_name.clone()),
                            },
                            field_name: "function".into(),
                            field_ty: oomir::Type::Interface(source_interface.into()),
                            owner_class: class_name.clone(),
                        },
                        oomir::Instruction::Return {
                            operand: Some(oomir::Operand::Variable {
                                name: "target".into(),
                                ty: oomir::Type::Interface(source_interface.into()),
                            }),
                        },
                    ],
                },
            )]),
        },
    });
    data_types.insert(
        class_name.clone(),
        oomir::DataType::Class {
            fields: vec![(
                "function".into(),
                oomir::Type::Interface(source_interface.into()),
            )],
            is_abstract: false,
            methods: HashMap::from_iter([
                ("call".into(), call_method),
                ("functionPointerTarget".into(), identity),
            ]),
            super_class: Some("java/lang/Object".into()),
            interfaces: vec![
                target_interface.into(),
                "org/rustlang/runtime/FunctionPointerAdapter".into(),
            ],
        },
    );
    Ok(class_name)
}
