use rustc_abi::FieldIdx;
use rustc_middle::{
    mir::{
        BinOp, Body, BorrowKind as MirBorrowKind, CastKind, Operand as MirOperand, Place, Rvalue,
        UnOp,
    },
    ty::{ConstKind, Instance, TyCtxt, TyKind, TypingEnv, adjustment::PointerCoercion},
};
use std::collections::HashMap;

use super::{
    super::{
        operand::{
            convert_operand, extract_number_from_operand, get_placeholder_operand,
            handle_const_value,
        },
        place::{emit_instructions_to_get_on_own, get_place_type, make_jvm_safe, place_to_string},
        types::{
            ensure_fn_ptr_interface, fn_ptr_signature_from_ty, generate_adt_jvm_class_name,
            short_hash, ty_to_oomir_type,
        },
    },
    checked_ops::emit_checked_arithmetic_oomir_instructions,
    oomir::{self, DataTypeMethod},
};

use std::sync::atomic::{AtomicUsize, Ordering}; // For unique temp names

// Global or struct-based counter for temporary variables
static TEMP_VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn generate_temp_var_name(base_name: &str) -> String {
    let count = TEMP_VAR_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}_tmp{}", make_jvm_safe(base_name), count)
}

fn adapt_value_for_field(
    value_operand: oomir::Operand,
    field_ty: &oomir::Type,
    temp_base_name: &str,
    data_types: &HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let Some(value_ty) = value_operand.get_type() else {
        return value_operand;
    };

    if let oomir::Type::Class(class_name) = field_ty
        && !value_ty.is_jvm_reference_type()
    {
        let temp_name = generate_temp_var_name(temp_base_name);
        if class_name == "java/lang/Object" {
            instructions.push(oomir::Instruction::Cast {
                op: value_operand,
                ty: field_ty.clone(),
                dest: temp_name.clone(),
            });
            return oomir::Operand::Variable {
                name: temp_name,
                ty: field_ty.clone(),
            };
        }

        let is_marker_class = class_name.starts_with("PhantomData_")
            || matches!(
                data_types.get(class_name),
                Some(oomir::DataType::Class { fields, .. }) if fields.is_empty()
            );

        if is_marker_class {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: temp_name.clone(),
                class_name: class_name.clone(),
                args: Vec::new(),
            });
            return oomir::Operand::Variable {
                name: temp_name,
                ty: field_ty.clone(),
            };
        }
    }

    value_operand
}

fn ensure_fn_pointer_adapter_class(
    data_types: &mut HashMap<String, oomir::DataType>,
    target_function: Option<&str>,
    signature: &oomir::Signature,
    interface_name: &str,
) -> String {
    let descriptor = signature.to_jvm_descriptor_with_explicit_params();
    let target_name = target_function.unwrap_or("unsupported");
    let hash = short_hash(&format!("{target_name}:{descriptor}"), 10);
    let base_name: String = make_jvm_safe(target_name).chars().take(80).collect();
    let class_name = format!("FnPtrImpl_{}_{}", base_name, hash);

    let mut method_params = Vec::with_capacity(signature.params.len() + 1);
    method_params.push(("self".to_string(), oomir::Type::Class(class_name.clone())));
    method_params.extend(signature.params.iter().cloned());

    let instructions = if let Some(target_function) = target_function {
        let call_dest = if *signature.ret == oomir::Type::Void {
            None
        } else {
            Some("_ret".to_string())
        };
        let call_args = signature
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, ty))| oomir::Operand::Variable {
                name: format!("_{}", i + 2),
                ty: ty.clone(),
            })
            .collect();

        let mut instructions = vec![oomir::Instruction::Call {
            dest: call_dest.clone(),
            function: target_function.to_string(),
            args: call_args,
        }];

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
        signature: oomir::Signature {
            params: method_params,
            ret: signature.ret.clone(),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "bb0".to_string(),
            basic_blocks: HashMap::from([(
                "bb0".to_string(),
                oomir::BasicBlock {
                    label: "bb0".to_string(),
                    instructions,
                },
            )]),
        },
    });

    match data_types.get_mut(&class_name) {
        Some(oomir::DataType::Class {
            methods,
            interfaces,
            ..
        }) => {
            methods.entry("call".to_string()).or_insert(call_method);
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
            data_types.insert(
                class_name.clone(),
                oomir::DataType::Class {
                    fields: vec![],
                    is_abstract: false,
                    methods: HashMap::from([("call".to_string(), call_method)]),
                    super_class: Some("java/lang/Object".to_string()),
                    interfaces: vec![interface_name.to_string()],
                },
            );
        }
    }

    class_name
}

/// Evaluates an Rvalue and returns the resulting OOMIR Operand and any
/// intermediate instructions needed to calculate it.
///
/// The `original_dest_place` is used *only* for naming temporary variables
/// to make debugging easier, not for the final assignment.
pub fn convert_rvalue_to_operand<'a>(
    rvalue: &Rvalue<'a>,
    original_dest_place: &Place<'a>, // Used for naming temps
    mir: &Body<'a>,
    tcx: TyCtxt<'a>,
    instance: Instance<'a>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> (Vec<oomir::Instruction>, oomir::Operand) {
    let mut instructions = Vec::new();
    let result_operand: oomir::Operand;
    let base_temp_name = place_to_string(original_dest_place, tcx); // For temp naming

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "mir-lowering",
        format!(
            "convert_rvalue_to_operand: rvalue={:?}, original_dest_place={:?}, dest_ty={:?}",
            rvalue,
            original_dest_place,
            original_dest_place.ty(&mir.local_decls, tcx).ty
        )
    );

    match rvalue {
        Rvalue::Use(mir_operand, _) => {
            match mir_operand {
                MirOperand::Copy(src_place) | MirOperand::Move(src_place) => {
                    // Need to get the value from the source place first
                    let (temp_var_name, get_instructions, temp_var_type) =
                        emit_instructions_to_get_on_own(src_place, tcx, instance, mir, data_types);
                    instructions.extend(get_instructions);
                    result_operand = oomir::Operand::Variable {
                        name: temp_var_name,
                        ty: temp_var_type,
                    };
                }
                MirOperand::Constant(_) => {
                    // Constant is already an operand, no extra instructions
                    result_operand = convert_operand(
                        mir_operand,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
                }
                MirOperand::RuntimeChecks(_) => {
                    todo!("RuntimeChecks operand not yet supported")
                }
            }
        }

        Rvalue::Repeat(element_op, len_const) => {
            // Create a temporary variable to hold the new array
            let temp_array_var = generate_temp_var_name(&base_temp_name);
            let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty; // Use original dest for type info

            if let rustc_middle::ty::TyKind::Array(elem_ty, _) = place_ty.kind() {
                let oomir_elem_type = ty_to_oomir_type(elem_ty.clone(), tcx, data_types, instance);
                let oomir_elem_op = convert_operand(
                    element_op,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );
                let array_size = match len_const.kind() {
                    ConstKind::Value(val) => {
                        /* ... extract size ... */
                        extract_number_from_operand(handle_const_value(
                            None,
                            tcx.valtree_to_const_val(val),
                            &val.ty,
                            tcx,
                            data_types,
                            instance,
                        ))
                        .unwrap_or(0)
                    } // Simplified extraction
                    _ => panic!("Expected constant value for array size"),
                };
                let size_operand =
                    oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));

                instructions.push(oomir::Instruction::NewArray {
                    dest: temp_array_var.clone(), // Store in temp var
                    element_type: oomir_elem_type.clone(),
                    size: size_operand,
                });

                for i in 0..array_size {
                    let index_operand = oomir::Operand::Constant(oomir::Constant::I32(i as i32));
                    instructions.push(oomir::Instruction::ArrayStore {
                        array: temp_array_var.clone(),
                        index: index_operand,
                        value: oomir_elem_op.clone(),
                    });
                }
                result_operand = oomir::Operand::Variable {
                    name: temp_array_var,
                    ty: oomir::Type::Array(Box::new(oomir_elem_type)), // Correct array type
                };
            } else {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!(
                        "Warning: Rvalue::Repeat applied on non-array type: {:?}",
                        place_ty
                    )
                );
                result_operand =
                    get_placeholder_operand(original_dest_place, mir, tcx, instance, data_types);
            }
        }
        Rvalue::Ref(_region, borrow_kind, source_place) => {
            // Check if the result type (destination place type) is a trait object reference
            let dest_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "mir-lowering",
                format!(
                    "Rvalue::Ref start: original_dest_place={:?}, dest_ty={:?}, borrow_kind={:?}, source_place={:?}, source_ty={:?}",
                    original_dest_place,
                    dest_ty,
                    borrow_kind,
                    source_place,
                    source_place.ty(&mir.local_decls, tcx).ty
                )
            );
            let is_trait_object = match dest_ty.kind() {
                rustc_middle::ty::TyKind::Ref(_, pointee_ty, _) => {
                    let is_dyn =
                        matches!(pointee_ty.kind(), rustc_middle::ty::TyKind::Dynamic(_, _));
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Rvalue::Ref dest_ty is Ref: pointee_ty={:?}, is_dyn={}, is_trait_object={}",
                            pointee_ty, is_dyn, is_dyn
                        )
                    );
                    is_dyn
                }
                _ => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Rvalue::Ref dest_ty is not Ref: {:?}, is_trait_object=false",
                            dest_ty
                        )
                    );
                    false
                }
            };

            match borrow_kind {
                MirBorrowKind::Mut { .. } if !is_trait_object => {
                    // --- MUTABLE BORROW (&mut T) -> Use Array Hack (but NOT for trait objects) ---
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Rvalue::Ref(Mut) for place '{}' -> Temp Array Var",
                            place_to_string(source_place, tcx)
                        )
                    );

                    // 1. Get the value of the place being referenced (the 'pointee').
                    let (pointee_value_var_name, pointee_get_instructions, pointee_oomir_type) =
                        emit_instructions_to_get_on_own(
                            source_place,
                            tcx,
                            instance,
                            mir,
                            data_types,
                        );
                    instructions.extend(pointee_get_instructions); // Add instructions to get the value

                    // 2. Determine the OOMIR type for the array reference itself.
                    let array_ref_oomir_type =
                        oomir::Type::MutableReference(Box::new(pointee_oomir_type.clone()));

                    // 3. Create a temporary variable name for the new array.
                    let array_ref_var_name = generate_temp_var_name(&base_temp_name);

                    // 4. Emit instruction to allocate the single-element array (new T[1]).
                    instructions.push(oomir::Instruction::NewArray {
                        dest: array_ref_var_name.clone(),
                        element_type: pointee_oomir_type.clone(),
                        size: oomir::Operand::Constant(oomir::Constant::I32(1)),
                    });

                    // 5. Emit instruction to store the pointee's value into the array's first element.
                    let pointee_value_operand = oomir::Operand::Variable {
                        name: pointee_value_var_name,
                        ty: pointee_oomir_type,
                    };
                    instructions.push(oomir::Instruction::ArrayStore {
                        array: array_ref_var_name.clone(),
                        index: oomir::Operand::Constant(oomir::Constant::I32(0)),
                        value: pointee_value_operand,
                    });

                    // 6. The result is the reference to the newly created array.
                    result_operand = oomir::Operand::Variable {
                        name: array_ref_var_name,
                        ty: array_ref_oomir_type,
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: -> Temp Array Var '{}' ({:?})",
                            result_operand.get_name().unwrap_or("<unknown>"),
                            result_operand.get_type()
                        )
                    );
                }
                MirBorrowKind::Mut { .. } | MirBorrowKind::Shared | MirBorrowKind::Fake { .. } => {
                    // Treat Fake like Shared (used for closures etc.)
                    // --- SHARED BORROW (&T) or others -> Pass Through Value ---
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Rvalue::Ref({:?}) for place '{}' -> Direct Value",
                            borrow_kind,
                            place_to_string(source_place, tcx)
                        )
                    );

                    let source_mir_ty = source_place.ty(&mir.local_decls, tcx).ty;
                    if matches!(source_mir_ty.kind(), TyKind::Closure(..)) {
                        let closure_oomir_type =
                            ty_to_oomir_type(source_mir_ty, tcx, data_types, instance);
                        if let oomir::Type::Class(class_name) = closure_oomir_type.clone() {
                            let has_captures = matches!(
                                data_types.get(&class_name),
                                Some(oomir::DataType::Class { fields, .. }) if !fields.is_empty()
                            );
                            if has_captures {
                                let (temp_var_name, get_instructions, temp_var_type) =
                                    emit_instructions_to_get_on_own(
                                        source_place,
                                        tcx,
                                        instance,
                                        mir,
                                        data_types,
                                    );
                                instructions.extend(get_instructions);
                                result_operand = oomir::Operand::Variable {
                                    name: temp_var_name,
                                    ty: temp_var_type,
                                };
                            } else {
                                let closure_var_name = generate_temp_var_name(&base_temp_name);
                                instructions.push(oomir::Instruction::ConstructObject {
                                    dest: closure_var_name.clone(),
                                    class_name,
                                    args: Vec::new(),
                                });
                                result_operand = oomir::Operand::Variable {
                                    name: closure_var_name,
                                    ty: closure_oomir_type,
                                };
                            }
                        } else {
                            result_operand = get_placeholder_operand(
                                original_dest_place,
                                mir,
                                tcx,
                                instance,
                                data_types,
                            );
                        }
                    } else {
                        // 1. Get the value/reference of the place being borrowed directly.
                        //    `emit_instructions_to_get_on_own` handles loading/accessing the value.
                        let (pointee_value_var_name, pointee_get_instructions, pointee_oomir_type) =
                            emit_instructions_to_get_on_own(
                                source_place,
                                tcx,
                                instance,
                                mir,
                                data_types,
                            );

                        // 2. Add the instructions needed to get this value.
                        instructions.extend(pointee_get_instructions);

                        // 3. The result *is* the operand representing the borrowed value itself.
                        //    No array wrapping is done.
                        result_operand = oomir::Operand::Variable {
                            name: pointee_value_var_name,
                            ty: pointee_oomir_type,
                        };
                    }
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: -> Direct Value Operand '{}' ({:?})",
                            result_operand.get_name().unwrap_or("<unknown>"),
                            result_operand.get_type()
                        )
                    );
                }
            }
        }

        Rvalue::Cast(cast_kind, operand, target_mir_ty) => {
            let temp_cast_var = generate_temp_var_name(&base_temp_name);
            let oomir_target_type = ty_to_oomir_type(*target_mir_ty, tcx, data_types, instance);
            let source_mir_ty = operand.ty(&mir.local_decls, tcx);

            if matches!(
                cast_kind,
                CastKind::PointerCoercion(PointerCoercion::ReifyFnPointer(_), _)
            ) {
                if let TyKind::FnDef(def_id, substs) = source_mir_ty.kind() {
                    let func_instance = Instance::resolve_for_fn_ptr(
                        tcx,
                        TypingEnv::post_analysis(tcx, mir.source.def_id()),
                        *def_id,
                        substs,
                    )
                    .unwrap();
                    let fn_name =
                        super::super::naming::mono_fn_name_from_instance(tcx, func_instance)
                            .method_name;
                    let signature =
                        fn_ptr_signature_from_ty(*target_mir_ty, tcx, data_types, instance);
                    let interface_name = ensure_fn_ptr_interface(&signature, data_types);
                    let callable_target = func_instance
                        .def_id()
                        .is_local()
                        .then_some(fn_name.as_str());
                    if callable_target.is_none() {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "mir-lowering",
                            format!(
                                "Warning: Reified non-local function pointer '{}' will use an UnsupportedOperationException stub if invoked.",
                                fn_name
                            )
                        );
                    }
                    let adapter_class = ensure_fn_pointer_adapter_class(
                        data_types,
                        callable_target,
                        &signature,
                        &interface_name,
                    );
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Reifying FnDef to FnPtr: '{}' -> '{}' as '{}'",
                            source_mir_ty, fn_name, adapter_class
                        )
                    );
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: temp_cast_var.clone(),
                        class_name: adapter_class,
                        args: Vec::new(),
                    });
                    result_operand = oomir::Operand::Variable {
                        name: temp_cast_var,
                        ty: oomir::Type::Interface(interface_name),
                    };
                } else {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!(
                            "Warning: ReifyFnPointer cast with non-FnDef source type: {:?}",
                            source_mir_ty
                        )
                    );
                    result_operand =
                        oomir::Operand::Constant(oomir::Constant::Null(oomir_target_type));
                }
            } else {
                if matches!(source_mir_ty.kind(), TyKind::FnPtr(..))
                    && matches!(target_mir_ty.kind(), TyKind::FnPtr(..))
                {
                    let source_signature =
                        fn_ptr_signature_from_ty(source_mir_ty, tcx, data_types, instance);
                    let target_signature =
                        fn_ptr_signature_from_ty(*target_mir_ty, tcx, data_types, instance);
                    let target_interface = ensure_fn_ptr_interface(&target_signature, data_types);

                    if source_signature.to_jvm_descriptor_with_explicit_params()
                        == target_signature.to_jvm_descriptor_with_explicit_params()
                    {
                        let oomir_operand = convert_operand(
                            operand,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        instructions.push(oomir::Instruction::Move {
                            dest: temp_cast_var.clone(),
                            src: oomir_operand,
                        });
                    } else {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "mir-lowering",
                            format!(
                                "Warning: Function pointer cast from '{}' to '{}' changes the JVM descriptor; generating an UnsupportedOperationException stub.",
                                source_signature.to_jvm_descriptor_with_explicit_params(),
                                target_signature.to_jvm_descriptor_with_explicit_params()
                            )
                        );
                        let adapter_class = ensure_fn_pointer_adapter_class(
                            data_types,
                            None,
                            &target_signature,
                            &target_interface,
                        );
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_cast_var.clone(),
                            class_name: adapter_class,
                            args: Vec::new(),
                        });
                    }

                    result_operand = oomir::Operand::Variable {
                        name: temp_cast_var,
                        ty: oomir::Type::Interface(target_interface),
                    };
                } else {
                    let oomir_source_type =
                        ty_to_oomir_type(source_mir_ty, tcx, data_types, instance);
                    let oomir_operand =
                        convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);

                    if let oomir::Type::Class(class_name) = &oomir_target_type
                        && class_name.starts_with("NonNull_")
                    {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            "Info: Handling Rvalue::Cast to NonNull wrapper."
                        );
                        let mut constructor_args = Vec::new();
                        if let Some(oomir::DataType::Class { fields, .. }) =
                            data_types.get(class_name)
                        {
                            if let Some((_field_name, field_ty)) = fields.first().cloned() {
                                let value_ty =
                                    oomir_operand.get_type().unwrap_or(oomir_source_type);
                                let needs_cast = field_ty != value_ty
                                    && field_ty.to_jvm_descriptor() != value_ty.to_jvm_descriptor();
                                let primitive_sentinel_for_reference = value_ty
                                    .is_jvm_primitive_like()
                                    && field_ty.is_jvm_reference_type();

                                let value_operand =
                                    if needs_cast && primitive_sentinel_for_reference {
                                        oomir::Operand::Constant(oomir::Constant::Null(
                                            field_ty.clone(),
                                        ))
                                    } else if needs_cast {
                                        let cast_value_name = format!("{}_value", temp_cast_var);
                                        instructions.push(oomir::Instruction::Cast {
                                            op: oomir_operand,
                                            ty: field_ty.clone(),
                                            dest: cast_value_name.clone(),
                                        });
                                        oomir::Operand::Variable {
                                            name: cast_value_name,
                                            ty: field_ty.clone(),
                                        }
                                    } else {
                                        oomir_operand
                                    };

                                constructor_args.push((value_operand, field_ty));
                            }
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_cast_var.clone(),
                            class_name: class_name.clone(),
                            args: constructor_args,
                        });
                    } else if oomir_target_type == oomir_source_type {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            "Info: Handling Rvalue::Cast (Same OOMIR Types) -> Temp Move."
                        );
                        instructions.push(oomir::Instruction::Move {
                            dest: temp_cast_var.clone(),
                            src: oomir_operand,
                        });
                    } else {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            "Info: Handling Rvalue::Cast (Different OOMIR Types) -> Temp Cast."
                        );
                        instructions.push(oomir::Instruction::Cast {
                            op: oomir_operand,
                            ty: oomir_target_type.clone(),
                            dest: temp_cast_var.clone(),
                        });
                    }
                    result_operand = oomir::Operand::Variable {
                        name: temp_cast_var,
                        ty: oomir_target_type,
                    };
                }
            }
        }

        Rvalue::BinaryOp(bin_op, box (op1, op2)) => {
            let temp_binop_var = generate_temp_var_name(&base_temp_name);
            let oomir_op1 = convert_operand(op1, tcx, instance, mir, data_types, &mut instructions);
            let oomir_op2 = convert_operand(op2, tcx, instance, mir, data_types, &mut instructions);
            // Determine result type based on operands or destination hint
            let oomir_result_type =
                get_place_type(original_dest_place, mir, tcx, instance, data_types);

            match bin_op {
                BinOp::Add | BinOp::AddUnchecked => instructions.push(oomir::Instruction::Add {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Sub | BinOp::SubUnchecked => instructions.push(oomir::Instruction::Sub {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Mul | BinOp::MulUnchecked => instructions.push(oomir::Instruction::Mul {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Div => instructions.push(oomir::Instruction::Div {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Rem => instructions.push(oomir::Instruction::Rem {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitAnd => instructions.push(oomir::Instruction::BitAnd {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitOr => instructions.push(oomir::Instruction::BitOr {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitXor => instructions.push(oomir::Instruction::BitXor {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Shl | BinOp::ShlUnchecked => instructions.push(oomir::Instruction::Shl {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Shr | BinOp::ShrUnchecked => instructions.push(oomir::Instruction::Shr {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Eq => instructions.push(oomir::Instruction::Eq {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Lt => instructions.push(oomir::Instruction::Lt {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Le => instructions.push(oomir::Instruction::Le {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Ne => instructions.push(oomir::Instruction::Ne {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Ge => instructions.push(oomir::Instruction::Ge {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Gt => instructions.push(oomir::Instruction::Gt {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                // Checked ops need special handling as they produce a tuple
                BinOp::AddWithOverflow | BinOp::SubWithOverflow | BinOp::MulWithOverflow => {
                    // This case needs to return the *tuple* variable, and the instructions
                    // generated inside it are already correct for creating that tuple.

                    // Reuse the logic from the original handle_rvalue for checked ops,
                    // but target the temp_tuple_var instead of the final dest.
                    let (result_mir_ty, _) = {
                        let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                        if let TyKind::Tuple(elements) = place_ty.kind() {
                            (elements[0], elements[1])
                        } else {
                            panic!("Checked op dest type mismatch")
                        }
                    };
                    let op_oomir_ty = ty_to_oomir_type(result_mir_ty, tcx, data_types, instance);

                    let operation_string = match bin_op {
                        /* ... */ BinOp::AddWithOverflow => "add",
                        BinOp::SubWithOverflow => "sub",
                        BinOp::MulWithOverflow => "mul",
                        _ => unreachable!(),
                    };
                    let (checked_instructions, tmp_pair_var, _tmp_result_var, _tmp_overflow_var) =
                        emit_checked_arithmetic_oomir_instructions(
                            &base_temp_name, // Use base temp name for context
                            &oomir_op1,
                            &oomir_op2,
                            &op_oomir_ty,
                            operation_string,
                            instructions.len(),
                        );
                    instructions.extend(checked_instructions);
                    // The checked intrinsic returns a tuple object (Tuple_i32_bool, etc.) in tmp_pair
                    // Determine the tuple type name based on the operation type
                    // Note: Must match the names generated by types.rs:readable_oomir_type_name
                    let tuple_type_name = match &op_oomir_ty {
                        oomir::Type::I8 => "Tuple_i8_bool".to_string(),
                        oomir::Type::I16 => "Tuple_i16_bool".to_string(),
                        oomir::Type::I32 => "Tuple_i32_bool".to_string(),
                        oomir::Type::I64 => "Tuple_i64_bool".to_string(),
                        oomir::Type::Class(c) if c == crate::lower2::BIG_INTEGER_CLASS => {
                            "Tuple_BigInteger_bool".to_string()
                        }
                        oomir::Type::Class(c) if c == crate::lower2::BIG_DECIMAL_CLASS => {
                            "Tuple_BigDecimal_bool".to_string()
                        }
                        _ => panic!("Unsupported type for checked arithmetic: {:?}", op_oomir_ty),
                    };
                    // Return the object as the operand
                    result_operand = oomir::Operand::Variable {
                        name: tmp_pair_var,
                        ty: oomir::Type::Class(tuple_type_name),
                    };
                    return (instructions, result_operand);
                }
                _ => {
                    /* Handle Offset, etc. or panic */
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!("Warning: Unhandled binary op {:?}", bin_op)
                    );
                    result_operand = get_placeholder_operand(
                        original_dest_place,
                        mir,
                        tcx,
                        instance,
                        data_types,
                    );
                    // No instruction needed for placeholder
                    return (instructions, result_operand);
                }
            }
            result_operand = oomir::Operand::Variable {
                name: temp_binop_var,
                ty: oomir_result_type, // Use determined result type
            };
        }

        Rvalue::UnaryOp(operation, operand) => {
            let temp_unop_var = generate_temp_var_name(&base_temp_name);
            let oomir_src_operand =
                convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);
            // Determine result type (often same as operand, except for PtrMetadata)
            let oomir_result_type = match operation {
                UnOp::PtrMetadata => oomir::Type::I32,
                _ => get_place_type(original_dest_place, mir, tcx, instance, data_types), // Use dest type hint
            };

            match operation {
                UnOp::Not => instructions.push(oomir::Instruction::Not {
                    dest: temp_unop_var.clone(),
                    src: oomir_src_operand,
                }),
                UnOp::Neg => instructions.push(oomir::Instruction::Neg {
                    dest: temp_unop_var.clone(),
                    src: oomir_src_operand,
                }),
                UnOp::PtrMetadata => {
                    // Handle length specifically
                    match oomir_src_operand {
                        oomir::Operand::Variable {
                            name: source_name,
                            ty: var_ty,
                        } => {
                            // Check if source is actually an array/slice/str type
                            let operand_place = match operand {
                                MirOperand::Copy(p) | MirOperand::Move(p) => Some(p),
                                _ => None,
                            };
                            if let Some(op_place) = operand_place {
                                let op_place_ty = op_place.ty(&mir.local_decls, tcx).ty;
                                let is_target_for_length = match op_place_ty.kind() {
                                    TyKind::Slice(_) | TyKind::Str => true,
                                    TyKind::Ref(_, inner_ty, _)
                                        if matches!(
                                            inner_ty.kind(),
                                            TyKind::Slice(_) | TyKind::Str
                                        ) =>
                                    {
                                        true
                                    }
                                    TyKind::RawPtr(ty, _) if ty.is_slice() || ty.is_str() => true,
                                    _ => false,
                                };
                                if is_target_for_length {
                                    breadcrumbs::log!(
                                        breadcrumbs::LogLevel::Info,
                                        "mir-lowering",
                                        "Info: Detected Length op via PtrMetadata."
                                    );
                                    instructions.push(oomir::Instruction::Length {
                                        dest: temp_unop_var.clone(),
                                        array: oomir::Operand::Variable {
                                            name: source_name,
                                            ty: var_ty,
                                        },
                                    });
                                } else {
                                    breadcrumbs::log!(
                                        breadcrumbs::LogLevel::Warn,
                                        "mir-lowering",
                                        format!(
                                            "Warning: PtrMetadata on unexpected type {:?}. Emitting placeholder.",
                                            op_place_ty
                                        )
                                    );
                                    // No instruction needed for placeholder, result_operand set below
                                }
                            } else {
                                // PtrMetadata on constant? Invalid MIR?
                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Warn,
                                    "mir-lowering",
                                    format!(
                                        "Warning: PtrMetadata on constant operand {:?}. Emitting placeholder.",
                                        operand
                                    )
                                );
                            }
                        }
                        _ => {
                            // PtrMetadata on constant? Invalid MIR?
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "mir-lowering",
                                format!(
                                    "Warning: PtrMetadata on constant operand {:?}. Emitting placeholder.",
                                    operand
                                )
                            );
                        }
                    }
                }
            }

            // Handle cases where no instruction was pushed (e.g., PtrMetadata warning)
            if instructions.last().map_or(true, |inst| match inst {
                oomir::Instruction::Length { dest, .. }
                | oomir::Instruction::Not { dest, .. }
                | oomir::Instruction::Neg { dest, .. } => dest != &temp_unop_var.clone(),
                _ => true, // If last instruction wasn't for this temp var
            }) && *operation != UnOp::PtrMetadata
            /* Allow PtrMetadata failure */
            {
                // If no instruction *actually* assigned to temp_unop_var, it's likely a placeholder case
                result_operand =
                    get_placeholder_operand(original_dest_place, mir, tcx, instance, data_types);
            } else {
                result_operand = oomir::Operand::Variable {
                    name: temp_unop_var.clone(),
                    ty: oomir_result_type,
                };
            }
        }

        Rvalue::Aggregate(box kind, operands) => {
            // Create a temporary variable to hold the aggregate
            let temp_aggregate_var = generate_temp_var_name(&base_temp_name);
            // Get the type from the original destination place
            let aggregate_oomir_type =
                get_place_type(original_dest_place, mir, tcx, instance, data_types);

            match kind {
                rustc_middle::mir::AggregateKind::Tuple => {
                    let tuple_class_name = match &aggregate_oomir_type {
                        oomir::Type::Class(name) => name.clone(),
                        _ => panic!("Tuple aggregate type error"),
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Tuple Aggregate -> Temp Var '{}'",
                            temp_aggregate_var
                        )
                    );
                    let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                    let mut constructor_args = Vec::new();
                    for (i, mir_op) in operands.iter().enumerate() {
                        let element_mir_ty = if let TyKind::Tuple(elements) = place_ty.kind() {
                            elements[i]
                        } else {
                            panic!("...")
                        };
                        let element_oomir_type =
                            ty_to_oomir_type(element_mir_ty.clone(), tcx, data_types, instance);
                        let value_operand = convert_operand(
                            mir_op,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        let value_operand = adapt_value_for_field(
                            value_operand,
                            &element_oomir_type,
                            &temp_aggregate_var,
                            data_types,
                            &mut instructions,
                        );
                        constructor_args.push((value_operand, element_oomir_type));
                    }
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: temp_aggregate_var.clone(),
                        class_name: tuple_class_name.clone(),
                        args: constructor_args,
                    });
                }
                rustc_middle::mir::AggregateKind::Array(mir_element_ty) => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Array Aggregate -> Temp Var '{}'",
                            temp_aggregate_var
                        )
                    );
                    let oomir_element_type =
                        ty_to_oomir_type(*mir_element_ty, tcx, data_types, instance);
                    let array_size = operands.len();
                    let size_operand =
                        oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));
                    instructions.push(oomir::Instruction::NewArray {
                        dest: temp_aggregate_var.clone(),
                        element_type: oomir_element_type.clone(),
                        size: size_operand,
                    });
                    // Store elements into the temporary array
                    for (i, mir_operand) in operands.iter().enumerate() {
                        let value_operand = convert_operand(
                            mir_operand,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        let index_operand =
                            oomir::Operand::Constant(oomir::Constant::I32(i as i32));
                        instructions.push(oomir::Instruction::ArrayStore {
                            array: temp_aggregate_var.clone(),
                            index: index_operand,
                            value: value_operand,
                        });
                    }
                }
                rustc_middle::mir::AggregateKind::Closure(_, _) => {
                    let closure_class_name = match &aggregate_oomir_type {
                        oomir::Type::Class(name) => name.clone(),
                        _ => panic!("Closure aggregate type error"),
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Closure Aggregate -> Temp Var '{}' (Class: {})",
                            temp_aggregate_var, closure_class_name
                        )
                    );
                    let closure_fields = match data_types.get(&closure_class_name) {
                        Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
                        _ => Vec::new(),
                    };

                    let mut constructor_args = Vec::new();
                    for (i, mir_operand) in operands.iter().enumerate() {
                        let (_field_name, field_ty) =
                            closure_fields.get(i).cloned().unwrap_or_else(|| {
                                (
                                    format!("arg{}", i),
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                )
                            });
                        let value_operand = convert_operand(
                            mir_operand,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        let value_operand = adapt_value_for_field(
                            value_operand,
                            &field_ty,
                            &temp_aggregate_var,
                            data_types,
                            &mut instructions,
                        );
                        constructor_args.push((value_operand, field_ty));
                    }
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: temp_aggregate_var.clone(),
                        class_name: closure_class_name.clone(),
                        args: constructor_args,
                    });
                }
                rustc_middle::mir::AggregateKind::Adt(def_id, variant_idx, substs, _, _) => {
                    let adt_def = tcx.adt_def(*def_id);
                    if adt_def.is_struct() {
                        let variant = adt_def.variant(*variant_idx);
                        let jvm_class_name = generate_adt_jvm_class_name(
                            &adt_def, substs, tcx, data_types, instance,
                        );
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Struct Aggregate -> Temp Var '{}' (Class: {})",
                                temp_aggregate_var, jvm_class_name
                            )
                        );
                        let oomir_fields: Vec<(String, oomir::Type)> = variant
                            .fields
                            .iter()
                            .map(|f| {
                                (
                                    f.ident(tcx).to_string(),
                                    ty_to_oomir_type(
                                        f.ty(tcx, substs).skip_norm_wip(),
                                        tcx,
                                        data_types,
                                        instance,
                                    ),
                                )
                            })
                            .collect();
                        if !data_types.contains_key(&jvm_class_name) {
                            let mut methods = HashMap::new();
                            methods.insert(
                                "eq".to_string(),
                                DataTypeMethod::AdtHelperMethod {
                                    kind: oomir::AdtHelperKind::PartialEqClass {
                                        fields: oomir_fields.clone(),
                                    },
                                },
                            );
                            data_types.insert(
                                jvm_class_name.clone(),
                                oomir::DataType::Class {
                                    fields: oomir_fields.clone(),
                                    is_abstract: false,
                                    methods,
                                    super_class: None,
                                    interfaces: vec![],
                                },
                            );
                        } else if let Some(oomir::DataType::Class {
                            fields, methods, ..
                        }) = data_types.get_mut(&jvm_class_name)
                        {
                            methods.entry("eq".to_string()).or_insert_with(|| {
                                DataTypeMethod::AdtHelperMethod {
                                    kind: oomir::AdtHelperKind::PartialEqClass {
                                        fields: fields.clone(),
                                    },
                                }
                            });
                        }

                        let mut constructor_args = Vec::new();
                        for (field_def, mir_operand) in variant.fields.iter().zip(operands.iter()) {
                            let field_mir_ty = field_def.ty(tcx, substs).skip_norm_wip();
                            let field_oomir_type =
                                ty_to_oomir_type(field_mir_ty, tcx, data_types, instance);
                            let value_operand = convert_operand(
                                mir_operand,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let value_operand = adapt_value_for_field(
                                value_operand,
                                &field_oomir_type,
                                &temp_aggregate_var,
                                data_types,
                                &mut instructions,
                            );
                            constructor_args.push((value_operand, field_oomir_type));
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: jvm_class_name.clone(),
                            args: constructor_args,
                        });
                    } else if adt_def.is_enum() {
                        let variant_def = adt_def.variant(*variant_idx);
                        let base_enum_name = generate_adt_jvm_class_name(
                            &adt_def, substs, tcx, data_types, instance,
                        );
                        let variant_class_name = format!(
                            "{}${}",
                            base_enum_name,
                            make_jvm_safe(&variant_def.name.to_string())
                        );

                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Enum Aggregate (Variant: {}) -> Temp Var '{}' (Class: {})",
                                variant_def.name, temp_aggregate_var, variant_class_name
                            )
                        );

                        /*
                        i.e. consider rust code:
                        ```rust
                        enum MyEnum {
                            A(i32),
                            B{x: String},
                            C,
                        }
                        ```

                        psuedo-java for the plan on how to handle this
                        ```java
                        abstract class MyEnum {
                            public abstract int getVariantIdx();
                        }

                        class MyEnum$A extends MyEnum {
                            public int field0;

                            public MyEnum$A(int field0) {
                                this.field0 = field0;
                            }

                            @Override
                            public final int getVariantIdx() { return 0; }
                        }

                        class MyEnum$B extends MyEnum {
                            public String field0;

                            public MyEnum$B(String field0) {
                                this.field0 = field0;
                            }

                            @Override
                            public final int getVariantIdx() { return 1; }
                        }

                        class MyEnum$C extends MyEnum {
                            @Override
                            public final int getVariantIdx() { return 2; }
                        }
                        ```
                        */

                        // the enum in general - always ensure helper methods are present
                        {
                            let variants_info: Vec<_> = adt_def
                                .variants()
                                .iter()
                                .map(|v| {
                                    let v_name = make_jvm_safe(&v.name.to_string());
                                    let v_fields: Vec<oomir::Type> = v
                                        .fields
                                        .iter()
                                        .map(|f| {
                                            ty_to_oomir_type(
                                                f.ty(tcx, substs).skip_norm_wip(),
                                                tcx,
                                                data_types,
                                                instance,
                                            )
                                        })
                                        .collect();
                                    (v_name, v_fields)
                                })
                                .collect();

                            if !data_types.contains_key(&base_enum_name) {
                                let mut methods = HashMap::new();
                                methods.insert(
                                    "getVariantIdx".to_string(),
                                    DataTypeMethod::SimpleConstantReturn(oomir::Type::I32, None),
                                );

                                // Add AdtHelperMethod for PartialEq
                                methods.insert(
                                    "eq".to_string(),
                                    DataTypeMethod::AdtHelperMethod {
                                        kind: oomir::AdtHelperKind::PartialEqEnum {
                                            variants: variants_info.clone(),
                                        },
                                    },
                                );

                                // Add is_none/is_some for two-variant enums (like Option)
                                if adt_def.variants().len() == 2 {
                                    // Find the actual variant indices by name
                                    let mut none_variant_idx = 1u32;
                                    let mut some_variant_idx = 0u32;
                                    for (idx, v) in adt_def.variants().iter().enumerate() {
                                        let name = v.name.to_string();
                                        if name == "None" {
                                            none_variant_idx = idx as u32;
                                        } else if name == "Some" {
                                            some_variant_idx = idx as u32;
                                        }
                                    }
                                    methods.insert(
                                        "is_none".to_string(),
                                        DataTypeMethod::AdtHelperMethod {
                                            kind: oomir::AdtHelperKind::IsVariant {
                                                variant_idx: none_variant_idx,
                                            },
                                        },
                                    );
                                    methods.insert(
                                        "is_some".to_string(),
                                        DataTypeMethod::AdtHelperMethod {
                                            kind: oomir::AdtHelperKind::IsVariant {
                                                variant_idx: some_variant_idx,
                                            },
                                        },
                                    );
                                }

                                data_types.insert(
                                    base_enum_name.clone(),
                                    oomir::DataType::Class {
                                        fields: vec![],
                                        is_abstract: true,
                                        methods,
                                        super_class: None,
                                        interfaces: vec![],
                                    },
                                );
                            } else {
                                // Merge helper methods into existing enum class
                                if let oomir::DataType::Class { methods, .. } =
                                    data_types.get_mut(&base_enum_name).unwrap()
                                {
                                    if !methods.contains_key("eq") {
                                        methods.insert(
                                            "eq".to_string(),
                                            DataTypeMethod::AdtHelperMethod {
                                                kind: oomir::AdtHelperKind::PartialEqEnum {
                                                    variants: variants_info.clone(),
                                                },
                                            },
                                        );
                                    }
                                    if adt_def.variants().len() == 2 {
                                        // Find the actual variant indices by name
                                        let mut none_variant_idx = 1u32;
                                        let mut some_variant_idx = 0u32;
                                        for (idx, v) in adt_def.variants().iter().enumerate() {
                                            let name = v.name.to_string();
                                            if name == "None" {
                                                none_variant_idx = idx as u32;
                                            } else if name == "Some" {
                                                some_variant_idx = idx as u32;
                                            }
                                        }
                                        if !methods.contains_key("is_none") {
                                            methods.insert(
                                                "is_none".to_string(),
                                                DataTypeMethod::AdtHelperMethod {
                                                    kind: oomir::AdtHelperKind::IsVariant {
                                                        variant_idx: none_variant_idx,
                                                    },
                                                },
                                            );
                                        }
                                        if !methods.contains_key("is_some") {
                                            methods.insert(
                                                "is_some".to_string(),
                                                DataTypeMethod::AdtHelperMethod {
                                                    kind: oomir::AdtHelperKind::IsVariant {
                                                        variant_idx: some_variant_idx,
                                                    },
                                                },
                                            );
                                        }
                                    }
                                }
                            }
                        }

                        // this variant
                        if !data_types.contains_key(&variant_class_name) {
                            let mut fields = vec![];
                            for (i, field) in variant_def.fields.iter().enumerate() {
                                let field_name = format!("field{}", i);
                                let field_type = ty_to_oomir_type(
                                    field.ty(tcx, substs).skip_norm_wip(),
                                    tcx,
                                    data_types,
                                    instance,
                                );
                                fields.push((field_name, field_type));
                            }

                            let mut methods = HashMap::new();
                            methods.insert(
                                "getVariantIdx".to_string(),
                                DataTypeMethod::SimpleConstantReturn(
                                    oomir::Type::I32,
                                    Some(oomir::Constant::I32(variant_idx.as_u32() as i32)),
                                ),
                            );

                            data_types.insert(
                                variant_class_name.clone(),
                                oomir::DataType::Class {
                                    fields,
                                    is_abstract: false,
                                    methods,
                                    super_class: Some(base_enum_name.clone()),
                                    interfaces: vec![],
                                },
                            );
                        }

                        let mut constructor_args = Vec::new();
                        for (i, field) in variant_def.fields.iter().enumerate() {
                            let field_mir_ty = field.ty(tcx, substs).skip_norm_wip();
                            let field_oomir_type =
                                ty_to_oomir_type(field_mir_ty, tcx, data_types, instance);
                            let value_operand = convert_operand(
                                &operands[FieldIdx::from_usize(i)],
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let value_operand = adapt_value_for_field(
                                value_operand,
                                &field_oomir_type,
                                &temp_aggregate_var,
                                data_types,
                                &mut instructions,
                            );
                            constructor_args.push((value_operand, field_oomir_type));
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: variant_class_name.clone(),
                            args: constructor_args,
                        });
                    } else {
                        // Union
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "mir-lowering",
                            format!("Warning: Unhandled ADT Aggregate Kind -> Temp Placeholder")
                        );
                        // make a placeholder (Class("java/lang/Object"))
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: "java/lang/Object".to_string(),
                            args: Vec::new(),
                        });
                    }
                }
                _ => {
                    /* Unknown aggregate */
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!("Warning: Unhandled Aggregate Kind -> Temp Placeholder")
                    );
                    // No instructions needed for placeholder, result set below
                }
            }

            result_operand = oomir::Operand::Variable {
                name: temp_aggregate_var,
                ty: aggregate_oomir_type,
            };
        }
        Rvalue::RawPtr(kind, place) => {
            // Get the operand and instructions for the *place* being pointed to.
            // We need this regardless of the RawPtrKind.
            let (place_temp_var_name, place_get_instructions, place_temp_var_type) =
                emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);

            instructions.extend(place_get_instructions);

            match kind {
                rustc_middle::mir::RawPtrKind::FakeForPtrMetadata => {
                    // This pointer is *only* created to get metadata (like length)
                    // from the underlying place. The actual pointer value is irrelevant
                    // in the target code. The subsequent PtrMetadata operation will
                    // operate on the operand representing the place itself.
                    // So, we just pass the place's operand through.
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Rvalue::RawPtr(FakeForPtrMetadata) for place '{:?}'. Passing through place operand '{}' ({:?}).",
                            place_to_string(place, tcx),
                            place_temp_var_name,
                            place_temp_var_type
                        )
                    );
                    result_operand = oomir::Operand::Variable {
                        name: place_temp_var_name, // Use the operand computed for the place
                        ty: place_temp_var_type,
                    };
                }
                rustc_middle::mir::RawPtrKind::Const | rustc_middle::mir::RawPtrKind::Mut => {
                    // For 'real' raw pointers (*const T, *mut T), the JVM has no direct
                    // equivalent. I currently use the operand of the place itself.
                    // Really, we shouldn't be getting these kinds of things unless
                    // `unsafe` is being used in a strange way, and it is a non-goal of this
                    // project to support native-like non-trivial unsafe.
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!(
                            "Warning: Handling Rvalue::RawPtr({:?}) for place '{:?}' by using the place's operand '{}' ({:?}). True pointer semantics (arithmetic, etc.) not fully supported.",
                            kind,
                            place_to_string(place, tcx),
                            place_temp_var_name,
                            place_temp_var_type
                        )
                    );

                    let pointer_oomir_type = place_temp_var_type.clone();

                    result_operand = oomir::Operand::Variable {
                        name: place_temp_var_name,
                        ty: pointer_oomir_type,
                    };
                }
            }
        }
        Rvalue::Discriminant(place) => {
            // 1. Generate instructions to get the actual value from the place
            let (actual_value_var_name, get_instructions, actual_value_oomir_type) =
                emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);

            // Add the instructions needed to get the value (e.g., ArrayGet)
            instructions.extend(get_instructions);

            // 2. Now operate on the variable holding the actual value
            let temp_discriminant_var = generate_temp_var_name(&base_temp_name);

            let place_class_name = match actual_value_oomir_type.clone() {
                oomir::Type::Class(name) => name.clone(),
                // Handle potential references if get_on_own returns Ref(Class)
                oomir::Type::Reference(inner) => {
                    if let oomir::Type::Class(name) = inner.as_ref() {
                        name.clone()
                    } else {
                        panic!("Discriminant on Ref to non-class type: {:?}", inner)
                    }
                }
                oomir::Type::MutableReference(inner) => {
                    if let oomir::Type::Class(name) = inner.as_ref() {
                        name.clone()
                    } else {
                        panic!("Discriminant on MutableRef to non-class type: {:?}", inner)
                    }
                }
                _ => panic!(
                    "Discriminant on non-class type: {:?}",
                    actual_value_oomir_type
                ),
            };

            let method_name = "getVariantIdx".to_string();
            let method_return_type = oomir::Type::I32;

            let method_ty = oomir::Signature {
                params: vec![],
                ret: Box::new(method_return_type.clone()),
                is_static: false,
            };

            // 3. Call InvokeVirtual on the CORRECT variable
            instructions.push(oomir::Instruction::InvokeVirtual {
                class_name: place_class_name.clone(),
                method_name,
                args: vec![],
                dest: Some(temp_discriminant_var.clone()),
                method_ty,
                operand: oomir::Operand::Variable {
                    name: actual_value_var_name,
                    ty: actual_value_oomir_type,
                },
            });

            // 4. The result is the temporary variable holding the discriminant value
            result_operand = oomir::Operand::Variable {
                name: temp_discriminant_var,
                ty: method_return_type, // Should be I32
            };
        }
        Rvalue::CopyForDeref(place) => {
            // Need to get the value from the source place first
            let (temp_var_name, get_instructions, temp_var_type) =
                emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
            instructions.extend(get_instructions);
            result_operand = oomir::Operand::Variable {
                name: temp_var_name,
                ty: temp_var_type,
            };
        }
        // Handle other Rvalue variants by generating a placeholder
        _ => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "mir-lowering",
                format!(
                    "Warning: Unhandled Rvalue: {:?} for temp based on {:?}. Emitting placeholder.",
                    rvalue, original_dest_place
                )
            );
            result_operand =
                get_placeholder_operand(original_dest_place, mir, tcx, instance, data_types);
            // No instructions needed to "calculate" a placeholder
        }
    }

    (instructions, result_operand)
}
