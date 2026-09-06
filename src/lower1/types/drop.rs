use super::*;
use crate::lower1::context::Definitions;

pub(super) fn enum_variant_drop_glue_function<'tcx>(
    variant: &rustc_middle::ty::VariantDef,
    substs: GenericArgsRef<'tcx>,
    enum_class_name: &str,
    variant_class_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let self_ty = oomir::Type::Class(variant_class_name.to_string());
    let self_operand = operand_var("_1", self_ty.clone());
    let mut actions = Vec::new();
    for (field_index, field) in variant.fields.iter().enumerate() {
        let raw_field_ty = field.ty(tcx, substs).skip_norm_wip();
        let field_ty =
            resolve_union_ty(tcx, raw_field_ty, instance_context).unwrap_or(raw_field_ty);
        let field_oomir_ty = ty_to_oomir_type(field_ty, tcx, data_types, instance_context);
        let mut action = Vec::new();
        let field_value = if field_oomir_ty.has_jvm_value() {
            let field_name = enum_variant_field_name(variant, field_index, tcx);
            let dest = format!("_drop_field_{field_index}");
            action.push(oomir::Instruction::GetField {
                dest: dest.clone(),
                object: self_operand.clone(),
                field_name,
                field_ty: field_oomir_ty.clone(),
                owner_class: variant_class_name.to_string(),
            });
            operand_var(dest, field_oomir_ty)
        } else {
            oomir::Operand::Constant(oomir::Constant::Unit)
        };

        // Enum data types can first be discovered below a higher-ranked
        // function, leaving otherwise irrelevant bound lifetimes in a payload
        // type. `needs_drop` cannot query a type with escaping bound vars, but
        // lifetimes do not affect either JVM representation or drop glue.
        let drop_field_ty = erase_all_regions(tcx, field_ty);
        if drop_field_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
            emit_managed_value_drop(
                drop_field_ty,
                field_value,
                &format!("_drop_field_{field_index}"),
                tcx,
                instance_context,
                data_types,
                &mut action,
            );
            actions.push(action);
        }
    }

    oomir::Function {
        name: enum_scoped_method_name(enum_class_name, ENUM_DROP_FIELDS_METHOD),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![("self".to_string(), self_ty)],
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        body: cleanup_safe_drop_body(actions).into(),
    }
}

pub(super) fn enum_transparent_variant_drop_glue_function<'tcx>(
    payload_ty: Ty<'tcx>,
    enum_class_name: &str,
    receiver_class_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let self_ty = oomir::Type::Class(receiver_class_name.to_string());
    let mut instructions = Vec::new();
    crate::lower1::control_flow::emit_rust_drop_value(
        payload_ty,
        operand_var("_1", self_ty.clone()),
        "_transparent_enum_drop",
        tcx,
        instance_context,
        data_types,
        &mut instructions,
    );
    instructions.push(oomir::Instruction::Return { operand: None });
    oomir::Function {
        name: enum_scoped_method_name(enum_class_name, ENUM_DROP_FIELDS_METHOD),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![("self".to_string(), self_ty)],
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        body: simple_body(instructions).into(),
    }
}

pub(super) fn erase_all_regions<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Ty<'tcx> {
    ty.fold_with(&mut AllRegionEraser { tcx })
}

pub(super) fn emit_managed_value_drop<'tcx>(
    rust_ty: Ty<'tcx>,
    mut value: oomir::Operand,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    let value_ty = ty_to_oomir_type(rust_ty, tcx, data_types, instance_context);
    if !value_ty.has_jvm_value() {
        let Some(materialized) = crate::lower1::value_repr::materialize_implicit_zst(
            rust_ty,
            &format!("{temp_prefix}_zst"),
            tcx,
            instance_context,
            data_types,
            instructions,
        ) else {
            return;
        };
        value = materialized;
    }
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: None,
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "dropRustValue".to_string(),
        method_ty: oomir::Signature {
            params: vec![(
                "value".to_string(),
                oomir::Type::Class("java/lang/Object".to_string()),
            )],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args: vec![value],
    });
}

pub(super) fn cleanup_safe_drop_body(actions: Vec<Vec<oomir::Instruction>>) -> oomir::CodeBlock {
    if actions.is_empty() {
        return simple_body(vec![oomir::Instruction::Return { operand: None }]);
    }

    let mut basic_blocks = HashMap::default();
    for (index, action) in actions.iter().enumerate() {
        let mut instructions = vec![oomir::Instruction::UnwindStart {
            target: if index + 1 < actions.len() {
                format!("cleanup_{}", index + 1)
            } else {
                "rethrow".to_string()
            },
        }];
        instructions.extend(action.clone());
        instructions.push(oomir::Instruction::UnwindEnd);
        instructions.push(if index + 1 < actions.len() {
            oomir::Instruction::Jump {
                target: format!("drop_{}", index + 1),
            }
        } else {
            oomir::Instruction::Return { operand: None }
        });
        let label = format!("drop_{index}");
        basic_blocks.insert(
            label.clone(),
            oomir::BasicBlock {
                label,
                instructions,
            },
        );
    }

    for (index, action) in actions.iter().enumerate().skip(1) {
        let mut instructions = vec![oomir::Instruction::UnwindStart {
            target: "abort".to_string(),
        }];
        instructions.extend(action.clone());
        instructions.push(oomir::Instruction::UnwindEnd);
        instructions.push(if index + 1 < actions.len() {
            oomir::Instruction::Jump {
                target: format!("cleanup_{}", index + 1),
            }
        } else {
            oomir::Instruction::Rethrow
        });
        let label = format!("cleanup_{index}");
        basic_blocks.insert(
            label.clone(),
            oomir::BasicBlock {
                label,
                instructions,
            },
        );
    }

    basic_blocks.insert(
        "rethrow".to_string(),
        oomir::BasicBlock {
            label: "rethrow".to_string(),
            instructions: vec![oomir::Instruction::Rethrow],
        },
    );
    basic_blocks.insert(
        "abort".to_string(),
        oomir::BasicBlock {
            label: "abort".to_string(),
            instructions: vec![
                oomir::Instruction::InvokeStatic {
                    dest: None,
                    class_name: "org/rustlang/runtime/PanicSupport".to_string(),
                    method_name: "abort".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![(
                            "failure".to_string(),
                            oomir::Type::Class("java/lang/Throwable".to_string()),
                        )],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![operand_var(
                        "__rust_unwind_exception",
                        oomir::Type::Class("java/lang/Throwable".to_string()),
                    )],
                },
                oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/AssertionError".to_string(),
                    message: "Rust abort unexpectedly returned".to_string(),
                },
            ],
        },
    );

    oomir::CodeBlock {
        entry: "drop_0".to_string(),
        basic_blocks,
    }
}

pub(super) fn managed_drop_actions<'tcx>(
    rust_ty: Ty<'tcx>,
    value: oomir::Operand,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Vec<Vec<oomir::Instruction>> {
    let oomir_ty = ty_to_oomir_type(rust_ty, tcx, data_types, instance_context);
    let mut actions = Vec::new();

    match rust_ty.kind() {
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            if adt_def.is_box() {
                let pointee_ty = substs.type_at(0);
                if pointee_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    let mut action = Vec::new();
                    let mut pointer = value.clone();
                    let mut pointer_ty = oomir_ty.clone();
                    let mut pointer_rust_ty = rust_ty;
                    for depth in 0..3 {
                        let class_name = pointer_ty
                            .get_class_name()
                            .expect("Box pointer carrier must be a JVM class")
                            .to_string();
                        let TyKind::Adt(carrier_def, carrier_args) = pointer_rust_ty.kind() else {
                            panic!("Box pointer carrier {pointer_rust_ty:?} is not a struct");
                        };
                        let field = carrier_def
                            .variant(VariantIdx::from_usize(0))
                            .fields
                            .iter()
                            .next()
                            .expect("Box pointer carrier must have a field");
                        let field_rust_ty = field.ty(tcx, carrier_args).skip_norm_wip();
                        let field_ty =
                            ty_to_oomir_type(field_rust_ty, tcx, data_types, instance_context);
                        let dest = format!("{temp_prefix}_box_pointer_{depth}");
                        action.push(oomir::Instruction::GetField {
                            dest: dest.clone(),
                            object: pointer,
                            field_name: field.ident(tcx).to_string(),
                            field_ty: field_ty.clone(),
                            owner_class: class_name,
                        });
                        pointer = operand_var(dest, field_ty.clone());
                        pointer_ty = field_ty;
                        pointer_rust_ty = field_rust_ty;
                    }
                    if matches!(pointee_ty.kind(), TyKind::Dynamic(..)) {
                        action.push(oomir::Instruction::InvokeStatic {
                            dest: None,
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "dropTraitPointer".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![(
                                    "pointer".to_string(),
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                )],
                                ret: Box::new(oomir::Type::Void),
                                is_static: true,
                            },
                            args: vec![pointer],
                        });
                    } else if matches!(pointee_ty.kind(), TyKind::Slice(_)) {
                        crate::lower1::control_flow::emit_rust_drop_value(
                            pointee_ty,
                            pointer,
                            &format!("{temp_prefix}_box_slice"),
                            tcx,
                            instance_context,
                            data_types,
                            &mut action,
                        );
                    } else {
                        let pointee = format!("{temp_prefix}_box_pointee");
                        action.push(oomir::Instruction::InvokeVirtual {
                            dest: Some(pointee.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "getObject".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![("self".to_string(), pointer_ty)],
                                ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                                is_static: false,
                            },
                            args: Vec::new(),
                            operand: pointer,
                        });
                        emit_managed_value_drop(
                            pointee_ty,
                            operand_var(
                                pointee,
                                oomir::Type::Class("java/lang/Object".to_string()),
                            ),
                            &format!("{temp_prefix}_box_pointee"),
                            tcx,
                            instance_context,
                            data_types,
                            &mut action,
                        );
                    }
                    actions.push(action);
                }
            }

            if adt_def.destructor(tcx).is_some() {
                actions.push(vec![oomir::Instruction::InvokeStatic {
                    class_name: oomir_ty
                        .get_class_name()
                        .expect("a Rust Drop ADT has a JVM class")
                        .to_string(),
                    method_name: "drop".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![value.clone()],
                    dest: None,
                }]);
            }

            for (field_index, field) in adt_def
                .variant(VariantIdx::from_usize(0))
                .fields
                .iter()
                .enumerate()
            {
                let field_ty = field.ty(tcx, substs).skip_norm_wip();
                if !field_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let field_oomir_ty = ty_to_oomir_type(field_ty, tcx, data_types, instance_context);
                let mut action = Vec::new();
                let field_value = if field_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_field_{field_index}");
                    action.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: field.ident(tcx).to_string(),
                        field_ty: field_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a struct field owner has a JVM class")
                            .to_string(),
                    });
                    operand_var(dest, field_oomir_ty)
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_managed_value_drop(
                    field_ty,
                    field_value,
                    &format!("{temp_prefix}_field_{field_index}"),
                    tcx,
                    instance_context,
                    data_types,
                    &mut action,
                );
                actions.push(action);
            }
        }
        TyKind::Adt(adt_def, _) if adt_def.is_enum() => {
            let enum_class = oomir_ty
                .get_class_name()
                .expect("a Rust enum has a JVM class")
                .to_string();
            if adt_def.destructor(tcx).is_some() {
                actions.push(vec![oomir::Instruction::InvokeStatic {
                    class_name: enum_class.clone(),
                    method_name: "drop".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![value.clone()],
                    dest: None,
                }]);
            }
            actions.push(vec![oomir::Instruction::InvokeVirtual {
                class_name: enum_class.clone(),
                method_name: enum_scoped_method_name(&enum_class, ENUM_DROP_FIELDS_METHOD),
                method_ty: oomir::Signature {
                    params: vec![("self".to_string(), oomir_ty)],
                    ret: Box::new(oomir::Type::Void),
                    is_static: false,
                },
                args: Vec::new(),
                dest: None,
                operand: value,
            }]);
        }
        TyKind::Tuple(fields) => {
            for (field_index, field_ty) in fields.iter().enumerate() {
                if !field_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let field_oomir_ty = ty_to_oomir_type(field_ty, tcx, data_types, instance_context);
                let mut action = Vec::new();
                let field_value = if field_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_tuple_{field_index}");
                    action.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: format!("field{field_index}"),
                        field_ty: field_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a non-empty tuple has a JVM class")
                            .to_string(),
                    });
                    operand_var(dest, field_oomir_ty)
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_managed_value_drop(
                    field_ty,
                    field_value,
                    &format!("{temp_prefix}_tuple_{field_index}"),
                    tcx,
                    instance_context,
                    data_types,
                    &mut action,
                );
                actions.push(action);
            }
        }
        TyKind::Closure(_, closure_args) => {
            for (capture_index, capture_ty) in
                closure_args.as_closure().upvar_tys().iter().enumerate()
            {
                if !capture_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let capture_oomir_ty =
                    ty_to_oomir_type(capture_ty, tcx, data_types, instance_context);
                let mut action = Vec::new();
                let capture_value = if capture_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_capture_{capture_index}");
                    action.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: format!("arg{capture_index}"),
                        field_ty: capture_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a closure with captures has a JVM class")
                            .to_string(),
                    });
                    operand_var(dest, capture_oomir_ty)
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_managed_value_drop(
                    capture_ty,
                    capture_value,
                    &format!("{temp_prefix}_capture_{capture_index}"),
                    tcx,
                    instance_context,
                    data_types,
                    &mut action,
                );
                actions.push(action);
            }
        }
        TyKind::Array(..) | TyKind::Dynamic(..) => {
            let mut action = Vec::new();
            emit_managed_value_drop(
                rust_ty,
                value,
                temp_prefix,
                tcx,
                instance_context,
                data_types,
                &mut action,
            );
            actions.push(action);
        }
        _ => {}
    }
    actions
}

pub(super) fn managed_struct_drop_fields_function<'tcx>(
    rust_ty: Ty<'tcx>,
    class_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let self_ty = oomir::Type::Class(class_name.to_string());
    let mut actions = managed_drop_actions(
        rust_ty,
        operand_var("_1", self_ty.clone()),
        "_managed_drop_fields",
        tcx,
        data_types,
        instance_context,
    );
    let TyKind::Adt(adt_def, _) = rust_ty.kind() else {
        unreachable!("managed struct drop fields require an ADT");
    };
    if adt_def.destructor(tcx).is_some() {
        actions.remove(0);
    }
    oomir::Function {
        name: ENUM_DROP_FIELDS_METHOD.to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![("self".to_string(), self_ty)],
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        body: cleanup_safe_drop_body(actions).into(),
    }
}

pub(super) fn managed_drop_glue_function<'tcx>(
    rust_ty: Ty<'tcx>,
    class_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let self_ty = oomir::Type::Class(class_name.to_string());
    let mut instructions = Vec::new();
    crate::lower1::control_flow::emit_rust_drop_value(
        rust_ty,
        operand_var("_1", self_ty.clone()),
        "_managed_drop",
        tcx,
        instance_context,
        data_types,
        &mut instructions,
    );
    instructions.push(oomir::Instruction::Return { operand: None });

    oomir::Function {
        name: MANAGED_DROP_METHOD.to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![("self".to_string(), self_ty)],
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "entry".to_string(),
            basic_blocks: HashMap::from_iter([(
                "entry".to_string(),
                oomir::BasicBlock {
                    label: "entry".to_string(),
                    instructions,
                },
            )]),
        }
        .into(),
    }
}
