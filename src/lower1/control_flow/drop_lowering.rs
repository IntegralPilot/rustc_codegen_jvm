//! Drop lowering.
use super::*;

pub(in crate::lower1) fn emit_rust_drop_value<'tcx>(
    rust_ty: Ty<'tcx>,
    mut value: oomir::Operand,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    let instantiated = EarlyBinder::bind(tcx, rust_ty).instantiate(tcx, instance.args);
    let rust_ty = tcx
        .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), instantiated)
        .unwrap_or_else(|_| instantiated.skip_norm_wip());
    let oomir_ty = crate::lower1::types::ty_to_oomir_type(rust_ty, tcx, data_types, instance);
    if !oomir_ty.has_jvm_value() {
        let Some(materialized) = crate::lower1::value_repr::materialize_implicit_zst(
            rust_ty,
            &format!("{temp_prefix}_zst"),
            tcx,
            instance,
            data_types,
            instructions,
        ) else {
            return;
        };
        value = materialized;
    }

    match rust_ty.kind() {
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            if adt_def.is_box() {
                let pointee_ty = substs.type_at(0);
                if pointee_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    let mut pointer = value.clone();
                    let mut pointer_ty = oomir_ty.clone();
                    let mut pointer_rust_ty = rust_ty;
                    for depth in 0..3 {
                        if matches!(
                            pointer_ty,
                            oomir::Type::Pointer(_) | oomir::Type::Slice(_) | oomir::Type::Str
                        ) {
                            break;
                        }
                        let class_name = pointer_ty
                            .get_class_name()
                            .expect("Box pointer carrier must be a JVM class")
                            .to_string();
                        let TyKind::Adt(carrier_def, carrier_args) = pointer_rust_ty.kind() else {
                            panic!("Box pointer carrier {pointer_rust_ty:?} is not a struct");
                        };
                        let field = carrier_def
                            .variant(rustc_abi::VariantIdx::from_usize(0))
                            .fields
                            .iter()
                            .next()
                            .expect("Box pointer carrier must have a field");
                        let field_name = field.ident(tcx).to_string();
                        let field_rust_ty = field.ty(tcx, carrier_args).skip_norm_wip();
                        let field_ty = crate::lower1::types::ty_to_oomir_type(
                            field_rust_ty,
                            tcx,
                            data_types,
                            instance,
                        );
                        let dest = format!("{temp_prefix}_box_pointer_{depth}");
                        instructions.push(oomir::Instruction::GetField {
                            dest: dest.clone(),
                            object: pointer,
                            field_name,
                            field_ty: field_ty.clone(),
                            owner_class: class_name,
                        });
                        pointer = oomir::Operand::Variable {
                            name: dest,
                            ty: field_ty.clone(),
                        };
                        pointer_ty = field_ty;
                        pointer_rust_ty = field_rust_ty;
                    }
                    let drop_instance = Instance::resolve_drop_glue(tcx, pointee_ty);
                    let target =
                        crate::lower1::naming::mono_fn_name_from_instance(tcx, drop_instance);
                    let pointee_signature = oomir::Signature {
                        params: vec![("pointee".to_string(), pointer_ty)],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    };
                    let box_class = oomir_ty
                        .get_class_name()
                        .expect("Box has a JVM class")
                        .to_string();
                    let box_signature = oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    };
                    let (pointee_owner, pointee_method, pointee_descriptor) =
                        if matches!(pointee_ty.kind(), TyKind::Dynamic(..)) {
                            (String::new(), String::new(), String::new())
                        } else {
                            (
                                target
                                    .class_to_call_on
                                    .expect("Box pointee drop glue has a JVM owner"),
                                target.method_name,
                                pointee_signature.to_jvm_descriptor_with_explicit_params(),
                            )
                        };
                    instructions.push(oomir::Instruction::InvokeStatic {
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "dropBoxWithCleanup".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                (
                                    "pointee".to_string(),
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                ),
                                (
                                    "box".to_string(),
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                ),
                                ("pointee_owner".to_string(), oomir::Type::java_string()),
                                ("pointee_method".to_string(), oomir::Type::java_string()),
                                ("pointee_descriptor".to_string(), oomir::Type::java_string()),
                                ("box_owner".to_string(), oomir::Type::java_string()),
                                ("box_method".to_string(), oomir::Type::java_string()),
                                ("box_descriptor".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(oomir::Type::Void),
                            is_static: true,
                        },
                        args: vec![
                            pointer,
                            value,
                            oomir::Operand::Constant(oomir::Constant::String(pointee_owner)),
                            oomir::Operand::Constant(oomir::Constant::String(pointee_method)),
                            oomir::Operand::Constant(oomir::Constant::String(pointee_descriptor)),
                            oomir::Operand::Constant(oomir::Constant::String(box_class)),
                            oomir::Operand::Constant(oomir::Constant::String("drop".to_string())),
                            oomir::Operand::Constant(oomir::Constant::String(
                                box_signature.to_jvm_descriptor_with_explicit_params(),
                            )),
                        ],
                        dest: None,
                    });
                    return;
                }
            }

            if adt_def.destructor(tcx).is_some() {
                let class_name = oomir_ty
                    .get_class_name()
                    .expect("a Rust Drop ADT has a JVM class")
                    .to_string();
                if !adt_def.is_box() {
                    let drop_signature = oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    };
                    instructions.push(oomir::Instruction::InvokeStatic {
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "dropAdtWithCleanup".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                (
                                    "value".to_string(),
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                ),
                                ("owner".to_string(), oomir::Type::java_string()),
                                ("drop_method".to_string(), oomir::Type::java_string()),
                                ("drop_descriptor".to_string(), oomir::Type::java_string()),
                                ("fields_method".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(oomir::Type::Void),
                            is_static: true,
                        },
                        args: vec![
                            value,
                            oomir::Operand::Constant(oomir::Constant::String(class_name)),
                            oomir::Operand::Constant(oomir::Constant::String("drop".to_string())),
                            oomir::Operand::Constant(oomir::Constant::String(
                                drop_signature.to_jvm_descriptor_with_explicit_params(),
                            )),
                            oomir::Operand::Constant(oomir::Constant::String(
                                "_rust_drop_fields".to_string(),
                            )),
                        ],
                        dest: None,
                    });
                    return;
                }
                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name,
                    method_name: "drop".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![value.clone()],
                    dest: None,
                });
            }

            for (field_index, field) in adt_def
                .variant(rustc_abi::VariantIdx::from_usize(0))
                .fields
                .iter()
                .enumerate()
            {
                let field_rust_ty = field.ty(tcx, substs).skip_norm_wip();
                if !field_rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let field_oomir_ty = crate::lower1::types::ty_to_oomir_type(
                    field_rust_ty,
                    tcx,
                    data_types,
                    instance,
                );
                let field_value = if field_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_field_{field_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: field.ident(tcx).to_string(),
                        field_ty: field_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a struct field owner has a JVM class")
                            .to_string(),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: field_oomir_ty,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    field_rust_ty,
                    field_value,
                    &format!("{temp_prefix}_field_{field_index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Tuple(fields) => {
            for (field_index, field_rust_ty) in fields.iter().enumerate() {
                if !field_rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let field_oomir_ty = crate::lower1::types::ty_to_oomir_type(
                    field_rust_ty,
                    tcx,
                    data_types,
                    instance,
                );
                let field_value = if field_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_tuple_{field_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: format!("field{field_index}"),
                        field_ty: field_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a non-empty tuple has a JVM class")
                            .to_string(),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: field_oomir_ty,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    field_rust_ty,
                    field_value,
                    &format!("{temp_prefix}_tuple_{field_index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Closure(_, closure_args) => {
            for (capture_index, capture_rust_ty) in
                closure_args.as_closure().upvar_tys().iter().enumerate()
            {
                if !capture_rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let capture_oomir_ty = crate::lower1::types::ty_to_oomir_type(
                    capture_rust_ty,
                    tcx,
                    data_types,
                    instance,
                );
                let capture_value = if capture_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_capture_{capture_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: format!("arg{capture_index}"),
                        field_ty: capture_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a closure with captures has a JVM class")
                            .to_string(),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: capture_oomir_ty,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    capture_rust_ty,
                    capture_value,
                    &format!("{temp_prefix}_capture_{capture_index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Array(element_ty, length) => {
            let Some(length) = length.try_to_target_usize(tcx) else {
                return;
            };
            if !element_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                return;
            }
            let element_oomir_ty =
                crate::lower1::types::ty_to_oomir_type(*element_ty, tcx, data_types, instance);
            for index in 0..length {
                let element_value = if element_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_array_{index}");
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: dest.clone(),
                        array: value.clone(),
                        index: oomir::Operand::Constant(oomir::Constant::I32(index as i32)),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: element_oomir_ty.clone(),
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    *element_ty,
                    element_value,
                    &format!("{temp_prefix}_array_{index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Slice(element_ty) => {
            if !element_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                return;
            }
            let drop_instance = Instance::resolve_drop_glue(tcx, *element_ty);
            let target = crate::lower1::naming::mono_fn_name_from_instance(tcx, drop_instance);
            let drop_mir = tcx.instance_mir(drop_instance.def);
            let drop_param_ty = EarlyBinder::bind(
                tcx,
                drop_mir.local_decls[rustc_middle::mir::Local::from_usize(1)].ty,
            )
            .instantiate(tcx, drop_instance.args)
            .skip_norm_wip();
            let element_oomir_ty = crate::lower1::types::ty_to_oomir_type(
                drop_param_ty,
                tcx,
                data_types,
                drop_instance,
            );
            let drop_signature = oomir::Signature {
                params: vec![("element".to_string(), element_oomir_ty)],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            };
            instructions.push(oomir::Instruction::InvokeStatic {
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "dropSlice".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        (
                            "slice".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("owner".to_string(), oomir::Type::java_string()),
                        ("method".to_string(), oomir::Type::java_string()),
                        ("descriptor".to_string(), oomir::Type::java_string()),
                        ("element_size".to_string(), oomir::Type::U64),
                        ("element_codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir::Type::Void),
                    is_static: true,
                },
                args: vec![
                    value,
                    oomir::Operand::Constant(oomir::Constant::String(
                        target.class_to_call_on.expect("drop glue has a JVM owner"),
                    )),
                    oomir::Operand::Constant(oomir::Constant::String(target.method_name)),
                    oomir::Operand::Constant(oomir::Constant::String(
                        drop_signature.to_jvm_descriptor_with_explicit_params(),
                    )),
                    oomir::Operand::Constant(oomir::Constant::U64(
                        u64::try_from(
                            crate::lower1::types::layout_size_bytes(tcx, *element_ty)
                                .unwrap_or_else(|error| {
                                    panic!("could not determine slice drop element size: {error}")
                                }),
                        )
                        .expect("slice drop element size exceeds the JVM address space"),
                    )),
                    crate::lower1::types::pointer_view_codec_operand(
                        *element_ty,
                        tcx,
                        data_types,
                        instance,
                    ),
                ],
                dest: None,
            });
        }
        TyKind::Coroutine(..) => {
            let pointer_ty = oomir::Type::Pointer(Box::new(oomir_ty.clone()));
            let pointer_name = format!("{temp_prefix}_coroutine_pointer");
            let size = crate::lower1::types::layout_size_bytes(tcx, rust_ty)
                .unwrap_or_else(|error| panic!("could not size coroutine drop value: {error}"));
            let alignment = crate::lower1::types::layout_align_bytes(tcx, rust_ty)
                .unwrap_or_else(|error| panic!("could not align coroutine drop value: {error}"));
            let codec = crate::lower1::types::pointer_memory_codec_operand(
                rust_ty, tcx, data_types, instance,
            );
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(pointer_name.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "receiverCellAligned".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        (
                            "value".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("size".to_string(), oomir::Type::I32),
                        ("codec".to_string(), oomir::Type::java_string()),
                        ("alignment".to_string(), oomir::Type::I32),
                    ],
                    ret: Box::new(pointer_ty.clone()),
                    is_static: true,
                },
                args: vec![
                    value,
                    oomir::Operand::Constant(oomir::Constant::I32(
                        i32::try_from(size)
                            .expect("coroutine drop value exceeds the JVM address space"),
                    )),
                    codec,
                    oomir::Operand::Constant(oomir::Constant::I32(
                        i32::try_from(alignment)
                            .expect("coroutine drop alignment exceeds the JVM address space"),
                    )),
                ],
            });
            let drop_instance = Instance::resolve_drop_glue(tcx, rust_ty);
            let target = crate::lower1::naming::mono_fn_name_from_instance(tcx, drop_instance);
            instructions.push(oomir::Instruction::InvokeStatic {
                class_name: target
                    .class_to_call_on
                    .expect("coroutine drop glue has a JVM owner"),
                method_name: target.method_name,
                method_ty: oomir::Signature {
                    params: vec![("coroutine".to_string(), pointer_ty.clone())],
                    ret: Box::new(oomir::Type::Void),
                    is_static: true,
                },
                args: vec![oomir::Operand::Variable {
                    name: pointer_name,
                    ty: pointer_ty,
                }],
                dest: None,
            });
        }
        TyKind::Dynamic(..) => {
            instructions.push(oomir::Instruction::InvokeStatic {
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
                dest: None,
            });
        }
        // Rust unions require ManuallyDrop fields. Enums use a virtual helper
        // generated on every variant so the active payload is destroyed without
        // trying to reinterpret the JVM subclass as an inactive variant.
        TyKind::Adt(adt_def, substs) if adt_def.is_enum() => {
            crate::lower1::types::force_define_named_adt(rust_ty, tcx, data_types, instance);
            let class_name = crate::lower1::types::generate_adt_jvm_class_name(
                adt_def, substs, tcx, data_types, instance,
            );
            if adt_def.destructor(tcx).is_some() {
                let drop_signature = oomir::Signature {
                    params: vec![("self".to_string(), oomir_ty.clone())],
                    ret: Box::new(oomir::Type::Void),
                    is_static: true,
                };
                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name: oomir::POINTER_CLASS.to_string(),
                    method_name: "dropAdtWithCleanup".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![
                            (
                                "value".to_string(),
                                oomir::Type::Class("java/lang/Object".to_string()),
                            ),
                            ("owner".to_string(), oomir::Type::java_string()),
                            ("drop_method".to_string(), oomir::Type::java_string()),
                            ("drop_descriptor".to_string(), oomir::Type::java_string()),
                            ("fields_method".to_string(), oomir::Type::java_string()),
                        ],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![
                        value,
                        oomir::Operand::Constant(oomir::Constant::String(class_name.clone())),
                        oomir::Operand::Constant(oomir::Constant::String("drop".to_string())),
                        oomir::Operand::Constant(oomir::Constant::String(
                            drop_signature.to_jvm_descriptor_with_explicit_params(),
                        )),
                        oomir::Operand::Constant(oomir::Constant::String(enum_scoped_method_name(
                            &class_name,
                            "_rust_drop_fields",
                        ))),
                    ],
                    dest: None,
                });
                return;
            }
            instructions.push(oomir::Instruction::InvokeVirtual {
                class_name: class_name.clone(),
                method_name: enum_scoped_method_name(&class_name, "_rust_drop_fields"),
                method_ty: oomir::Signature {
                    params: vec![("self".to_string(), oomir_ty)],
                    ret: Box::new(oomir::Type::Void),
                    is_static: false,
                },
                args: Vec::new(),
                dest: None,
                operand: value,
            });
        }
        _ => {}
    }
}
