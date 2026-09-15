//! Emit conversions after callable, pointer and Rust layout analysis.
use super::*;

pub(super) struct PreparedCast<'a, 'tcx> {
    pub(super) tcx: TyCtxt<'tcx>,
    pub(super) instance: Instance<'tcx>,
    pub(super) data_types: &'a mut Definitions<'tcx>,
    pub(super) cast_kind: &'a CastKind,
    pub(super) target_mir_ty: &'a Ty<'tcx>,
    pub(super) source_mir_ty: Ty<'tcx>,
    pub(super) resolved_target_mir_ty: Ty<'tcx>,
    pub(super) oomir_operand: oomir::Operand,
    pub(super) oomir_source_type: oomir::Type,
    pub(super) oomir_target_type: oomir::Type,
    pub(super) temp_cast_var: String,
    pub(super) instructions: Vec<oomir::Instruction>,
    pub(super) callable_closure_bridge: bool,
    pub(super) callable_fn_def_adapter: Option<String>,
    pub(super) trait_object_adapter: Option<String>,
}

impl<'tcx> PreparedCast<'_, 'tcx> {
    pub(super) fn lower(self) -> (Vec<oomir::Instruction>, oomir::Operand) {
        let Self {
            tcx,
            instance,
            data_types,
            cast_kind,
            target_mir_ty,
            source_mir_ty,
            resolved_target_mir_ty,
            oomir_operand,
            oomir_source_type,
            oomir_target_type,
            temp_cast_var,
            mut instructions,
            callable_closure_bridge,
            callable_fn_def_adapter,
            trait_object_adapter,
        } = self;
        let result_operand;
        let exact_transmute_helper = matches!(cast_kind, CastKind::Transmute)
        .then(|| {
            ensure_exact_transmute_helper(
                source_mir_ty,
                *target_mir_ty,
                tcx,
                data_types,
                instance,
            )
        })
        .transpose()
        .unwrap_or_else(|error| {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "mir-lowering",
                format!(
                    "Exact-layout transmute codec is unavailable for {source_mir_ty:?} -> {target_mir_ty:?}: {error}"
                )
            );
            None
        });

        if callable_closure_bridge {
            instructions.push(oomir::Instruction::Cast {
                op: oomir_operand,
                ty: oomir_target_type.clone(),
                dest: temp_cast_var.clone(),
            });
        } else if let Some(adapter_class) = callable_fn_def_adapter {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: temp_cast_var.clone(),
                class_name: adapter_class,
                args: Vec::new(),
            });
        } else if let Some(adapter_class) = trait_object_adapter {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: temp_cast_var.clone(),
                class_name: adapter_class,
                args: vec![(oomir_operand, oomir_source_type.clone())],
            });
        } else if let Some(helper) = exact_transmute_helper {
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: oomir_target_type
                    .has_jvm_value()
                    .then(|| temp_cast_var.clone()),
                class_name: helper.class_name,
                method_name: helper.method_name,
                method_ty: helper.signature,
                args: oomir_source_type
                    .has_jvm_value()
                    .then_some(oomir_operand)
                    .into_iter()
                    .collect(),
            });
        } else if matches!(cast_kind, CastKind::Transmute)
            && oomir_source_type == oomir::Type::Class(crate::lower2::F128_CLASS.to_string())
            && oomir_target_type == oomir::Type::Class(crate::lower2::U128_CLASS.to_string())
        {
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: Some(temp_cast_var.clone()),
                class_name: crate::lower2::F128_CLASS.to_string(),
                method_name: "toU128".to_string(),
                method_ty: oomir::Signature {
                    params: Vec::new(),
                    ret: Box::new(oomir_target_type.clone()),
                    is_static: false,
                },
                args: Vec::new(),
                operand: oomir_operand,
            });
        } else if matches!(cast_kind, CastKind::Transmute)
            && oomir_source_type == oomir::Type::Class(crate::lower2::U128_CLASS.to_string())
            && oomir_target_type == oomir::Type::Class(crate::lower2::F128_CLASS.to_string())
        {
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(temp_cast_var.clone()),
                class_name: crate::lower2::F128_CLASS.to_string(),
                method_name: "fromU128".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("bits".to_string(), oomir_source_type.clone())],
                    ret: Box::new(oomir_target_type.clone()),
                    is_static: true,
                },
                args: vec![oomir_operand],
            });
        } else if matches!(&oomir_source_type, oomir::Type::Slice(_))
            && matches!(&oomir_target_type, oomir::Type::Slice(_))
            && pointer_pointee_ty(source_mir_ty).is_slice()
            && pointer_pointee_ty(*target_mir_ty).is_slice()
            && pointer_pointee_ty(source_mir_ty).sequence_element_type(tcx)
                != pointer_pointee_ty(*target_mir_ty).sequence_element_type(tcx)
        {
            let source_element = EarlyBinder::bind(
                tcx,
                pointer_pointee_ty(source_mir_ty).sequence_element_type(tcx),
            )
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
            let target_element = EarlyBinder::bind(
                tcx,
                pointer_pointee_ty(*target_mir_ty).sequence_element_type(tcx),
            )
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
            let source_element_oomir = ty_to_oomir_type(source_element, tcx, data_types, instance);
            let target_element_oomir = ty_to_oomir_type(target_element, tcx, data_types, instance);
            let source_pointer_ty = oomir::Type::Pointer(Box::new(source_element_oomir));
            let target_pointer_ty = oomir::Type::Pointer(Box::new(target_element_oomir));
            let data_name = format!("{temp_cast_var}_slice_data");
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(data_name.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "fromSlice".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        (
                            "slice".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("element_size".to_string(), oomir::Type::U64),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(source_pointer_ty.clone()),
                    is_static: true,
                },
                args: vec![
                    oomir_operand.clone(),
                    rust_layout_size_operand(source_element, tcx, instance),
                    crate::lower1::types::pointer_view_codec_operand(
                        source_element,
                        tcx,
                        data_types,
                        instance,
                    ),
                ],
            });
            let retyped_name = format!("{temp_cast_var}_slice_retyped");
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: Some(retyped_name.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "retype".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("self".to_string(), source_pointer_ty.clone()),
                        ("view_size".to_string(), oomir::Type::U64),
                        ("view_codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(target_pointer_ty.clone()),
                    is_static: false,
                },
                args: vec![
                    rust_layout_size_operand(target_element, tcx, instance),
                    crate::lower1::types::pointer_view_codec_operand(
                        target_element,
                        tcx,
                        data_types,
                        instance,
                    ),
                ],
                operand: oomir::Operand::Variable {
                    name: data_name,
                    ty: source_pointer_ty,
                },
            });
            let length_name = format!("{temp_cast_var}_slice_length");
            instructions.push(oomir::Instruction::GetField {
                dest: length_name.clone(),
                object: oomir_operand,
                field_name: "rustLength".to_string(),
                field_ty: oomir::Type::U64,
                owner_class: oomir::SLICE_VIEW_CLASS.to_string(),
            });
            let slice_object_name = format!("{temp_cast_var}_slice_object");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: slice_object_name.clone(),
                class_name: oomir::SLICE_VIEW_CLASS.to_string(),
                args: vec![
                    (
                        oomir::Operand::Variable {
                            name: retyped_name,
                            ty: target_pointer_ty,
                        },
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    (
                        oomir::Operand::Constant(oomir::Constant::I32(0)),
                        oomir::Type::I32,
                    ),
                    (
                        oomir::Operand::Variable {
                            name: length_name,
                            ty: oomir::Type::U64,
                        },
                        oomir::Type::U64,
                    ),
                ],
            });
            instructions.push(oomir::Instruction::Cast {
                op: oomir::Operand::Variable {
                    name: slice_object_name,
                    ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
                },
                ty: oomir_target_type.clone(),
                dest: temp_cast_var.clone(),
            });
        } else if matches!(oomir_source_type, oomir::Type::Slice(_) | oomir::Type::Str)
            && matches!(oomir_target_type, oomir::Type::Pointer(_))
        {
            let source_pointee = match source_mir_ty.kind() {
                TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                    if pointee.is_slice() {
                        pointee.sequence_element_type(tcx)
                    } else if pointee.is_str() {
                        tcx.types.u8
                    } else {
                        pointer_pointee_ty(*target_mir_ty)
                    }
                }
                _ => pointer_pointee_ty(*target_mir_ty),
            };
            let source_element_size = crate::lower1::types::layout_size_bytes(tcx, source_pointee)
                .expect("fat pointer element has a concrete layout");
            let data_pointer = format!("{temp_cast_var}_fat_data");
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(data_pointer.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "fromSlice".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        (
                            "slice".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("element_size".to_string(), oomir::Type::U64),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir_target_type.clone()),
                    is_static: true,
                },
                args: vec![
                    oomir_operand,
                    oomir::Operand::Constant(oomir::Constant::U64(
                        u64::try_from(source_element_size)
                            .expect("Rust slice element layout exceeds u64"),
                    )),
                    crate::lower1::types::pointer_view_codec_operand(
                        source_pointee,
                        tcx,
                        data_types,
                        instance,
                    ),
                ],
            });
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: Some(temp_cast_var.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "retype".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("self".to_string(), oomir_target_type.clone()),
                        ("view_size".to_string(), oomir::Type::U64),
                        ("view_codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir_target_type.clone()),
                    is_static: false,
                },
                args: vec![
                    pointer_view_size_operand(*target_mir_ty, tcx, instance),
                    crate::lower1::types::pointer_view_codec_operand(
                        pointer_pointee_ty(*target_mir_ty),
                        tcx,
                        data_types,
                        instance,
                    ),
                ],
                operand: oomir::Operand::Variable {
                    name: data_pointer,
                    ty: oomir_target_type.clone(),
                },
            });
        } else if matches!(
            cast_kind,
            CastKind::PointerCoercion(PointerCoercion::Unsize, _)
        ) && matches!(source_mir_ty.kind(), TyKind::Adt(..))
            && emit_unsize_value(
                source_mir_ty,
                *target_mir_ty,
                oomir_operand.clone(),
                &temp_cast_var,
                tcx,
                instance,
                data_types,
                &mut instructions,
            )
            .is_some()
        {
            // The recursive helper emitted the complete target wrapper.
        } else if matches!(
            cast_kind,
            CastKind::PointerCoercion(PointerCoercion::Unsize, _)
        ) && emit_struct_trait_tail_pointer_unsize(
            source_mir_ty,
            resolved_target_mir_ty,
            oomir_operand.clone(),
            &temp_cast_var,
            tcx,
            instance,
            data_types,
            &mut instructions,
        )
        .is_some()
        {
            // The helper emitted the outer pointer and the tail vtable.
        } else if matches!(
            cast_kind,
            CastKind::PointerCoercion(PointerCoercion::Unsize, _)
        ) && let Some(target_class) = struct_tail_unsize_target_class(
            source_mir_ty,
            resolved_target_mir_ty,
            &oomir_source_type,
            &oomir_target_type,
            tcx,
            instance,
        ) {
            let source_pointee =
                normalize_unsize_ty(pointer_pointee_ty(source_mir_ty), tcx, instance);
            let source_tail =
                tcx.struct_tail_for_codegen(source_pointee, TypingEnv::fully_monomorphized());
            let TyKind::Array(element_ty, length) = source_tail.kind() else {
                unreachable!("struct-tail unsizing source was validated")
            };
            let length = length
                .try_to_target_usize(tcx)
                .expect("struct-tail array length is concrete");
            let target_pointee =
                normalize_unsize_ty(pointer_pointee_ty(resolved_target_mir_ty), tcx, instance);
            let target_tail =
                tcx.struct_tail_for_codegen(target_pointee, TypingEnv::fully_monomorphized());
            let tail_view_class = if target_tail.is_str() {
                oomir::UTF8_VIEW_CLASS
            } else {
                oomir::SLICE_VIEW_CLASS
            };
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(temp_cast_var.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "unsizeStructTail".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("pointer".to_string(), oomir_source_type.clone()),
                        ("prefix_size".to_string(), oomir::Type::U64),
                        ("target_class".to_string(), oomir::Type::java_string()),
                        ("tail_view_class".to_string(), oomir::Type::java_string()),
                        ("element_size".to_string(), oomir::Type::U64),
                        ("element_codec".to_string(), oomir::Type::java_string()),
                        ("length".to_string(), oomir::Type::U64),
                    ],
                    ret: Box::new(oomir_target_type.clone()),
                    is_static: true,
                },
                args: vec![
                    oomir_operand,
                    pointer_view_size_operand(resolved_target_mir_ty, tcx, instance),
                    oomir::Operand::Constant(oomir::Constant::String(target_class)),
                    oomir::Operand::Constant(oomir::Constant::String(tail_view_class.to_string())),
                    rust_layout_size_operand(*element_ty, tcx, instance),
                    crate::lower1::types::pointer_view_codec_operand(
                        *element_ty,
                        tcx,
                        data_types,
                        instance,
                    ),
                    oomir::Operand::Constant(oomir::Constant::U64(length)),
                ],
            });
        } else if matches!(oomir_source_type, oomir::Type::Pointer(_))
            && matches!(oomir_target_type, oomir::Type::Pointer(_))
        {
            if let Some(pointer) = emit_struct_tail_reborrow_view(
                pointer_pointee_ty(resolved_target_mir_ty),
                oomir_operand.clone(),
                &oomir_target_type,
                &temp_cast_var,
                tcx,
                instance,
                data_types,
                &mut instructions,
            ) {
                return (instructions, pointer);
            }
            if let Some(target_class) =
                struct_tail_pointer_target_class(*target_mir_ty, &oomir_target_type, tcx, instance)
            {
                instructions.push(oomir::Instruction::InvokeStatic {
                    dest: Some(temp_cast_var.clone()),
                    class_name: oomir::POINTER_CLASS.to_string(),
                    method_name: "unsizeStruct".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![
                            ("pointer".to_string(), oomir_source_type),
                            ("view_size".to_string(), oomir::Type::U64),
                            ("target_class".to_string(), oomir::Type::java_string()),
                        ],
                        ret: Box::new(oomir_target_type.clone()),
                        is_static: true,
                    },
                    args: vec![
                        oomir_operand,
                        pointer_view_size_operand(*target_mir_ty, tcx, instance),
                        oomir::Operand::Constant(oomir::Constant::String(target_class)),
                    ],
                });
                return (
                    instructions,
                    oomir::Operand::Variable {
                        name: temp_cast_var,
                        ty: oomir_target_type,
                    },
                );
            }
            if matches!(
                pointer_pointee_ty(*target_mir_ty).kind(),
                TyKind::Dynamic(_, _)
            ) {
                instructions.push(oomir::Instruction::InvokeVirtual {
                    dest: Some(temp_cast_var.clone()),
                    class_name: oomir::POINTER_CLASS.to_string(),
                    method_name: "retype".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![
                            ("self".to_string(), oomir_source_type),
                            ("view_size".to_string(), oomir::Type::U64),
                        ],
                        ret: Box::new(oomir_target_type.clone()),
                        is_static: false,
                    },
                    args: vec![oomir::Operand::Constant(oomir::Constant::U64(0))],
                    operand: oomir_operand,
                });
                return (
                    instructions,
                    oomir::Operand::Variable {
                        name: temp_cast_var,
                        ty: oomir_target_type,
                    },
                );
            }
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: Some(temp_cast_var.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "retype".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("self".to_string(), oomir_source_type.clone()),
                        ("view_size".to_string(), oomir::Type::U64),
                        ("view_codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir_target_type.clone()),
                    is_static: false,
                },
                args: vec![
                    pointer_view_size_operand(*target_mir_ty, tcx, instance),
                    crate::lower1::types::pointer_view_codec_operand(
                        pointer_pointee_ty(*target_mir_ty),
                        tcx,
                        data_types,
                        instance,
                    ),
                ],
                operand: oomir_operand,
            });
        } else if matches!(
            (&oomir_source_type, &oomir_target_type),
            (oomir::Type::Str, oomir::Type::Slice(element_type))
                if matches!(element_type.as_ref(), oomir::Type::I16)
        ) {
            instructions.push(oomir::Instruction::InvokeStatic {
                class_name: oomir::UTF8_VIEW_CLASS.to_string(),
                method_name: "asSlice".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("value".to_string(), oomir::Type::Str)],
                    ret: Box::new(oomir_target_type.clone()),
                    is_static: true,
                },
                args: vec![oomir_operand],
                dest: Some(temp_cast_var.clone()),
            });
        } else if matches!(
            (&oomir_source_type, &oomir_target_type),
            (oomir::Type::Slice(element_type), oomir::Type::Str)
                if matches!(element_type.as_ref(), oomir::Type::I16)
        ) {
            instructions.push(oomir::Instruction::InvokeStatic {
                class_name: oomir::UTF8_VIEW_CLASS.to_string(),
                method_name: "fromSlice".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("value".to_string(), oomir_source_type.clone())],
                    ret: Box::new(oomir::Type::Str),
                    is_static: true,
                },
                args: vec![oomir_operand],
                dest: Some(temp_cast_var.clone()),
            });
        } else if matches!(oomir_target_type, oomir::Type::Slice(_))
            && (matches!(
                oomir_source_type,
                oomir::Type::Array(_) | oomir::Type::Slice(_)
            ) || matches!(
                &oomir_source_type,
                oomir::Type::MutableReference(inner)
                    if matches!(inner.as_ref(), oomir::Type::Array(_))
            ) || matches!(
                &oomir_source_type,
                oomir::Type::Pointer(inner)
                    if matches!(inner.as_ref(), oomir::Type::Array(_))
            ))
        {
            let slice_source = match &oomir_source_type {
                oomir::Type::Pointer(inner) if matches!(inner.as_ref(), oomir::Type::Array(_)) => {
                    let source_array_ty = pointer_pointee_ty(source_mir_ty);
                    let TyKind::Array(element_rust_ty, length) = source_array_ty.kind() else {
                        unreachable!("array pointer OOMIR carrier has non-array Rust pointee")
                    };
                    let element_oomir_ty = match &oomir_target_type {
                        oomir::Type::Slice(element) => element.as_ref().clone(),
                        _ => unreachable!(),
                    };
                    let element_pointer_ty = oomir::Type::Pointer(Box::new(element_oomir_ty));
                    let element_pointer_name = format!("{temp_cast_var}_element_pointer");
                    instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(element_pointer_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "retype".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("pointer".to_string(), oomir_source_type.clone()),
                                ("view_size".to_string(), oomir::Type::U64),
                                ("view_codec".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(element_pointer_ty.clone()),
                            is_static: true,
                        },
                        args: vec![
                            oomir_operand,
                            rust_layout_size_operand(*element_rust_ty, tcx, instance),
                            crate::lower1::types::pointer_view_codec_operand(
                                *element_rust_ty,
                                tcx,
                                data_types,
                                instance,
                            ),
                        ],
                    });
                    let length = EarlyBinder::bind(tcx, *length)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip()
                        .try_to_target_usize(tcx)
                        .expect("array-to-slice coercion length must be concrete");
                    let slice_object_name = format!("{temp_cast_var}_slice_object");
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: slice_object_name.clone(),
                        class_name: oomir::SLICE_VIEW_CLASS.to_string(),
                        args: vec![
                            (
                                oomir::Operand::Variable {
                                    name: element_pointer_name,
                                    ty: element_pointer_ty,
                                },
                                oomir::Type::Class("java/lang/Object".to_string()),
                            ),
                            (
                                oomir::Operand::Constant(oomir::Constant::I32(0)),
                                oomir::Type::I32,
                            ),
                            (
                                oomir::Operand::Constant(oomir::Constant::U64(length)),
                                oomir::Type::U64,
                            ),
                        ],
                    });
                    instructions.push(oomir::Instruction::Cast {
                        op: oomir::Operand::Variable {
                            name: slice_object_name,
                            ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
                        },
                        ty: oomir_target_type.clone(),
                        dest: temp_cast_var.clone(),
                    });
                    None
                }
                oomir::Type::MutableReference(inner)
                    if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                {
                    let unwrapped_name = format!("{}_array", temp_cast_var);
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: unwrapped_name.clone(),
                        array: oomir_operand,
                        index: oomir::Operand::Constant(oomir::Constant::I32(0)),
                    });
                    Some((
                        oomir::Operand::Variable {
                            name: unwrapped_name,
                            ty: inner.as_ref().clone(),
                        },
                        inner.as_ref().clone(),
                    ))
                }
                _ => Some((oomir_operand, oomir_source_type.clone())),
            };
            if let Some((slice_source, slice_source_type)) = slice_source {
                emit_slice_view(
                    slice_source,
                    &slice_source_type,
                    0,
                    0,
                    true,
                    &temp_cast_var,
                    &mut instructions,
                );
            }
        } else if let oomir::Type::Class(class_name) = &oomir_target_type
            && oomir::is_non_null_class_name(class_name)
        {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "mir-lowering",
                "Info: Handling Rvalue::Cast to NonNull wrapper."
            );
            let mut constructor_args = Vec::new();
            if let Some(oomir::DataType::Class { fields, .. }) = data_types.get(class_name) {
                if let Some((_field_name, field_ty)) = fields.first().cloned() {
                    let value_ty = oomir_operand.get_type().unwrap_or(oomir_source_type);
                    let needs_cast = field_ty != value_ty && !field_ty.same_jvm_type(&value_ty);
                    let erased_object_field = field_ty.to_jvm_descriptor() == "Ljava/lang/Object;";
                    let constructor_arg_ty = if erased_object_field {
                        oomir::Type::Class("java/lang/Object".to_string())
                    } else {
                        field_ty.clone()
                    };

                    let value_operand = if matches!(source_mir_ty.kind(), TyKind::Ref(..))
                        && erased_object_field
                        && value_ty.has_jvm_value()
                    {
                        // Sized references already are stable
                        // Pointer objects and can be carried
                        // through NonNull's erased Object field
                        // without another wrapper allocation.
                        oomir_operand
                    } else if erased_object_field && value_ty.is_jvm_reference_type() {
                        oomir_operand
                    } else if needs_cast
                        && value_ty.is_jvm_primitive_like()
                        && field_ty.is_jvm_reference_type()
                    {
                        oomir::Operand::Constant(oomir::Constant::Null(field_ty.clone()))
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

                    constructor_args.push((value_operand, constructor_arg_ty));
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
        result_operand = if oomir_target_type.has_jvm_value() {
            oomir::Operand::Variable {
                name: temp_cast_var,
                ty: oomir_target_type,
            }
        } else {
            oomir::Operand::Constant(oomir::Constant::Unit)
        };
        (instructions, result_operand)
    }
}
