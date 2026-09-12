use super::*;

pub(super) fn struct_tail_unsize_target_class<'tcx>(
    source_pointer_ty: rustc_middle::ty::Ty<'tcx>,
    target_pointer_ty: rustc_middle::ty::Ty<'tcx>,
    source_oomir_ty: &oomir::Type,
    target_oomir_ty: &oomir::Type,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Option<String> {
    let (source_pointee, target_pointee) =
        match (source_pointer_ty.kind(), target_pointer_ty.kind()) {
            (
                TyKind::Ref(_, source, _) | TyKind::RawPtr(source, _),
                TyKind::Ref(_, target, _) | TyKind::RawPtr(target, _),
            ) => (*source, *target),
            _ => return None,
        };
    let source_pointee = EarlyBinder::bind(tcx, source_pointee)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let target_pointee = EarlyBinder::bind(tcx, target_pointee)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let typing_env = TypingEnv::fully_monomorphized();
    let source_pointee = tcx
        .try_normalize_erasing_regions(
            typing_env,
            rustc_middle::ty::Unnormalized::new_wip(source_pointee),
        )
        .ok()?;
    let target_pointee = tcx
        .try_normalize_erasing_regions(
            typing_env,
            rustc_middle::ty::Unnormalized::new_wip(target_pointee),
        )
        .ok()?;
    let (TyKind::Adt(source_def, _), TyKind::Adt(target_def, _)) =
        (source_pointee.kind(), target_pointee.kind())
    else {
        return None;
    };
    if source_def.did() != target_def.did() || !source_def.is_struct() {
        return None;
    }
    let source_tail = tcx.struct_tail_for_codegen(source_pointee, typing_env);
    let target_tail = tcx.struct_tail_for_codegen(target_pointee, typing_env);
    if !matches!(source_tail.kind(), TyKind::Array(_, _))
        || !matches!(target_tail.kind(), TyKind::Slice(_) | TyKind::Str)
    {
        return None;
    }
    match (source_oomir_ty, target_oomir_ty) {
        (oomir::Type::Pointer(source), oomir::Type::Pointer(target))
            if matches!(source.as_ref(), oomir::Type::Class(_)) =>
        {
            let oomir::Type::Class(target_class) = target.as_ref() else {
                return None;
            };
            Some(target_class.clone())
        }
        _ => None,
    }
}

pub(super) fn struct_tail_pointer_target_class<'tcx>(
    target_pointer_ty: rustc_middle::ty::Ty<'tcx>,
    target_oomir_ty: &oomir::Type,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Option<String> {
    let target_pointee = match target_pointer_ty.kind() {
        TyKind::Ref(_, target, _) | TyKind::RawPtr(target, _) => *target,
        _ => return None,
    };
    let target_pointee = normalize_unsize_ty(target_pointee, tcx, instance);
    let tail = tcx.struct_tail_for_codegen(target_pointee, TypingEnv::fully_monomorphized());
    if !matches!(tail.kind(), TyKind::Slice(_) | TyKind::Str) {
        return None;
    }
    let oomir::Type::Pointer(target) = target_oomir_ty else {
        return None;
    };
    let oomir::Type::Class(target_class) = target.as_ref() else {
        return None;
    };
    Some(target_class.clone())
}

/// Restores the nominal view of a slice-tailed DST when reborrowing through a
/// pointer wrapper. Allocator-backed pointers can retain the correct data word
/// and metadata while carrying only the allocation's element view.
pub(super) fn emit_struct_tail_reborrow_view<'tcx>(
    pointee_ty: Ty<'tcx>,
    source: oomir::Operand,
    pointer_ty: &oomir::Type,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let pointee_ty = normalize_unsize_ty(pointee_ty, tcx, instance);
    let tail = tcx.struct_tail_for_codegen(pointee_ty, TypingEnv::fully_monomorphized());
    if tail == pointee_ty {
        return None;
    }
    let (element_ty, tail_view_class) = match tail.kind() {
        TyKind::Str => (tcx.types.u8, oomir::UTF8_VIEW_CLASS),
        TyKind::Slice(element_ty) => (*element_ty, oomir::SLICE_VIEW_CLASS),
        _ => return None,
    };
    let oomir::Type::Pointer(target) = pointer_ty else {
        return None;
    };
    let oomir::Type::Class(target_class) = target.as_ref() else {
        return None;
    };
    let source_ty = source.get_type()?;
    let metadata_dest = format!("{dest}_metadata");
    instructions.push(oomir::Instruction::InvokeVirtual {
        dest: Some(metadata_dest.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "metadata".to_string(),
        method_ty: oomir::Signature {
            params: vec![("self".to_string(), source_ty.clone())],
            ret: Box::new(oomir::Type::U64),
            is_static: false,
        },
        args: vec![],
        operand: source.clone(),
    });
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest.to_string()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "unsizeStructTail".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), source_ty),
                ("prefix_size".to_string(), oomir::Type::U64),
                ("target_class".to_string(), oomir::Type::java_string()),
                ("tail_view_class".to_string(), oomir::Type::java_string()),
                ("element_size".to_string(), oomir::Type::U64),
                ("element_codec".to_string(), oomir::Type::java_string()),
                ("length".to_string(), oomir::Type::U64),
            ],
            ret: Box::new(pointer_ty.clone()),
            is_static: true,
        },
        args: vec![
            source,
            rust_layout_size_operand(pointee_ty, tcx, instance),
            oomir::Operand::Constant(oomir::Constant::String(target_class.clone())),
            oomir::Operand::Constant(oomir::Constant::String(tail_view_class.to_string())),
            rust_layout_size_operand(element_ty, tcx, instance),
            crate::lower1::types::pointer_view_codec_operand(element_ty, tcx, data_types, instance),
            oomir::Operand::Variable {
                name: metadata_dest,
                ty: oomir::Type::U64,
            },
        ],
    });
    Some(oomir::Operand::Variable {
        name: dest.to_string(),
        ty: pointer_ty.clone(),
    })
}

pub(super) fn emit_struct_tail_pointer_cast<'tcx>(
    source_pointer_ty: Ty<'tcx>,
    target_pointer_ty: Ty<'tcx>,
    source: oomir::Operand,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let source_pointee = normalize_unsize_ty(pointer_pointee_ty(source_pointer_ty), tcx, instance);
    let target_pointee = normalize_unsize_ty(pointer_pointee_ty(target_pointer_ty), tcx, instance);
    let TyKind::Adt(source_def, _) = source_pointee.kind() else {
        return None;
    };
    if !source_def.is_struct() || !matches!(target_pointee.kind(), TyKind::Slice(_) | TyKind::Str) {
        return None;
    }
    let source_tail = tcx.struct_tail_for_codegen(source_pointee, TypingEnv::fully_monomorphized());
    let compatible_tail = match (source_tail.kind(), target_pointee.kind()) {
        (TyKind::Str, TyKind::Str) => true,
        (TyKind::Slice(source_element), TyKind::Slice(target_element)) => {
            crate::lower1::types::layout_size_bytes(tcx, *source_element).ok()
                == crate::lower1::types::layout_size_bytes(tcx, *target_element).ok()
        }
        _ => false,
    };
    if !compatible_tail {
        return None;
    }

    emit_slice_pointer_carrier(
        target_pointer_ty,
        source,
        dest,
        tcx,
        instance,
        data_types,
        instructions,
    )
}

pub(super) fn emit_slice_pointer_carrier<'tcx>(
    target_pointer_ty: Ty<'tcx>,
    source: oomir::Operand,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let target_pointee = normalize_unsize_ty(pointer_pointee_ty(target_pointer_ty), tcx, instance);
    let source_ty = source.get_type()?;
    let oomir::Type::Pointer(_) = &source_ty else {
        return None;
    };
    let target_ty = ty_to_oomir_type(target_pointer_ty, tcx, data_types, instance);
    if !matches!(target_ty, oomir::Type::Slice(_) | oomir::Type::Str) {
        return None;
    }

    let element_ty = if target_pointee.is_str() {
        tcx.types.u8
    } else {
        target_pointee.sequence_element_type(tcx)
    };
    let element_type = ty_to_oomir_type(element_ty, tcx, data_types, instance);
    let element_pointer_type = oomir::Type::Pointer(Box::new(element_type));
    let metadata = format!("{dest}_metadata");
    instructions.push(oomir::Instruction::InvokeVirtual {
        dest: Some(metadata.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "metadata".to_string(),
        method_ty: oomir::Signature {
            params: vec![("self".to_string(), source_ty.clone())],
            ret: Box::new(oomir::Type::U64),
            is_static: false,
        },
        args: Vec::new(),
        operand: source.clone(),
    });
    let retyped = format!("{dest}_data");
    instructions.push(oomir::Instruction::InvokeVirtual {
        dest: Some(retyped.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "retype".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("self".to_string(), source_ty),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(element_pointer_type.clone()),
            is_static: false,
        },
        args: vec![
            rust_layout_size_operand(element_ty, tcx, instance),
            crate::lower1::types::pointer_view_codec_operand(element_ty, tcx, data_types, instance),
        ],
        operand: source,
    });
    let view_class = if target_pointee.is_str() {
        oomir::UTF8_VIEW_CLASS
    } else {
        oomir::SLICE_VIEW_CLASS
    };
    let view = format!("{dest}_view");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: view.clone(),
        class_name: view_class.to_string(),
        args: vec![
            (
                oomir::Operand::Variable {
                    name: retyped,
                    ty: element_pointer_type,
                },
                oomir::Type::Class("java/lang/Object".to_string()),
            ),
            (
                oomir::Operand::Constant(oomir::Constant::I32(0)),
                oomir::Type::I32,
            ),
            (
                oomir::Operand::Variable {
                    name: metadata,
                    ty: oomir::Type::U64,
                },
                oomir::Type::U64,
            ),
        ],
    });
    instructions.push(oomir::Instruction::Cast {
        dest: dest.to_string(),
        op: oomir::Operand::Variable {
            name: view,
            ty: oomir::Type::Class(view_class.to_string()),
        },
        ty: target_ty.clone(),
    });
    Some(oomir::Operand::Variable {
        name: dest.to_string(),
        ty: target_ty,
    })
}

pub(super) fn emit_trait_object_to_struct_tail_cast<'tcx>(
    source_pointer_ty: Ty<'tcx>,
    target_pointer_ty: Ty<'tcx>,
    source: oomir::Operand,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let source_pointee = normalize_unsize_ty(pointer_pointee_ty(source_pointer_ty), tcx, instance);
    if !matches!(source_pointee.kind(), TyKind::Dynamic(..)) {
        return None;
    }
    let target_pointee = normalize_unsize_ty(pointer_pointee_ty(target_pointer_ty), tcx, instance);
    let TyKind::Adt(target_def, _) = target_pointee.kind() else {
        return None;
    };
    if !target_def.is_struct()
        || !matches!(
            tcx.struct_tail_for_codegen(target_pointee, TypingEnv::fully_monomorphized(),)
                .kind(),
            TyKind::Dynamic(..)
        )
    {
        return None;
    }

    let source_ty = source.get_type()?;
    let target_ty = ty_to_oomir_type(target_pointer_ty, tcx, data_types, instance);
    if !matches!(
        (&source_ty, &target_ty),
        (oomir::Type::Pointer(_), oomir::Type::Pointer(_))
    ) {
        return None;
    }
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest.to_string()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "retypeStructTailFromTraitPointer".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), source_ty),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(target_ty.clone()),
            is_static: true,
        },
        args: vec![
            source,
            pointer_view_size_operand(target_pointer_ty, tcx, instance),
            crate::lower1::types::pointer_view_codec_operand(
                target_pointee,
                tcx,
                data_types,
                instance,
            ),
        ],
    });
    Some(oomir::Operand::Variable {
        name: dest.to_string(),
        ty: target_ty,
    })
}

pub(super) struct StructTraitTailUnsize<'tcx> {
    target_class: String,
    source_tail: Ty<'tcx>,
    target_tail: Ty<'tcx>,
    source_tail_offset: u64,
}

pub(super) fn struct_trait_tail_unsize<'tcx>(
    source_pointer_ty: Ty<'tcx>,
    target_pointer_ty: Ty<'tcx>,
    target_oomir_ty: &oomir::Type,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Option<StructTraitTailUnsize<'tcx>> {
    let pointee = |pointer_ty: Ty<'tcx>| match pointer_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => Some(*pointee),
        _ => None,
    };
    let source_pointee = normalize_unsize_ty(pointee(source_pointer_ty)?, tcx, instance);
    let target_pointee = normalize_unsize_ty(pointee(target_pointer_ty)?, tcx, instance);
    let (TyKind::Adt(source_def, source_args), TyKind::Adt(target_def, target_args)) =
        (source_pointee.kind(), target_pointee.kind())
    else {
        return None;
    };
    if source_def.did() != target_def.did() || !source_def.is_struct() {
        return None;
    }
    let typing_env = TypingEnv::fully_monomorphized();
    let tail_field = FieldIdx::from_usize(
        source_def
            .variant(0usize.into())
            .fields
            .len()
            .checked_sub(1)?,
    );
    let field = &source_def.variant(0usize.into()).fields[tail_field];
    let source_tail =
        normalize_unsize_ty(field.ty(tcx, source_args).skip_norm_wip(), tcx, instance);
    let target_tail =
        normalize_unsize_ty(field.ty(tcx, target_args).skip_norm_wip(), tcx, instance);
    if !matches!(
        tcx.struct_tail_for_codegen(target_tail, typing_env).kind(),
        TyKind::Dynamic(..)
    ) || matches!(
        tcx.struct_tail_for_codegen(source_tail, typing_env).kind(),
        TyKind::Dynamic(..)
    ) {
        return None;
    }
    let source_layout = tcx
        .layout_of(typing_env.as_query_input(source_pointee))
        .ok()?;
    let source_tail_offset = source_layout.fields.offset(tail_field.as_usize()).bytes();
    let oomir::Type::Pointer(target) = target_oomir_ty else {
        return None;
    };
    let oomir::Type::Class(target_class) = target.as_ref() else {
        return None;
    };
    Some(StructTraitTailUnsize {
        target_class: target_class.clone(),
        source_tail,
        target_tail,
        source_tail_offset,
    })
}

pub(super) fn emit_struct_trait_tail_pointer_unsize<'tcx>(
    source_ty: Ty<'tcx>,
    target_ty: Ty<'tcx>,
    source: oomir::Operand,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let checkpoint = instructions.len();
    let result = (|| {
        let source_oomir_ty = ty_to_oomir_type(source_ty, tcx, data_types, instance);
        let target_oomir_ty = ty_to_oomir_type(target_ty, tcx, data_types, instance);
        let info = struct_trait_tail_unsize(source_ty, target_ty, &target_oomir_ty, tcx, instance)?;
        let source_tail_pointer_ty =
            Ty::new_ptr(tcx, info.source_tail, rustc_middle::ty::Mutability::Not);
        let target_tail_pointer_ty =
            Ty::new_ptr(tcx, info.target_tail, rustc_middle::ty::Mutability::Not);
        let source_tail_oomir_ty =
            ty_to_oomir_type(source_tail_pointer_ty, tcx, data_types, instance);
        let offset_pointer = format!("{dest}_tail_offset");
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: Some(offset_pointer.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "byte_offset".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), source_oomir_ty.clone()),
                    ("byte_count".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(source_oomir_ty.clone()),
                is_static: false,
            },
            args: vec![oomir::Operand::Constant(oomir::Constant::U64(
                info.source_tail_offset,
            ))],
            operand: source.clone(),
        });
        let source_tail_pointer = format!("{dest}_tail_source");
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: Some(source_tail_pointer.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "retype".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), source_oomir_ty.clone()),
                    ("view_size".to_string(), oomir::Type::U64),
                    ("view_codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(source_tail_oomir_ty.clone()),
                is_static: false,
            },
            args: vec![
                rust_layout_size_operand(info.source_tail, tcx, instance),
                crate::lower1::types::pointer_view_codec_operand(
                    info.source_tail,
                    tcx,
                    data_types,
                    instance,
                ),
            ],
            operand: oomir::Operand::Variable {
                name: offset_pointer,
                ty: source_oomir_ty.clone(),
            },
        });
        let tail_trait_dest = format!("{dest}_tail_trait");
        let tail_trait = emit_unsize_value(
            source_tail_pointer_ty,
            target_tail_pointer_ty,
            oomir::Operand::Variable {
                name: source_tail_pointer,
                ty: source_tail_oomir_ty,
            },
            &tail_trait_dest,
            tcx,
            instance,
            data_types,
            instructions,
        )?;
        let outer_dest = format!("{dest}_outer");
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(outer_dest.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "unsizeStruct".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), source_oomir_ty),
                    ("view_size".to_string(), oomir::Type::U64),
                    ("target_class".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(target_oomir_ty.clone()),
                is_static: true,
            },
            args: vec![
                source,
                pointer_view_size_operand(target_ty, tcx, instance),
                oomir::Operand::Constant(oomir::Constant::String(info.target_class)),
            ],
        });
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest.to_string()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "attachStructTailTraitMetadata".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), target_oomir_ty.clone()),
                    ("tail_pointer".to_string(), tail_trait.get_type()?),
                ],
                ret: Box::new(target_oomir_ty.clone()),
                is_static: true,
            },
            args: vec![
                oomir::Operand::Variable {
                    name: outer_dest,
                    ty: target_oomir_ty.clone(),
                },
                tail_trait,
            ],
        });
        Some(oomir::Operand::Variable {
            name: dest.to_string(),
            ty: target_oomir_ty,
        })
    })();
    if result.is_none() {
        instructions.truncate(checkpoint);
    }
    result
}
