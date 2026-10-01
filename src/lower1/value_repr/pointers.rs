//! Pointer and mutable-carrier representation boundaries.
use super::*;

pub(super) fn reference_chain_contains(outer: &oomir::Type, candidate: &oomir::Type) -> bool {
    let mut current = outer;
    while let oomir::Type::Pointer(oomir::Pointee { value: inner, .. }) = current {
        if inner.as_ref() == candidate {
            return true;
        }
        current = inner;
    }
    false
}

pub(super) fn adapt_reference_carrier<'tcx>(
    source: oomir::Operand,
    target_rust_ty: Ty<'tcx>,
    target_jvm_ty: &oomir::Type,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    if source.get_type().as_ref() == Some(target_jvm_ty) {
        return source;
    }
    if source.get_type().as_ref().is_some_and(|source_ty| {
        matches!(source_ty, oomir::Type::Pointer(_))
            && reference_chain_contains(target_jvm_ty, source_ty)
    }) {
        return source;
    }

    if matches!(source.get_type(), Some(oomir::Type::Pointer(_)))
        && let oomir::Type::Slice(element_ty) = target_jvm_ty
        && let TyKind::Ref(_, pointee_ty, _) = target_rust_ty.kind()
        && let TyKind::Array(array_element_ty, length) = pointee_ty.kind()
        && let Some(length) = length.try_to_target_usize(tcx)
    {
        let source_ty = source
            .get_type()
            .expect("fixed-array reference pointer is typed");
        let element_pointer_ty = oomir::Type::pointer(*element_ty.clone());
        let pointer_name = format!("{temp_prefix}_array_ref_element_pointer");
        instructions.push(oomir::Instruction::AddressRetype {
            dest: Some(pointer_name.clone()),
            source: source,
            layout: Box::new(oomir::AddressLayout {
                pointer_type: element_pointer_ty.clone(),
                size: oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(
                        super::super::types::layout_size_bytes(
                            tcx,
                            resolved_ty(*array_element_ty, tcx, instance),
                        )
                        .expect("fixed-array element must have a layout"),
                    )
                    .expect("Rust array element layout exceeds u64"),
                )),
                codec: super::super::types::pointer_view_codec_operand(
                    resolved_ty(*array_element_ty, tcx, instance),
                    tcx,
                    data_types,
                    instance,
                ),
            }),
        });
        return crate::lower1::place::emit_pointer_slice_view(
            operand_var(pointer_name, element_pointer_ty),
            oomir::Operand::Constant(oomir::Constant::U64(length)),
            &format!("{temp_prefix}_array_ref_view"),
            instructions,
        );
    }

    if matches!(source.get_type(), Some(oomir::Type::Slice(_)))
        && matches!(target_jvm_ty, oomir::Type::Pointer(_))
    {
        let dest = format!("{temp_prefix}_slice_pointer");
        instructions.push(oomir::Instruction::ViewAddress {
            dest: Some(dest.clone()),
            source: source,
            layout: Box::new(oomir::AddressLayout {
                pointer_type: target_jvm_ty.clone(),
                size: oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(
                        match target_rust_ty.kind() {
                            TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => {
                                super::super::types::layout_size_bytes(
                                    tcx,
                                    resolved_ty(*pointee, tcx, instance),
                                )
                            }
                            _ => unreachable!("pointer carrier target must be a pointer type"),
                        }
                        .unwrap_or_else(|error| {
                            panic!("could not determine slice pointer layout: {error}")
                        }),
                    )
                    .expect("Rust slice pointer layout exceeds u64"),
                )),
                codec: match target_rust_ty.kind() {
                    TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => {
                        super::super::types::pointer_view_codec_operand(
                            resolved_ty(*pointee, tcx, instance),
                            tcx,
                            data_types,
                            instance,
                        )
                    }
                    _ => unreachable!("pointer carrier target must be a pointer type"),
                },
            }),
        });
        return operand_var(dest, target_jvm_ty.clone());
    }

    if matches!(source.get_type(), Some(oomir::Type::Pointer(_)))
        && let TyKind::Ref(_, pointee, _) = target_rust_ty.kind()
        && matches!(pointee.kind(), TyKind::Dynamic(..))
    {
        // Raw dyn pointers contain an address and vtable. References use a JVM trait adapter.
        return crate::lower1::place::emit_pointer_read(
            source,
            target_jvm_ty,
            &format!("{temp_prefix}_trait_reference"),
            instructions,
        );
    }

    let target_pointer_ty = match target_rust_ty.kind() {
        TyKind::Pat(inner, _) => resolved_ty(*inner, tcx, instance),
        _ => target_rust_ty,
    };
    if let oomir::Type::Pointer(_) = target_jvm_ty
        && let TyKind::Ref(_, pointee_ty, _) | TyKind::RawPtr(pointee_ty, _) =
            target_pointer_ty.kind()
        && matches!(
            resolved_ty(*pointee_ty, tcx, instance).kind(),
            TyKind::Dynamic(..)
        )
        && !matches!(source.get_type(), Some(oomir::Type::Pointer(_)))
    {
        return emit_trait_object_reference_pointer(
            source,
            target_jvm_ty,
            temp_prefix,
            instructions,
        );
    }
    if let (Some(oomir::Type::Pointer(source_inner)), oomir::Type::Pointer(_)) =
        (source.get_type(), target_jvm_ty)
        && let TyKind::Ref(_, pointee_ty, _) | TyKind::RawPtr(pointee_ty, _) =
            target_pointer_ty.kind()
    {
        let pointee_ty = resolved_ty(*pointee_ty, tcx, instance);
        let source_ty = oomir::Type::Pointer(source_inner);
        let dest = format!("{temp_prefix}_retyped_pointer");
        instructions.push(oomir::Instruction::AddressRetype {
            dest: Some(dest.clone()),
            source: source,
            layout: Box::new(oomir::AddressLayout {
                pointer_type: target_jvm_ty.clone(),
                size: oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(
                        super::super::types::layout_size_bytes(tcx, pointee_ty).unwrap_or_else(
                            |error| panic!("could not determine pointer view layout: {error}"),
                        ),
                    )
                    .expect("pointer view layout exceeds u64"),
                )),
                codec: super::super::types::pointer_view_codec_operand(
                    pointee_ty, tcx, data_types, instance,
                ),
            }),
        });
        return operand_var(dest, target_jvm_ty.clone());
    }

    if let oomir::Type::Pointer(target_inner) = target_jvm_ty
        && let TyKind::Ref(_, pointee_ty, _) | TyKind::RawPtr(pointee_ty, _) =
            target_pointer_ty.kind()
    {
        let mut pointee = adapt_operand_to_rust_type(
            source,
            *pointee_ty,
            &format!("{temp_prefix}_pointee"),
            tcx,
            instance,
            data_types,
            instructions,
        );
        if pointee.get_type().as_ref() != Some(target_inner.as_ref()) {
            if pointee
                .get_type()
                .is_some_and(|ty| ty.is_jvm_reference_type())
                && target_inner.is_jvm_reference_type()
            {
                pointee = cast_direct_operand(
                    pointee,
                    target_inner,
                    &format!("{temp_prefix}_pointee_class"),
                    instructions,
                );
            } else {
                return pointee;
            }
        }
        let dest = format!("{temp_prefix}_pointer");
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "cell".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "value".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    ("size".to_string(), oomir::Type::I32),
                    ("codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(target_jvm_ty.clone()),
                is_static: true,
            },
            args: vec![
                pointee,
                oomir::Operand::Constant(oomir::Constant::I32(
                    i32::try_from(
                        super::super::types::layout_size_bytes(
                            tcx,
                            resolved_ty(*pointee_ty, tcx, instance),
                        )
                        .unwrap_or_else(|error| {
                            panic!("could not determine pointer cell layout: {error}")
                        }),
                    )
                    .expect("pointer cell layout exceeds the JVM runtime address space"),
                )),
                super::super::types::pointer_memory_codec_operand(
                    resolved_ty(*pointee_ty, tcx, instance),
                    tcx,
                    data_types,
                    instance,
                ),
            ],
        });
        return operand_var(dest, target_jvm_ty.clone());
    }

    let target_is_scalar_adt = matches!(target_rust_ty.kind(), TyKind::Adt(..))
        && tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(target_rust_ty))
            .is_ok_and(|layout| matches!(layout.backend_repr, BackendRepr::Scalar(_)));
    if let Some(oomir::Type::Pointer(oomir::Pointee { value: inner, .. })) = source.get_type()
        && !matches!(target_rust_ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..))
        && !target_is_scalar_adt
    {
        return super::super::place::emit_pointer_read(
            source,
            inner.as_ref(),
            &format!("{temp_prefix}_pointer_value"),
            instructions,
        );
    }

    source
}
