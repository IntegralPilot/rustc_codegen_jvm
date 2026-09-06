//! Pointer and mutable-carrier representation boundaries.
use super::*;

pub(super) fn mutable_reference_chain_contains(source: &oomir::Type, target: &oomir::Type) -> bool {
    let mut current = source;
    while let oomir::Type::MutableReference(inner) = current {
        if inner.as_ref() == target {
            return true;
        }
        current = inner;
    }
    false
}

pub(super) fn reference_chain_contains(outer: &oomir::Type, candidate: &oomir::Type) -> bool {
    let mut current = outer;
    while let oomir::Type::MutableReference(inner)
    | oomir::Type::Reference(inner)
    | oomir::Type::Pointer(inner) = current
    {
        if inner.as_ref() == candidate {
            return true;
        }
        current = inner;
    }
    false
}

pub(super) fn unwrap_mutable_references(
    mut source: oomir::Operand,
    target_jvm_ty: &oomir::Type,
    temp_prefix: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let mut depth = 0;
    loop {
        let Some(oomir::Type::MutableReference(inner)) = source.get_type() else {
            return source;
        };
        let dest = format!("{temp_prefix}_deref_{depth}");
        instructions.push(oomir::Instruction::ArrayGet {
            dest: dest.clone(),
            array: source,
            index: oomir::Operand::Constant(oomir::Constant::I32(0)),
        });
        source = operand_var(dest, inner.as_ref().clone());
        if inner.as_ref() == target_jvm_ty {
            return source;
        }
        depth += 1;
    }
}

pub(super) fn adapt_mutable_reference_carrier<'tcx>(
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
        matches!(
            source_ty,
            oomir::Type::MutableReference(_) | oomir::Type::Reference(_) | oomir::Type::Pointer(_)
        ) && reference_chain_contains(target_jvm_ty, source_ty)
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
        let element_pointer_ty = oomir::Type::Pointer(element_ty.clone());
        let pointer_name = format!("{temp_prefix}_array_ref_element_pointer");
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: Some(pointer_name.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "retype".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), source_ty),
                    ("view_size".to_string(), oomir::Type::U64),
                    ("view_codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(element_pointer_ty.clone()),
                is_static: false,
            },
            args: vec![
                oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(
                        super::super::types::layout_size_bytes(
                            tcx,
                            resolved_ty(*array_element_ty, tcx, instance),
                        )
                        .expect("fixed-array element must have a layout"),
                    )
                    .expect("Rust array element layout exceeds u64"),
                )),
                super::super::types::pointer_view_codec_operand(
                    resolved_ty(*array_element_ty, tcx, instance),
                    tcx,
                    data_types,
                    instance,
                ),
            ],
            operand: source,
        });
        let object_name = format!("{temp_prefix}_array_ref_view_object");
        instructions.push(oomir::Instruction::ConstructObject {
            dest: object_name.clone(),
            class_name: oomir::SLICE_VIEW_CLASS.to_string(),
            args: vec![
                (
                    operand_var(pointer_name, element_pointer_ty),
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
        let dest = format!("{temp_prefix}_array_ref_view");
        let slice_ty = oomir::Type::Slice(element_ty.clone());
        instructions.push(oomir::Instruction::Cast {
            dest: dest.clone(),
            op: operand_var(
                object_name,
                oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
            ),
            ty: slice_ty.clone(),
        });
        return operand_var(dest, slice_ty);
    }

    if matches!(source.get_type(), Some(oomir::Type::Slice(_)))
        && matches!(target_jvm_ty, oomir::Type::Pointer(_))
    {
        let dest = format!("{temp_prefix}_slice_pointer");
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest.clone()),
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
                ret: Box::new(target_jvm_ty.clone()),
                is_static: true,
            },
            args: vec![
                source,
                oomir::Operand::Constant(oomir::Constant::U64(
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
                match target_rust_ty.kind() {
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
            ],
        });
        return operand_var(dest, target_jvm_ty.clone());
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
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: Some(dest.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "retype".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), source_ty),
                    ("view_size".to_string(), oomir::Type::U64),
                    ("view_codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(target_jvm_ty.clone()),
                is_static: false,
            },
            args: vec![
                oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(
                        super::super::types::layout_size_bytes(tcx, pointee_ty).unwrap_or_else(
                            |error| panic!("could not determine pointer view layout: {error}"),
                        ),
                    )
                    .expect("pointer view layout exceeds u64"),
                )),
                super::super::types::pointer_view_codec_operand(
                    pointee_ty, tcx, data_types, instance,
                ),
            ],
            operand: source,
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
    if let Some(oomir::Type::Pointer(inner)) = source.get_type()
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

    if source
        .get_type()
        .as_ref()
        .is_some_and(|source_ty| mutable_reference_chain_contains(source_ty, target_jvm_ty))
    {
        return unwrap_mutable_references(source, target_jvm_ty, temp_prefix, instructions);
    }

    if let TyKind::Ref(_, pointee_ty, mutability) = target_rust_ty.kind()
        && mutability.is_mut()
        && let oomir::Type::MutableReference(target_inner) = target_jvm_ty
        && target_inner.has_jvm_value()
    {
        let pointee = adapt_operand_to_rust_type(
            source,
            *pointee_ty,
            &format!("{temp_prefix}_pointee"),
            tcx,
            instance,
            data_types,
            instructions,
        );
        if pointee.get_type().as_ref() != Some(target_inner.as_ref()) {
            return pointee;
        }

        let dest = format!("{temp_prefix}_ref");
        instructions.push(oomir::Instruction::NewArray {
            dest: dest.clone(),
            element_type: target_inner.as_ref().clone(),
            size: oomir::Operand::Constant(oomir::Constant::I32(1)),
        });
        instructions.push(oomir::Instruction::ArrayStore {
            array: oomir::Operand::Variable {
                name: dest.clone(),
                ty: target_jvm_ty.clone(),
            },
            index: oomir::Operand::Constant(oomir::Constant::I32(0)),
            value: pointee,
            copy_value: false,
        });
        return operand_var(dest, target_jvm_ty.clone());
    }

    if matches!(source.get_type(), Some(oomir::Type::MutableReference(_)))
        && !matches!(target_rust_ty.kind(), TyKind::RawPtr(..))
    {
        return unwrap_mutable_references(source, target_jvm_ty, temp_prefix, instructions);
    }

    source
}
