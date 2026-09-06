use super::*;

pub(super) fn normalize_unsize_ty<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Ty<'tcx> {
    let instantiated = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let normalized = tcx
        .try_normalize_erasing_regions(
            TypingEnv::fully_monomorphized(),
            rustc_middle::ty::Unnormalized::new_wip(instantiated),
        )
        .unwrap_or(instantiated);
    match normalized.kind() {
        TyKind::Pat(inner, _) => normalize_unsize_ty(*inner, tcx, instance),
        _ => normalized,
    }
}

pub(super) fn emit_unsize_value<'tcx>(
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
    let source_ty = normalize_unsize_ty(source_ty, tcx, instance);
    let target_ty = normalize_unsize_ty(target_ty, tcx, instance);
    ensure_nested_callable_unsize(source_ty, target_ty, tcx, instance, data_types);
    let source_oomir_ty = ty_to_oomir_type(source_ty, tcx, data_types, instance);
    let target_oomir_ty = ty_to_oomir_type(target_ty, tcx, data_types, instance);

    let result = if source_oomir_ty == target_oomir_ty {
        instructions.push(oomir::Instruction::Move {
            dest: dest.to_string(),
            src: source,
        });
        Some(oomir::Operand::Variable {
            name: dest.to_string(),
            ty: target_oomir_ty,
        })
    } else if let (TyKind::Adt(source_def, source_args), TyKind::Adt(target_def, target_args)) =
        (source_ty.kind(), target_ty.kind())
        && source_def.did() == target_def.did()
        && crate::lower1::is_non_null_lang_item(tcx, source_def.did())
        && matches!(source_oomir_ty, oomir::Type::Pointer(_))
        && let oomir::Type::Class(target_class) = &target_oomir_ty
    {
        let field = source_def
            .variant(0usize.into())
            .fields
            .iter()
            .next()
            .expect("NonNull has a pointer field");
        let source_field_ty =
            normalize_unsize_ty(field.ty(tcx, source_args).skip_norm_wip(), tcx, instance);
        let target_field_ty =
            normalize_unsize_ty(field.ty(tcx, target_args).skip_norm_wip(), tcx, instance);
        emit_unsize_value(
            source_field_ty,
            target_field_ty,
            source,
            &format!("{dest}_pointer"),
            tcx,
            instance,
            data_types,
            instructions,
        )
        .map(|pointer| {
            let pointer_ty = pointer
                .get_type()
                .expect("unsized NonNull pointer has a JVM value");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.to_string(),
                class_name: target_class.clone(),
                args: vec![(pointer, pointer_ty)],
            });
            oomir::Operand::Variable {
                name: dest.to_string(),
                ty: target_oomir_ty.clone(),
            }
        })
    } else if let (TyKind::Adt(source_def, source_args), TyKind::Adt(target_def, target_args)) =
        (source_ty.kind(), target_ty.kind())
        && source_def.did() == target_def.did()
        && source_def.is_struct()
        && crate::lower1::types::should_define_named_data_type(tcx, source_def.did())
        && let (oomir::Type::Class(source_class), oomir::Type::Class(target_class)) =
            (&source_oomir_ty, &target_oomir_ty)
    {
        let mut constructor_args = Vec::new();
        let mut valid = true;
        for (field_index, field) in source_def.variant(0usize.into()).fields.iter().enumerate() {
            let source_field_ty =
                normalize_unsize_ty(field.ty(tcx, source_args).skip_norm_wip(), tcx, instance);
            let target_field_ty =
                normalize_unsize_ty(field.ty(tcx, target_args).skip_norm_wip(), tcx, instance);
            let target_field_oomir_ty =
                ty_to_oomir_type(target_field_ty, tcx, data_types, instance);
            if !target_field_oomir_ty.has_jvm_value() {
                continue;
            }

            let target_is_zst = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(target_field_ty))
                .is_ok_and(|layout| layout.size.bytes() == 0);
            let field_value = if target_is_zst {
                crate::lower1::value_repr::materialize_implicit_zst(
                    target_field_ty,
                    &format!("{dest}_field_{field_index}_zst"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                )
            } else {
                let source_field_oomir_ty =
                    ty_to_oomir_type(source_field_ty, tcx, data_types, instance);
                if !source_field_oomir_ty.has_jvm_value() {
                    None
                } else {
                    let source_field_name = format!("{dest}_field_{field_index}_source");
                    instructions.push(oomir::Instruction::GetField {
                        dest: source_field_name.clone(),
                        object: source.clone(),
                        field_name: field.ident(tcx).to_string(),
                        field_ty: source_field_oomir_ty.clone(),
                        owner_class: source_class.clone(),
                    });
                    let source_field = oomir::Operand::Variable {
                        name: source_field_name,
                        ty: source_field_oomir_ty.clone(),
                    };
                    if source_field_ty == target_field_ty {
                        Some(source_field)
                    } else {
                        emit_unsize_value(
                            source_field_ty,
                            target_field_ty,
                            source_field,
                            &format!("{dest}_field_{field_index}_unsized"),
                            tcx,
                            instance,
                            data_types,
                            instructions,
                        )
                    }
                }
            };
            let Some(field_value) = field_value else {
                valid = false;
                break;
            };
            constructor_args.push((field_value, target_field_oomir_ty));
        }
        valid.then(|| {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.to_string(),
                class_name: target_class.clone(),
                args: constructor_args,
            });
            oomir::Operand::Variable {
                name: dest.to_string(),
                ty: target_oomir_ty.clone(),
            }
        })
    } else if let Some(result) = emit_struct_trait_tail_pointer_unsize(
        source_ty,
        target_ty,
        source.clone(),
        dest,
        tcx,
        instance,
        data_types,
        instructions,
    ) {
        Some(result)
    } else if let Some(target_class) = struct_tail_unsize_target_class(
        source_ty,
        target_ty,
        &source_oomir_ty,
        &target_oomir_ty,
        tcx,
        instance,
    ) {
        let source_pointee = normalize_unsize_ty(pointer_pointee_ty(source_ty), tcx, instance);
        let TyKind::Array(element_ty, length) = tcx
            .struct_tail_for_codegen(source_pointee, TypingEnv::fully_monomorphized())
            .kind()
        else {
            unreachable!("struct-tail unsizing source was validated")
        };
        let length = length.try_to_target_usize(tcx)?;
        let target_pointee = normalize_unsize_ty(pointer_pointee_ty(target_ty), tcx, instance);
        let target_tail =
            tcx.struct_tail_for_codegen(target_pointee, TypingEnv::fully_monomorphized());
        let tail_view_class = if target_tail.is_str() {
            oomir::UTF8_VIEW_CLASS
        } else {
            oomir::SLICE_VIEW_CLASS
        };
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest.to_string()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "unsizeStructTail".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), source_oomir_ty),
                    ("prefix_size".to_string(), oomir::Type::U64),
                    ("target_class".to_string(), oomir::Type::java_string()),
                    ("tail_view_class".to_string(), oomir::Type::java_string()),
                    ("element_size".to_string(), oomir::Type::U64),
                    ("element_codec".to_string(), oomir::Type::java_string()),
                    ("length".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(target_oomir_ty.clone()),
                is_static: true,
            },
            args: vec![
                source,
                pointer_view_size_operand(target_ty, tcx, instance),
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
        Some(oomir::Operand::Variable {
            name: dest.to_string(),
            ty: target_oomir_ty,
        })
    } else {
        emit_raw_array_pointer_unsize(
            source_ty,
            target_ty,
            source,
            dest,
            tcx,
            instance,
            data_types,
            instructions,
        )
    };

    if result.is_none() {
        instructions.truncate(checkpoint);
    }
    result
}

/// Coercions such as `Box<Closure> -> Box<dyn FnOnce()>` can pass through
/// several transparent pointer wrappers whose JVM carriers are already ABI-
/// compatible. Walk the Rust types as well so that an equal-carrier fast path
/// cannot skip installing the callable interface on the concrete closure.
pub(super) fn ensure_nested_callable_unsize<'tcx>(
    source_ty: Ty<'tcx>,
    target_ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> bool {
    let source_ty = normalize_unsize_ty(source_ty, tcx, instance);
    let target_ty = normalize_unsize_ty(target_ty, tcx, instance);
    if matches!(source_ty.kind(), TyKind::Closure(..))
        && matches!(target_ty.kind(), TyKind::Dynamic(..))
        && let Some(callable_abi) =
            crate::lower1::types::callable_trait_object_abi(target_ty, tcx, data_types, instance)
    {
        return ensure_closure_callable_bridge(source_ty, &callable_abi, data_types, tcx, instance);
    }

    match (source_ty.kind(), target_ty.kind()) {
        (TyKind::Ref(_, source, _), TyKind::Ref(_, target, _))
        | (TyKind::RawPtr(source, _), TyKind::RawPtr(target, _))
        | (TyKind::Pat(source, _), TyKind::Pat(target, _)) => {
            ensure_nested_callable_unsize(*source, *target, tcx, instance, data_types)
        }
        (TyKind::Adt(source_def, source_args), TyKind::Adt(target_def, target_args))
            if source_def.did() == target_def.did() =>
        {
            source_def.variants().iter().any(|variant| {
                variant.fields.iter().any(|field| {
                    ensure_nested_callable_unsize(
                        field.ty(tcx, source_args).skip_norm_wip(),
                        field.ty(tcx, target_args).skip_norm_wip(),
                        tcx,
                        instance,
                        data_types,
                    )
                })
            })
        }
        (TyKind::Tuple(source), TyKind::Tuple(target)) if source.len() == target.len() => {
            source.iter().zip(target.iter()).any(|(source, target)| {
                ensure_nested_callable_unsize(source, target, tcx, instance, data_types)
            })
        }
        _ => false,
    }
}
