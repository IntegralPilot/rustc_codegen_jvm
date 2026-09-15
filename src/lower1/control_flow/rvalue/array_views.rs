use super::*;

pub(super) fn emit_borrowed_array_view<'tcx>(
    source: oomir::Operand,
    array_ty: Ty<'tcx>,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let array_ty = normalize_unsize_ty(array_ty, tcx, instance);
    let TyKind::Array(element_ty, length) = array_ty.kind() else {
        return None;
    };
    let length = EarlyBinder::bind(tcx, *length)
        .instantiate(tcx, instance.args)
        .skip_norm_wip()
        .try_to_target_usize(tcx)?;
    let element_type = ty_to_oomir_type(*element_ty, tcx, data_types, instance);
    let element_is_zst = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(*element_ty))
        .ok()?
        .is_zst();
    if !element_is_zst {
        let source_type = source.get_type()?;
        let slice_type = emit_slice_view(source, &source_type, 0, 0, true, dest, instructions);
        return Some(oomir::Operand::Variable {
            name: dest.to_string(),
            ty: slice_type,
        });
    }
    // ZST arrays have no physical JVM elements, even when their element type
    // has a nominal carrier such as `[u32; 0]`.  Preserve that carrier in one
    // typed zero-byte cell so every logical index can materialize the value
    // while retaining Rust's rule that all ZST element addresses are equal.
    let backing = if element_type.has_jvm_value() {
        let pointer_type = oomir::Type::Pointer(Box::new(element_type.clone()));
        let template = const_eval::read_zero_sized_constant(tcx, *element_ty, data_types, instance)
            .unwrap_or_else(|error| {
                panic!("could not materialize borrowed ZST array element {element_ty:?}: {error}")
            });
        emit_pointer_factory(
            "cell",
            vec![
                oomir::Operand::Constant(template),
                oomir::Operand::Constant(oomir::Constant::I32(0)),
                oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string())),
            ],
            &pointer_type,
            &format!("{dest}_zst_backing"),
            instructions,
        )
    } else {
        let empty = format!("{dest}_zst_backing");
        instructions.push(oomir::Instruction::NewArray {
            dest: empty.clone(),
            element_type: element_type.clone(),
            size: oomir::Operand::Constant(oomir::Constant::I32(0)),
        });
        oomir::Operand::Variable {
            name: empty,
            ty: oomir::Type::Array(Box::new(element_type.clone())),
        }
    };
    let object = format!("{dest}_object");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: object.clone(),
        class_name: oomir::SLICE_VIEW_CLASS.to_string(),
        args: vec![
            (backing, oomir::Type::Class("java/lang/Object".to_string())),
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
    let slice_type = oomir::Type::Slice(Box::new(element_type));
    instructions.push(oomir::Instruction::Cast {
        dest: dest.to_string(),
        op: oomir::Operand::Variable {
            name: object,
            ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
        },
        ty: slice_type.clone(),
    });
    Some(oomir::Operand::Variable {
        name: dest.to_string(),
        ty: slice_type,
    })
}

pub(super) fn emit_borrowed_projected_array_view<'tcx>(
    source_place: &Place<'tcx>,
    array_ty: Ty<'tcx>,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let array_ty = normalize_unsize_ty(array_ty, tcx, instance);
    let TyKind::Array(element_ty, length) = array_ty.kind() else {
        return None;
    };
    let length = EarlyBinder::bind(tcx, *length)
        .instantiate(tcx, instance.args)
        .skip_norm_wip()
        .try_to_target_usize(tcx)?;
    let array_type = ty_to_oomir_type(array_ty, tcx, data_types, instance);
    let element_type = ty_to_oomir_type(*element_ty, tcx, data_types, instance);
    let array_pointer_type = oomir::Type::Pointer(Box::new(array_type));
    let array_pointer = emit_pointer_to_place(
        source_place,
        &array_pointer_type,
        &format!("{dest}_array"),
        tcx,
        instance,
        mir,
        data_types,
        instructions,
    );
    let element_pointer_type = oomir::Type::Pointer(Box::new(element_type.clone()));
    let element_pointer_name = format!("{dest}_element_pointer");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(element_pointer_name.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "retype".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), array_pointer_type),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(element_pointer_type.clone()),
            is_static: true,
        },
        args: vec![
            array_pointer,
            rust_layout_size_operand(*element_ty, tcx, instance),
            crate::lower1::types::pointer_view_codec_operand(
                *element_ty,
                tcx,
                data_types,
                instance,
            ),
        ],
    });
    let object = format!("{dest}_object");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: object.clone(),
        class_name: oomir::SLICE_VIEW_CLASS.to_string(),
        args: vec![
            (
                oomir::Operand::Variable {
                    name: element_pointer_name,
                    ty: element_pointer_type,
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
    let slice_type = oomir::Type::Slice(Box::new(element_type));
    instructions.push(oomir::Instruction::Cast {
        dest: dest.to_string(),
        op: oomir::Operand::Variable {
            name: object,
            ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
        },
        ty: slice_type.clone(),
    });
    Some(oomir::Operand::Variable {
        name: dest.to_string(),
        ty: slice_type,
    })
}

pub(super) fn emit_raw_array_pointer_unsize<'tcx>(
    source_ty: Ty<'tcx>,
    target_ty: Ty<'tcx>,
    source: oomir::Operand,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    // NonNull's field is a pattern-refined raw pointer. Pattern types retain
    // the raw pointer's runtime representation and unsizing behavior.
    let source_pointer_ty = match source_ty.kind() {
        TyKind::Pat(inner, _) => normalize_unsize_ty(*inner, tcx, instance),
        _ => source_ty,
    };
    let target_pointer_ty = match target_ty.kind() {
        TyKind::Pat(inner, _) => normalize_unsize_ty(*inner, tcx, instance),
        _ => target_ty,
    };
    let pointer_pointee = |ty: Ty<'tcx>| match ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => Some(*pointee),
        _ => None,
    };
    let source_pointee = pointer_pointee(source_pointer_ty)?;
    let target_pointee = pointer_pointee(target_pointer_ty)?;
    let source_pointee = normalize_unsize_ty(source_pointee, tcx, instance);
    let target_pointee = normalize_unsize_ty(target_pointee, tcx, instance);
    let source_pointee_size = crate::lower1::types::layout_size_bytes(tcx, source_pointee).ok()?;
    let source_pointee_alignment =
        crate::lower1::types::layout_align_bytes(tcx, source_pointee).ok()?;
    let layout_args = || {
        vec![
            oomir::Operand::Constant(oomir::Constant::U64(source_pointee_size as u64)),
            oomir::Operand::Constant(oomir::Constant::U64(source_pointee_alignment as u64)),
        ]
    };
    let source_oomir_ty = ty_to_oomir_type(source_ty, tcx, data_types, instance);
    let target_oomir_ty = ty_to_oomir_type(target_ty, tcx, data_types, instance);

    if matches!(target_pointee.kind(), TyKind::Dynamic(..))
        && let oomir::Type::Interface(interface_name) = &target_oomir_ty
    {
        let adapter_class = ensure_trait_object_adapter_class(
            source_pointer_ty,
            target_pointer_ty,
            &source_oomir_ty,
            interface_name,
            data_types,
            tcx,
            instance,
        )
        .ok()?;
        instructions.push(oomir::Instruction::ConstructObject {
            dest: dest.to_string(),
            class_name: adapter_class,
            args: vec![(source, source_oomir_ty)],
        });
        return Some(oomir::Operand::Variable {
            name: dest.to_string(),
            ty: target_oomir_ty,
        });
    }

    if matches!(target_pointee.kind(), TyKind::Dynamic(..))
        && matches!(source_oomir_ty, oomir::Type::Pointer(_))
        && matches!(target_oomir_ty, oomir::Type::Pointer(_))
    {
        let callable_abi = crate::lower1::types::callable_trait_object_abi(
            target_pointer_ty,
            tcx,
            data_types,
            instance,
        );
        let callable_closure_bridge = callable_abi.as_ref().is_some_and(|callable_abi| {
            ensure_closure_callable_bridge(source_pointee, &callable_abi, data_types, tcx, instance)
        });
        let callable_fn_def_adapter = if callable_closure_bridge {
            None
        } else {
            callable_abi.as_ref().and_then(|callable_abi| {
                let TyKind::FnDef(def_id, args) = source_pointee.kind() else {
                    return None;
                };
                let function_instance = Instance::resolve_for_fn_ptr(
                    tcx,
                    TypingEnv::post_analysis(tcx, instance.def_id()),
                    *def_id,
                    args.no_bound_vars()?,
                )?;
                let target =
                    fn_pointer_target(tcx, data_types, function_instance, &callable_abi.signature);
                Some(ensure_fn_pointer_adapter_class(
                    data_types,
                    target.as_ref(),
                    &callable_abi.signature,
                    &callable_abi.interface_name,
                    tcx,
                    instance,
                ))
            })
        };
        let erased_pointer_dest = format!("{dest}_pointer");
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: Some(erased_pointer_dest.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "retype".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), source_oomir_ty.clone()),
                    ("view_size".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(target_oomir_ty.clone()),
                is_static: false,
            },
            args: vec![oomir::Operand::Constant(oomir::Constant::U64(0))],
            operand: source.clone(),
        });
        let erased_pointer = oomir::Operand::Variable {
            name: erased_pointer_dest,
            ty: target_oomir_ty.clone(),
        };
        if callable_closure_bridge {
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.to_string()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "attachPointeeTraitObjectCarrier".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("pointer".to_string(), target_oomir_ty.clone()),
                        ("pointee_size".to_string(), oomir::Type::U64),
                        ("pointee_alignment".to_string(), oomir::Type::U64),
                    ],
                    ret: Box::new(target_oomir_ty.clone()),
                    is_static: true,
                },
                args: std::iter::once(erased_pointer)
                    .chain(layout_args())
                    .collect(),
            });
        } else if let Some(adapter_class) = callable_fn_def_adapter {
            let carrier_dest = format!("{dest}_carrier");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: carrier_dest.clone(),
                class_name: adapter_class.clone(),
                args: Vec::new(),
            });
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.to_string()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "attachTraitObjectCarrier".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("pointer".to_string(), target_oomir_ty.clone()),
                        (
                            "carrier".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("pointee_size".to_string(), oomir::Type::U64),
                        ("pointee_alignment".to_string(), oomir::Type::U64),
                    ],
                    ret: Box::new(target_oomir_ty.clone()),
                    is_static: true,
                },
                args: vec![
                    erased_pointer,
                    oomir::Operand::Variable {
                        name: carrier_dest,
                        ty: oomir::Type::Class(adapter_class),
                    },
                ]
                .into_iter()
                .chain(layout_args())
                .collect(),
            });
        } else {
            let oomir::Type::Interface(interface_name) =
                ty_to_oomir_type(target_pointee, tcx, data_types, instance)
            else {
                return None;
            };
            let adapter_class = ensure_trait_object_adapter_class(
                source_pointer_ty,
                target_pointer_ty,
                &source_oomir_ty,
                &interface_name,
                data_types,
                tcx,
                instance,
            )
            .ok()?;
            let carrier_dest = format!("{dest}_carrier");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: carrier_dest.clone(),
                class_name: adapter_class.clone(),
                args: vec![(source.clone(), source_oomir_ty.clone())],
            });
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.to_string()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "attachTraitObjectCarrier".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("pointer".to_string(), target_oomir_ty.clone()),
                        (
                            "carrier".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("pointee_size".to_string(), oomir::Type::U64),
                        ("pointee_alignment".to_string(), oomir::Type::U64),
                    ],
                    ret: Box::new(target_oomir_ty.clone()),
                    is_static: true,
                },
                args: vec![
                    erased_pointer,
                    oomir::Operand::Variable {
                        name: carrier_dest,
                        ty: oomir::Type::Class(adapter_class),
                    },
                ]
                .into_iter()
                .chain(layout_args())
                .collect(),
            });
        }
        return Some(oomir::Operand::Variable {
            name: dest.to_string(),
            ty: target_oomir_ty,
        });
    }

    let (TyKind::Array(source_element, length), TyKind::Slice(target_element)) =
        (source_pointee.kind(), target_pointee.kind())
    else {
        return None;
    };
    let source_element = normalize_unsize_ty(*source_element, tcx, instance);
    let target_element = normalize_unsize_ty(*target_element, tcx, instance);
    if source_element != target_element {
        return None;
    }

    if !matches!(source_oomir_ty, oomir::Type::Pointer(_))
        || !matches!(target_oomir_ty, oomir::Type::Slice(_))
    {
        return None;
    }

    let element_oomir_ty = ty_to_oomir_type(target_element, tcx, data_types, instance);
    let element_pointer_ty = oomir::Type::Pointer(Box::new(element_oomir_ty));
    let element_pointer = format!("{dest}_element_pointer");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(element_pointer.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "retype".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), source_oomir_ty),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(element_pointer_ty.clone()),
            is_static: true,
        },
        args: vec![
            source,
            rust_layout_size_operand(target_element, tcx, instance),
            crate::lower1::types::pointer_view_codec_operand(
                target_element,
                tcx,
                data_types,
                instance,
            ),
        ],
    });

    let length = EarlyBinder::bind(tcx, *length)
        .instantiate(tcx, instance.args)
        .skip_norm_wip()
        .try_to_target_usize(tcx)?;
    let slice_object = format!("{dest}_slice_object");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: slice_object.clone(),
        class_name: oomir::SLICE_VIEW_CLASS.to_string(),
        args: vec![
            (
                oomir::Operand::Variable {
                    name: element_pointer,
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
        dest: dest.to_string(),
        op: oomir::Operand::Variable {
            name: slice_object,
            ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
        },
        ty: target_oomir_ty.clone(),
    });
    Some(oomir::Operand::Variable {
        name: dest.to_string(),
        ty: target_oomir_ty,
    })
}
