use super::*;

pub(super) fn emit_pointer_factory(
    method_name: &str,
    args: Vec<oomir::Operand>,
    pointer_ty: &oomir::Type,
    dest: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    assert!(matches!(pointer_ty, oomir::Type::Pointer(_)));
    let params = match method_name {
        "cell" => vec![
            (
                "value".to_string(),
                oomir::Type::Class("java/lang/Object".to_string()),
            ),
            ("size".to_string(), oomir::Type::I32),
            ("codec".to_string(), oomir::Type::java_string()),
        ],
        "array" => vec![
            (
                "array".to_string(),
                oomir::Type::Class("java/lang/Object".to_string()),
            ),
            ("offset".to_string(), oomir::Type::I32),
            ("element_size".to_string(), oomir::Type::U64),
            ("codec".to_string(), oomir::Type::java_string()),
        ],
        "nullPointer" => Vec::new(),
        other => panic!("unknown pointer factory {other}"),
    };
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest.to_string()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: method_name.to_string(),
        method_ty: oomir::Signature {
            params,
            ret: Box::new(pointer_ty.clone()),
            is_static: true,
        },
        args,
    });
    oomir::Operand::Variable {
        name: dest.to_string(),
        ty: pointer_ty.clone(),
    }
}

/// Takes the address of a MIR place. Array elements retain their real backing
/// allocation; scalar/aggregate locals and fields receive a stable heap cell.
/// A dereference followed by another borrow is a reborrow and therefore keeps
/// the exact pointer object/allocation identity.
pub(crate) fn emit_pointer_to_place<'tcx>(
    place: &Place<'tcx>,
    pointer_ty: &oomir::Type,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    if matches!(pointer_ty, oomir::Type::Slice(_) | oomir::Type::Str) {
        let rust_ty = normalize_unsize_ty(place.ty(&mir.local_decls, tcx).ty, tcx, instance);
        let projects_struct_tail = place.projection.split_last().is_some_and(|(last, prefix)| {
            matches!(last, ProjectionElem::Field(..))
                && crate::lower1::place::has_slice_or_str_struct_tail(
                    tcx,
                    normalize_unsize_ty(
                        Place {
                            local: place.local,
                            projection: tcx.mk_place_elems(prefix),
                        }
                        .ty(&mir.local_decls, tcx)
                        .ty,
                        tcx,
                        instance,
                    ),
                )
        });
        if projects_struct_tail {
            let storage = emit_pointer_to_place(
                place,
                &oomir::Type::pointer(pointer_ty.clone()),
                &format!("{temp_prefix}_unsized_storage"),
                tcx,
                instance,
                mir,
                data_types,
                instructions,
            );
            return emit_slice_pointer_carrier(
                Ty::new_ptr(tcx, rust_ty, rustc_middle::ty::Mutability::Mut),
                storage,
                temp_prefix,
                tcx,
                instance,
                data_types,
                instructions,
            )
            .expect("slice-like place must retain its metadata");
        }
        // Slice and string places already contain an address and length. A new cell would store the
        // view itself.
        let (name, get_instructions, ty) =
            emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
        instructions.extend(get_instructions);
        assert_eq!(&ty, pointer_ty, "unsized borrow must preserve its carrier");
        return oomir::Operand::Variable { name, ty };
    }
    if place.projection.is_empty() && data_types.local_uses_stable_cell(place.local) {
        let dest = format!("{temp_prefix}_local_ptr");
        instructions.push(oomir::Instruction::Move {
            dest: dest.clone(),
            src: oomir::Operand::Variable {
                name: crate::lower1::place::local_cell_name(place.local),
                ty: pointer_ty.clone(),
            },
        });
        return oomir::Operand::Variable {
            name: dest,
            ty: pointer_ty.clone(),
        };
    }

    if let Some((last, prefix)) = place.projection.split_last() {
        let base_place = Place {
            local: place.local,
            projection: tcx.mk_place_elems(prefix),
        };
        match last {
            ProjectionElem::Downcast(..) => {
                // A downcast refines an enum pointer's JVM class but does not
                // create new Rust storage. Preserve the pointer allocation so
                // a later borrow of a variant field writes through to the
                // original enum instead of a detached decoded value.
                let base_rust_ty =
                    normalize_unsize_ty(base_place.ty(&mir.local_decls, tcx).ty, tcx, instance);
                let base_oomir_ty = get_place_type(&base_place, mir, tcx, instance, data_types);
                let base_pointer_ty = oomir::Type::pointer(base_oomir_ty);
                let base_pointer = emit_pointer_to_place(
                    &base_place,
                    &base_pointer_ty,
                    &format!("{temp_prefix}_downcast_base"),
                    tcx,
                    instance,
                    mir,
                    data_types,
                    instructions,
                );
                let dest = format!("{temp_prefix}_downcast_pointer");
                instructions.push(oomir::Instruction::AddressRetype {
                    dest: Some(dest.clone()),
                    source: base_pointer,
                    layout: Box::new(oomir::AddressLayout {
                        pointer_type: pointer_ty.clone(),
                        size: rust_layout_size_operand(base_rust_ty, tcx, instance),
                        codec: crate::lower1::types::pointer_view_codec_operand(
                            base_rust_ty,
                            tcx,
                            data_types,
                            instance,
                        ),
                    }),
                });
                return oomir::Operand::Variable {
                    name: dest,
                    ty: pointer_ty.clone(),
                };
            }
            ProjectionElem::Deref => {
                let (base_name, base_instructions, base_ty) =
                    emit_instructions_to_get_on_own(&base_place, tcx, instance, mir, data_types);
                instructions.extend(base_instructions);
                if matches!(
                    normalize_unsize_ty(place.ty(&mir.local_decls, tcx).ty, tcx, instance).kind(),
                    TyKind::Dynamic(..)
                ) && matches!(base_ty, oomir::Type::Interface(_))
                    && matches!(pointer_ty, oomir::Type::Pointer(_))
                {
                    return crate::lower1::value_repr::emit_trait_object_reference_pointer(
                        oomir::Operand::Variable {
                            name: base_name,
                            ty: base_ty,
                        },
                        pointer_ty,
                        &format!("{temp_prefix}_reborrow"),
                        instructions,
                    );
                }
                if matches!(base_ty, oomir::Type::Pointer(_)) {
                    let dest = format!("{temp_prefix}_reborrow");
                    let source = oomir::Operand::Variable {
                        name: base_name.clone(),
                        ty: base_ty.clone(),
                    };
                    if let Some(pointer) = emit_struct_tail_reborrow_view(
                        place.ty(&mir.local_decls, tcx).ty,
                        source.clone(),
                        pointer_ty,
                        &dest,
                        tcx,
                        instance,
                        data_types,
                        instructions,
                    ) {
                        return pointer;
                    }
                    instructions.push(oomir::Instruction::Move {
                        dest: dest.clone(),
                        src: source,
                    });
                    return oomir::Operand::Variable {
                        name: dest,
                        ty: pointer_ty.clone(),
                    };
                }
                if matches!(base_ty, oomir::Type::Slice(_))
                    && matches!(pointer_ty, oomir::Type::Pointer(_))
                {
                    // Resolve generic projections through the monomorphized instance;
                    // querying an unresolved parameter's element type can ICE rustc.
                    let pointee_ty =
                        normalize_unsize_ty(place.ty(&mir.local_decls, tcx).ty, tcx, instance);
                    let element_ty = match pointee_ty.kind() {
                        TyKind::Array(element, _) | TyKind::Slice(element) => *element,
                        other => panic!(
                            "slice-backed dereference resolved to non-sequence pointee {other:?}"
                        ),
                    };
                    let element_pointer = emit_array_pointer(
                        oomir::Operand::Variable {
                            name: base_name,
                            ty: base_ty,
                        },
                        oomir::Operand::Constant(oomir::Constant::I32(0)),
                        rust_layout_size_operand(element_ty, tcx, instance),
                        crate::lower1::types::pointer_view_codec_operand(
                            element_ty, tcx, data_types, instance,
                        ),
                        pointer_ty,
                        &format!("{temp_prefix}_slice_base"),
                        instructions,
                    );
                    let dest = format!("{temp_prefix}_slice_array");
                    instructions.push(oomir::Instruction::AddressRetype {
                        dest: Some(dest.clone()),
                        source: element_pointer,
                        layout: Box::new(oomir::AddressLayout {
                            pointer_type: pointer_ty.clone(),
                            size: rust_layout_size_operand(pointee_ty, tcx, instance),
                            codec: crate::lower1::types::pointer_view_codec_operand(
                                pointee_ty, tcx, data_types, instance,
                            ),
                        }),
                    });
                    return oomir::Operand::Variable {
                        name: dest,
                        ty: pointer_ty.clone(),
                    };
                }
            }
            ProjectionElem::Index(index_local) => {
                let base = array_place_storage(
                    &base_place,
                    temp_prefix,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    instructions,
                );
                let base_ty = base.get_type().expect("array place type");
                if matches!(base_ty, oomir::Type::Array(_) | oomir::Type::Slice(_)) {
                    let index = convert_operand(
                        &MirOperand::Copy(Place::from(*index_local)),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    return emit_array_pointer(
                        base,
                        index,
                        rust_layout_size_operand(place.ty(&mir.local_decls, tcx).ty, tcx, instance),
                        crate::lower1::types::pointer_view_codec_operand(
                            place.ty(&mir.local_decls, tcx).ty,
                            tcx,
                            data_types,
                            instance,
                        ),
                        pointer_ty,
                        &format!("{temp_prefix}_array_ptr"),
                        instructions,
                    );
                }
            }
            ProjectionElem::ConstantIndex {
                offset, from_end, ..
            } => {
                let base = array_place_storage(
                    &base_place,
                    temp_prefix,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    instructions,
                );
                let base_ty = base.get_type().expect("array place type");
                if matches!(base_ty, oomir::Type::Array(_) | oomir::Type::Slice(_)) {
                    let base_operand = base;
                    let index = if *from_end {
                        let length_name = format!("{temp_prefix}_pointer_length");
                        instructions.push(oomir::Instruction::Length {
                            dest: length_name.clone(),
                            array: base_operand.clone(),
                        });
                        let index_name = format!("{temp_prefix}_pointer_index");
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Sub,
                            dest: index_name.clone(),
                            op1: oomir::Operand::Variable {
                                name: length_name,
                                ty: oomir::Type::I32,
                            },
                            op2: oomir::Operand::Constant(oomir::Constant::I32(*offset as i32)),
                        });
                        oomir::Operand::Variable {
                            name: index_name,
                            ty: oomir::Type::I32,
                        }
                    } else {
                        oomir::Operand::Constant(oomir::Constant::I32(*offset as i32))
                    };
                    return emit_array_pointer(
                        base_operand,
                        index,
                        rust_layout_size_operand(place.ty(&mir.local_decls, tcx).ty, tcx, instance),
                        crate::lower1::types::pointer_view_codec_operand(
                            place.ty(&mir.local_decls, tcx).ty,
                            tcx,
                            data_types,
                            instance,
                        ),
                        pointer_ty,
                        &format!("{temp_prefix}_array_ptr"),
                        instructions,
                    );
                }
            }
            ProjectionElem::Field(field_index, _) => {
                let field_rust_ty = place.ty(&mir.local_decls, tcx).ty;
                let base_rust_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                if let Some(payload) = crate::lower1::types::transparent_payload(base_rust_ty, tcx)
                {
                    if field_index.index() == payload.field {
                        return emit_pointer_to_place(
                            &base_place,
                            pointer_ty,
                            temp_prefix,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            instructions,
                        );
                    }
                    let parent_ty = get_place_type(&base_place, mir, tcx, instance, data_types);
                    let mut parent = emit_pointer_to_place(
                        &base_place,
                        &oomir::Type::pointer(parent_ty),
                        temp_prefix,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    let layout = tcx
                        .layout_of(TypingEnv::fully_monomorphized().as_query_input(base_rust_ty))
                        .expect("owner layout was checked by transparent_payload");
                    let offset = layout.fields.offset(field_index.index()).bytes();
                    if offset != 0 {
                        let dest = format!("{temp_prefix}_marker_offset");
                        let ty = parent.get_type().expect("owner has an address");
                        instructions.push(oomir::Instruction::AddressOffset {
                            dest: Some(dest.clone()),
                            source: parent,
                            count: oomir::Operand::Constant(oomir::Constant::U64(offset)),
                            ty: ty.clone(),
                            bytes: true,
                            wrapping: false,
                            subtract: false,
                        });
                        parent = oomir::Operand::Variable { name: dest, ty };
                    }
                    let dest = format!("{temp_prefix}_marker_pointer");
                    instructions.push(oomir::Instruction::AddressRetype {
                        dest: Some(dest.clone()),
                        source: parent,
                        layout: Box::new(oomir::AddressLayout {
                            pointer_type: pointer_ty.clone(),
                            size: rust_layout_size_operand(field_rust_ty, tcx, instance),
                            codec: crate::lower1::types::pointer_view_codec_operand(
                                field_rust_ty,
                                tcx,
                                data_types,
                                instance,
                            ),
                        }),
                    });
                    return oomir::Operand::Variable {
                        name: dest,
                        ty: pointer_ty.clone(),
                    };
                }
                if let Some(word) = crate::lower1::types::packed_word(base_rust_ty, tcx) {
                    let owner_type = oomir::Type::pointer(word.jvm_type());
                    let owner = emit_pointer_to_place(
                        &base_place,
                        &owner_type,
                        &format!("{temp_prefix}_owner"),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    let field_type = ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                    return word.field_address(
                        owner,
                        field_index.index(),
                        &field_type,
                        temp_prefix,
                        instructions,
                    );
                }
                if field_index.index() == 0
                    && let Some(layout) = crate::lower1::types::tagged_scalar(base_rust_ty, tcx)
                {
                    let Some((ProjectionElem::Downcast(_, variant), parent)) = prefix.split_last()
                    else {
                        panic!("tagged payload needs a variant");
                    };
                    assert_eq!(variant.as_u32(), 1);
                    let owner_type = oomir::Type::pointer(oomir::Type::TaggedI64);
                    let owner = emit_pointer_to_place(
                        &Place {
                            local: place.local,
                            projection: tcx.mk_place_elems(parent),
                        },
                        &owner_type,
                        &format!("{temp_prefix}_owner"),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    let offset = format!("{temp_prefix}_payload_offset");
                    instructions.push(oomir::Instruction::AddressOffset {
                        dest: Some(offset.clone()),
                        source: owner,
                        count: oomir::Operand::Constant(oomir::Constant::U64(
                            layout.payload_offset as u64,
                        )),
                        ty: owner_type.clone(),
                        bytes: true,
                        wrapping: false,
                        subtract: false,
                    });
                    let dest = format!("{temp_prefix}_payload_address");
                    instructions.push(oomir::Instruction::AddressRetype {
                        dest: Some(dest.clone()),
                        source: oomir::Operand::Variable {
                            name: offset,
                            ty: owner_type,
                        },
                        layout: Box::new(oomir::AddressLayout {
                            pointer_type: pointer_ty.clone(),
                            size: oomir::Operand::Constant(oomir::Constant::I32(8)),
                            codec: oomir::Operand::Constant(oomir::Constant::Null(
                                oomir::Type::java_string(),
                            )),
                        }),
                    });
                    return oomir::Operand::Variable {
                        name: dest,
                        ty: pointer_ty.clone(),
                    };
                }
                if field_index.index() == 0
                    && crate::lower1::types::direct_enum_payload(base_rust_ty, tcx).is_some()
                {
                    let Some((ProjectionElem::Downcast(_, variant), parent)) = prefix.split_last()
                    else {
                        panic!("nullable enum field needs a variant projection");
                    };
                    if *variant
                        != crate::lower1::types::enum_carrier(base_rust_ty, tcx)
                            .unwrap()
                            .variant
                    {
                        instructions.push(oomir::Instruction::Unreachable);
                        return oomir::Operand::Constant(oomir::Constant::Null(pointer_ty.clone()));
                    }
                    return emit_pointer_to_place(
                        &Place {
                            local: place.local,
                            projection: tcx.mk_place_elems(parent),
                        },
                        pointer_ty,
                        temp_prefix,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                }
                let managed_base_rust_ty = match base_rust_ty.kind() {
                    TyKind::Ref(_, inner, _) => *inner,
                    _ => base_rust_ty,
                };
                // Preserve allocation provenance for aggregate field pointers;
                // the runtime still uses write-through cells for managed receivers.
                let use_managed_field = (matches!(
                    managed_base_rust_ty.kind(),
                    TyKind::Adt(adt_def, _) if adt_def.is_struct() || adt_def.is_enum()
                ) || matches!(
                    managed_base_rust_ty.kind(),
                    TyKind::Tuple(_) | TyKind::Closure(..) | TyKind::Coroutine(..)
                )) && matches!(pointer_ty, oomir::Type::Pointer(_));
                let managed_field = if use_managed_field {
                    // For an enum downcast the place carrier is the concrete
                    // variant class, while the Rust type remains the base ADT.
                    let base_oomir_ty = get_place_type(&base_place, mir, tcx, instance, data_types);
                    let owner_class = match &base_oomir_ty {
                        oomir::Type::Class(owner_class) => Some(owner_class),
                        oomir::Type::Pointer(inner) => match inner.as_ref() {
                            oomir::Type::Class(owner_class) => Some(owner_class),
                            _ => None,
                        },
                        _ => None,
                    };
                    match owner_class {
                        Some(owner_class) => {
                            let coroutine_variant =
                                base_place.projection.iter().rev().find_map(|projection| {
                                    match projection {
                                        ProjectionElem::Downcast(_, variant) => Some(variant),
                                        _ => None,
                                    }
                                });
                            coroutine_variant
                                .and_then(|variant| {
                                    coroutine_saved_field_name(
                                        base_rust_ty,
                                        variant,
                                        field_index.index(),
                                        tcx,
                                    )
                                })
                                .or_else(|| {
                                    crate::lower1::place::field_name_for_projection(
                                        owner_class,
                                        field_index.index(),
                                        base_rust_ty,
                                        tcx,
                                        data_types,
                                    )
                                    .ok()
                                })
                                .map(|field_name| {
                                    (base_oomir_ty.clone(), owner_class.clone(), field_name)
                                })
                        }
                        _ => None,
                    }
                } else {
                    None
                };
                if let Some((base_oomir_ty, owner_class, field_name)) = managed_field {
                    let base_pointer_ty = match base_oomir_ty {
                        oomir::Type::Pointer(_) => base_oomir_ty.clone(),
                        _ => oomir::Type::pointer(base_oomir_ty.clone()),
                    };
                    let base_pointer = emit_pointer_to_place(
                        &base_place,
                        &base_pointer_ty,
                        &format!("{temp_prefix}_dst_field_base"),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    if matches!(managed_base_rust_ty.kind(), TyKind::Tuple(_))
                        || matches!(managed_base_rust_ty.kind(), TyKind::Closure(..))
                        || matches!(managed_base_rust_ty.kind(), TyKind::Adt(adt_def, _) if adt_def.is_struct())
                    {
                        let base_layout = tcx
                            .layout_of(
                                TypingEnv::fully_monomorphized()
                                    .as_query_input(managed_base_rust_ty),
                            )
                            .unwrap_or_else(|error| {
                                panic!("could not determine struct field pointer layout: {error:?}")
                            });
                        let field_offset = base_layout
                            .fields
                            .offset((*field_index).into())
                            .bytes_usize();
                        let result_name = format!("{temp_prefix}_struct_field_pointer");
                        instructions.push(crate::lower1::place::project_memory(
                            result_name.clone(),
                            base_pointer,
                            owner_class,
                            field_name,
                            field_rust_ty,
                            field_offset as u64,
                            tcx,
                            instance,
                            data_types,
                        ));
                        return oomir::Operand::Variable {
                            name: result_name,
                            ty: pointer_ty.clone(),
                        };
                    }
                    let base_value_name = format!("{temp_prefix}_dst_field_owner");
                    let base_value = emit_pointer_read(
                        base_pointer,
                        &base_oomir_ty,
                        &base_value_name,
                        instructions,
                    );
                    let result_name = format!("{temp_prefix}_dst_field_pointer");
                    let field_alignment = rust_layout_alignment(field_rust_ty, tcx, instance);
                    let explicitly_aligned = field_alignment > 16;
                    let mut field_params = vec![
                        (
                            "owner".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("field_name".to_string(), oomir::Type::java_string()),
                        ("size".to_string(), oomir::Type::U64),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ];
                    let mut field_args = vec![
                        base_value,
                        oomir::Operand::Constant(oomir::Constant::LiteralString(field_name)),
                        rust_layout_size_operand(field_rust_ty, tcx, instance),
                        crate::lower1::types::pointer_view_codec_operand(
                            field_rust_ty,
                            tcx,
                            data_types,
                            instance,
                        ),
                    ];
                    if explicitly_aligned {
                        field_params.push(("alignment".to_string(), oomir::Type::U64));
                        field_args.push(oomir::Operand::Constant(oomir::Constant::U64(
                            u64::try_from(field_alignment)
                                .expect("Rust layout alignment exceeds u64"),
                        )));
                    }
                    instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(result_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: if explicitly_aligned {
                            "fieldAligned".to_string()
                        } else {
                            "field".to_string()
                        },
                        method_ty: oomir::Signature {
                            params: field_params,
                            ret: Box::new(pointer_ty.clone()),
                            is_static: true,
                        },
                        args: field_args,
                    });
                    return oomir::Operand::Variable {
                        name: result_name,
                        ty: pointer_ty.clone(),
                    };
                }
                let is_aggregate = matches!(base_rust_ty.kind(), TyKind::Tuple(_))
                    || matches!(
                        base_rust_ty.kind(),
                        TyKind::Adt(adt_def, _) if adt_def.is_struct() || adt_def.is_union()
                    );
                if is_aggregate {
                    let base_layout = tcx
                        .layout_of(TypingEnv::fully_monomorphized().as_query_input(base_rust_ty))
                        .unwrap_or_else(|error| {
                            panic!("could not determine aggregate field pointer layout: {error:?}")
                        });
                    let field_offset = base_layout
                        .fields
                        .offset((*field_index).into())
                        .bytes_usize();
                    let base_oomir_ty = ty_to_oomir_type(base_rust_ty, tcx, data_types, instance);
                    let base_pointer_ty = oomir::Type::pointer(base_oomir_ty);
                    let base_pointer = emit_pointer_to_place(
                        &base_place,
                        &base_pointer_ty,
                        &format!("{temp_prefix}_field_base"),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    let offset_pointer_name = format!("{temp_prefix}_field_offset_pointer");
                    instructions.push(oomir::Instruction::AddressOffset {
                        dest: Some(offset_pointer_name.clone()),
                        source: base_pointer,
                        count: oomir::Operand::Constant(oomir::Constant::U64(
                            u64::try_from(field_offset)
                                .expect("Rust aggregate field offset exceeds u64"),
                        )),
                        ty: base_pointer_ty.clone(),
                        bytes: true,
                        wrapping: false,
                        subtract: false,
                    });
                    let result_name = format!("{temp_prefix}_field_pointer");
                    instructions.push(oomir::Instruction::AddressRetype {
                        dest: Some(result_name.clone()),
                        source: oomir::Operand::Variable {
                            name: offset_pointer_name,
                            ty: pointer_ty.clone(),
                        },
                        layout: Box::new(oomir::AddressLayout {
                            pointer_type: pointer_ty.clone(),
                            size: rust_layout_size_operand(
                                place.ty(&mir.local_decls, tcx).ty,
                                tcx,
                                instance,
                            ),
                            codec: crate::lower1::types::pointer_view_codec_operand(
                                place.ty(&mir.local_decls, tcx).ty,
                                tcx,
                                data_types,
                                instance,
                            ),
                        }),
                    });
                    return oomir::Operand::Variable {
                        name: result_name,
                        ty: pointer_ty.clone(),
                    };
                }
            }
            _ => {}
        }
    }

    let (value_name, value_instructions, value_ty) =
        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
    instructions.extend(value_instructions);
    emit_pointer_factory(
        "cell",
        vec![
            oomir::Operand::Variable {
                name: value_name,
                ty: value_ty,
            },
            rust_layout_size_operand(place.ty(&mir.local_decls, tcx).ty, tcx, instance),
            crate::lower1::types::pointer_memory_codec_operand(
                place.ty(&mir.local_decls, tcx).ty,
                tcx,
                data_types,
                instance,
            ),
        ],
        pointer_ty,
        &format!("{temp_prefix}_cell_ptr"),
        instructions,
    )
}
