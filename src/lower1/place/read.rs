//! Read operations on Rust places.
use super::*;

pub(crate) fn emit_instructions_to_get_recursive<'tcx>(
    place: &Place<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> (String, Vec<Instruction>, oomir::Type) {
    // Start with the base local.
    let current_place = Place {
        local: place.local,
        projection: tcx.mk_place_elems(&[]),
    };
    let mut current_var = place_to_string(&current_place, tcx);
    let mut current_type = get_place_type(&current_place, mir, tcx, instance, data_types);
    let mut instructions = vec![];
    let mut coroutine_variant = None;
    if data_types.local_uses_stable_cell(place.local) {
        let pointee_type = current_type.clone();
        let value_name = format!("{}_value", local_cell_name(place.local));
        let value = emit_pointer_read(
            Operand::Variable {
                name: local_cell_name(place.local),
                ty: oomir::Type::Pointer(Box::new(pointee_type.clone())),
            },
            &pointee_type,
            &value_name,
            &mut instructions,
        );
        if let Operand::Variable { name, ty } = value {
            current_var = name;
            current_type = ty;
        }
    }
    let base_rust_ty = current_place.ty(&mir.local_decls, tcx).ty;
    if let Some(value) = super::super::value_repr::materialize_implicit_zst(
        base_rust_ty,
        &format!("{}_implicit", current_var),
        tcx,
        instance,
        data_types,
        &mut instructions,
    ) {
        current_var = value
            .get_name()
            .expect("materialized value must use a temporary")
            .to_string();
        current_type = value
            .get_type()
            .expect("materialized value must have a JVM type");
    }

    // Iterate over each projection element in the order they appear.
    for (proj_index, proj) in place.projection.iter().enumerate() {
        let type_before_proj = current_type.clone();
        match proj {
            ProjectionElem::Field(field_index, field_ty) => {
                let base_place_for_field = projection_prefix_place(place, proj_index, tcx);
                let base_rust_ty =
                    EarlyBinder::bind(tcx, base_place_for_field.ty(&mir.local_decls, tcx).ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();

                if field_index.index() == 0
                    && proj_index > 0
                    && let ProjectionElem::Downcast(_, variant_idx) =
                        place.projection[proj_index - 1]
                    && let TyKind::Adt(adt_def, substs) = base_rust_ty.kind()
                    && jvm_subtype_payload_ty(adt_def, adt_def.variant(variant_idx), substs, tcx)
                        .is_some()
                {
                    // A transparent subtype case has no wrapper field: after
                    // the downcast, projecting its sole Rust payload is an
                    // identity at the JVM level.
                    current_type = ty_to_oomir_type(field_ty, tcx, data_types, instance);
                    continue;
                }

                if field_index.index() == 0
                    && matches!(
                        base_rust_ty.kind(),
                        TyKind::Adt(adt_def, _)
                            if crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
                    )
                    && matches!(current_type, oomir::Type::Pointer(_))
                {
                    // Sized NonNull<T> is represented by its pointer carrier, so
                    // projecting its sole transparent field is an identity.
                    current_type = ty_to_oomir_type(field_ty, tcx, data_types, instance);
                    continue;
                }

                let has_slice_tail = matches!(current_type, oomir::Type::Pointer(_))
                    && has_slice_or_str_struct_tail(tcx, base_rust_ty);
                if has_slice_tail {
                    let layout = tcx
                        .layout_of(
                            TypingEnv::fully_monomorphized().as_query_input(base_rust_ty),
                        )
                        .unwrap_or_else(|error| {
                            panic!(
                                "could not determine slice-tailed struct layout for {base_rust_ty:?}: {error:?}"
                            )
                        });
                    let field_offset = layout.fields.offset(field_index.index()).bytes_usize();
                    let field_rust_ty = EarlyBinder::bind(tcx, field_ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let base_pointer_name = current_var.clone();
                    let base_pointer_ty = current_type.clone();
                    let oomir::Type::Pointer(base_pointee_ty) = &base_pointer_ty else {
                        unreachable!();
                    };
                    let oomir::Type::Class(owner_class) = base_pointee_ty.as_ref() else {
                        panic!("slice-tailed Rust struct did not map to a JVM class");
                    };
                    let owner_class = owner_class.clone();
                    let field_name = field_name_for_projection(
                        &owner_class,
                        field_index.index(),
                        base_rust_ty,
                        tcx,
                        data_types,
                    )
                    .unwrap_or_else(|error| panic!("Error getting DST field name: {error}"));
                    let field_offset = Operand::Constant(oomir::Constant::U64(
                        u64::try_from(field_offset).expect("Rust DST field offset exceeds u64"),
                    ));

                    if field_rust_ty.is_str() {
                        current_type = oomir::Type::Str;
                        let object_name = format!("{current_var}_str_object");
                        instructions.push(Instruction::InvokeVirtual {
                            dest: Some(object_name.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "projectStructStrField".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), base_pointer_ty.clone()),
                                    ("owner_class".to_string(), oomir::Type::java_string()),
                                    ("field_name".to_string(), oomir::Type::java_string()),
                                    ("field_offset".to_string(), oomir::Type::U64),
                                ],
                                ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                                is_static: false,
                            },
                            args: vec![
                                Operand::Constant(oomir::Constant::String(owner_class)),
                                Operand::Constant(oomir::Constant::String(field_name)),
                                field_offset,
                            ],
                            operand: Operand::Variable {
                                name: base_pointer_name,
                                ty: base_pointer_ty,
                            },
                        });
                        let next_var = format!("{current_var}_{}", field_index.index());
                        instructions.push(Instruction::Cast {
                            dest: next_var.clone(),
                            op: Operand::Variable {
                                name: object_name,
                                ty: oomir::Type::Class("java/lang/Object".to_string()),
                            },
                            ty: current_type.clone(),
                        });
                        current_var = next_var;
                    } else if let TyKind::Slice(element_rust_ty) = field_rust_ty.kind() {
                        let element_oomir_ty =
                            ty_to_oomir_type(*element_rust_ty, tcx, data_types, instance);
                        current_type = oomir::Type::Slice(Box::new(element_oomir_ty));
                        let object_name = format!("{current_var}_slice_object");
                        instructions.push(Instruction::InvokeVirtual {
                            dest: Some(object_name.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "projectStructSliceField".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), base_pointer_ty.clone()),
                                    ("owner_class".to_string(), oomir::Type::java_string()),
                                    ("field_name".to_string(), oomir::Type::java_string()),
                                    ("field_offset".to_string(), oomir::Type::U64),
                                    ("element_size".to_string(), oomir::Type::U64),
                                    ("element_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                                is_static: false,
                            },
                            args: vec![
                                Operand::Constant(oomir::Constant::String(owner_class)),
                                Operand::Constant(oomir::Constant::String(field_name)),
                                field_offset,
                                Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(
                                        super::super::types::layout_size_bytes(
                                            tcx,
                                            *element_rust_ty,
                                        )
                                        .expect("slice tail element must have a layout"),
                                    )
                                    .expect("Rust slice-tail element layout exceeds u64"),
                                )),
                                pointer_view_codec_operand(
                                    *element_rust_ty,
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                            operand: Operand::Variable {
                                name: base_pointer_name,
                                ty: base_pointer_ty,
                            },
                        });
                        let next_var = format!("{current_var}_{}", field_index.index());
                        instructions.push(Instruction::Cast {
                            dest: next_var.clone(),
                            op: Operand::Variable {
                                name: object_name,
                                ty: oomir::Type::Class("java/lang/Object".to_string()),
                            },
                            ty: current_type.clone(),
                        });
                        current_var = next_var;
                    } else {
                        current_type = ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                        let field_pointer_ty = oomir::Type::Pointer(Box::new(current_type.clone()));
                        let typed_pointer_name = format!("{current_var}_typed_field_pointer");
                        instructions.push(Instruction::InvokeVirtual {
                            dest: Some(typed_pointer_name.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "projectStructField".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), base_pointer_ty.clone()),
                                    ("owner_class".to_string(), oomir::Type::java_string()),
                                    ("field_name".to_string(), oomir::Type::java_string()),
                                    ("field_offset".to_string(), oomir::Type::U64),
                                    ("field_size".to_string(), oomir::Type::U64),
                                    ("field_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(field_pointer_ty.clone()),
                                is_static: false,
                            },
                            args: vec![
                                Operand::Constant(oomir::Constant::String(owner_class)),
                                Operand::Constant(oomir::Constant::String(field_name)),
                                field_offset,
                                Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(
                                        super::super::types::layout_size_bytes(tcx, field_rust_ty)
                                            .expect("sized DST field must have a layout"),
                                    )
                                    .expect("Rust DST field layout exceeds u64"),
                                )),
                                pointer_view_codec_operand(
                                    field_rust_ty,
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                            operand: Operand::Variable {
                                name: base_pointer_name,
                                ty: base_pointer_ty,
                            },
                        });
                        let next_var = format!("{current_var}_{}", field_index.index());
                        let value = emit_pointer_read(
                            Operand::Variable {
                                name: typed_pointer_name,
                                ty: field_pointer_ty,
                            },
                            &current_type,
                            &next_var,
                            &mut instructions,
                        );
                        current_var = value.get_name().unwrap_or(&next_var).to_string();
                    }
                    continue;
                }
                if let Some((adt_def, _substs)) = union_parts_from_ty(base_rust_ty) {
                    let owner_class_name =
                        match ty_to_oomir_type(base_rust_ty, tcx, data_types, instance) {
                            oomir::Type::Class(name) => name,
                            oomir::Type::Reference(inner)
                                if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                            {
                                if let oomir::Type::Class(name) = inner.as_ref() {
                                    name.clone()
                                } else {
                                    unreachable!()
                                }
                            }
                            other => {
                                panic!("Union field access on non-class OOMIR type: {:?}", other)
                            }
                        };
                    let field_name = union_field_name(adt_def, field_index.index(), tcx);
                    let next_var = format!("{}_{}", current_var, field_index.index());
                    let obj_type = current_type.clone();
                    current_type = ty_to_oomir_type(field_ty, tcx, data_types, instance);
                    let is_unit = !current_type.has_jvm_value();
                    instructions.push(oomir::Instruction::InvokeVirtual {
                        dest: (!is_unit).then_some(next_var.clone()),
                        class_name: owner_class_name.clone(),
                        method_name: union_getter_method_name(&field_name),
                        method_ty: oomir::Signature {
                            params: vec![(
                                "self".to_string(),
                                oomir::Type::Class(owner_class_name),
                            )],
                            ret: Box::new(current_type.clone()),
                            is_static: false,
                        },
                        args: vec![],
                        operand: Operand::Variable {
                            name: current_var.clone(),
                            ty: obj_type,
                        },
                    });
                    current_var = next_var;
                    continue;
                }

                // Get the owner class name and field name.
                let owner_class_name = match &current_type {
                    oomir::Type::Class(name) => name.clone(),
                    oomir::Type::Reference(inner)
                        if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                    {
                        if let oomir::Type::Class(name) = inner.as_ref() {
                            name.clone()
                        } else {
                            unreachable!()
                        }
                    }
                    _ => panic!(
                        "Field access on non-class type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };

                let field_name = match coroutine_variant
                    .and_then(|variant| {
                        coroutine_saved_field_name(base_rust_ty, variant, field_index.index(), tcx)
                    })
                    .map(Ok)
                    .unwrap_or_else(|| {
                        field_name_for_projection(
                            &owner_class_name,
                            field_index.index(),
                            base_rust_ty,
                            tcx,
                            data_types,
                        )
                    }) {
                    Ok(name) => name,
                    Err(e) => panic!("Error getting field name: {}", e),
                };
                coroutine_variant = None;

                // Create a temporary name for the result of this field access.
                let next_var = format!("{}_{}", current_var, field_index.index());
                let obj_type = current_type.clone();
                // The class declaration is the source of truth for the JVM field
                // descriptor. A projected Rust field can carry a reference type
                // that is semantically equivalent but uses another JVM carrier
                // (notably &[T; N] as SliceView versus *const [T; N] as Pointer).
                let projected_type = ty_to_oomir_type(field_ty, tcx, data_types, instance);
                current_type = match data_types.get(&owner_class_name) {
                    Some(oomir::DataType::Class { fields, .. }) => fields
                        .iter()
                        .find_map(|(name, ty)| (name == &field_name).then(|| ty.clone()))
                        .unwrap_or(projected_type),
                    _ => projected_type,
                };
                instructions.push(oomir::Instruction::GetField {
                    dest: next_var.clone(),
                    object: Operand::Variable {
                        name: current_var.clone(),
                        ty: obj_type,
                    },
                    field_name,
                    field_ty: current_type.clone(),
                    owner_class: owner_class_name,
                });
                // Update current variable.
                current_var = next_var;
            }
            ProjectionElem::Index(index_local) => {
                let type_before_proj = current_type.clone();
                // Convert the MIR index operand.
                let index_operand = convert_operand(
                    &MirOperand::Copy(Place::from(index_local)),
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );
                // Create a temporary name for the array element.
                let next_var = format!("{}_elem", current_var);
                // Determine element type from the current type (which should be an array or reference-to-array).
                current_type = match &current_type {
                    oomir::Type::Array(inner) | oomir::Type::Slice(inner) => inner.as_ref().clone(),
                    oomir::Type::Reference(inner)
                        if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                    {
                        if let oomir::Type::Array(element_type) = inner.as_ref() {
                            element_type.as_ref().clone()
                        } else {
                            unreachable!()
                        }
                    }
                    _ => panic!(
                        "Index access on non-array type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };
                instructions.push(oomir::Instruction::ArrayGet {
                    dest: next_var.clone(),
                    array: Operand::Variable {
                        name: current_var.clone(),
                        ty: type_before_proj,
                    },
                    index: index_operand,
                });
                current_var = next_var;
            }
            ProjectionElem::ConstantIndex {
                offset,
                min_length: _,
                from_end,
            } => {
                let type_before_proj = current_type.clone();
                let next_var = format!("{}_elem", current_var);
                // Determine element type based on current_type being an array or reference-to-array.
                current_type = match &current_type {
                    oomir::Type::Array(inner) | oomir::Type::Slice(inner) => inner.as_ref().clone(),
                    oomir::Type::Reference(inner)
                        if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                    {
                        if let oomir::Type::Array(element_type) = inner.as_ref() {
                            element_type.as_ref().clone()
                        } else {
                            unreachable!()
                        }
                    }
                    _ => panic!(
                        "Constant index access on non-array type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };

                if !from_end {
                    // Simple constant index: array[offset]
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: next_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                        index: Operand::Constant(oomir::Constant::I32(offset as i32)),
                    });
                } else {
                    // For access from the end: calculate length - offset.
                    let len_var = format!("{}_len", current_var);
                    instructions.push(oomir::Instruction::Length {
                        dest: len_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                    });
                    let calc_idx_var = format!("{}_calc_idx", current_var);
                    instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Sub,
                        dest: calc_idx_var.clone(),
                        op1: Operand::Variable {
                            name: len_var,
                            ty: oomir::Type::I32,
                        },
                        op2: Operand::Constant(oomir::Constant::I32(offset as i32)),
                    });
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: next_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                        index: Operand::Variable {
                            name: calc_idx_var,
                            ty: oomir::Type::I32,
                        },
                    });
                }
                current_var = next_var;
            }
            ProjectionElem::Subslice { from, to, from_end } => {
                let next_var = format!("{}_slice_{}", current_var, proj_index);
                current_type = emit_slice_view(
                    Operand::Variable {
                        name: current_var,
                        ty: type_before_proj.clone(),
                    },
                    &type_before_proj,
                    from,
                    to,
                    from_end,
                    &next_var,
                    &mut instructions,
                );
                current_var = next_var;
            }
            ProjectionElem::Deref => {
                let base_place_for_deref = projection_prefix_place(place, proj_index, tcx);
                let base_rust_ty =
                    EarlyBinder::bind(tcx, base_place_for_deref.ty(&mir.local_decls, tcx).ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                let pointer_pointee = match base_rust_ty.kind() {
                    TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => Some(*pointee),
                    _ => None,
                };
                let preserve_slice_tailed_pointer = pointer_pointee
                    .is_some_and(|pointee| has_slice_or_str_struct_tail(tcx, pointee));
                if preserve_slice_tailed_pointer {
                    continue;
                }
                if matches!(type_before_proj, oomir::Type::Slice(_))
                    && pointer_pointee.is_some_and(|pointee| pointee.is_array())
                {
                    // Borrowed fixed arrays use SliceView so references can retain an
                    // offset into shared storage. Dereferencing the reference produces
                    // an array value again; make that carrier transition explicit so a
                    // subsequent Copy detaches the array as Rust requires.
                    let pointee_type =
                        ty_to_oomir_type(pointer_pointee.unwrap(), tcx, data_types, instance);
                    let next_var = format!("{}_deref", current_var);
                    instructions.push(oomir::Instruction::Cast {
                        dest: next_var.clone(),
                        op: Operand::Variable {
                            name: current_var,
                            ty: type_before_proj,
                        },
                        ty: pointee_type.clone(),
                    });
                    current_var = next_var;
                    current_type = pointee_type;
                    continue;
                }
                match type_before_proj {
                    oomir::Type::Pointer(element_type) => {
                        let next_var = format!("{}_deref", current_var);
                        let result = emit_pointer_read(
                            Operand::Variable {
                                name: current_var.clone(),
                                ty: oomir::Type::Pointer(element_type.clone()),
                            },
                            element_type.as_ref(),
                            &next_var,
                            &mut instructions,
                        );
                        current_var = result.get_name().unwrap_or(&next_var).to_string();
                        current_type = element_type.as_ref().clone();
                    }
                    oomir::Type::MutableReference(_) => {
                        let type_before_deref = current_type.clone();

                        match type_before_deref.clone() {
                            oomir::Type::MutableReference(element_type) => {
                                // Create a temporary variable name for the dereferenced value
                                let next_var = format!("{}_deref", current_var);

                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Info,
                                    "place-lowering",
                                    format!(
                                        "Info: Handling Deref: Var '{}' ({:?}) -> Temp Var '{}' (Type: {:?})",
                                        current_var,
                                        type_before_deref,
                                        next_var,
                                        element_type.as_ref()
                                    )
                                );

                                instructions.push(oomir::Instruction::ArrayGet {
                                    dest: next_var.clone(),
                                    array: Operand::Variable {
                                        name: current_var.clone(),
                                        ty: type_before_deref, // The type is Array(T)
                                    },
                                    // Index is always 0 for our reference representation
                                    index: Operand::Constant(oomir::Constant::I32(0)),
                                });

                                // Update current_var and current_type for subsequent projections
                                current_var = next_var;
                                current_type = element_type.as_ref().clone(); // Type becomes T
                            }
                            _ => {
                                panic!(
                                    "Attempted to Deref a non-reference (non-array) type: \
                             Variable '{}' has type {:?}. Place: {:?}",
                                    current_var,
                                    current_type, // Use the original current_type for the error
                                    place
                                );
                            }
                        }
                    }
                    _ => {
                        // no op
                    }
                }
            }
            ProjectionElem::Downcast(_, variant_idx) => {
                // A downcast changes the *effective type* for subsequent projections.

                // 1. Get the AdtDef of the enum to find the variant's actual name.
                //    We need the Rust Ty of the base enum *before* the downcast.
                let base_place_proj_slice = &place.projection[..proj_index]; // Projections *before* this downcast
                let base_place_for_downcast = Place {
                    local: place.local,
                    projection: tcx.mk_place_elems(base_place_proj_slice),
                };
                let base_rust_ty = base_place_for_downcast.ty(&mir.local_decls, tcx).ty;
                let base_rust_ty = EarlyBinder::bind(tcx, base_rust_ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();

                let coroutine_ty = match base_rust_ty.kind() {
                    TyKind::Coroutine(..) => Some(base_rust_ty),
                    TyKind::Ref(_, inner, _) if matches!(inner.kind(), TyKind::Coroutine(..)) => {
                        Some(*inner)
                    }
                    _ => None,
                };
                if coroutine_ty.is_some() {
                    // Coroutine variants occupy one mutable state-machine
                    // object. The downcast only selects how the following
                    // field index is interpreted; no JVM cast is required.
                    coroutine_variant = Some(variant_idx);
                    continue;
                }

                let (adt_def, substs) = match base_rust_ty.kind() {
                    TyKind::Adt(adt, s) => (*adt, s),
                    TyKind::Ref(_, ty, _) => match ty.kind() {
                        // Handle reference to enum
                        TyKind::Adt(adt, s) => (*adt, s),
                        _ => panic!(
                            "Downcast base is Ref to non-ADT: {:?} ({:?})",
                            base_rust_ty, place
                        ),
                    },
                    _ => panic!(
                        "Downcast base is not an ADT: {:?} ({:?})",
                        base_rust_ty, place
                    ),
                };

                if !adt_def.is_enum() {
                    panic!(
                        "Downcast applied to non-enum ADT: {:?} ({:?})",
                        adt_def, place
                    );
                }

                // Derive the base from the canonical Rust ADT rather than the
                // current lowered carrier. The latter may already be refined
                // to a variant by an earlier projection, which would otherwise
                // manufacture invalid names such as `$Break$Break`.
                let base_enum_oomir_name =
                    generate_adt_jvm_class_name(&adt_def, substs, tcx, data_types, instance);
                force_define_named_adt(
                    Ty::new_adt(tcx, adt_def, substs),
                    tcx,
                    data_types,
                    instance,
                );

                // 2. Get the specific variant definition using the index.
                let variant_def = adt_def.variant(variant_idx);

                // 3. Construct the OOMIR variant class name
                let variant_class_name = format!(
                    "{}${}",
                    base_enum_oomir_name, // Use OOMIR name already derived
                    jvm_names::member_name(&variant_def.name.to_string())
                );
                let transparent_payload =
                    jvm_subtype_payload_ty(&adt_def, variant_def, substs, tcx);
                let variant_runtime_type = transparent_payload
                    .map(|payload_ty| ty_to_oomir_type(payload_ty, tcx, data_types, instance))
                    .unwrap_or_else(|| oomir::Type::Class(variant_class_name.clone()));

                current_type = variant_runtime_type.clone();

                // insert a Cast instruction to convert the base enum to the variant class
                instructions.push(oomir::Instruction::Cast {
                    dest: current_var.clone(),
                    op: Operand::Variable {
                        name: current_var.clone(),
                        ty: type_before_proj,
                    },
                    ty: variant_runtime_type,
                });

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "place-lowering",
                    format!(
                        "Info: Handled Downcast: Variant {}({}), BaseEnum='{}', New Type (Variant Class): {:?}, Var: {}",
                        variant_def.name,
                        variant_idx.index(),
                        base_enum_oomir_name,
                        current_type,
                        current_var
                    )
                );
            }
            // Will add more projection kinds when needed.
            _ => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "place-lowering",
                    format!(
                        "Warning: Unhandled projection element in nested access: {:?}. Skipping.",
                        proj
                    )
                );
            }
        }
    }

    (current_var, instructions, current_type)
}

/// Helper to get the OOMIR type for a Place.
pub(crate) fn emit_instructions_to_get_on_own<'tcx>(
    place: &Place<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> (String, Vec<Instruction>, oomir::Type) {
    // Delegate the recursive handling.
    emit_instructions_to_get_recursive(place, tcx, instance, mir, data_types)
}
