//! Write operations on Rust places.
use super::*;

/// Generates OOMIR instructions to store the `source_operand` value into the `dest_place`.
///
/// This function handles assignments recursively. It first generates instructions
/// to get the object or array that contains the final field/element, and then
/// generates the appropriate SetField or ArrayStore instruction.
pub(crate) fn emit_instructions_to_set_value<'tcx>(
    dest_place: &Place<'tcx>,
    source_operand: Operand, // The OOMIR value to store
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    let target_rust_ty = dest_place.ty(&mir.local_decls, tcx).ty;
    let source_operand = super::super::value_repr::adapt_operand_to_rust_type(
        source_operand,
        target_rust_ty,
        &format!("{}_assignment", place_to_string(dest_place, tcx)),
        tcx,
        instance,
        data_types,
        &mut instructions,
    );
    if let Some((pointer, ty)) = indirect_field_address(
        dest_place,
        tcx,
        instance,
        mir,
        data_types,
        &mut instructions,
    ) {
        emit_pointer_write(pointer, &ty, source_operand, &mut instructions);
        return instructions;
    }

    if dest_place.projection.is_empty() {
        if data_types.local_uses_stable_cell(dest_place.local) {
            let pointee_type = get_place_type(dest_place, mir, tcx, instance, data_types);
            emit_pointer_write(
                Operand::Variable {
                    name: local_cell_name(dest_place.local),
                    ty: oomir::Type::pointer(pointee_type.clone()),
                },
                &pointee_type,
                source_operand,
                &mut instructions,
            );
        } else {
            // e.g., _1 = source_operand
            let dest_var_name = place_to_string(dest_place, tcx);
            instructions.push(Instruction::Move {
                dest: dest_var_name,
                src: source_operand,
            });
        }
    } else {
        // 1. Separate the destination into the base and the last projection element.
        let (last_projection, base_projection_elems) = dest_place.projection.split_last().unwrap(); // Safe because we checked is_empty()
        let base_place = Place {
            local: dest_place.local,
            projection: tcx.mk_place_elems(base_projection_elems),
        };
        if let ProjectionElem::Field(field, field_ty) = last_projection {
            let base_rust_ty =
                data_types.normalize(tcx, base_place.ty(&mir.local_decls, tcx).ty, instance);
            if let Some(payload) = super::super::types::transparent_payload(base_rust_ty, tcx) {
                if field.index() == payload.field {
                    instructions.extend(emit_instructions_to_set_value(
                        &base_place,
                        source_operand,
                        tcx,
                        instance,
                        mir,
                        data_types,
                    ));
                }
                return instructions;
            }
            if let Some(word) = super::super::types::packed_word(base_rust_ty, tcx) {
                let dest = format!("{}_packed_store", place_to_string(dest_place, tcx));
                if let Some((ProjectionElem::Deref, prefix)) = base_place.projection.split_last() {
                    // Partial field writes must preserve adjacent bytes.
                    let pointer_place = Place {
                        local: base_place.local,
                        projection: tcx.mk_place_elems(prefix),
                    };
                    let (name, code, ty) = emit_instructions_to_get_on_own(
                        &pointer_place,
                        tcx,
                        instance,
                        mir,
                        data_types,
                    );
                    instructions.extend(code);
                    let field_type = ty_to_oomir_type(*field_ty, tcx, data_types, instance);
                    let address = word.field_address(
                        Operand::Variable { name, ty },
                        field.index(),
                        &field_type,
                        &dest,
                        &mut instructions,
                    );
                    emit_pointer_write(address, &field_type, source_operand, &mut instructions);
                } else {
                    let (name, code, ty) = emit_instructions_to_get_on_own(
                        &base_place,
                        tcx,
                        instance,
                        mir,
                        data_types,
                    );
                    instructions.extend(code);
                    let value = word.insert(
                        Operand::Variable { name, ty },
                        field.index(),
                        source_operand,
                        &dest,
                        &mut instructions,
                    );
                    instructions.extend(emit_instructions_to_set_value(
                        &base_place,
                        value,
                        tcx,
                        instance,
                        mir,
                        data_types,
                    ));
                }
                return instructions;
            }
        }
        if matches!(last_projection, ProjectionElem::Field(field, _) if field.index() == 0) {
            let base_rust_ty =
                data_types.normalize(tcx, base_place.ty(&mir.local_decls, tcx).ty, instance);
            if super::super::types::tagged_scalar(base_rust_ty, tcx).is_some() {
                let Some((ProjectionElem::Downcast(_, variant), prefix)) =
                    base_projection_elems.split_last()
                else {
                    panic!("tagged payload needs a variant");
                };
                assert_eq!(variant.as_u32(), 1);
                let dest = format!("{}_tagged", place_to_string(dest_place, tcx));
                instructions.push(super::super::types::tagged_value(
                    dest.clone(),
                    source_operand,
                    1,
                ));
                instructions.extend(emit_instructions_to_set_value(
                    &Place {
                        local: dest_place.local,
                        projection: tcx.mk_place_elems(prefix),
                    },
                    Operand::Variable {
                        name: dest,
                        ty: oomir::Type::TaggedI64,
                    },
                    tcx,
                    instance,
                    mir,
                    data_types,
                ));
                return instructions;
            }
            if super::super::types::direct_enum_payload(base_rust_ty, tcx).is_some() {
                let Some((ProjectionElem::Downcast(_, variant), prefix)) =
                    base_projection_elems.split_last()
                else {
                    panic!("nullable enum field needs a variant projection");
                };
                if *variant
                    != super::super::types::enum_carrier(base_rust_ty, tcx)
                        .unwrap()
                        .variant
                {
                    instructions.push(oomir::Instruction::Unreachable);
                    return instructions;
                }
                let storage = Place {
                    local: dest_place.local,
                    projection: tcx.mk_place_elems(prefix),
                };
                instructions.extend(emit_instructions_to_set_value(
                    &storage,
                    source_operand,
                    tcx,
                    instance,
                    mir,
                    data_types,
                ));
                return instructions;
            }
            if matches!(
                base_rust_ty.kind(),
                TyKind::Adt(adt_def, _)
                    if crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
            ) && matches!(
                ty_to_oomir_type(base_rust_ty, tcx, data_types, instance),
                oomir::Type::Pointer(_)
            ) {
                instructions.extend(emit_instructions_to_set_value(
                    &base_place,
                    source_operand,
                    tcx,
                    instance,
                    mir,
                    data_types,
                ));
                return instructions;
            }
        }

        // 2. Generate instructions to get the value of the *base* place.
        //    This base value is the object we'll call SetField on, or the array
        //    we'll call ArrayStore on.
        //    We use `get_on_own` which internally handles recursion if base_place itself is nested.
        // Taking an element through `&[T; N]` or `&mut [T; N]` only needs the
        // borrowed sequence carrier. Reading `*reference` first would
        // materialize an array value and detach pointer-backed references such
        // as `array::from_mut` from their original storage. Likewise,
        // projecting a struct field must retain its pointer instead of reading
        // neighboring fields that may not have been initialized yet.
        let direct_pointer_base =
            base_place
                .projection
                .split_last()
                .and_then(|(projection, prefix)| {
                    if !matches!(projection, ProjectionElem::Deref) {
                        return None;
                    }
                    let reference_place = Place {
                        local: base_place.local,
                        projection: tcx.mk_place_elems(prefix),
                    };
                    let reference_type =
                        get_place_type(&reference_place, mir, tcx, instance, data_types);
                    let reference_rust_ty =
                        EarlyBinder::bind(tcx, reference_place.ty(&mir.local_decls, tcx).ty)
                            .instantiate(tcx, instance.args)
                            .skip_norm_wip();
                    let pointer_field = matches!(reference_rust_ty.kind(), TyKind::RawPtr(..))
                        && matches!(last_projection, ProjectionElem::Field(_, ty)
                        if ty_to_oomir_type(*ty, tcx, data_types, instance).is_jvm_primitive())
                        && matches!(&reference_type, oomir::Type::Pointer(inner)
                            if matches!(inner.as_ref(), oomir::Type::Class(_)))
                        && {
                            let ty = data_types.normalize(
                                tcx,
                                base_place.ty(&mir.local_decls, tcx).ty,
                                instance,
                            );
                            matches!(ty.kind(), TyKind::Tuple(_))
                                || matches!(ty.kind(), TyKind::Adt(def, _) if def.is_struct())
                        };
                    (matches!(reference_type, oomir::Type::Slice(_)) || pointer_field).then(|| {
                        emit_instructions_to_get_on_own(
                            &reference_place,
                            tcx,
                            instance,
                            mir,
                            data_types,
                        )
                    })
                });
        let (base_var_name, get_base_instructions, base_oomir_type) = direct_pointer_base
            .unwrap_or_else(|| {
                emit_instructions_to_get_on_own(&base_place, tcx, instance, mir, data_types)
            });
        let union_writebacks = collect_union_writebacks(
            &base_place,
            &get_base_instructions,
            tcx,
            instance,
            mir,
            data_types,
        );
        let memory_view_writebacks = collect_memory_view_writebacks(&get_base_instructions);
        instructions.extend(get_base_instructions); // Add instructions to get the base

        // 3. Generate the final store instruction based on the *last* projection.
        match last_projection {
            ProjectionElem::Field(field_index, field_mir_ty) => {
                let base_rust_ty =
                    data_types.normalize(tcx, base_place.ty(&mir.local_decls, tcx).ty, instance);
                if matches!(&base_oomir_type, oomir::Type::Pointer(inner)
                    if matches!(inner.as_ref(), oomir::Type::Class(_)))
                    && (matches!(base_rust_ty.kind(), TyKind::Tuple(_))
                        || matches!(base_rust_ty.kind(), TyKind::Adt(def, _) if def.is_struct()))
                {
                    let layout = tcx
                        .layout_of(TypingEnv::fully_monomorphized().as_query_input(base_rust_ty))
                        .unwrap_or_else(|error| {
                            panic!(
                                "could not determine struct field layout for field assignment to {base_rust_ty:?}: {error:?}"
                            )
                        });
                    let field_offset = layout.fields.offset(field_index.index()).bytes_usize();
                    let field_rust_ty = EarlyBinder::bind(tcx, *field_mir_ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let field_oomir_ty = ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                    let field_pointer_ty = oomir::Type::pointer(field_oomir_ty.clone());
                    let oomir::Type::Pointer(base_pointee_ty) = &base_oomir_type else {
                        unreachable!();
                    };
                    let oomir::Type::Class(owner_class) = base_pointee_ty.as_ref() else {
                        panic!("Rust struct pointer did not map to a JVM class");
                    };
                    let owner_class = owner_class.clone();
                    let field_name = field_name_for_projection(
                        &owner_class,
                        field_index.index(),
                        base_rust_ty,
                        tcx,
                        data_types,
                    )
                    .unwrap_or_else(|error| panic!("Error getting struct field name: {error}"));
                    let typed_pointer_name = format!("{base_var_name}_typed_field_pointer");
                    instructions.push(super::project_memory(
                        typed_pointer_name.clone(),
                        Operand::Variable {
                            name: base_var_name,
                            ty: base_oomir_type,
                        },
                        owner_class,
                        field_name,
                        field_rust_ty,
                        field_offset as u64,
                        tcx,
                        instance,
                        data_types,
                    ));
                    emit_pointer_write(
                        Operand::Variable {
                            name: typed_pointer_name,
                            ty: field_pointer_ty,
                        },
                        &field_oomir_ty,
                        source_operand,
                        &mut instructions,
                    );
                    emit_union_writebacks(&union_writebacks, &mut instructions);
                    emit_memory_view_writebacks(&memory_view_writebacks, &mut instructions);
                    return instructions;
                }
                if let Some((adt_def, _substs)) = union_parts_from_ty(base_rust_ty) {
                    let owner_class_name =
                        match ty_to_oomir_type(base_rust_ty, tcx, data_types, instance) {
                            oomir::Type::Class(name) => name,
                            other => panic!(
                                "Union field assignment on non-class OOMIR type: {:?}",
                                other
                            ),
                        };
                    let field_name = union_field_name(adt_def, field_index.index(), tcx);
                    let field_ty = ty_to_oomir_type(*field_mir_ty, tcx, data_types, instance);
                    let is_unit = !field_ty.has_jvm_value();
                    let source_operand = adapt_simple_enum_operand(
                        source_operand,
                        &field_ty,
                        &format!("{}_{}_union", base_var_name, field_name),
                        data_types,
                        &mut instructions,
                    );
                    instructions.push(Instruction::InvokeVirtual {
                        dest: None,
                        class_name: owner_class_name.clone(),
                        method_name: union_setter_method_name(&field_name),
                        method_ty: oomir::Signature {
                            params: vec![(
                                "self".to_string(),
                                oomir::Type::Class(owner_class_name),
                            )]
                            .into_iter()
                            .chain((!is_unit).then_some(("value".to_string(), field_ty)))
                            .collect(),
                            ret: Box::new(oomir::Type::Void),
                            is_static: false,
                        },
                        args: if is_unit {
                            vec![]
                        } else {
                            vec![source_operand]
                        },
                        operand: Operand::Variable {
                            name: base_var_name,
                            ty: base_oomir_type,
                        },
                    });
                    emit_union_writebacks(&union_writebacks, &mut instructions);
                    emit_memory_view_writebacks(&memory_view_writebacks, &mut instructions);
                    return instructions;
                }

                // Target is a field: base_var_name.field = source_operand
                let owner_class_name = match &base_oomir_type {
                    oomir::Type::Class(name) => name.clone(),
                    _ => panic!(
                        "SetField target base '{}' (Place: {:?}) is not a class or reference-to-class type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                };

                let coroutine_variant =
                    base_place
                        .projection
                        .iter()
                        .rev()
                        .find_map(|projection| match projection {
                            ProjectionElem::Downcast(_, variant) => Some(variant),
                            _ => None,
                        });
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
                    Err(e) => panic!("Error getting field name for SetField: {}", e),
                };
                let field_ty = ty_to_oomir_type(*field_mir_ty, tcx, data_types, instance);

                instructions.push(Instruction::SetField {
                    object: base_var_name.clone(), // The object/struct retrieved in step 2
                    field_name,
                    field_ty,
                    value: source_operand, // The value we want to store
                    owner_class: owner_class_name,
                });
            }

            ProjectionElem::Index(index_local) => {
                // Target is an array element: base_var_name[index] = source_operand
                // Ensure the base is actually an array or ref-to-array
                match &base_oomir_type {
                    oomir::Type::Array(_) | oomir::Type::Slice(_) => {}

                    _ => panic!(
                        "ArrayStore target base '{}' (Place: {:?}) is not an array or reference-to-array type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                }

                // Convert the MIR index operand (_local) to an OOMIR operand
                let mir_index_operand = MirOperand::Copy(Place::from(*index_local)); // Or Move? Copy usually safer.
                let oomir_index_operand = convert_operand(
                    &mir_index_operand,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );

                instructions.push(Instruction::ArrayStore {
                    array: oomir::Operand::Variable {
                        name: base_var_name.clone(),
                        ty: base_oomir_type.clone(),
                    },
                    index: oomir_index_operand, // The index operand
                    value: source_operand,      // The value to store
                    copy_value: false,
                });
            }

            ProjectionElem::ConstantIndex {
                offset,
                min_length: _,
                from_end,
            } => {
                // Target is array element with constant index: base_var_name[const_idx] = source_operand
                // Ensure the base is actually an array or ref-to-array
                match &base_oomir_type {
                    oomir::Type::Array(_) | oomir::Type::Slice(_) => {}

                    _ => panic!(
                        "ArrayStore target base '{}' (Place: {:?}) is not an array or reference-to-array type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                }

                let index_operand: Operand;

                if !from_end {
                    // Simple constant index from the start
                    index_operand = Operand::Constant(oomir::Constant::I32(*offset as i32));
                    // No extra instructions needed for the index itself
                } else {
                    // Index is calculated as length - offset
                    // We need to insert Length and Sub *before* the ArrayStore

                    // Temp name for length result (avoid collision)
                    let len_var_name = format!("{}_len_set", base_var_name);
                    instructions.push(Instruction::Length {
                        dest: len_var_name.clone(),
                        array: Operand::Variable {
                            name: base_var_name.clone(),
                            ty: base_oomir_type.clone(),
                        },
                    });

                    // Temp name for calculated index (avoid collision)
                    let index_var_name = format!("{}_calc_idx_set", base_var_name);
                    let offset_op = Operand::Constant(oomir::Constant::I32(*offset as i32));
                    instructions.push(Instruction::Binary {
                        op: crate::oomir::BinaryOp::Sub,
                        dest: index_var_name.clone(),
                        op1: Operand::Variable {
                            name: len_var_name,
                            ty: oomir::Type::I32,
                        },
                        op2: offset_op,
                    });

                    // Use the calculated index variable
                    index_operand = Operand::Variable {
                        name: index_var_name,
                        ty: oomir::Type::I32,
                    };
                }

                instructions.push(Instruction::ArrayStore {
                    array: oomir::Operand::Variable {
                        name: base_var_name.clone(),
                        ty: base_oomir_type.clone(),
                    }, // The array retrieved in step 2
                    index: index_operand,  // The constant or calculated index
                    value: source_operand, // The value to store
                    copy_value: false,
                });
            }

            ProjectionElem::Deref => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "place-lowering",
                    format!(
                        "Info: Handling Set via Deref: Target Base Var '{}' ({:?}), Source: {:?}",
                        base_var_name, base_oomir_type, source_operand
                    )
                );

                match &base_oomir_type {
                    oomir::Type::Pointer(element_type) => {
                        emit_pointer_write(
                            Operand::Variable {
                                name: base_var_name,
                                ty: base_oomir_type.clone(),
                            },
                            element_type,
                            source_operand,
                            &mut instructions,
                        );
                    }
                    _ => {
                        // no-op - non-mutable reference
                    }
                }
            }
            ProjectionElem::Downcast(..) => {
                // Downcast should not be the last element for an assignment.
                // You assign to a field/index within the downcast variant.
                panic!(
                    "Downcast cannot be the final projection element for an assignment. Place: {:?}",
                    dest_place
                );
            }
            _ => {
                panic!(
                    "Unsupported projection element type {:?} found at the end of destination Place during assignment: {:?}",
                    last_projection, dest_place
                );
            }
        }
        emit_union_writebacks(&union_writebacks, &mut instructions);
        emit_memory_view_writebacks(&memory_view_writebacks, &mut instructions);
    }

    instructions
}
