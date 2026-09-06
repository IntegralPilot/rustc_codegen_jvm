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

    if dest_place.projection.is_empty() {
        if data_types.local_uses_stable_cell(dest_place.local) {
            let pointee_type = get_place_type(dest_place, mir, tcx, instance, data_types);
            emit_pointer_write(
                Operand::Variable {
                    name: local_cell_name(dest_place.local),
                    ty: oomir::Type::Pointer(Box::new(pointee_type.clone())),
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
        if matches!(last_projection, ProjectionElem::Field(field, _) if field.index() == 0) {
            let base_rust_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
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
        // as `array::from_mut` from their original storage.
        let direct_slice_base =
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
                    matches!(
                        get_place_type(&reference_place, mir, tcx, instance, data_types),
                        oomir::Type::Slice(_)
                    )
                    .then(|| {
                        emit_instructions_to_get_on_own(
                            &reference_place,
                            tcx,
                            instance,
                            mir,
                            data_types,
                        )
                    })
                });
        let (base_var_name, get_base_instructions, base_oomir_type) = direct_slice_base
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
                let base_rust_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                if matches!(base_oomir_type, oomir::Type::Pointer(_))
                    && has_slice_or_str_struct_tail(tcx, base_rust_ty)
                {
                    let layout = tcx
                        .layout_of(TypingEnv::fully_monomorphized().as_query_input(base_rust_ty))
                        .unwrap_or_else(|error| {
                            panic!(
                                "could not determine slice-tailed struct layout for field assignment to {base_rust_ty:?}: {error:?}"
                            )
                        });
                    let field_offset = layout.fields.offset(field_index.index()).bytes_usize();
                    let field_rust_ty = EarlyBinder::bind(tcx, *field_mir_ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let field_oomir_ty = ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                    let field_pointer_ty = oomir::Type::Pointer(Box::new(field_oomir_ty.clone()));
                    let oomir::Type::Pointer(base_pointee_ty) = &base_oomir_type else {
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
                    let typed_pointer_name = format!("{base_var_name}_typed_field_pointer");
                    instructions.push(Instruction::InvokeVirtual {
                        dest: Some(typed_pointer_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "projectStructField".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("self".to_string(), base_oomir_type.clone()),
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
                            Operand::Constant(oomir::Constant::U64(
                                u64::try_from(field_offset)
                                    .expect("Rust DST field offset exceeds u64"),
                            )),
                            Operand::Constant(oomir::Constant::U64(
                                u64::try_from(
                                    super::super::types::layout_size_bytes(tcx, field_rust_ty)
                                        .expect("sized DST field must have a layout"),
                                )
                                .expect("Rust DST field layout exceeds u64"),
                            )),
                            pointer_view_codec_operand(field_rust_ty, tcx, data_types, instance),
                        ],
                        operand: Operand::Variable {
                            name: base_var_name,
                            ty: base_oomir_type,
                        },
                    });
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
                            oomir::Type::Reference(inner)
                                if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                            {
                                if let oomir::Type::Class(name) = inner.as_ref() {
                                    name.clone()
                                } else {
                                    unreachable!()
                                }
                            }
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
                    oomir::Type::Reference(t) if matches!(t.as_ref(), oomir::Type::Array(_)) => {}
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
                    oomir::Type::Reference(t) if matches!(t.as_ref(), oomir::Type::Array(_)) => {}
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
                    oomir::Type::MutableReference(_element_type) => {
                        instructions.push(Instruction::ArrayStore {
                            array: oomir::Operand::Variable {
                                name: base_var_name.clone(),
                                ty: base_oomir_type.clone(),
                            }, // The variable holding the array reference
                            // Index is always 0 for our reference representation
                            index: Operand::Constant(oomir::Constant::I32(0)),
                            value: source_operand, // The value being assigned
                            copy_value: false,
                        });
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
