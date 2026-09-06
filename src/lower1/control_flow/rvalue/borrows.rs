use super::*;

impl<'tcx> RvalueContext<'_, 'tcx> {
    pub(super) fn lower_borrows(
        self,
        rvalue: &Rvalue<'tcx>,
    ) -> (Vec<oomir::Instruction>, oomir::Operand) {
        let Self {
            original_dest_place,
            mir,
            tcx,
            instance,
            data_types,
            pointer_origins,
            available_pointer_locals,
            ..
        } = self;
        let mut instructions = Vec::new();
        let result_operand;
        let base_temp_name = place_to_string(original_dest_place, tcx);
        match rvalue {
            Rvalue::Ref(_region, borrow_kind, source_place) => {
                // Check if the result type (destination place type) is a trait object reference
                let dest_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!(
                        "Rvalue::Ref start: original_dest_place={:?}, dest_ty={:?}, borrow_kind={:?}, source_place={:?}, source_ty={:?}",
                        original_dest_place,
                        dest_ty,
                        borrow_kind,
                        source_place,
                        source_place.ty(&mir.local_decls, tcx).ty
                    )
                );
                let reference_oomir_ty = ty_to_oomir_type(dest_ty, tcx, data_types, instance);
                let is_trait_object = matches!(dest_ty.kind(), TyKind::Ref(..))
                    && matches!(reference_oomir_ty, oomir::Type::Interface(_));
                if matches!(reference_oomir_ty, oomir::Type::Pointer(_)) {
                    result_operand = reuse_pointer_to_place(
                        source_place,
                        &reference_oomir_ty,
                        &base_temp_name,
                        pointer_origins,
                        available_pointer_locals,
                        data_types,
                        &mut instructions,
                    )
                    .unwrap_or_else(|| {
                        emit_pointer_to_place(
                            source_place,
                            &reference_oomir_ty,
                            &base_temp_name,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        )
                    });
                    return (instructions, result_operand);
                }

                let source_mir_ty = source_place.ty(&mir.local_decls, tcx).ty;
                let normalized_source_mir_ty = normalize_unsize_ty(source_mir_ty, tcx, instance);
                if !source_place.projection.is_empty()
                    && matches!(normalized_source_mir_ty.kind(), TyKind::Array(..))
                    && matches!(reference_oomir_ty, oomir::Type::Slice(_))
                    && let Some(array_reference) = emit_borrowed_projected_array_view(
                        source_place,
                        normalized_source_mir_ty,
                        &generate_temp_var_name(data_types, &base_temp_name),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    )
                {
                    return (instructions, array_reference);
                }

                match borrow_kind {
                    MirBorrowKind::Mut { .. } if !is_trait_object => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Rvalue::Ref(Mut) for place '{}' -> Temp Array Var",
                                place_to_string(source_place, tcx)
                            )
                        );

                        // 1. Get the value of the place being referenced (the 'pointee').
                        let (pointee_value_var_name, pointee_get_instructions, pointee_oomir_type) =
                            emit_instructions_to_get_on_own(
                                source_place,
                                tcx,
                                instance,
                                mir,
                                data_types,
                            );
                        instructions.extend(pointee_get_instructions); // Add instructions to get the value

                        if matches!(pointee_oomir_type, oomir::Type::Array(_)) {
                            let slice_name = generate_temp_var_name(data_types, &base_temp_name);
                            if let Some(slice) = emit_borrowed_array_view(
                                oomir::Operand::Variable {
                                    name: pointee_value_var_name.clone(),
                                    ty: pointee_oomir_type.clone(),
                                },
                                source_place.ty(&mir.local_decls, tcx).ty,
                                &slice_name,
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            ) {
                                return (instructions, slice);
                            }
                        }

                        if matches!(pointee_oomir_type, oomir::Type::Slice(_) | oomir::Type::Str) {
                            result_operand = oomir::Operand::Variable {
                                name: pointee_value_var_name,
                                ty: pointee_oomir_type,
                            };
                            return (instructions, result_operand);
                        }

                        // 2. Determine the OOMIR type for the array reference itself.
                        let array_ref_oomir_type =
                            oomir::Type::MutableReference(Box::new(pointee_oomir_type.clone()));

                        // 3. Create a temporary variable name for the new array.
                        let array_ref_var_name =
                            generate_temp_var_name(data_types, &base_temp_name);

                        // 4. Emit instruction to allocate the single-element array (new T[1]).
                        instructions.push(oomir::Instruction::NewArray {
                            dest: array_ref_var_name.clone(),
                            element_type: pointee_oomir_type.clone(),
                            size: oomir::Operand::Constant(oomir::Constant::I32(1)),
                        });

                        // 5. Emit instruction to store the pointee's value into the array's first element.
                        let pointee_value_operand = oomir::Operand::Variable {
                            name: pointee_value_var_name,
                            ty: pointee_oomir_type,
                        };
                        instructions.push(oomir::Instruction::ArrayStore {
                            array: oomir::Operand::Variable {
                                name: array_ref_var_name.clone(),
                                ty: array_ref_oomir_type.clone(),
                            },
                            index: oomir::Operand::Constant(oomir::Constant::I32(0)),
                            value: pointee_value_operand,
                            copy_value: false,
                        });

                        // 6. The result is the reference to the newly created array.
                        result_operand = oomir::Operand::Variable {
                            name: array_ref_var_name,
                            ty: array_ref_oomir_type,
                        };
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: -> Temp Array Var '{}' ({:?})",
                                result_operand.get_name().unwrap_or("<unknown>"),
                                result_operand.get_type()
                            )
                        );
                    }
                    MirBorrowKind::Mut { .. }
                    | MirBorrowKind::Shared
                    | MirBorrowKind::Fake { .. } => {
                        // Treat Fake like Shared (used for closures etc.)
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Rvalue::Ref({:?}) for place '{}' -> Direct Value",
                                borrow_kind,
                                place_to_string(source_place, tcx)
                            )
                        );

                        let source_mir_ty = source_place.ty(&mir.local_decls, tcx).ty;
                        if let TyKind::Closure(_, closure_args) = source_mir_ty.kind() {
                            let closure_oomir_type =
                                ty_to_oomir_type(source_mir_ty, tcx, data_types, instance);
                            if let oomir::Type::Class(class_name) = closure_oomir_type.clone() {
                                let has_captures = closure_args
                                    .as_closure()
                                    .upvar_tys()
                                    .iter()
                                    .next()
                                    .is_some();
                                if has_captures {
                                    let (temp_var_name, get_instructions, temp_var_type) =
                                        emit_instructions_to_get_on_own(
                                            source_place,
                                            tcx,
                                            instance,
                                            mir,
                                            data_types,
                                        );
                                    instructions.extend(get_instructions);
                                    result_operand = oomir::Operand::Variable {
                                        name: temp_var_name,
                                        ty: temp_var_type,
                                    };
                                } else {
                                    let closure_var_name =
                                        generate_temp_var_name(data_types, &base_temp_name);
                                    instructions.push(oomir::Instruction::ConstructObject {
                                        dest: closure_var_name.clone(),
                                        class_name,
                                        args: Vec::new(),
                                    });
                                    result_operand = oomir::Operand::Variable {
                                        name: closure_var_name,
                                        ty: closure_oomir_type,
                                    };
                                }
                            } else {
                                result_operand = get_placeholder_operand(
                                    original_dest_place,
                                    mir,
                                    tcx,
                                    instance,
                                    data_types,
                                );
                            }
                        } else {
                            // 1. Get the value/reference of the place being borrowed directly.
                            //    `emit_instructions_to_get_on_own` handles loading/accessing the value.
                            let (
                                pointee_value_var_name,
                                pointee_get_instructions,
                                pointee_oomir_type,
                            ) = emit_instructions_to_get_on_own(
                                source_place,
                                tcx,
                                instance,
                                mir,
                                data_types,
                            );

                            // 2. Add the instructions needed to get this value.
                            instructions.extend(pointee_get_instructions);

                            if matches!(pointee_oomir_type, oomir::Type::Array(_)) {
                                let slice_name =
                                    generate_temp_var_name(data_types, &base_temp_name);
                                if let Some(slice) = emit_borrowed_array_view(
                                    oomir::Operand::Variable {
                                        name: pointee_value_var_name.clone(),
                                        ty: pointee_oomir_type.clone(),
                                    },
                                    source_mir_ty,
                                    &slice_name,
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                ) {
                                    return (instructions, slice);
                                }
                            }

                            // 3. The result *is* the operand representing the borrowed value itself.
                            //    No array wrapping is done.
                            result_operand = oomir::Operand::Variable {
                                name: pointee_value_var_name,
                                ty: pointee_oomir_type,
                            };
                        }
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: -> Direct Value Operand '{}' ({:?})",
                                result_operand.get_name().unwrap_or("<unknown>"),
                                result_operand.get_type()
                            )
                        );
                    }
                }
            }

            Rvalue::RawPtr(kind, place) => {
                match kind {
                    rustc_middle::mir::RawPtrKind::FakeForPtrMetadata => {
                        let (place_temp_var_name, place_get_instructions, place_temp_var_type) =
                            emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                        instructions.extend(place_get_instructions);
                        // This pointer is *only* created to get metadata (like length)
                        // from the underlying place. The actual pointer value is irrelevant
                        // in the target code. The subsequent PtrMetadata operation will
                        // operate on the operand representing the place itself.
                        // So, we just pass the place's operand through.
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Rvalue::RawPtr(FakeForPtrMetadata) for place '{:?}'. Passing through place operand '{}' ({:?}).",
                                place_to_string(place, tcx),
                                place_temp_var_name,
                                place_temp_var_type
                            )
                        );
                        result_operand = oomir::Operand::Variable {
                            name: place_temp_var_name, // Use the operand computed for the place
                            ty: place_temp_var_type,
                        };
                    }
                    rustc_middle::mir::RawPtrKind::Const | rustc_middle::mir::RawPtrKind::Mut => {
                        let pointer_oomir_type =
                            get_place_type(original_dest_place, mir, tcx, instance, data_types);
                        if !matches!(pointer_oomir_type, oomir::Type::Pointer(_)) {
                            let pointer_mir_ty = normalize_unsize_ty(
                                original_dest_place.ty(&mir.local_decls, tcx).ty,
                                tcx,
                                instance,
                            );
                            let pointee_mir_ty = pointer_pointee_ty(pointer_mir_ty);
                            let projects_struct_tail =
                                place.projection.split_last().is_some_and(|(last, prefix)| {
                                    if !matches!(last, ProjectionElem::Field(..)) {
                                        return false;
                                    }
                                    let base_place = Place {
                                        local: place.local,
                                        projection: tcx.mk_place_elems(prefix),
                                    };
                                    let base_ty = normalize_unsize_ty(
                                        base_place.ty(&mir.local_decls, tcx).ty,
                                        tcx,
                                        instance,
                                    );
                                    crate::lower1::place::has_slice_or_str_struct_tail(tcx, base_ty)
                                });
                            if projects_struct_tail
                                && matches!(pointee_mir_ty.kind(), TyKind::Slice(_) | TyKind::Str)
                            {
                                let storage_pointer_ty = oomir::Type::Pointer(Box::new(
                                    ty_to_oomir_type(pointee_mir_ty, tcx, data_types, instance),
                                ));
                                let storage = emit_pointer_to_place(
                                    place,
                                    &storage_pointer_ty,
                                    &format!("{base_temp_name}_unsized_storage"),
                                    tcx,
                                    instance,
                                    mir,
                                    data_types,
                                    &mut instructions,
                                );
                                result_operand = emit_slice_pointer_carrier(
                                    pointer_mir_ty,
                                    storage,
                                    &format!("{base_temp_name}_unsized_pointer"),
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                )
                                .expect("slice-like raw pointer must have a slice carrier");
                                return (instructions, result_operand);
                            }
                            if matches!(pointee_mir_ty.kind(), TyKind::Dynamic(..)) {
                                let storage_pointer_ty =
                                    oomir::Type::Pointer(Box::new(pointer_oomir_type.clone()));
                                result_operand = emit_pointer_to_place(
                                    place,
                                    &storage_pointer_ty,
                                    &format!("{base_temp_name}_trait_storage"),
                                    tcx,
                                    instance,
                                    mir,
                                    data_types,
                                    &mut instructions,
                                );
                                return (instructions, result_operand);
                            }
                            // Unsized raw pointers retain their existing fat
                            // carrier (SliceView/Utf8View/trait interface). In
                            // optimized MIR this is the first half of operations
                            // such as slice::as_ptr; allocating a scalar Pointer
                            // cell here would lose the metadata and invent an
                            // impossible runtime return type.
                            let (name, get_instructions, ty) = emit_instructions_to_get_on_own(
                                place, tcx, instance, mir, data_types,
                            );
                            instructions.extend(get_instructions);
                            result_operand = oomir::Operand::Variable { name, ty };
                            return (instructions, result_operand);
                        }
                        result_operand = reuse_pointer_to_place(
                            place,
                            &pointer_oomir_type,
                            &base_temp_name,
                            pointer_origins,
                            available_pointer_locals,
                            data_types,
                            &mut instructions,
                        )
                        .unwrap_or_else(|| {
                            emit_pointer_to_place(
                                place,
                                &pointer_oomir_type,
                                &base_temp_name,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            )
                        });
                    }
                }
            }
            _ => unreachable!("rvalue routed to borrows"),
        }
        (instructions, result_operand)
    }
}
