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
            ..
        } = self;
        let mut instructions = Vec::new();
        let result_operand;
        let base_temp_name = place_to_string(original_dest_place, tcx);
        match rvalue {
            Rvalue::Ref(_, _, source_place) => {
                let dest_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                let reference_ty = ty_to_oomir_type(dest_ty, tcx, data_types, instance);
                if matches!(reference_ty, oomir::Type::Pointer(_)) {
                    result_operand = emit_pointer_to_place(
                        source_place,
                        &reference_ty,
                        &base_temp_name,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
                    return (instructions, result_operand);
                }
                let source_ty = source_place.ty(&mir.local_decls, tcx).ty;
                let normalized = normalize_unsize_ty(source_ty, tcx, instance);
                if !source_place.projection.is_empty()
                    && matches!(normalized.kind(), TyKind::Array(..))
                    && matches!(reference_ty, oomir::Type::Slice(_))
                    && let Some(view) = emit_borrowed_projected_array_view(
                        source_place,
                        normalized,
                        &generate_temp_var_name(data_types, &base_temp_name),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    )
                {
                    return (instructions, view);
                }
                // These carriers already identify their storage. Mutable borrows use the same
                // representation.
                let (name, read, ty) =
                    emit_instructions_to_get_on_own(source_place, tcx, instance, mir, data_types);
                instructions.extend(read);
                let source = oomir::Operand::Variable {
                    name,
                    ty: ty.clone(),
                };
                if matches!(ty, oomir::Type::Array(_))
                    && let Some(view) = emit_borrowed_array_view(
                        source.clone(),
                        source_ty,
                        &generate_temp_var_name(data_types, &base_temp_name),
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    )
                {
                    return (instructions, view);
                }
                result_operand = source;
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
                            if matches!(
                                pointer_oomir_type,
                                oomir::Type::Slice(_) | oomir::Type::Str
                            ) {
                                result_operand = emit_pointer_to_place(
                                    place,
                                    &pointer_oomir_type,
                                    &base_temp_name,
                                    tcx,
                                    instance,
                                    mir,
                                    data_types,
                                    &mut instructions,
                                );
                                return (instructions, result_operand);
                            }
                            let pointer_mir_ty = normalize_unsize_ty(
                                original_dest_place.ty(&mir.local_decls, tcx).ty,
                                tcx,
                                instance,
                            );
                            let pointee_mir_ty = pointer_pointee_ty(pointer_mir_ty, tcx);
                            if matches!(pointee_mir_ty.kind(), TyKind::Dynamic(..)) {
                                let storage_pointer_ty =
                                    oomir::Type::pointer(pointer_oomir_type.clone());
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
                        result_operand = emit_pointer_to_place(
                            place,
                            &pointer_oomir_type,
                            &base_temp_name,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                    }
                }
            }
            _ => unreachable!("rvalue routed to borrows"),
        }
        (instructions, result_operand)
    }
}
