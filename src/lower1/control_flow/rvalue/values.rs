use super::*;

impl<'tcx> RvalueContext<'_, 'tcx> {
    pub(super) fn lower_values(
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
            Rvalue::Use(mir_operand, _) => {
                result_operand = convert_operand(
                    mir_operand,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );
            }

            Rvalue::Repeat(element_op, len_const) => {
                // Create a temporary variable to hold the new array
                let temp_array_var = generate_temp_var_name(data_types, &base_temp_name);
                let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty; // Use original dest for type info

                if let rustc_middle::ty::TyKind::Array(elem_ty, _) = place_ty.kind() {
                    let oomir_elem_type =
                        ty_to_oomir_type(elem_ty.clone(), tcx, data_types, instance);
                    let oomir_elem_op = convert_operand(
                        element_op,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
                    let oomir_elem_op = adapt_value_for_field(
                        oomir_elem_op,
                        *elem_ty,
                        &oomir_elem_type,
                        &temp_array_var,
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    );
                    let array_size = EarlyBinder::bind(tcx, *len_const)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip()
                        .try_to_target_usize(tcx)
                        .unwrap_or_else(|| {
                            panic!("Could not resolve array repeat length {:?}", len_const)
                        });
                    let size_operand =
                        oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));

                    instructions.push(oomir::Instruction::NewArray {
                        dest: temp_array_var.clone(), // Store in temp var
                        element_type: oomir_elem_type.clone(),
                        size: size_operand,
                    });

                    if !is_jvm_array_default_value(&oomir_elem_op, &oomir_elem_type) {
                        instructions.push(oomir::Instruction::ArrayFill {
                            array: oomir::Operand::Variable {
                                name: temp_array_var.clone(),
                                ty: oomir::Type::Array(Box::new(oomir_elem_type.clone())),
                            },
                            value: oomir_elem_op,
                            copy_value: true,
                        });
                    }
                    result_operand = oomir::Operand::Variable {
                        name: temp_array_var,
                        ty: oomir::Type::Array(Box::new(oomir_elem_type)), // Correct array type
                    };
                } else {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!(
                            "Warning: Rvalue::Repeat applied on non-array type: {:?}",
                            place_ty
                        )
                    );
                    result_operand = get_placeholder_operand(
                        original_dest_place,
                        mir,
                        tcx,
                        instance,
                        data_types,
                    );
                }
            }
            Rvalue::Discriminant(place) => {
                // 1. Generate instructions to get the actual value from the place
                let (actual_value_var_name, get_instructions, actual_value_oomir_type) =
                    emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);

                // Add the instructions needed to get the value (e.g., ArrayGet)
                instructions.extend(get_instructions);

                // 2. Now operate on the variable holding the actual value
                let temp_discriminant_var = generate_temp_var_name(data_types, &base_temp_name);

                let place_class_name = match actual_value_oomir_type.clone() {
                    oomir::Type::Class(name) => name.clone(),
                    // Handle potential references if get_on_own returns Ref(Class)
                    oomir::Type::Reference(inner) => {
                        if let oomir::Type::Class(name) = inner.as_ref() {
                            name.clone()
                        } else {
                            panic!("Discriminant on Ref to non-class type: {:?}", inner)
                        }
                    }
                    oomir::Type::MutableReference(inner) => {
                        if let oomir::Type::Class(name) = inner.as_ref() {
                            name.clone()
                        } else {
                            panic!("Discriminant on MutableRef to non-class type: {:?}", inner)
                        }
                    }
                    _ => panic!(
                        "Discriminant on non-class type: {:?}",
                        actual_value_oomir_type
                    ),
                };

                let place_mir_ty =
                    normalize_unsize_ty(place.ty(&mir.local_decls, tcx).ty, tcx, instance);
                if matches!(place_mir_ty.kind(), TyKind::Coroutine(..)) {
                    instructions.push(oomir::Instruction::GetField {
                        dest: temp_discriminant_var.clone(),
                        object: oomir::Operand::Variable {
                            name: actual_value_var_name,
                            ty: actual_value_oomir_type,
                        },
                        field_name: "__state".to_string(),
                        field_ty: oomir::Type::I32,
                        owner_class: place_class_name,
                    });
                    let result_ty =
                        get_place_type(original_dest_place, mir, tcx, instance, data_types);
                    if result_ty == oomir::Type::I32 {
                        result_operand = oomir::Operand::Variable {
                            name: temp_discriminant_var,
                            ty: oomir::Type::I32,
                        };
                    } else {
                        let cast_dest = generate_temp_var_name(data_types, &base_temp_name);
                        instructions.push(oomir::Instruction::Cast {
                            op: oomir::Operand::Variable {
                                name: temp_discriminant_var,
                                ty: oomir::Type::I32,
                            },
                            ty: result_ty.clone(),
                            dest: cast_dest.clone(),
                        });
                        result_operand = oomir::Operand::Variable {
                            name: cast_dest,
                            ty: result_ty,
                        };
                    }
                    return (instructions, result_operand);
                }
                let (enum_class_name, use_numeric_discriminant) = match place_mir_ty.kind() {
                    TyKind::Adt(adt_def, substs) if adt_def.is_enum() => {
                        force_define_named_adt(place_mir_ty, tcx, data_types, instance);
                        (
                            generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance),
                            enum_union_discriminant_supported(adt_def, tcx),
                        )
                    }
                    _ => (place_class_name.clone(), false),
                };
                let method_name = if use_numeric_discriminant {
                    ENUM_UNION_DISCRIMINANT_METHOD.to_string()
                } else {
                    "variantIndex".to_string()
                };
                let method_return_type = if use_numeric_discriminant {
                    oomir::Type::I64
                } else {
                    oomir::Type::I32
                };

                let method_ty = oomir::Signature {
                    params: vec![(
                        "value".to_string(),
                        oomir::Type::Class(enum_class_name.clone()),
                    )],
                    ret: Box::new(method_return_type.clone()),
                    is_static: true,
                };

                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name: enum_class_name,
                    method_name,
                    args: vec![oomir::Operand::Variable {
                        name: actual_value_var_name,
                        ty: actual_value_oomir_type,
                    }],
                    dest: Some(temp_discriminant_var.clone()),
                    method_ty,
                });

                // 4. Convert a numeric enum discriminant to the MIR destination's
                // integer type. Other enum shapes retain the variant-index path.
                if use_numeric_discriminant {
                    let result_ty =
                        get_place_type(original_dest_place, mir, tcx, instance, data_types);
                    if result_ty == method_return_type {
                        result_operand = oomir::Operand::Variable {
                            name: temp_discriminant_var,
                            ty: method_return_type,
                        };
                    } else {
                        let cast_dest = generate_temp_var_name(data_types, &base_temp_name);
                        instructions.push(oomir::Instruction::Cast {
                            op: oomir::Operand::Variable {
                                name: temp_discriminant_var,
                                ty: method_return_type,
                            },
                            ty: result_ty.clone(),
                            dest: cast_dest.clone(),
                        });
                        result_operand = oomir::Operand::Variable {
                            name: cast_dest,
                            ty: result_ty,
                        };
                    }
                } else {
                    result_operand = oomir::Operand::Variable {
                        name: temp_discriminant_var,
                        ty: method_return_type,
                    };
                }
            }
            Rvalue::CopyForDeref(place) => {
                // Need to get the value from the source place first
                let (temp_var_name, get_instructions, temp_var_type) =
                    emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                instructions.extend(get_instructions);
                result_operand = oomir::Operand::Variable {
                    name: temp_var_name,
                    ty: temp_var_type,
                };
            }
            // Handle other Rvalue variants by generating a placeholder
            _ => unreachable!("rvalue routed to values"),
        }
        (instructions, result_operand)
    }
}
