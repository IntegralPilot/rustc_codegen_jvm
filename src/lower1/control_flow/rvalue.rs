use rustc_abi::FieldIdx;
use rustc_middle::{
    mir::{BinOp, Body, BorrowKind as MirBorrowKind, Operand as MirOperand, Place, Rvalue, UnOp},
    ty::{ConstKind, TyCtxt, TyKind, inherent::ValueConst},
};
use std::collections::HashMap;

use super::{
    super::{
        operand::{
            convert_operand, extract_number_from_operand, get_placeholder_operand,
            handle_const_value,
        },
        place::{emit_instructions_to_get_on_own, get_place_type, make_jvm_safe, place_to_string},
        types::ty_to_oomir_type,
    },
    checked_ops::emit_checked_arithmetic_oomir_instructions,
    oomir,
};

use std::sync::atomic::{AtomicUsize, Ordering}; // For unique temp names

// Global or struct-based counter for temporary variables
static TEMP_VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn generate_temp_var_name(base_name: &str) -> String {
    let count = TEMP_VAR_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}_tmp{}", make_jvm_safe(base_name), count)
}

/// Evaluates an Rvalue and returns the resulting OOMIR Operand and any
/// intermediate instructions needed to calculate it.
///
/// The `original_dest_place` is used *only* for naming temporary variables
/// to make debugging easier, not for the final assignment.
pub fn convert_rvalue_to_operand<'a>(
    rvalue: &Rvalue<'a>,
    original_dest_place: &Place<'a>, // Used for naming temps
    mir: &Body<'a>,
    tcx: TyCtxt<'a>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> (Vec<oomir::Instruction>, oomir::Operand) {
    let mut instructions = Vec::new();
    let result_operand: oomir::Operand;
    let base_temp_name = place_to_string(original_dest_place, tcx); // For temp naming

    match rvalue {
        Rvalue::Use(mir_operand) => {
            match mir_operand {
                MirOperand::Copy(src_place) | MirOperand::Move(src_place) => {
                    // Need to get the value from the source place first
                    let (temp_var_name, get_instructions, temp_var_type) =
                        emit_instructions_to_get_on_own(src_place, tcx, mir, data_types);
                    instructions.extend(get_instructions);
                    result_operand = oomir::Operand::Variable {
                        name: temp_var_name,
                        ty: temp_var_type,
                    };
                }
                MirOperand::Constant(_) => {
                    // Constant is already an operand, no extra instructions
                    result_operand =
                        convert_operand(mir_operand, tcx, mir, data_types, &mut instructions);
                }
            }
        }

        Rvalue::Repeat(element_op, len_const) => {
            // Create a temporary variable to hold the new array
            let temp_array_var = generate_temp_var_name(&base_temp_name);
            let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty; // Use original dest for type info

            if let rustc_middle::ty::TyKind::Array(elem_ty, _) = place_ty.kind() {
                let oomir_elem_type = ty_to_oomir_type(elem_ty.clone(), tcx, data_types);
                let oomir_elem_op =
                    convert_operand(element_op, tcx, mir, data_types, &mut instructions);
                let array_size = match len_const.kind() {
                    ConstKind::Value(val) => {
                        /* ... extract size ... */
                        extract_number_from_operand(handle_const_value(
                            None,
                            tcx.valtree_to_const_val(val),
                            &val.ty(),
                            tcx,
                            data_types,
                        ))
                        .unwrap_or(0)
                    } // Simplified extraction
                    _ => panic!("Expected constant value for array size"),
                };
                let size_operand =
                    oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));

                instructions.push(oomir::Instruction::NewArray {
                    dest: temp_array_var.clone(), // Store in temp var
                    element_type: oomir_elem_type.clone(),
                    size: size_operand,
                });

                for i in 0..array_size {
                    let index_operand = oomir::Operand::Constant(oomir::Constant::I32(i as i32));
                    instructions.push(oomir::Instruction::ArrayStore {
                        array: temp_array_var.clone(),
                        index: index_operand,
                        value: oomir_elem_op.clone(),
                    });
                }
                result_operand = oomir::Operand::Variable {
                    name: temp_array_var,
                    ty: oomir::Type::Array(Box::new(oomir_elem_type)), // Correct array type
                };
            } else {
                println!(
                    "Warning: Rvalue::Repeat applied on non-array type: {:?}",
                    place_ty
                );
                result_operand = get_placeholder_operand(original_dest_place, mir, tcx, data_types);
            }
        }
        Rvalue::Ref(_region, borrow_kind, source_place) => {
            match borrow_kind {
                MirBorrowKind::Mut { .. } => {
                    // --- MUTABLE BORROW (&mut T) -> Use Array Hack ---
                    println!(
                        "Info: Handling Rvalue::Ref(Mut) for place '{}' -> Temp Array Var",
                        place_to_string(source_place, tcx)
                    );

                    // 1. Get the value of the place being referenced (the 'pointee').
                    let (pointee_value_var_name, pointee_get_instructions, pointee_oomir_type) =
                        emit_instructions_to_get_on_own(source_place, tcx, mir, data_types);
                    instructions.extend(pointee_get_instructions); // Add instructions to get the value

                    // 2. Determine the OOMIR type for the array reference itself.
                    let array_ref_oomir_type =
                        oomir::Type::MutableReference(Box::new(pointee_oomir_type.clone()));

                    // 3. Create a temporary variable name for the new array.
                    let array_ref_var_name = generate_temp_var_name(&base_temp_name);

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
                        array: array_ref_var_name.clone(),
                        index: oomir::Operand::Constant(oomir::Constant::I32(0)),
                        value: pointee_value_operand,
                    });

                    // 6. The result is the reference to the newly created array.
                    result_operand = oomir::Operand::Variable {
                        name: array_ref_var_name,
                        ty: array_ref_oomir_type,
                    };
                    println!(
                        "Info: -> Temp Array Var '{}' ({:?})",
                        result_operand.get_name().unwrap_or("<unknown>"),
                        result_operand.get_type()
                    );
                }

                MirBorrowKind::Shared | MirBorrowKind::Fake { .. } => {
                    // Treat Fake like Shared (used for closures etc.)
                    // --- SHARED BORROW (&T) or others -> Pass Through Value ---
                    println!(
                        "Info: Handling Rvalue::Ref({:?}) for place '{}' -> Direct Value",
                        borrow_kind,
                        place_to_string(source_place, tcx)
                    );

                    // 1. Get the value/reference of the place being borrowed directly.
                    //    `emit_instructions_to_get_on_own` handles loading/accessing the value.
                    let (pointee_value_var_name, pointee_get_instructions, pointee_oomir_type) =
                        emit_instructions_to_get_on_own(source_place, tcx, mir, data_types);

                    // 2. Add the instructions needed to get this value.
                    instructions.extend(pointee_get_instructions);

                    // 3. The result *is* the operand representing the borrowed value itself.
                    //    No array wrapping is done.
                    result_operand = oomir::Operand::Variable {
                        name: pointee_value_var_name,
                        ty: pointee_oomir_type,
                    };
                    println!(
                        "Info: -> Direct Value Operand '{}' ({:?})",
                        result_operand.get_name().unwrap_or("<unknown>"),
                        result_operand.get_type()
                    );
                }
            }
        }

        Rvalue::Len(source_place) => {
            let temp_len_var = generate_temp_var_name(&base_temp_name);
            let source_name = place_to_string(source_place, tcx);
            instructions.push(oomir::Instruction::Length {
                dest: temp_len_var.clone(),
                array: oomir::Operand::Variable {
                    name: source_name,
                    ty: ty_to_oomir_type(
                        source_place.ty(&mir.local_decls, tcx).ty,
                        tcx,
                        data_types,
                    ),
                },
            });
            result_operand = oomir::Operand::Variable {
                name: temp_len_var,
                ty: oomir::Type::I32,
            };
        }

        Rvalue::Cast(_cast_kind, operand, target_mir_ty) => {
            let temp_cast_var = generate_temp_var_name(&base_temp_name);
            let oomir_target_type = ty_to_oomir_type(*target_mir_ty, tcx, data_types);
            let source_mir_ty = operand.ty(&mir.local_decls, tcx);
            let oomir_source_type = ty_to_oomir_type(source_mir_ty, tcx, data_types);
            let oomir_operand = convert_operand(operand, tcx, mir, data_types, &mut instructions);

            if oomir_target_type == oomir_source_type {
                println!("Info: Handling Rvalue::Cast (Same OOMIR Types) -> Temp Move.");
                instructions.push(oomir::Instruction::Move {
                    dest: temp_cast_var.clone(),
                    src: oomir_operand,
                });
            } else {
                println!("Info: Handling Rvalue::Cast (Different OOMIR Types) -> Temp Cast.");
                instructions.push(oomir::Instruction::Cast {
                    op: oomir_operand,
                    ty: oomir_target_type.clone(),
                    dest: temp_cast_var.clone(),
                });
            }
            result_operand = oomir::Operand::Variable {
                name: temp_cast_var,
                ty: oomir_target_type,
            };
        }

        Rvalue::BinaryOp(bin_op, box (op1, op2)) => {
            let temp_binop_var = generate_temp_var_name(&base_temp_name);
            let oomir_op1 = convert_operand(op1, tcx, mir, data_types, &mut instructions);
            let oomir_op2 = convert_operand(op2, tcx, mir, data_types, &mut instructions);
            // Determine result type based on operands or destination hint
            let oomir_result_type = get_place_type(original_dest_place, mir, tcx, data_types);

            match bin_op {
                BinOp::Add => instructions.push(oomir::Instruction::Add {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Sub => instructions.push(oomir::Instruction::Sub {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Mul => instructions.push(oomir::Instruction::Mul {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Div => instructions.push(oomir::Instruction::Div {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Rem => instructions.push(oomir::Instruction::Rem {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitAnd => instructions.push(oomir::Instruction::BitAnd {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitOr => instructions.push(oomir::Instruction::BitOr {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitXor => instructions.push(oomir::Instruction::BitXor {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Shl => instructions.push(oomir::Instruction::Shl {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Shr => instructions.push(oomir::Instruction::Shr {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Eq => instructions.push(oomir::Instruction::Eq {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Lt => instructions.push(oomir::Instruction::Lt {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Le => instructions.push(oomir::Instruction::Le {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Ne => instructions.push(oomir::Instruction::Ne {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Ge => instructions.push(oomir::Instruction::Ge {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Gt => instructions.push(oomir::Instruction::Gt {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                // Checked ops need special handling as they produce a tuple
                BinOp::AddWithOverflow | BinOp::SubWithOverflow | BinOp::MulWithOverflow => {
                    // This case needs to return the *tuple* variable, and the instructions
                    // generated inside it are already correct for creating that tuple.
                    let temp_tuple_var = generate_temp_var_name(&base_temp_name); // Temp name for the result tuple

                    // Reuse the logic from the original handle_rvalue for checked ops,
                    // but target the temp_tuple_var instead of the final dest.
                    let (result_mir_ty, _) = {
                        let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                        if let TyKind::Tuple(elements) = place_ty.kind() {
                            (elements[0], elements[1])
                        } else {
                            panic!("Checked op dest type mismatch")
                        }
                    };
                    let op_oomir_ty = ty_to_oomir_type(result_mir_ty, tcx, data_types);
                    // ... get tuple_class_name ...
                    let tuple_class_type =
                        get_place_type(original_dest_place, mir, tcx, data_types);
                    let tuple_class_name = match &tuple_class_type {
                        oomir::Type::Class(name) => name.clone(),
                        _ => panic!("..."),
                    };

                    let operation_string = match bin_op {
                        /* ... */ BinOp::AddWithOverflow => "add",
                        BinOp::SubWithOverflow => "sub",
                        BinOp::MulWithOverflow => "mul",
                        _ => unreachable!(),
                    };
                    let (checked_instructions, tmp_result_var, tmp_overflow_var) =
                        emit_checked_arithmetic_oomir_instructions(
                            &temp_tuple_var, // Pass temp name for context
                            &oomir_op1,
                            &oomir_op2,
                            &op_oomir_ty,
                            operation_string,
                            instructions.len(), // Use current instruction count for offset
                        );
                    instructions.extend(checked_instructions); // Add calc instructions

                    // Construct the *temporary* tuple object
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: temp_tuple_var.clone(), // Assign to temp tuple var
                        class_name: tuple_class_name.clone(),
                    });
                    // Set fields on the *temporary* tuple
                    instructions.push(oomir::Instruction::SetField {
                        object: temp_tuple_var.clone(),
                        field_name: "field0".to_string(),
                        value: oomir::Operand::Variable {
                            name: tmp_result_var,
                            ty: op_oomir_ty.clone(),
                        },
                        field_ty: op_oomir_ty.clone(),
                        owner_class: tuple_class_name.clone(),
                    });
                    instructions.push(oomir::Instruction::SetField {
                        object: temp_tuple_var.clone(),
                        field_name: "field1".to_string(),
                        value: oomir::Operand::Variable {
                            name: tmp_overflow_var,
                            ty: oomir::Type::Boolean,
                        },
                        field_ty: oomir::Type::Boolean,
                        owner_class: tuple_class_name.clone(),
                    });

                    result_operand = oomir::Operand::Variable {
                        // Return the temp tuple var
                        name: temp_tuple_var,
                        ty: tuple_class_type, // The OOMIR tuple class type
                    };
                    // Early return because checked ops handle their own assignment structure internally (now to a temp)
                    return (instructions, result_operand);
                }
                _ => {
                    /* Handle Offset, etc. or panic */
                    println!("Warning: Unhandled binary op {:?}", bin_op);
                    result_operand =
                        get_placeholder_operand(original_dest_place, mir, tcx, data_types);
                    // No instruction needed for placeholder
                    return (instructions, result_operand);
                }
            }
            result_operand = oomir::Operand::Variable {
                name: temp_binop_var,
                ty: oomir_result_type, // Use determined result type
            };
        }

        Rvalue::UnaryOp(operation, operand) => {
            let temp_unop_var = generate_temp_var_name(&base_temp_name);
            let oomir_src_operand =
                convert_operand(operand, tcx, mir, data_types, &mut instructions);
            // Determine result type (often same as operand, except for PtrMetadata)
            let oomir_result_type = match operation {
                UnOp::PtrMetadata => oomir::Type::I32,
                _ => get_place_type(original_dest_place, mir, tcx, data_types), // Use dest type hint
            };

            match operation {
                UnOp::Not => instructions.push(oomir::Instruction::Not {
                    dest: temp_unop_var.clone(),
                    src: oomir_src_operand,
                }),
                UnOp::Neg => instructions.push(oomir::Instruction::Neg {
                    dest: temp_unop_var.clone(),
                    src: oomir_src_operand,
                }),
                UnOp::PtrMetadata => {
                    // Handle length specifically
                    match oomir_src_operand {
                        oomir::Operand::Variable {
                            name: source_name,
                            ty: var_ty,
                        } => {
                            // Check if source is actually an array/slice/str type
                            let operand_place = match operand {
                                MirOperand::Copy(p) | MirOperand::Move(p) => Some(p),
                                _ => None,
                            };
                            if let Some(op_place) = operand_place {
                                let op_place_ty = op_place.ty(&mir.local_decls, tcx).ty;
                                let is_target_for_length = match op_place_ty.kind() {
                                    TyKind::Slice(_) | TyKind::Str => true,
                                    TyKind::Ref(_, inner_ty, _)
                                        if matches!(
                                            inner_ty.kind(),
                                            TyKind::Slice(_) | TyKind::Str
                                        ) =>
                                    {
                                        true
                                    }
                                    TyKind::RawPtr(ty, _) if ty.is_slice() || ty.is_str() => true,
                                    _ => false,
                                };
                                if is_target_for_length {
                                    println!("Info: Detected Length op via PtrMetadata.");
                                    instructions.push(oomir::Instruction::Length {
                                        dest: temp_unop_var.clone(),
                                        array: oomir::Operand::Variable {
                                            name: source_name,
                                            ty: var_ty,
                                        },
                                    });
                                } else {
                                    println!(
                                        "Warning: PtrMetadata on unexpected type {:?}. Emitting placeholder.",
                                        op_place_ty
                                    );
                                    // No instruction needed for placeholder, result_operand set below
                                }
                            } else {
                                // PtrMetadata on constant? Invalid MIR?
                                println!(
                                    "Warning: PtrMetadata on constant operand {:?}. Emitting placeholder.",
                                    operand
                                );
                            }
                        }
                        _ => {
                            // PtrMetadata on constant? Invalid MIR?
                            println!(
                                "Warning: PtrMetadata on constant operand {:?}. Emitting placeholder.",
                                operand
                            );
                        }
                    }
                }
            }

            // Handle cases where no instruction was pushed (e.g., PtrMetadata warning)
            if instructions.last().map_or(true, |inst| match inst {
                oomir::Instruction::Length { dest, .. }
                | oomir::Instruction::Not { dest, .. }
                | oomir::Instruction::Neg { dest, .. } => dest != &temp_unop_var.clone(),
                _ => true, // If last instruction wasn't for this temp var
            }) && *operation != UnOp::PtrMetadata
            /* Allow PtrMetadata failure */
            {
                // If no instruction *actually* assigned to temp_unop_var, it's likely a placeholder case
                result_operand = get_placeholder_operand(original_dest_place, mir, tcx, data_types);
            } else {
                result_operand = oomir::Operand::Variable {
                    name: temp_unop_var.clone(),
                    ty: oomir_result_type,
                };
            }
        }

        Rvalue::Aggregate(box kind, operands) => {
            // Create a temporary variable to hold the aggregate
            let temp_aggregate_var = generate_temp_var_name(&base_temp_name);
            // Get the type from the original destination place
            let aggregate_oomir_type = get_place_type(original_dest_place, mir, tcx, data_types);

            match kind {
                rustc_middle::mir::AggregateKind::Tuple => {
                    let tuple_class_name = match &aggregate_oomir_type {
                        oomir::Type::Class(name) => name.clone(),
                        _ => panic!("Tuple aggregate type error"),
                    };
                    println!(
                        "Info: Handling Tuple Aggregate -> Temp Var '{}'",
                        temp_aggregate_var
                    );
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: temp_aggregate_var.clone(),
                        class_name: tuple_class_name.clone(),
                    });
                    // Set fields on the temporary variable
                    let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                    for (i, mir_op) in operands.iter().enumerate() {
                        let field_name = format!("field{}", i);
                        let element_mir_ty = if let TyKind::Tuple(elements) = place_ty.kind() {
                            elements[i]
                        } else {
                            panic!("...")
                        };
                        let element_oomir_type =
                            ty_to_oomir_type(element_mir_ty.clone(), tcx, data_types);
                        let value_operand =
                            convert_operand(mir_op, tcx, mir, data_types, &mut instructions);
                        instructions.push(oomir::Instruction::SetField {
                            object: temp_aggregate_var.clone(),
                            field_name,
                            value: value_operand,
                            field_ty: element_oomir_type,
                            owner_class: tuple_class_name.clone(),
                        });
                    }
                }
                rustc_middle::mir::AggregateKind::Array(mir_element_ty) => {
                    println!(
                        "Info: Handling Array Aggregate -> Temp Var '{}'",
                        temp_aggregate_var
                    );
                    let oomir_element_type = ty_to_oomir_type(*mir_element_ty, tcx, data_types);
                    let array_size = operands.len();
                    let size_operand =
                        oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));
                    instructions.push(oomir::Instruction::NewArray {
                        dest: temp_aggregate_var.clone(),
                        element_type: oomir_element_type.clone(),
                        size: size_operand,
                    });
                    // Store elements into the temporary array
                    for (i, mir_operand) in operands.iter().enumerate() {
                        let value_operand =
                            convert_operand(mir_operand, tcx, mir, data_types, &mut instructions);
                        let index_operand =
                            oomir::Operand::Constant(oomir::Constant::I32(i as i32));
                        instructions.push(oomir::Instruction::ArrayStore {
                            array: temp_aggregate_var.clone(),
                            index: index_operand,
                            value: value_operand,
                        });
                    }
                }
                rustc_middle::mir::AggregateKind::Adt(def_id, variant_idx, substs, _, _) => {
                    let adt_def = tcx.adt_def(*def_id);
                    if adt_def.is_struct() {
                        let variant = adt_def.variant(*variant_idx);
                        let rust_path = tcx.def_path_str(adt_def.did());
                        let jvm_class_name = make_jvm_safe(&rust_path).replace("::", "/");
                        println!(
                            "Info: Handling Struct Aggregate -> Temp Var '{}' (Class: {})",
                            temp_aggregate_var, jvm_class_name
                        );
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: jvm_class_name.clone(),
                        });
                        // Set fields on the temporary struct object
                        let oomir_fields = /* ... collect field info ... */ variant.fields.iter().map(|f| (f.ident(tcx).to_string(), ty_to_oomir_type(f.ty(tcx, substs), tcx, data_types))).collect();
                        for (field_def, mir_operand) in variant.fields.iter().zip(operands.iter()) {
                            let field_name = field_def.ident(tcx).to_string();
                            let field_mir_ty = field_def.ty(tcx, substs);
                            let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);
                            let value_operand = convert_operand(
                                mir_operand,
                                tcx,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            instructions.push(oomir::Instruction::SetField {
                                object: temp_aggregate_var.clone(),
                                field_name,
                                value: value_operand,
                                field_ty: field_oomir_type,
                                owner_class: jvm_class_name.clone(),
                            });
                        }
                        // Ensure DataType exists
                        if !data_types.contains_key(&jvm_class_name) {
                            data_types.insert(
                                jvm_class_name.clone(),
                                oomir::DataType {
                                    fields: oomir_fields,
                                    is_abstract: false,
                                    methods: HashMap::new(),
                                    super_class: None,
                                },
                            );
                        }
                    } else if adt_def.is_enum() {
                        let variant_def = adt_def.variant(*variant_idx);
                        let base_enum_name = make_jvm_safe(&tcx.def_path_str(adt_def.did()));
                        let variant_class_name = format!(
                            "{}${}",
                            base_enum_name,
                            make_jvm_safe(&variant_def.name.to_string())
                        );

                        println!(
                            "Info: Handling Enum Aggregate (Variant: {}) -> Temp Var '{}' (Class: {})",
                            variant_def.name, temp_aggregate_var, variant_class_name
                        );

                        /*
                        i.e. consider rust code:
                        ```rust
                        enum MyEnum {
                            A(i32),
                            B{x: String},
                            C,
                        }
                        ```

                        psuedo-java for the plan on how to handle this
                        ```java
                        abstract class MyEnum {
                            public abstract int getVariantIdx();
                        }

                        class MyEnum$A extends MyEnum {
                            public int field0;

                            public MyEnum$A(int field0) {
                                this.field0 = field0;
                            }

                            @Override
                            public final int getVariantIdx() { return 0; }
                        }

                        class MyEnum$B extends MyEnum {
                            public String field0;

                            public MyEnum$B(String field0) {
                                this.field0 = field0;
                            }

                            @Override
                            public final int getVariantIdx() { return 1; }
                        }

                        class MyEnum$C extends MyEnum {
                            @Override
                            public final int getVariantIdx() { return 2; }
                        }
                        ```
                        */

                        // the enum in general
                        if !data_types.contains_key(&base_enum_name) {
                            let mut methods = HashMap::new();
                            methods.insert("getVariantIdx".to_string(), (oomir::Type::I32, None));
                            data_types.insert(
                                base_enum_name.clone(),
                                oomir::DataType {
                                    fields: vec![], // No fields in the abstract class
                                    is_abstract: true,
                                    methods,
                                    super_class: None,
                                },
                            );
                        }

                        // this variant
                        if !data_types.contains_key(&variant_class_name) {
                            let mut fields = vec![];
                            for (i, field) in variant_def.fields.iter().enumerate() {
                                let field_name = format!("field{}", i);
                                let field_type =
                                    ty_to_oomir_type(field.ty(tcx, substs), tcx, data_types);
                                fields.push((field_name, field_type));
                            }

                            let mut methods = HashMap::new();
                            methods.insert(
                                "getVariantIdx".to_string(),
                                (
                                    oomir::Type::I32,
                                    Some(oomir::Constant::I32(variant_idx.as_u32() as i32)),
                                ),
                            );

                            data_types.insert(
                                variant_class_name.clone(),
                                oomir::DataType {
                                    fields,
                                    is_abstract: false,
                                    methods,
                                    super_class: Some(base_enum_name.clone()),
                                },
                            );
                        }

                        // Construct the enum variant object
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: variant_class_name.clone(),
                        });

                        // Set fields
                        for (i, field) in variant_def.fields.iter().enumerate() {
                            let field_name = format!("field{}", i);
                            let field_mir_ty = field.ty(tcx, substs);
                            let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);
                            let value_operand = convert_operand(
                                &operands[FieldIdx::from_usize(i)],
                                tcx,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            instructions.push(oomir::Instruction::SetField {
                                object: temp_aggregate_var.clone(),
                                field_name,
                                value: value_operand,
                                field_ty: field_oomir_type,
                                owner_class: variant_class_name.clone(),
                            });
                        }
                    } else {
                        // Union
                        println!("Warning: Unhandled ADT Aggregate Kind -> Temp Placeholder");
                        // make a placeholder (Class("java/lang/Object"))
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: "java/lang/Object".to_string(),
                        });
                    }
                }
                _ => {
                    /* Unknown aggregate */
                    println!("Warning: Unhandled Aggregate Kind -> Temp Placeholder");
                    // No instructions needed for placeholder, result set below
                }
            }

            result_operand = oomir::Operand::Variable {
                name: temp_aggregate_var,
                ty: aggregate_oomir_type,
            };
        }
        Rvalue::RawPtr(kind, place) => {
            // Get the operand and instructions for the *place* being pointed to.
            // We need this regardless of the RawPtrKind.
            let (place_temp_var_name, place_get_instructions, place_temp_var_type) =
                emit_instructions_to_get_on_own(place, tcx, mir, data_types);

            instructions.extend(place_get_instructions);

            match kind {
                rustc_middle::mir::RawPtrKind::FakeForPtrMetadata => {
                    // This pointer is *only* created to get metadata (like length)
                    // from the underlying place. The actual pointer value is irrelevant
                    // in the target code. The subsequent PtrMetadata operation will
                    // operate on the operand representing the place itself.
                    // So, we just pass the place's operand through.
                    println!(
                        "Info: Handling Rvalue::RawPtr(FakeForPtrMetadata) for place '{:?}'. Passing through place operand '{}' ({:?}).",
                        place_to_string(place, tcx), // Use place_to_string for readability
                        place_temp_var_name,
                        place_temp_var_type
                    );
                    result_operand = oomir::Operand::Variable {
                        name: place_temp_var_name, // Use the operand computed for the place
                        ty: place_temp_var_type,
                    };
                }
                rustc_middle::mir::RawPtrKind::Const | rustc_middle::mir::RawPtrKind::Mut => {
                    // For 'real' raw pointers (*const T, *mut T), the JVM has no direct
                    // equivalent. I currently use the operand of the place itself.
                    // Really, we shouldn't be getting these kinds of things unless
                    // `unsafe` is being used in a strange way, and it is a non-goal of this
                    // project to support native-like non-trivial unsafe.
                    println!(
                        "Warning: Handling Rvalue::RawPtr({:?}) for place '{:?}' by using the place's operand '{}' ({:?}). True pointer semantics (arithmetic, etc.) not fully supported.",
                        kind,
                        place_to_string(place, tcx),
                        place_temp_var_name,
                        place_temp_var_type
                    );

                    let pointer_oomir_type = place_temp_var_type.clone();

                    result_operand = oomir::Operand::Variable {
                        name: place_temp_var_name,
                        ty: pointer_oomir_type,
                    };
                }
            }
        }
        Rvalue::Discriminant(place) => {
            // 1. Generate instructions to get the actual value from the place
            let (actual_value_var_name, get_instructions, actual_value_oomir_type) =
                emit_instructions_to_get_on_own(place, tcx, mir, data_types);

            // Add the instructions needed to get the value (e.g., ArrayGet)
            instructions.extend(get_instructions);

            // 2. Now operate on the variable holding the actual value
            let temp_discriminant_var = generate_temp_var_name(&base_temp_name);

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

            let method_name = "getVariantIdx".to_string();
            let method_return_type = oomir::Type::I32;

            let method_ty = oomir::Signature {
                params: vec![],
                ret: Box::new(method_return_type.clone()),
            };

            // 3. Call InvokeVirtual on the CORRECT variable
            instructions.push(oomir::Instruction::InvokeVirtual {
                class_name: place_class_name.clone(),
                method_name,
                args: vec![],
                dest: Some(temp_discriminant_var.clone()),
                method_ty,
                operand: oomir::Operand::Variable {
                    name: actual_value_var_name,
                    ty: actual_value_oomir_type,
                },
            });

            // 4. The result is the temporary variable holding the discriminant value
            result_operand = oomir::Operand::Variable {
                name: temp_discriminant_var,
                ty: method_return_type, // Should be I32
            };
        }
        // Handle other Rvalue variants by generating a placeholder
        _ => {
            println!(
                "Warning: Unhandled Rvalue: {:?} for temp based on {:?}. Emitting placeholder.",
                rvalue, original_dest_place
            );
            result_operand = get_placeholder_operand(original_dest_place, mir, tcx, data_types);
            // No instructions needed to "calculate" a placeholder
        }
    }

    (instructions, result_operand)
}
