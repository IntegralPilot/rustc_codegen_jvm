use rustc_middle::{
    mir::{BinOp, Body, Operand as MirOperand, Place, Rvalue},
    ty::{TyCtxt, TyKind},
};
use std::collections::HashMap;

use super::{
    super::{
        operand::{convert_operand, get_placeholder_operand},
        place::{extract_base_and_field, get_place_type, make_jvm_safe, place_to_string},
        types::{get_field_name_from_index, ty_to_oomir_type},
    },
    checked_ops::emit_checked_arithmetic_oomir_instructions,
};
use crate::oomir;

pub fn handle_rvalue<'a>(
    rvalue: &Rvalue<'a>,
    mir: &Body<'a>,
    tcx: TyCtxt<'a>,
    data_types: &mut HashMap<String, oomir::DataType>,
    place: &Place<'a>,
    dest: String,
    instructions: &mut Vec<oomir::Instruction>,
) {
    match rvalue {
        Rvalue::BinaryOp(bin_op, box (op1, op2)) => {
            let oomir_op1 = { convert_operand(op1, tcx, mir, data_types) };
            let oomir_op2 = { convert_operand(op2, tcx, mir, data_types) };

            match bin_op {
                // Non-checked ops remain the same
                BinOp::Add => instructions.push(oomir::Instruction::Add {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Sub => instructions.push(oomir::Instruction::Sub {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Mul => instructions.push(oomir::Instruction::Mul {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Div => instructions.push(oomir::Instruction::Div {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Eq => instructions.push(oomir::Instruction::Eq {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitAnd => instructions.push(oomir::Instruction::BitAnd {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Rem => {
                    instructions.push(oomir::Instruction::Rem {
                        dest,
                        op1: oomir_op1,
                        op2: oomir_op2,
                    });
                }
                BinOp::AddWithOverflow | BinOp::SubWithOverflow | BinOp::MulWithOverflow => {
                    let dest_place = place;
                    let dest_name = place_to_string(dest_place, tcx);

                    // 1. Get OOMIR operands
                    let oomir_op1 = convert_operand(op1, tcx, mir, data_types);
                    let oomir_op2 = convert_operand(op2, tcx, mir, data_types);

                    // 2. Determine types
                    let place_ty = dest_place.ty(&mir.local_decls, tcx).ty;
                    let (result_mir_ty, overflow_mir_ty) =
                        if let TyKind::Tuple(elements) = place_ty.kind() {
                            if elements.len() == 2 {
                                (elements[0], elements[1])
                            } else {
                                panic!("Checked op destination is not a 2-tuple: {:?}", place_ty);
                            }
                        } else {
                            panic!("Checked op destination is not a tuple: {:?}", place_ty);
                        };
                    let op_oomir_ty = ty_to_oomir_type(result_mir_ty, tcx, data_types);
                    let overflow_oomir_ty = ty_to_oomir_type(overflow_mir_ty, tcx, data_types);
                    if overflow_oomir_ty != oomir::Type::Boolean {
                        println!(
                            "Warning: Expected boolean overflow type, got {:?}",
                            overflow_oomir_ty
                        );
                    }

                    // 3. Get tuple class name
                    let tuple_class_type = ty_to_oomir_type(place_ty, tcx, data_types); // Ensure DataType exists
                    let tuple_class_name = match &tuple_class_type {
                        oomir::Type::Class(name) => name.clone(),
                        _ => panic!(
                            "Internal Error: Checked op tuple type did not resolve to OOMIR Class"
                        ),
                    };

                    // 4. Emit the OOMIR instructions for the checked operation calculation
                    let operation_string = match bin_op {
                        BinOp::AddWithOverflow => "add",
                        BinOp::SubWithOverflow => "sub",
                        BinOp::MulWithOverflow => "mul",
                        _ => unreachable!(),
                    };
                    // Pass current instruction count as unique offset
                    let (checked_instructions, tmp_result_var, tmp_overflow_var) =
                        emit_checked_arithmetic_oomir_instructions(
                            // Call the renamed function
                            &dest_name,
                            &oomir_op1,
                            &oomir_op2,
                            &op_oomir_ty,
                            operation_string,
                            instructions.len(), // Use current length as offset
                        );
                    // Add the generated instructions to the current block
                    instructions.extend(checked_instructions);

                    // 5. Construct the tuple object instance *after* the check instructions
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: dest_name.clone(), // Assign to the original tuple variable _7
                        class_name: tuple_class_name.clone(),
                    });

                    // 6. Set the fields using the temporary variables returned by the helper
                    instructions.push(oomir::Instruction::SetField {
                        object_var: dest_name.clone(),
                        field_name: "field0".to_string(),
                        value: oomir::Operand::Variable {
                            name: tmp_result_var,
                            ty: op_oomir_ty.clone(),
                        }, // Use returned temp name
                        field_ty: op_oomir_ty.clone(),
                        owner_class: tuple_class_name.clone(),
                    });
                    instructions.push(oomir::Instruction::SetField {
                        object_var: dest_name.clone(),
                        field_name: "field1".to_string(),
                        value: oomir::Operand::Variable {
                            name: tmp_overflow_var,
                            ty: oomir::Type::Boolean,
                        }, // Use returned temp name
                        field_ty: oomir::Type::Boolean,
                        owner_class: tuple_class_name.clone(),
                    });
                    println!(
                        "Info: Finished emitting checked op {:?} -> {}",
                        bin_op, dest_name
                    );
                }
                BinOp::BitOr => instructions.push(oomir::Instruction::BitOr {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitXor => instructions.push(oomir::Instruction::BitXor {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Shl => instructions.push(oomir::Instruction::Shl {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Shr => instructions.push(oomir::Instruction::Shr {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Ne => instructions.push(oomir::Instruction::Ne {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Le => instructions.push(oomir::Instruction::Le {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Ge => instructions.push(oomir::Instruction::Ge {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Lt => instructions.push(oomir::Instruction::Lt {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Gt => instructions.push(oomir::Instruction::Gt {
                    dest,
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                _ => {
                    println!(
                        "Warning: Unhandled binary operation {:?} for dest {}",
                        bin_op, dest
                    );
                    instructions.push(oomir::Instruction::Move {
                        dest,
                        src: get_placeholder_operand(place, mir, tcx, data_types),
                    });
                }
            }
        }
        Rvalue::Use(mir_operand) => {
            let dest_name = place_to_string(place, tcx);
            match mir_operand {
                MirOperand::Copy(src_place) | MirOperand::Move(src_place) => {
                    // Check if the SOURCE place has a direct field projection
                    if let Some((base_place, field_index, field_mir_ty)) =
                        extract_base_and_field(tcx, src_place)
                    {
                        // It's a field access (struct OR tuple)! Generate GetField
                        let object_var_name = place_to_string(&base_place, tcx); // e.g., "_1" or "_7"

                        // Get the OOMIR type of the base object (_1 or _7)
                        // For tuples, this will now be Type::Class("rust/tuples/Tuple_...")
                        let owner_oomir_type = get_place_type(&base_place, mir, tcx, data_types);
                        let owner_class_name = match owner_oomir_type {
                            oomir::Type::Class(name) => name,
                            // Added check for Ref<Class> just in case base_place is a reference
                            oomir::Type::Reference(inner) => {
                                if let oomir::Type::Class(name) = *inner {
                                    name
                                } else {
                                    panic!(
                                        "GetField source ref inner type is not Class: {:?}, Place: {:?}",
                                        inner, src_place
                                    );
                                }
                            }
                            _ => panic!(
                                "GetField source '{}' is not a Class or Ref<Class> type: {:?}. Place: {:?}",
                                object_var_name, owner_oomir_type, src_place
                            ),
                        };

                        // Get the field NAME from the index.
                        // This helper works for both structs and tuples because the DataType
                        // for tuples now contains fields named "field0", "field1", etc.
                        let field_name = match get_field_name_from_index(
                            &owner_class_name,
                            field_index.index(),
                            data_types,
                        ) {
                            Ok(name) => name,
                            Err(e) => {
                                // This error is now more serious if it happens for a Class type
                                panic!(
                                    "Error getting field name for class '{}' index {}: {}. MIR Place: {:?}",
                                    owner_class_name,
                                    field_index.index(),
                                    e,
                                    src_place
                                );
                            }
                        };

                        // Convert the MIR field type to OOMIR type
                        let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);

                        println!(
                            "Info: Translating MIR assignment from field (struct/tuple): {} = {}.{} (GetField)",
                            dest_name, object_var_name, field_name
                        );

                        instructions.push(oomir::Instruction::GetField {
                            dest: dest_name,
                            object_var: object_var_name,
                            field_name,
                            field_ty: field_oomir_type,
                            owner_class: owner_class_name, // Use extracted name
                        });
                    } else {
                        // No field projection on source (e.g., let x = y;)
                        let src_oomir_operand = convert_operand(mir_operand, tcx, mir, data_types);
                        println!(
                            "Info: Translating MIR assignment from simple place: {} = {:?} (Move)",
                            dest_name, src_oomir_operand
                        );
                        instructions.push(oomir::Instruction::Move {
                            dest: dest_name,
                            src: src_oomir_operand,
                        });
                    }
                }
                MirOperand::Constant(_) => {
                    // Source is a constant, generate Move
                    let src_oomir_operand = convert_operand(mir_operand, tcx, mir, data_types);
                    instructions.push(oomir::Instruction::Move {
                        dest: dest_name,
                        src: src_oomir_operand,
                    });
                }
            }
        }
        Rvalue::Aggregate(box kind, operands) => {
            let dest_name = place_to_string(place, tcx);

            match kind {
                rustc_middle::mir::AggregateKind::Tuple => {
                    // 1. Get the OOMIR type of the destination (should be Class(tuple_class_name))
                    let dest_oomir_type = get_place_type(place, mir, tcx, data_types);
                    let tuple_class_name = match dest_oomir_type {
                        oomir::Type::Class(name) => name,
                        _ => panic!(
                            "Internal Error: Destination of Tuple Aggregate is not an OOMIR Class type, got {:?}",
                            dest_oomir_type
                        ),
                    };

                    println!(
                        "Info: Handling Tuple Aggregate for '{}' (Class: {})",
                        dest_name, tuple_class_name
                    );

                    // 2. Construct the tuple object
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: dest_name.clone(),
                        class_name: tuple_class_name.clone(),
                    });
                    println!(
                        "   -> Emitted ConstructObject: dest={}, class={}",
                        dest_name, tuple_class_name
                    );

                    // 3. Set fields ("field0", "field1", ...)
                    for (i, mir_op) in operands.iter().enumerate() {
                        let field_name = format!("field{}", i);
                        // Get the OOMIR type of the element being assigned
                        // We need the corresponding MIR type from the destination Place's type
                        let place_ty = place.ty(&mir.local_decls, tcx).ty;
                        let element_mir_ty = if let TyKind::Tuple(elements) = place_ty.kind() {
                            elements
                                .get(i)
                                .expect("Tuple index out of bounds for Aggregate")
                        } else {
                            panic!(
                                "Internal Error: AggregateKind::Tuple destination Place is not TyKind::Tuple"
                            );
                        };
                        let element_oomir_type =
                            ty_to_oomir_type(element_mir_ty.clone(), tcx, data_types);
                        let value_operand = convert_operand(mir_op, tcx, mir, data_types);

                        instructions.push(oomir::Instruction::SetField {
                            object_var: dest_name.clone(), // The tuple object
                            field_name: field_name.clone(),
                            value: value_operand.clone(),
                            field_ty: element_oomir_type.clone(),
                            owner_class: tuple_class_name.clone(),
                        });
                        println!(
                            "   -> Emitted SetField: object={}, field={}, type={:?}, value={:?}",
                            dest_name, field_name, element_oomir_type, value_operand
                        );
                    }
                }

                rustc_middle::mir::AggregateKind::Array(mir_element_ty) => {
                    println!(
                        "Info: Handling Rvalue::Aggregate Array for dest '{}'",
                        dest_name
                    );

                    // 1. Get the OOMIR element type
                    let oomir_element_type = ty_to_oomir_type(*mir_element_ty, tcx, data_types);

                    // 2. Get the array size
                    let array_size = operands.len();
                    let size_operand =
                        oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));

                    // 3. Generate the NewArray instruction
                    //    The dest_name ('a6') will hold the reference to the new array.
                    instructions.push(oomir::Instruction::NewArray {
                        dest: dest_name.clone(), // Assign the new array ref to _6
                        element_type: oomir_element_type.clone(),
                        size: size_operand,
                    });
                    println!(
                        "   -> Emitted NewArray: dest={}, type={:?}, size={}",
                        dest_name, oomir_element_type, array_size
                    );

                    // 4. Generate ArrayStore instructions for each element
                    for (i, mir_operand) in operands.iter().enumerate() {
                        let value_operand = convert_operand(mir_operand, tcx, mir, data_types);
                        let index_operand =
                            oomir::Operand::Constant(oomir::Constant::I32(i as i32));

                        instructions.push(oomir::Instruction::ArrayStore {
                            array_var: dest_name.clone(), // Store into the array ref in _6
                            index: index_operand,
                            value: value_operand.clone(), // Clone operand if needed elsewhere
                        });
                        println!(
                            "   -> Emitted ArrayStore: array={}, index={}, value={:?}",
                            dest_name, i, value_operand
                        );
                    }
                }
                rustc_middle::mir::AggregateKind::Adt(
                    def_id,
                    variant_idx,
                    substs,
                    _user_type_annotation_index,
                    _field_index,
                ) => {
                    let adt_def = tcx.adt_def(*def_id);
                    if adt_def.is_struct() {
                        let variant = adt_def.variant(*variant_idx);
                        let rust_path = tcx.def_path_str(adt_def.did());
                        let jvm_class_name = make_jvm_safe(&rust_path).replace("::", "/");

                        println!(
                            "Info: Handling Struct Aggregate for '{}' (Class: {})",
                            dest_name, // Use simple name "_1"
                            jvm_class_name
                        );

                        // 1. Construct object (assigns to simple name like "_1")
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: dest_name.clone(), // Correct: dest is "_1"
                            class_name: jvm_class_name.clone(),
                        });
                        println!(
                            // Log correction
                            "   -> Emitted ConstructObject: dest={}, class={}",
                            dest_name, // Log simple name
                            jvm_class_name
                        );

                        // 2. Set fields (uses simple name "_1" as object_var)
                        let oomir_fields = variant
                            .fields
                            .iter()
                            .map(|field_def| {
                                let field_name = field_def.ident(tcx).to_string();
                                let field_mir_ty = field_def.ty(tcx, substs);
                                let field_oomir_type =
                                    ty_to_oomir_type(field_mir_ty, tcx, data_types);
                                (field_name, field_oomir_type)
                            })
                            .collect::<Vec<_>>();

                        for (field_def, mir_operand) in variant.fields.iter().zip(operands.iter()) {
                            let field_name = field_def.ident(tcx).to_string();
                            // Get the *monomorphized* type of the field using substitutions
                            let field_mir_ty = field_def.ty(tcx, substs);
                            let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);
                            let value_operand = convert_operand(mir_operand, tcx, mir, data_types);

                            instructions.push(oomir::Instruction::SetField {
                                object_var: dest_name.clone(), // The struct object we just created
                                field_name: field_name.clone(),
                                value: value_operand.clone(),
                                field_ty: field_oomir_type.clone(),
                                owner_class: jvm_class_name.clone(), // The class where the field is defined
                            });
                            println!(
                                "   -> Emitted SetField: object={}, field={}, type={:?}, value={:?}",
                                dest_name, field_name, field_oomir_type, value_operand
                            );
                        }
                        // Ensure DataType insertion uses JVM name, not dest_name
                        if !data_types.contains_key(&jvm_class_name) {
                            // Use JVM name as key
                            data_types.insert(
                                jvm_class_name.clone(), // Key is JVM name
                                oomir::DataType {
                                    name: jvm_class_name,
                                    fields: oomir_fields,
                                }, // Ensure oomir_fields captured correctly
                            );
                        }
                    } else if adt_def.is_enum() {
                        // Enum handling is more complex (needs discriminant, potentially different layouts)
                        // do it later
                        println!(
                            "Warning: Enum aggregate kind not yet fully handled for dest {}. ADT: {:?}",
                            dest_name,
                            adt_def.did()
                        );
                        instructions.push(oomir::Instruction::Move {
                            dest: dest_name,
                            src: get_placeholder_operand(place, mir, tcx, data_types),
                        });
                    } else if adt_def.is_union() {
                        println!(
                            "Warning: Union aggregate kind not yet handled for dest {}. ADT: {:?}",
                            dest_name,
                            adt_def.did()
                        );
                        instructions.push(oomir::Instruction::Move {
                            dest: dest_name,
                            src: get_placeholder_operand(place, mir, tcx, data_types),
                        });
                    } else {
                        println!(
                            "Warning: Unhandled ADT kind {:?} for dest {}",
                            adt_def, dest_name
                        );
                        instructions.push(oomir::Instruction::Move {
                            dest: dest_name,
                            src: get_placeholder_operand(place, mir, tcx, data_types),
                        });
                    }
                }
                _ => {
                    println!(
                        "Warning: Unhandled aggregate kind {:?} for dest {}",
                        kind, dest_name
                    );
                    // Assign a type-aware placeholder
                    instructions.push(oomir::Instruction::Move {
                        dest: dest_name, // Use the extracted dest_name
                        src: get_placeholder_operand(place, mir, tcx, data_types), // Use helper
                    });
                }
            }
        }
        Rvalue::Ref(_region, _borrow_kind, source_place) => {
            let dest_name = dest; // e.g., "_9"

            // Check if the place being referenced (source_place) involves a field access
            if let Some((base_place, field_index, field_mir_ty)) =
                extract_base_and_field(tcx, source_place)
            {
                // --- Field access inside Ref ---
                let object_var_name = place_to_string(&base_place, tcx); // e.g., "_1"

                // Get the type of the base object to find the owner class name
                let owner_class_oomir_type = get_place_type(&base_place, mir, tcx, data_types);
                let owner_class_name = match &owner_class_oomir_type {
                    oomir::Type::Class(name) => name.clone(),
                    oomir::Type::Reference(inner) => {
                        // Handle if base is &Struct
                        if let oomir::Type::Class(name) = inner.as_ref() {
                            name.clone()
                        } else {
                            panic!("Ref source's inner type is not a Class: {:?}", inner);
                        }
                    }
                    _ => panic!(
                        "Ref source base '{}' is not a class type: {:?}",
                        object_var_name, owner_class_oomir_type
                    ),
                };

                // Get the field name
                let field_name = match get_field_name_from_index(
                    &owner_class_name,
                    field_index.index(),
                    data_types,
                ) {
                    Ok(name) => name,
                    Err(e) => {
                        println!(
                            "Error getting field name for Ref source: {}. Defaulting.",
                            e
                        );
                        format!("field{}", field_index.index()) // Fallback name
                    }
                };

                // Get the field type
                let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);

                // Create a temporary OOMIR variable name to hold the field value
                let temp_field_var = format!("{}_ref_field_temp", dest_name);

                println!(
                    // Add log
                    "Info: Handling Rvalue::Ref with field source: {} = &({}.{}). Temp var: {}",
                    dest_name, object_var_name, field_name, temp_field_var
                );

                // 1. Emit GetField to load the field value into the temporary variable
                instructions.push(oomir::Instruction::GetField {
                    dest: temp_field_var.clone(), // Store field value here
                    object_var: object_var_name,
                    field_name,
                    field_ty: field_oomir_type.clone(), // Type of the actual field
                    owner_class: owner_class_name,
                });

                // 2. Emit Move to assign the temporary variable to the final destination
                //    The type of the *source operand* here is the field's type.
                //    The destination variable (`dest_name`, e.g., "_9") will conceptually hold
                //    a reference, but in OOMIR/JVM this often means just moving the value/object ref.
                //    The type system in lower2 needs to know `_9` is a reference type.
                instructions.push(oomir::Instruction::Move {
                    dest: dest_name, // Final destination, e.g., "_9"
                    src: oomir::Operand::Variable {
                        name: temp_field_var,
                        ty: field_oomir_type, // Use the type of the value obtained from GetField
                    },
                });
            } else {
                // --- Simple Ref (no field access in source) ---
                let source_name = place_to_string(source_place, tcx); // e.g., "_3" if it was _9 = &_3
                let source_type = get_place_type(source_place, mir, tcx, data_types);

                println!(
                    // Keep existing log for simple refs
                    "Info: Handled Rvalue::Ref: {} = &{} (via Move with types src={:?})",
                    dest_name, // Use dest_name calculated earlier
                    source_name,
                    source_type
                );

                instructions.push(oomir::Instruction::Move {
                    dest: dest_name, // Final destination
                    src: oomir::Operand::Variable {
                        name: source_name,
                        ty: source_type, // Type of the variable being referenced
                    },
                });
            }
        }
        Rvalue::Cast(_cast_kind, operand, target_mir_ty) => {
            // 1. Get OOMIR type of the target MIR type
            let oomir_target_type = ty_to_oomir_type(*target_mir_ty, tcx, data_types);

            // 2. Get OOMIR type of the source MIR operand
            let source_mir_ty = operand.ty(&mir.local_decls, tcx);
            let oomir_source_type = ty_to_oomir_type(source_mir_ty, tcx, data_types);

            // 3. Convert the source MIR operand to an OOMIR operand
            let oomir_operand = convert_operand(operand, tcx, mir, data_types);

            // 4. Check if the OOMIR types are the same
            if oomir_target_type == oomir_source_type {
                // Types are the same in OOMIR (e.g., i32->u32 both become I32).
                // Generate a Move instruction to perform the assignment.
                println!(
                    "Info: Handling Rvalue::Cast (Same OOMIR Types): {} = {:?} (as {:?} from {:?}). Emitting Move.",
                    dest, oomir_operand, oomir_target_type, oomir_source_type
                );
                instructions.push(oomir::Instruction::Move {
                    dest,               // The destination variable name (e.g., "_3")
                    src: oomir_operand, // The source value (e.g., Constant(I32(10)))
                });
            } else {
                // Types differ in OOMIR (e.g., i32->i64). Generate a Cast instruction.
                println!(
                    "Info: Handling Rvalue::Cast (Different OOMIR Types): {} = {:?} as {:?} (from {:?}). Emitting Cast.",
                    dest, oomir_operand, oomir_target_type, oomir_source_type
                );
                instructions.push(oomir::Instruction::Cast {
                    op: oomir_operand,
                    ty: oomir_target_type, // The target OOMIR type
                    dest,                  // The destination variable name
                });
            }
        }
        _ => {
            println!("Warning: Unhandled rvalue {:?}", rvalue);
            // Assign a type-aware placeholder
            instructions.push(oomir::Instruction::Move {
                dest,                                                      // Use the calculated name
                src: get_placeholder_operand(place, mir, tcx, data_types), // Use helper
            });
        }
    }
}
