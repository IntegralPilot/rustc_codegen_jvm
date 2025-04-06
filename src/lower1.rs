//! This is the stage 1 lowering pass of the compiler.
//! It is responsible for coverting the MIR into a lower-level IR, called OOMIR (see src/oomir.rs).
//! It is a simple pass that converts the MIR into a more object-oriented representation.

// lower1.rs
//! This module converts Rust MIR into an object-oriented MIR (OOMIR)
//! that sits between MIR and JVM bytecode. It supports a subset of Rust constructs
//! (arithmetic, branching, returns) and can be extended to support more of Rust.

use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, BinOp, Operand as MirOperand, Rvalue, StatementKind,
    TerminatorKind, Place,
};
use rustc_middle::ty::{Instance, TyCtxt, Ty, IntTy, UintTy, FloatTy};
use rustc_middle::mir::{Const, ConstValue, interpret::Scalar};
use std::collections::HashMap;
use crate::oomir;

/// Converts a Rust MIR type (`Ty`) to an OOMIR type (`oomir::Type`).
fn ty_to_oomir_type<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> oomir::Type {
    match ty.kind() {
        rustc_middle::ty::TyKind::Bool => oomir::Type::Boolean,
        rustc_middle::ty::TyKind::Char => oomir::Type::Char,
        rustc_middle::ty::TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Type::I8,
            IntTy::I16 => oomir::Type::I16,
            IntTy::I32 => oomir::Type::I32,
            IntTy::I64 => oomir::Type::I64,
            IntTy::Isize => oomir::Type::I64,
            IntTy::I128 => oomir::Type::I64,
        },
        rustc_middle::ty::TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Type::U8,
            UintTy::U16 => oomir::Type::U16,
            UintTy::U32 => oomir::Type::U32,
            UintTy::U64 => oomir::Type::U64,
            UintTy::Usize => oomir::Type::U64,
            UintTy::U128 => oomir::Type::U64,
        },
        rustc_middle::ty::TyKind::Float(float_ty) => match float_ty {
            FloatTy::F32 => oomir::Type::F32,
            FloatTy::F64 => oomir::Type::F64,
            FloatTy::F16 => oomir::Type::F32,
            FloatTy::F128 => oomir::Type::F64,
        },
        rustc_middle::ty::TyKind::Adt(adt_def, _) => {
            oomir::Type::Class(format!("{:?}", adt_def.did()))
        },
        rustc_middle::ty::TyKind::Ref(_, inner_ty, _) | rustc_middle::ty::TyKind::RawPtr(inner_ty, _) => {
            oomir::Type::Reference(Box::new(ty_to_oomir_type(*inner_ty, tcx)))
        },
        rustc_middle::ty::TyKind::Array(component_ty, _) => {
            oomir::Type::Array(Box::new(ty_to_oomir_type(*component_ty, tcx)))
        },
        rustc_middle::ty::TyKind::Tuple(tuple_struct) if tuple_struct.len() == 0 => { // ADD THIS CASE
            oomir::Type::Void // Unit type maps to OOMIR Void
        },
        _ => {
            println!("Warning: Unhandled type {:?}", ty);
            oomir::Type::Class("UnsupportedType".to_string())
        }
    }
}

fn place_to_string<'tcx>(place: &Place<'tcx>, _tcx: TyCtxt<'tcx>) -> String {
    // Base variable name (e.g., "_1")
    let mut name = format!("_{}", place.local.index()); // Start with base local "_N"

    // Append projections cleanly
    for proj_elem in place.projection.iter() {
        match proj_elem {
            rustc_middle::mir::ProjectionElem::Field(field, _ty) => {
                // Append ".index" for field access
                name.push_str(&format!(".{}", field.index()));
            }
            // --- Handle other projections as needed, cleanly ---
            rustc_middle::mir::ProjectionElem::Deref => {
                name = format!("deref_{}", name); // Or maybe just skip for simple cases?
            }
            rustc_middle::mir::ProjectionElem::Index(local) => {
                 name.push_str(&format!(".idx_{}", local.index())); // Example for index
            }
            // ... etc ...
            _ => {
                 name.push_str(".?"); // Placeholder for others
            }
        }
    }
    name // Return the simple, projected name (e.g., "_7.0")
}

/// Convert a MIR operand to an OOMIR operand.
fn convert_operand<'tcx>(mir_op: &MirOperand<'tcx>, t: TyCtxt<'tcx>) -> oomir::Operand {
    match mir_op {
        MirOperand::Constant(box constant) => {
            match constant.const_ {
                Const::Val(val, ty) => {
                    match val {
                        ConstValue::Scalar(scalar) => {
                            match scalar {
                                Scalar::Int(scarint) => {
                                    match ty.kind() {
                                        rustc_middle::ty::TyKind::Int(_) => {
                                            let int_val = scarint.to_i32();
                                            oomir::Operand::Constant(oomir::Constant::I32(int_val))
                                        }
                                        rustc_middle::ty::TyKind::Uint(_) => {
                                            let uint_val = scarint.to_u32();
                                            oomir::Operand::Constant(oomir::Constant::U32(uint_val))
                                        }
                                        _ => {
                                            println!("Warning: Unhandled scalar type {:?}", ty);
                                            oomir::Operand::Constant(oomir::Constant::I32(0)) // Default case
                                        }
                                    }
                                },
                                _ => {
                                    println!("Warning: Unhandled scalar value {:?}", scalar);
                                    oomir::Operand::Constant(oomir::Constant::I32(0)) // Default case
                                }
                            }
                        }
                        _ => {
                            println!("Warning: Unhandled constant kind {:?}", constant);
                            oomir::Operand::Constant(oomir::Constant::I32(0)) // Default case
                        }
                    }
                }
                _ => {
                    println!("Warning: Unhandled constant literal {:?}", constant);
                    oomir::Operand::Constant(oomir::Constant::I32(0)) // Default case
                }
            }
        },
        MirOperand::Copy(place) | MirOperand::Move(place) => {
            // For now, we treat all places as variables.
            // In a real implementation, we would need to handle references and dereferencing.
            let var_name = place_to_string(place, t);
            oomir::Operand::Variable(var_name)
        }
    }
}

// Other functions (e.g., `convert_basic_block`, `mir_to_oomir`) should be updated similarly.
/// Convert a single MIR basic block into an OOMIR basic block.
fn convert_basic_block<'tcx>(
    bb: BasicBlock,
    bb_data: &BasicBlockData<'tcx>,
    tcx: TyCtxt<'tcx>,
    return_oomir_type: &oomir::Type, // Pass function return type
    basic_blocks: &mut HashMap<String, oomir::BasicBlock>,
) -> oomir::BasicBlock {
    // Use the basic block index as its label.
    let label = format!("bb{}", bb.index());
    let mut instructions = Vec::new();

    // Convert each MIR statement in the block.
    for stmt in &bb_data.statements {
        if let StatementKind::Assign(box (place, rvalue)) = &stmt.kind {
            let dest = format!("{:?}", place); // Destination variable
            match rvalue {
                Rvalue::BinaryOp(bin_op, box (op1, op2)) => {
                    let oomir_op1 = convert_operand(op1, tcx);
                    let oomir_op2 = convert_operand(op2, tcx);

                    match bin_op {
                        // Non-checked ops remain the same
                        BinOp::Add => instructions.push(oomir::Instruction::Add { dest, op1: oomir_op1, op2: oomir_op2 }),
                        BinOp::Sub => instructions.push(oomir::Instruction::Sub { dest, op1: oomir_op1, op2: oomir_op2 }),
                        BinOp::Mul => instructions.push(oomir::Instruction::Mul { dest, op1: oomir_op1, op2: oomir_op2 }),
                        BinOp::Eq => instructions.push(oomir::Instruction::Eq { dest, op1: oomir_op1, op2: oomir_op2 }),
                        BinOp::BitAnd => instructions.push(oomir::Instruction::BitAnd { dest, op1: oomir_op1, op2: oomir_op2 }),
                        BinOp::Rem => {
                            instructions.push(oomir::Instruction::Rem { dest, op1: oomir_op1, op2: oomir_op2 });
                        }
                        // Checked ops: Generate a tuple with result and overflow flag
                        BinOp::AddWithOverflow => {
                            let tuple_dest = format!("{}_tuple", dest);
                            instructions.push(oomir::Instruction::AddWithOverflow {
                                dest: tuple_dest.clone(),
                                op1: oomir_op1,
                                op2: oomir_op2,
                            });

                            // Split the tuple into result and overflow flag
                            let result_dest = format!("{}_result", tuple_dest);
                            let overflow_dest = format!("{}_overflow", tuple_dest);
                            instructions.push(oomir::Instruction::Move {
                                src: oomir::Operand::Variable(result_dest.clone()),
                                dest: format!("{}.0", dest),
                            });
                            instructions.push(oomir::Instruction::Move {
                                src: oomir::Operand::Variable(overflow_dest.clone()),
                                dest: format!("{}.1", dest),
                            });
                        },
                        BinOp::SubWithOverflow => {
                            let tuple_dest = format!("{}_tuple", dest);
                            instructions.push(oomir::Instruction::SubWithOverflow {
                                dest: tuple_dest.clone(),
                                op1: oomir_op1,
                                op2: oomir_op2,
                            });

                            // Split the tuple into result and overflow flag
                            let result_dest = format!("{}_result", tuple_dest);
                            let overflow_dest = format!("{}_overflow", tuple_dest);
                            instructions.push(oomir::Instruction::Move {
                                src: oomir::Operand::Variable(result_dest.clone()),
                                dest: format!("{}.0", dest),
                            });
                            instructions.push(oomir::Instruction::Move {
                                src: oomir::Operand::Variable(overflow_dest.clone()),
                                dest: format!("{}.1", dest),
                            });
                        }
                        // Default case for other binary ops
                         _ => {
                            println!("Warning: Unsupported binary operation {:?} in block {}", bin_op, bb.index());
                            // Optionally push a placeholder or skip
                        }
                    }
                }
                Rvalue::Use(mir_operand) => {
                    // Convert the MIR operand (_1, constant, _7.0, etc.)
                    let mut src_oomir_operand = convert_operand(mir_operand, tcx);

                    // Check if the *source* operand is a variable that looks like a tuple field access
                    if let oomir::Operand::Variable(ref src_name) = src_oomir_operand {
                        if src_name.contains('.') {
                            // Handle tuple projections (e.g., _7.0, _7.1)
                            if let Some((base_var, field)) = src_name.split_once('.') {
                                let projection_var = format!("{}_proj_{}", base_var, field);
                                instructions.push(oomir::Instruction::Move {
                                    dest: projection_var.clone(),
                                    src: oomir::Operand::Variable(src_name.clone()),
                                });
                                src_oomir_operand = oomir::Operand::Variable(projection_var);
                            }
                        }
                    }

                    // Generate the OOMIR Move instruction with the (potentially adjusted) source
                    instructions.push(oomir::Instruction::Move { dest: dest.clone(), src: src_oomir_operand });
                }
                Rvalue::Aggregate(box kind, operands) => {
                    match kind {
                        rustc_middle::mir::AggregateKind::Tuple => {
                            // 'dest' here is the name of the tuple itself (e.g., "_2")
                            // We need to define its fields.
                            if operands.len() > 2 {
                                println!("Warning: Tuple aggregate with >2 fields not fully handled: {}", dest);
                            }
                            for (i, mir_op) in operands.iter().enumerate() {
                                // Define the field variable name (e.g., "_2.0")
                                let field_dest = format!("{}.{}", dest, i);
                                let field_src = convert_operand(mir_op, tcx);
                                // Generate a Move to define the field variable
                                instructions.push(oomir::Instruction::Move {
                                    dest: field_dest,
                                    src: field_src,
                                });
                            }
                            // We might not need an explicit instruction for the tuple 'dest' itself
                            // if we only ever access its fields directly via the ".N" names.
                        }
                        _ => {
                            println!("Warning: Unhandled aggregate kind {:?} for dest {}", kind, dest);
                            // Assign a default/dummy value? Might cause issues later.
                            // For now, maybe push a placeholder move:
                             instructions.push(oomir::Instruction::Move {
                                 dest: dest.clone(),
                                 src: oomir::Operand::Constant(oomir::Constant::I32(0)), // Placeholder!
                             });
                        }
                    }
                }
                _ => {
                    println!("Warning: Unhandled rvalue {:?}", rvalue);
                }
            }
        }
    }

    // Convert the MIR terminator into corresponding OOMIR instructions.
    if let Some(terminator) = &bb_data.terminator {
        match &terminator.kind {
            TerminatorKind::Return => { // Handle Return without operand
                if *return_oomir_type == oomir::Type::Void {
                    instructions.push(oomir::Instruction::Return { operand: None });
                } else {
                    let return_operand = convert_operand(&MirOperand::Move(Place::return_place()), tcx);
                    instructions.push(oomir::Instruction::Return {
                        operand: Some(return_operand),
                    });
                }
            }
            TerminatorKind::Goto { target } => {
                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump { target: target_label });
            }
            TerminatorKind::SwitchInt { discr, targets, .. } => {
                // Check if the discriminant is the result of a Rem operation in the current block
                if let MirOperand::Copy(Place { local, .. }) | MirOperand::Move(Place { local, .. }) = discr {
                    let is_rem_result = bb_data.statements.iter().any(|stmt| {
                        if let StatementKind::Assign(box (place_assign, Rvalue::BinaryOp(BinOp::Rem, _))) = &stmt.kind {
                            place_assign.local == *local
                        } else {
                            false
                        }
                    });

                    if is_rem_result {
                        let condition_operand = convert_operand(discr, tcx);

                        // 1. Rem instruction is already generated (from statements processing)

                        // 2. Generate Eq instruction (compare remainder with 0)
                        let eq_dest = format!("eq_temp_{}", bb.index());
                        instructions.push(oomir::Instruction::Eq {
                            dest: eq_dest.clone(),
                            op1: condition_operand, // Use the result of Rem
                            op2: oomir::Operand::Constant(oomir::Constant::I32(0)),
                        });

                        // 3. Generate Branch instruction using the result of Eq as condition
                        let condition_variable = oomir::Operand::Variable(eq_dest);

                        let mut iter = targets.iter();
                        if let Some((_, target)) = iter.next() { // Assuming first target for '0' is 'true'
                            let true_block = format!("bb{}", target.index());
                            let default_target = targets.otherwise();
                            let false_block = format!("bb{}", default_target.index());
                            instructions.push(oomir::Instruction::Branch {
                                condition: condition_variable,
                                true_block,
                                false_block,
                            });
                        }
                        // Already handled by Rem+Eq+Branch, so return
                        return oomir::BasicBlock { label, instructions };
                    }
                }


                // Default SwitchInt handling (if not Rem-related or fallback) - simpler branch as before, might need refinement
                let condition = convert_operand(discr, tcx);
                let mut iter = targets.iter();
                if let Some((_, target)) = iter.next() {
                    let true_block = format!("bb{}", target.index());
                    // Use the default target if available.
                    let default_target = targets.otherwise();
                    let false_block = format!("bb{}", default_target.index());
                    instructions.push(oomir::Instruction::Branch {
                        condition,
                        true_block,
                        false_block,
                    });
                }
            }
            TerminatorKind::Call { func, args, destination, target, ..} => {
                let function_name = format!("{:?}", func); // Get function name - needs refinement to extract actual name
                let oomir_args = args.iter().map(|arg| convert_operand(&arg.node, tcx)).collect();
                let dest = Some(format!("{:?}", destination.local));
                
                instructions.push(oomir::Instruction::Call {
                    dest,
                    function: function_name,
                    args: oomir_args,
                });

                if let Some(target_bb) = target {
                    let target_label = format!("bb{}", target_bb.index());
                    println!("Info: Adding Jump to {} after Call in bb{}", target_label, bb.index()); // Add log
                    instructions.push(oomir::Instruction::Jump { target: target_label });
                } else {
                    // Function diverges (e.g., panic!) - No jump needed.
                    println!("Info: Call in bb{} has no return target (diverges).", bb.index());
                }
            }
            TerminatorKind::Assert { target, cond, expected, msg, unwind: _ } => {
                // Convert the condition operand
                let condition = convert_operand(cond, tcx);

                // Generate a comparison instruction to check if the condition matches the expected value
                let comparison_dest = format!("assert_cmp_{}", bb.index());
                instructions.push(oomir::Instruction::Eq {
                    dest: comparison_dest.clone(),
                    op1: condition,
                    op2: oomir::Operand::Constant(oomir::Constant::Boolean(*expected)),
                });

                // Generate a branch based on the comparison result
                let true_block = format!("bb{}", target.index()); // Success path
                let false_block = format!("assert_fail_{}", bb.index()); // Failure path label
                instructions.push(oomir::Instruction::Branch {
                    condition: oomir::Operand::Variable(comparison_dest),
                    true_block, // Jump here if assertion holds (cond == expected)
                    false_block: false_block.clone(), // Jump here if assertion fails
                });

                // Add a new basic block for the assertion failure (panic)
                let fail_instructions = vec![
                    oomir::Instruction::ThrowNewWithMessage {
                        // For now, hardcode RuntimeException. Could be configurable later.
                        exception_class: "java/lang/RuntimeException".to_string(),
                        // Extract the format string as the message.
                        // TODO: Handle msg.args() for formatted messages later.
                        message: format!("{:?}", msg),
                    },
                ];
                basic_blocks.insert( // Ensure 'basic_blocks' map is mutable and passed in
                    false_block.clone(),
                    oomir::BasicBlock {
                        label: false_block,
                        instructions: fail_instructions,
                    },
                );
            }
            // Other terminator kinds (like Resume, etc.) can be added as needed.
            _ => {
                println!("Warning: Unhandled terminator {:?}", terminator.kind);
            }
        }
    }

    oomir::BasicBlock { label, instructions }
}

/// Converts a MIR Body into an OOMIR Function.
/// This function extracts a function’s signature (currently minimal) and builds
/// a control flow graph of basic blocks.
pub fn mir_to_oomir<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &mut Body<'tcx>,
) -> oomir::Function {
    // Get a function name from the instance.
    let fn_name = tcx.item_name(instance.def_id()).to_string();

    // Extract function signature
    let mir_sig = tcx.type_of(instance.def_id()).skip_binder().fn_sig(tcx);
    let params_ty = mir_sig.inputs();
    let return_ty = mir_sig.output();

    let params_oomir_ty: Vec<oomir::Type> = params_ty.skip_binder().iter().map(|ty| ty_to_oomir_type(*ty, tcx)).collect();
    let return_oomir_ty: oomir::Type = ty_to_oomir_type(return_ty.skip_binder(), tcx);


    let mut signature = oomir::Signature {
        params: params_oomir_ty,
        ret: Box::new(return_oomir_ty.clone()), // Clone here to pass to convert_basic_block
    };

    // check if txc.entry_fn() matches the DefId of the function
    // note: libraries exist and don't have an entry function, handle that case
    if let Some(entry_fn) = tcx.entry_fn(()) {
        if entry_fn.0 == instance.def_id() {
            // see if the name is "main"
            if fn_name == "main" {
                // manually override the signature to match the JVM main method
                signature = oomir::Signature {
                    params: vec![oomir::Type::Array(Box::new(oomir::Type::Class("java/lang/String".to_string())))],
                    ret: Box::new(oomir::Type::Void),
                };
            }
        }
    }


    // Build a CodeBlock from the MIR basic blocks.
    let mut basic_blocks = HashMap::new();
    // MIR guarantees that the start block is BasicBlock 0.
    let entry_label = "bb0".to_string();

    for (bb, bb_data) in mir.basic_blocks_mut().iter_enumerated() {
        let bb_ir = convert_basic_block(bb, bb_data, tcx, &return_oomir_ty, &mut basic_blocks); // Pass return type here
        basic_blocks.insert(bb_ir.label.clone(), bb_ir);
    }

    let codeblock = oomir::CodeBlock {
        basic_blocks,
        entry: entry_label,
    };

    // Return the OOMIR representation of the function.
    oomir::Function {
        name: fn_name,
        signature,
        body: codeblock,
    }
}