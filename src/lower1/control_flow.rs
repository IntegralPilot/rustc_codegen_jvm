use super::{
    operand::convert_operand,
    place::{
        emit_instructions_to_get_on_own, emit_instructions_to_set_value, make_jvm_safe,
        place_to_string,
    },
    types::mir_int_to_oomir_const,
};
use crate::oomir;

use rustc_middle::{
    mir::{
        BasicBlock, BasicBlockData, Body, Operand as MirOperand, Place, StatementKind,
        TerminatorKind,
    },
    ty::TyCtxt,
};
use std::collections::HashMap;

mod checked_ops;
mod rvalue;

/// Convert a single MIR basic block into an OOMIR basic block.
pub fn convert_basic_block<'tcx>(
    bb: BasicBlock,
    bb_data: &BasicBlockData<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    return_oomir_type: &oomir::Type, // Pass function return type
    basic_blocks: &mut HashMap<String, oomir::BasicBlock>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::BasicBlock {
    // Use the basic block index as its label.
    let label = format!("bb{}", bb.index());
    let mut instructions = Vec::new();

    // Convert each MIR statement in the block.
    for stmt in &bb_data.statements {
        match &stmt.kind {
            StatementKind::Assign(box (place, rvalue)) => {
                // 1. Evaluate the Rvalue to get the source operand and temp instructions
                let (rvalue_instructions, source_operand) = rvalue::convert_rvalue_to_operand(
                    // Call the refactored function
                    rvalue, place, // Pass original destination for temp naming hints
                    mir, tcx, data_types,
                );

                // Add instructions needed to calculate the Rvalue
                instructions.extend(rvalue_instructions);

                // 2. Generate instructions to store the computed value into the destination place
                let assignment_instructions = emit_instructions_to_set_value(
                    place,          // The actual destination Place
                    source_operand, // The OOMIR operand holding the value from the Rvalue
                    tcx,
                    mir,
                    data_types,
                );

                // Add the final assignment instructions (Move, SetField, ArrayStore)
                instructions.extend(assignment_instructions);
            }
            StatementKind::StorageLive(_)
            | StatementKind::StorageDead(_)
            | StatementKind::Retag(_, _) => {
                // no-op, currently
            }
            StatementKind::Nop => {
                // Literally a no-op
            }
            StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                println!(
                    "Warning: StatementKind::SetDiscriminant NYI. Place: {:?}, Index: {:?}",
                    place, variant_index
                );
                // TODO: Need logic similar to emit_instructions_to_set_value but for discriminants
            }
            StatementKind::Deinit(place) => {
                println!("Warning: StatementKind::Deinit NYI. Place: {:?}", place);
                // Often a no-op in GC'd languages unless specific resource cleanup needed
            }
            // Handle other StatementKind variants if necessary
            _ => {
                println!("Warning: Unhandled StatementKind: {:?}", stmt.kind);
            }
        }
    }

    // Convert the MIR terminator into corresponding OOMIR instructions.
    if let Some(terminator) = &bb_data.terminator {
        match &terminator.kind {
            TerminatorKind::Return => {
                // Handle Return without operand
                if *return_oomir_type == oomir::Type::Void {
                    instructions.push(oomir::Instruction::Return { operand: None });
                } else {
                    let return_operand = convert_operand(
                        &MirOperand::Move(Place::return_place()),
                        tcx,
                        mir,
                        data_types,
                    );
                    instructions.push(oomir::Instruction::Return {
                        operand: Some(return_operand),
                    });
                }
            }
            TerminatorKind::Goto { target } => {
                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump {
                    target: target_label,
                });
            }
            TerminatorKind::SwitchInt { discr, targets, .. } => {
                // --- GENERAL SwitchInt Handling ---
                let discr_operand = convert_operand(discr, tcx, mir, data_types);
                // Get the actual type of the discriminant from MIR local declarations
                let discr_ty = discr.ty(&mir.local_decls, tcx);

                // Convert the MIR targets into OOMIR (Constant, Label) pairs
                let oomir_targets: Vec<(oomir::Constant, String)> = targets
                    .iter()
                    .map(|(value, target_bb)| {
                        // Convert MIR value (u128) to appropriate OOMIR constant based on discr_ty
                        let oomir_const = mir_int_to_oomir_const(value, discr_ty, tcx); // Use helper
                        // Check if the constant type is suitable for a JVM switch
                        if !oomir_const.is_integer_like() {
                             println!("Warning: SwitchInt target value {:?} for type {:?} cannot be directly used in JVM switch. Block: {}", oomir_const, discr_ty, label);
                             // Decide on fallback: error, skip target, default value?
                             // For now, let's potentially create an invalid switch target for lower2 to handle/error on.
                        }
                        let target_label = format!("bb{}", target_bb.index());
                        (oomir_const, target_label)
                    })
                    .collect();

                // Get the label for the 'otherwise' block
                let otherwise_label = format!("bb{}", targets.otherwise().index());

                // Add the single OOMIR Switch instruction
                instructions.push(oomir::Instruction::Switch {
                    discr: discr_operand,
                    targets: oomir_targets,
                    otherwise: otherwise_label,
                });
                // This Switch instruction terminates the current OOMIR basic block.
            }
            TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                ..
            } => {
                println!("the function name is {:?}", func);
                let function_name = make_jvm_safe(format!("{:?}", func).as_str()); // Get function name - needs refinement to extract actual name
                let oomir_args = args
                    .iter()
                    .map(|arg| convert_operand(&arg.node, tcx, mir, data_types))
                    .collect();
                let dest = Some(format!("{:?}", destination.local));

                instructions.push(oomir::Instruction::Call {
                    dest,
                    function: function_name,
                    args: oomir_args,
                });

                if let Some(target_bb) = target {
                    let target_label = format!("bb{}", target_bb.index());
                    println!(
                        "Info: Adding Jump to {} after Call in bb{}",
                        target_label,
                        bb.index()
                    ); // Add log
                    instructions.push(oomir::Instruction::Jump {
                        target: target_label,
                    });
                } else {
                    // Function diverges (e.g., panic!) - No jump needed.
                    println!(
                        "Info: Call in bb{} has no return target (diverges).",
                        bb.index()
                    );
                }
            }
            TerminatorKind::Assert {
                target,
                cond,
                expected,
                msg,
                unwind: _,
            } => {
                let condition_operand: oomir::Operand;

                // Check if the condition operand is a direct use of a place (Copy or Move)
                let condition_place_opt = match cond {
                    MirOperand::Copy(place) | MirOperand::Move(place) => Some(place),
                    _ => None, // If it's a constant, handle directly
                };

                if let Some(place) = condition_place_opt {
                    // Now, check if this place has a field projection
                    let (temp_dest, instrs, field_oomir_type) =
                        emit_instructions_to_get_on_own(place, tcx, mir, data_types);
                    instructions.extend(instrs);
                    // Use the temporary variable as the condition operand
                    condition_operand = oomir::Operand::Variable {
                        name: temp_dest.clone(),
                        ty: field_oomir_type,
                    };
                } else {
                    println!(
                        // Log constant case
                        "Info: Assert condition uses constant operand {:?}",
                        cond
                    );
                    // Condition is likely a constant itself
                    condition_operand = convert_operand(cond, tcx, mir, data_types);
                }
                // --- End of condition operand handling ---

                // The MIR assert checks `!cond == expected`. Rust asserts check `cond == expected`.
                // Standard Rust `assert!(expr)` lowers to MIR `assert(expr, expected: true, ...)`
                // Standard Rust `assert_eq!(a,b)` might lower differently, but `assert!(a==b)` lowers like above.
                // The `checked_add` MIR uses `assert(!move (_7.1: bool), expected: true, ...)` effectively meaning "panic if _7.1 is true".
                // So, we need to check if `condition_operand == *expected`.

                // Generate a comparison instruction to check if the *actual condition value*
                // matches the expected boolean value.
                let comparison_dest = format!("assert_cmp_{}", bb.index()); // e.g., assert_cmp_3

                // Handle potential negation: MIR `assert(!cond)` means panic if `cond` is true.
                // MIR `assert(cond)` means panic if `cond` is false.
                // The `expected` field tells us what the non-panic value should be.
                // We want to branch to the failure block if `condition_operand != expected`.

                println!(
                    // Log the comparison generation
                    "Info: Generating Assert comparison: '{}' = ({:?}) == {:?}",
                    comparison_dest, condition_operand, *expected
                );

                instructions.push(oomir::Instruction::Eq {
                    dest: comparison_dest.clone(),
                    op1: condition_operand, // Use the potentially GetField'd value
                    op2: oomir::Operand::Constant(oomir::Constant::Boolean(*expected)),
                });

                // Generate a branch based on the comparison result
                let success_block = format!("bb{}", target.index()); // Success path
                let failure_block = format!("assert_fail_{}", bb.index()); // Failure path label

                println!(
                    // Log the branch generation
                    "Info: Generating Assert branch: if '{}' == true goto {} else goto {}",
                    comparison_dest, success_block, failure_block
                );

                instructions.push(oomir::Instruction::Branch {
                    condition: oomir::Operand::Variable {
                        name: comparison_dest, // Use the result of the Eq comparison
                        ty: oomir::Type::Boolean,
                    },
                    true_block: success_block, // Jump here if condition == expected (assertion holds)
                    false_block: failure_block.clone(), // Jump here if assertion fails
                });

                // --- Add the failure block ---
                // Extract the message. msg is an AssertMessage.
                // We need to handle different kinds of AssertMessage.
                let panic_message = match &**msg {
                    rustc_middle::mir::AssertKind::BoundsCheck { len, index } => {
                        // TODO: More sophisticated message generation using len/index operands later
                        format!("BoundsCheck failed (len: {:?}, index: {:?})", len, index)
                    }
                    rustc_middle::mir::AssertKind::Overflow(op, l, r) => {
                        // TODO: Convert l and r operands to strings if possible later
                        format!("Overflow({:?}, {:?}, {:?})", op, l, r)
                    }
                    rustc_middle::mir::AssertKind::OverflowNeg(op) => {
                        format!("OverflowNeg({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::DivisionByZero(op) => {
                        format!("DivisionByZero({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::RemainderByZero(op) => {
                        format!("RemainderByZero({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::ResumedAfterReturn(_) => {
                        "ResumedAfterReturn".to_string()
                    }
                    rustc_middle::mir::AssertKind::ResumedAfterPanic(_) => {
                        "ResumedAfterPanic".to_string()
                    }
                    rustc_middle::mir::AssertKind::MisalignedPointerDereference {
                        required,
                        found,
                    } => {
                        format!(
                            "MisalignedPointerDereference (required: {:?}, found: {:?})",
                            required, found
                        )
                    }
                    rustc_middle::mir::AssertKind::NullPointerDereference => {
                        "NullPointerDereference".to_string()
                    }
                };

                let fail_instructions = vec![oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/RuntimeException".to_string(), // Or ArithmeticException for overflows?
                    message: panic_message,
                }];
                println!(
                    // Log the failure block creation
                    "Info: Creating failure block '{}'",
                    failure_block
                );
                basic_blocks.insert(
                    // Ensure 'basic_blocks' map is mutable and passed in
                    failure_block.clone(),
                    oomir::BasicBlock {
                        label: failure_block,
                        instructions: fail_instructions,
                    },
                );
            }
            TerminatorKind::Drop {
                place: dropped_place,
                target,
                unwind: _,
                replace: _,
            } => {
                // In simple cases (no custom Drop trait), a MIR drop often just signifies
                // the end of a scope before control flow continues.
                // We need to emit the jump to the target block.
                // We ignore unwind paths for now.
                // We also don't emit an explicit OOMIR 'drop' instruction yet,
                // as standard GC handles memory. If explicit resource cleanup (like file.close())
                // were needed, this would require much more complex handling (e.g., try-finally).

                println!(
                    "Info: Handling Drop terminator for place {:?}. Jumping to target bb{}.",
                    place_to_string(dropped_place, tcx), // Log which variable is conceptually dropped
                    target.index()
                );

                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump {
                    target: target_label,
                });
            }
            // Other terminator kinds (like Resume, etc.) can be added as needed.
            _ => {
                println!("Warning: Unhandled terminator {:?}", terminator.kind);
            }
        }
    }

    oomir::BasicBlock {
        label,
        instructions,
    }
}
