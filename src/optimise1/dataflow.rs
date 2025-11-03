use super::*;

// --- Dataflow Analysis for Constant Propagation ---

// Meet operator for constant propagation:
// Merges information from multiple predecessors.
// A variable is constant only if it has the same constant value coming from all paths.
fn meet_constants(map1: &ConstantMap, map2: &ConstantMap) -> ConstantMap {
    let mut result = ConstantMap::new();
    // Consider variables present in map1
    for (var, const1) in map1 {
        match map2.get(var) {
            Some(const2) if const1 == const2 => {
                // Same constant value on both paths
                result.insert(var.clone(), const1.clone());
            }
            _ => {
                // Different constants or only constant on one path -> not constant after merge
            }
        }
    }
    // Variables only in map2 were already handled implicitly (won't be in result)
    result
}

pub fn analyze_constant_propagation(
    entry_label: &String,
    cfg: &HashMap<String, BasicBlockInfo>,
    // Potentially add function signature here if needed for argument constants
) -> DataflowResult {
    let mut block_in_state: DataflowResult =
        cfg.keys().map(|l| (l.clone(), HashMap::new())).collect();
    let mut worklist: VecDeque<String> = VecDeque::new();

    if cfg.contains_key(entry_label) {
        // Initialize: Start with the entry block
        // Assume function arguments are initially non-constant unless specific info is passed
        worklist.push_back(entry_label.clone());
    }

    while let Some(current_label) = worklist.pop_front() {
        let current_info = match cfg.get(&current_label) {
            Some(info) => info,
            None => continue, // Should not happen if CFG is consistent
        };

        // Get the merged input state for this block
        let in_state = if current_info.predecessors.is_empty() {
            // Entry block or unreachable block - starts empty (or with args if provided)
            ConstantMap::new()
        } else {
            let mut first = true;
            let mut merged_state = ConstantMap::new(); // Placeholder

            for pred_label in &current_info.predecessors {
                // Calculate the output state of the predecessor
                // For simplicity here, we'll approximate using the input state
                // of the current block before this iteration's merge.
                // A more precise approach calculates predecessor's output state explicitly.
                let pred_output_state = calculate_block_output_state(
                    cfg.get(pred_label).expect("Predecessor must exist in CFG"),
                    block_in_state
                        .get(pred_label)
                        .expect("Predecessor state must exist"),
                );

                if first {
                    merged_state = pred_output_state; // Initialize with the first predecessor's output
                    first = false;
                } else {
                    merged_state = meet_constants(&merged_state, &pred_output_state);
                }
            }
            merged_state // This is the state at the entry of 'current_label'
        };

        // Calculate the output state by executing the block with the input state
        let out_state = calculate_block_output_state(current_info, &in_state);

        // Propagate changes to successors
        for successor_label in &current_info.successors {
            let successor_current_in = block_in_state
                .get(successor_label)
                .expect("Successor state must exist")
                .clone(); // Get the state before potential update

            // Merge the calculated 'out_state' of the current block into the successor's input state
            // This is slightly different from calculating the input state above. Here we merge
            // the newly calculated output into the existing input of the successor.
            let mut potential_new_in = ConstantMap::new(); // Placeholder for successor's potential new IN state
            let successor_info = cfg.get(successor_label).expect("Successor must exist"); // Needed to check predecessors

            if successor_info.predecessors.len() == 1 {
                potential_new_in = out_state.clone(); // Only one pred, its output becomes successor's input
            } else {
                // Re-calculate the meet based on all predecessors' current outputs
                // (including the one we just processed)
                let mut first = true;
                for pred_label in &successor_info.predecessors {
                    let pred_out = calculate_block_output_state(
                        cfg.get(pred_label).expect("Pred must exist"),
                        block_in_state
                            .get(pred_label)
                            .expect("Pred state must exist"),
                    );
                    if first {
                        potential_new_in = pred_out;
                        first = false;
                    } else {
                        potential_new_in = meet_constants(&potential_new_in, &pred_out);
                    }
                }
            }

            // If the successor's input state changed, update it and add to worklist
            if potential_new_in != successor_current_in {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "optimisation",
                    format!(
                        "State change for {:?}: {:?} -> {:?}",
                        successor_label, successor_current_in, potential_new_in
                    )
                );
                block_in_state.insert(successor_label.clone(), potential_new_in);
                if !worklist.contains(&successor_label) {
                    worklist.push_back(successor_label.clone());
                }
            }
        }
    }

    // The final `block_in_state` contains the converged constant info at the entry of each block
    block_in_state
}

// Helper to simulate block execution for dataflow analysis OR transformation
// When used for analysis, it just calculates the output state.
// When used for transformation, it produces new instructions.
pub fn process_block_instructions(
    info: &BasicBlockInfo,
    initial_state: &ConstantMap,
    transform: bool, // If true, generate new instructions; otherwise, just calculate state
    _data_types: &HashMap<String, DataType>, // needed later if we try to handle more advanced optimisation
) -> (ConstantMap, Vec<Instruction>) {
    // Returns final state and (if transform=true) new instructions

    let mut current_state = initial_state.clone();
    let mut new_instructions = Vec::with_capacity(info.original_block.instructions.len());

    let lookup_const = |op: &Operand, state: &ConstantMap| -> Option<Constant> {
        match op {
            Operand::Constant(c) => Some(c.clone()),
            Operand::Variable { name, .. } => state.get(name).cloned(),
        }
    };

    for instruction in &info.original_block.instructions {
        // --- Constant Folding & Propagation Logic ---

        let mut keep_original_instruction = true;
        let mut pre_extra_instructions: Vec<Instruction> = Vec::new();
        let mut optimised_instruction = instruction.clone(); // Start with original

        match instruction {
            // --- Arithmetic, Comparison, Bitwise Operations ---
            Instruction::Add { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                // Default optimized instruction uses propagated constants
                optimised_instruction = Instruction::Add {
                    dest: dest.clone(),
                    op1: new_op1.clone(),
                    op2: new_op2.clone(),
                };

                let mut simplified_algebraically = false;

                // --- Check Algebraic Simplifications ---
                if is_zero(&new_op2) {
                    // a + 0 = a
                    optimised_instruction = Instruction::Move {
                        dest: dest.clone(),
                        src: new_op1.clone(),
                    };
                    update_state_based_on_operand(
                        &mut current_state,
                        dest,
                        &new_op1,
                        initial_state,
                    );
                    simplified_algebraically = true;
                } else if is_zero(&new_op1) {
                    // 0 + a = a
                    optimised_instruction = Instruction::Move {
                        dest: dest.clone(),
                        src: new_op2.clone(),
                    };
                    update_state_based_on_operand(
                        &mut current_state,
                        dest,
                        &new_op2,
                        initial_state,
                    );
                    simplified_algebraically = true;
                }

                // --- Constant Folding (only if not already simplified) ---
                if !simplified_algebraically && let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::add_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const.clone());
                        // If transforming, replace Add with Move constant
                        if transform {
                            optimised_instruction = Instruction::Move {
                                dest: dest.clone(),
                                src: Operand::Constant(result_const),
                            };
                        } else {
                            keep_original_instruction = false;
                        }
                    } else {
                        // Calculation failed -> result not constant
                        current_state.remove(dest);
                    }
                } else if !simplified_algebraically {
                    // Operands not both constant, and no simplification -> result is not constant
                    current_state.remove(dest);
                }
                // If simplified_algebraically, state already updated, instruction is Move
            }

            Instruction::Sub { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Sub {
                    dest: dest.clone(),
                    op1: new_op1.clone(),
                    op2: new_op2.clone(),
                };

                let mut simplified_algebraically = false;

                // --- Check Algebraic Simplifications ---
                if is_zero(&new_op2) {
                    // a - 0 = a
                    optimised_instruction = Instruction::Move {
                        dest: dest.clone(),
                        src: new_op1.clone(),
                    };
                    update_state_based_on_operand(
                        &mut current_state,
                        dest,
                        &new_op1,
                        initial_state,
                    );
                    simplified_algebraically = true;
                } else if new_op1 == new_op2 {
                    // a - a = 0 (Check operand equality - careful with float)
                    if let Some(zero_const) = Constant::zero_for_operand(&new_op1) {
                        // Get zero of the correct type
                        optimised_instruction = Instruction::Move {
                            dest: dest.clone(),
                            src: Operand::Constant(zero_const.clone()),
                        };
                        current_state.insert(dest.clone(), zero_const);
                        simplified_algebraically = true;
                    }
                } else if is_zero(&new_op1) {
                    // 0 - a = -a
                    // If 'a' is constant, fold the negation
                    if let Some(c2) = const2.clone() {
                        if let Some(neg_const) = interpret::neg_constant(c2) {
                            optimised_instruction = Instruction::Move {
                                dest: dest.clone(),
                                src: Operand::Constant(neg_const.clone()),
                            };
                            current_state.insert(dest.clone(), neg_const);
                            simplified_algebraically = true;
                        } else {
                            // Negation failed (e.g. overflow MIN_INT)
                            current_state.remove(dest);
                            optimised_instruction = Instruction::Neg {
                                dest: dest.clone(),
                                src: new_op2.clone(),
                            }; // Keep as Neg instruction
                            simplified_algebraically = true; // Still simplified from Sub
                        }
                    } else {
                        // 'a' is not constant, replace with Neg instruction
                        optimised_instruction = Instruction::Neg {
                            dest: dest.clone(),
                            src: new_op2.clone(),
                        };
                        current_state.remove(dest); // Result is not constant
                        simplified_algebraically = true;
                    }
                }

                // --- Constant Folding (only if not already simplified) ---
                if !simplified_algebraically && let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::subtract_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const.clone());
                        if transform {
                            optimised_instruction = Instruction::Move {
                                dest: dest.clone(),
                                src: Operand::Constant(result_const),
                            };
                        } else {
                            keep_original_instruction = false;
                        }
                    } else {
                        current_state.remove(dest);
                    }
                } else if !simplified_algebraically {
                    current_state.remove(dest);
                }
            }

            Instruction::Mul { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Mul {
                    dest: dest.clone(),
                    op1: new_op1.clone(),
                    op2: new_op2.clone(),
                };

                let mut simplified_algebraically = false;

                // --- Check Algebraic Simplifications ---
                if is_zero(&new_op1) || is_zero(&new_op2) {
                    // a * 0 = 0 or 0 * a = 0
                    if let Some(zero_const) = Constant::zero_for_operand(&new_op1) {
                        optimised_instruction = Instruction::Move {
                            dest: dest.clone(),
                            src: Operand::Constant(zero_const.clone()),
                        };
                        current_state.insert(dest.clone(), zero_const);
                        simplified_algebraically = true;
                    }
                } else if is_one(&new_op2) {
                    // a * 1 = a
                    optimised_instruction = Instruction::Move {
                        dest: dest.clone(),
                        src: new_op1.clone(),
                    };
                    update_state_based_on_operand(
                        &mut current_state,
                        dest,
                        &new_op1,
                        initial_state,
                    );
                    simplified_algebraically = true;
                } else if is_one(&new_op1) {
                    // 1 * a = a
                    optimised_instruction = Instruction::Move {
                        dest: dest.clone(),
                        src: new_op2.clone(),
                    };
                    update_state_based_on_operand(
                        &mut current_state,
                        dest,
                        &new_op2,
                        initial_state,
                    );
                    simplified_algebraically = true;
                }

                // --- Constant Folding ---
                if !simplified_algebraically && let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::multiply_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const.clone());
                        if transform {
                            optimised_instruction = Instruction::Move {
                                dest: dest.clone(),
                                src: Operand::Constant(result_const),
                            };
                        } else {
                            keep_original_instruction = false;
                        }
                    } else {
                        current_state.remove(dest);
                    }
                } else if !simplified_algebraically {
                    current_state.remove(dest);
                }
            }

            Instruction::Div { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Div {
                    dest: dest.clone(),
                    op1: new_op1.clone(),
                    op2: new_op2.clone(),
                };

                let mut simplified_algebraically = false;

                // --- Check Algebraic Simplifications ---
                // Check for division by zero FIRST if operand 2 is constant zero
                if is_zero(&new_op2) {
                    // Division by zero!
                    // For optimization, we let runtime handle it. Rust should never allow a div by zero to get into MIR anyway.
                    // We'll just mark dest as non-constant.
                    current_state.remove(dest);
                } else if is_zero(&new_op1) {
                    // 0 / a = 0 (where a != 0)
                    if let Some(zero_const) = Constant::zero_for_operand(&new_op1) {
                        optimised_instruction = Instruction::Move {
                            dest: dest.clone(),
                            src: Operand::Constant(zero_const.clone()),
                        };
                        current_state.insert(dest.clone(), zero_const);
                        simplified_algebraically = true;
                    }
                } else if is_one(&new_op2) {
                    // a / 1 = a
                    optimised_instruction = Instruction::Move {
                        dest: dest.clone(),
                        src: new_op1.clone(),
                    };
                    update_state_based_on_operand(
                        &mut current_state,
                        dest,
                        &new_op1,
                        initial_state,
                    );
                    simplified_algebraically = true;
                } else if new_op1 == new_op2 {
                    // a / a = 1 (where a != 0)
                    // Again, careful with float, identity vs value. Assume ok for now.
                    if let Some(one_const) = Constant::one_for_operand(&new_op1) {
                        // Get one of the correct type
                        optimised_instruction = Instruction::Move {
                            dest: dest.clone(),
                            src: Operand::Constant(one_const.clone()),
                        };
                        current_state.insert(dest.clone(), one_const);
                        simplified_algebraically = true;
                    }
                }

                // --- Constant Folding ---
                if !simplified_algebraically && let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::divide_constants(c1, c2) {
                        // Assumes divide_constants handles division by zero internally
                        current_state.insert(dest.clone(), result_const.clone());
                        if transform {
                            optimised_instruction = Instruction::Move {
                                dest: dest.clone(),
                                src: Operand::Constant(result_const),
                            };
                        } else {
                            keep_original_instruction = false;
                        }
                    } else {
                        current_state.remove(dest); // Division failed (e.g., div by zero, overflow)
                    }
                } else if !simplified_algebraically {
                    current_state.remove(dest);
                }
            }

            Instruction::Rem { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Rem {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::rem_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::BitAnd { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::BitAnd {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::bit_and_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::BitOr { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::BitOr {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::bit_or_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::BitXor { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::BitXor {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::bit_xor_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Shl { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Shl {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::shl_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Shr { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Shr {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::shr_constants(c1, c2) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Eq { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Eq {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::eq_constants(c1, c2) {
                        current_state.insert(dest.clone(), Constant::Boolean(result_const));
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Ne { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Ne {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::eq_constants(c1, c2) {
                        current_state.insert(dest.clone(), Constant::Boolean(!result_const));
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Lt { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Lt {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::lt_constants(c1, c2) {
                        current_state.insert(dest.clone(), Constant::Boolean(result_const));
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Le { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Le {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::le_constants(c1, c2) {
                        current_state.insert(dest.clone(), Constant::Boolean(result_const));
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Gt { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Gt {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::gt_constants(c1, c2) {
                        current_state.insert(dest.clone(), Constant::Boolean(result_const));
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Ge { dest, op1, op2 } => {
                let const1 = lookup_const(op1, &current_state);
                let const2 = lookup_const(op2, &current_state);
                let new_op1 = const1.clone().map_or(op1.clone(), Operand::Constant);
                let new_op2 = const2.clone().map_or(op2.clone(), Operand::Constant);

                optimised_instruction = Instruction::Ge {
                    dest: dest.clone(),
                    op1: new_op1,
                    op2: new_op2,
                };

                if let (Some(c1), Some(c2)) = (const1, const2) {
                    if let Some(result_const) = interpret::ge_constants(c1, c2) {
                        current_state.insert(dest.clone(), Constant::Boolean(result_const));
                        keep_original_instruction = false; // Folded away entirely
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            // --- Move ---
            Instruction::Move { dest, src } => {
                let const_src = lookup_const(src, &current_state);
                let optimised_src_operand =
                    const_src.clone().map_or(src.clone(), Operand::Constant);
                optimised_instruction = Instruction::Move {
                    dest: dest.clone(),
                    src: optimised_src_operand,
                };

                if let Some(c) = const_src {
                    current_state.insert(dest.clone(), c);
                } else {
                    current_state.remove(dest);
                }
            }

            // --- Branch ---
            Instruction::Branch {
                condition,
                true_block,
                false_block,
            } => {
                let const_cond = lookup_const(condition, &current_state);
                optimised_instruction = Instruction::Branch {
                    condition: const_cond
                        .clone()
                        .map_or(condition.clone(), Operand::Constant),
                    true_block: true_block.clone(),
                    false_block: false_block.clone(),
                };

                if transform {
                    // Only simplify branches during the final transform phase
                    if let Some(Constant::Boolean(cond_val)) = const_cond {
                        let target = if cond_val { true_block } else { false_block };
                        optimised_instruction = Instruction::Jump {
                            target: target.clone(),
                        };
                        // keep_original_instruction = true; // Keep the new Jump instruction
                    }
                    // Else keep the (potentially optimised operand) Branch
                }
                // State doesn't change based on branch itself
            }

            // --- Switch ---
            Instruction::Switch {
                discr,
                targets,
                otherwise,
            } => {
                let const_discr = lookup_const(discr, &current_state);
                // Start with the potentially operand-optimized Switch as the default
                optimised_instruction = Instruction::Switch {
                    discr: const_discr.clone().map_or(discr.clone(), Operand::Constant),
                    targets: targets.clone(),
                    otherwise: otherwise.clone(),
                };
                // We want to keep the resulting instruction unless explicitly folded
                keep_original_instruction = true;

                if transform {
                    if let Some(discr_val) = const_discr {
                        // Try to find a matching target
                        if let Some(target) =
                            interpret::switch_constants(discr_val, targets.clone())
                        {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "optimisation",
                                format!("Switch matched target: {:?}", target)
                            );
                            // Match found: replace Switch with Jump to the specific target
                            optimised_instruction = Instruction::Jump { target };
                            // keep_original_instruction should remain true (we keep the new Jump)
                        } else {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "optimisation",
                                format!(
                                    "Switch did not match any target, using otherwise: {:?}",
                                    otherwise
                                )
                            );
                            // No explicit target matched: replace Switch with Jump to the otherwise target
                            optimised_instruction = Instruction::Jump {
                                target: otherwise.clone(),
                            };
                        }
                    }
                }
            }

            // --- Instructions that Kill Constants ---
            Instruction::Call {
                dest,
                args,
                function,
                ..
            } => {
                // Added 'function' for logging
                // Propagate constants into arguments
                let new_args: Vec<Operand> = args
                    .iter()
                    .map(|arg| {
                        lookup_const(arg, &current_state).map_or(arg.clone(), Operand::Constant)
                    })
                    .collect();

                // Determine if any argument could potentially have side effects
                // In our case, passing an Array(_) type signals potential mutation.
                // We should also consider Class(_) types for future object mutations.
                let mut has_potential_side_effect_arg = false;
                for arg_operand in &new_args {
                    // Check the arguments *after* constant prop
                    let arg_type = match arg_operand {
                        Operand::Variable { ty, .. } => Some(ty.clone()),
                        Operand::Constant(c) => Some(Type::from_constant(c)), // Get type from constant
                    };

                    if let Some(ty) = arg_type {
                        // Assume Arrays and Classes passed by reference can be mutated
                        if matches!(ty, Type::Array(_) | Type::Class(_)) {
                            has_potential_side_effect_arg = true;
                            break;
                        }
                    }
                }

                // 1. Invalidate the destination variable
                if let Some(d) = dest {
                    current_state.remove(d);
                }

                // 2. CONSERVATIVE INVALIDATION due to potential side effects:
                if has_potential_side_effect_arg {
                    // Keep track of keys to remove to avoid borrowing issues while iterating
                    let keys_to_remove: Vec<String> = current_state.iter()
                        .filter_map(|(key, constant_val)| {
                            // Decide which types are considered "primitive" and immutable
                            // and won't be affected by side effects through references.
                            match constant_val {
                                Constant::I8(_) | Constant::I16(_) | Constant::I32(_) | Constant::I64(_) |
                                Constant::F32(_) | Constant::F64(_) | Constant::Boolean(_) | Constant::Char(_) |
                                Constant::String(_) => None, // Keep truly immutable primitives/values
                                _ => {
                                     breadcrumbs::log!(breadcrumbs::LogLevel::Info, "optimisation", format!("Optimizer: Invalidating potentially mutable constant '{}' due to call to '{}' with array/class arg.", key, function)); // Debugging
                                     Some(key.clone()) // Remove others (Array, Instance, Class etc.)
                                }
                            }
                        })
                        .collect();

                    for key in keys_to_remove {
                        current_state.remove(&key);
                    }
                }

                // Keep the call instruction, but with potentially updated arguments
                optimised_instruction = Instruction::Call {
                    dest: dest.clone(),
                    function: function.clone(), // Use the captured function name
                    args: new_args,
                };
                keep_original_instruction = true; // Always keep the call itself
            }
            Instruction::ArrayStore {
                array,
                index,
                value,
            } => {
                let new_index =
                    lookup_const(index, &current_state).map_or(index.clone(), Operand::Constant);
                let new_value =
                    lookup_const(value, &current_state).map_or(value.clone(), Operand::Constant);

                if current_state.contains_key(array) {
                    pre_extra_instructions.push(Instruction::Move {
                        dest: array.clone(),
                        src: Operand::Constant(current_state.get(array).unwrap().clone()),
                    });
                    current_state.remove(array);
                }

                optimised_instruction = Instruction::ArrayStore {
                    array: array.clone(),
                    index: new_index,
                    value: new_value,
                };
                keep_original_instruction = true;
            }
            Instruction::SetField { object, value, .. } => {
                let new_value =
                    lookup_const(value, &current_state).map_or(value.clone(), Operand::Constant);
                // Treat field stores as killing constancy of the object variable.

                if current_state.contains_key(object) {
                    pre_extra_instructions.push(Instruction::Move {
                        dest: object.clone(),
                        src: Operand::Constant(current_state.get(object).unwrap().clone()),
                    });
                    current_state.remove(object);
                }

                optimised_instruction = Instruction::SetField {
                    object: object.clone(),
                    field_name: instruction.get_set_field_name().unwrap().clone(),
                    value: new_value,
                    field_ty: instruction.get_set_field_ty().unwrap().clone(),
                    owner_class: instruction.get_set_field_owner().unwrap().clone(),
                }; // Adjust
                keep_original_instruction = true;
            }
            // --- Instructions that Read Constants ---
            Instruction::GetField {
                dest,
                object,
                field_name,
                ..
            } => {
                let const_obj = lookup_const(object, &current_state);
                let new_obj_op = const_obj.clone().map_or(object.clone(), Operand::Constant);
                optimised_instruction = Instruction::GetField {
                    dest: dest.clone(),
                    object: new_obj_op,
                    field_name: field_name.clone(),
                    field_ty: instruction.get_get_field_ty().unwrap().clone(),
                    owner_class: instruction.get_get_field_owner().unwrap().clone(),
                }; // Adjust

                if let Some(c_obj) = const_obj {
                    if let Some(field_const) =
                        interpret::get_field_constant(c_obj, field_name.clone())
                    {
                        current_state.insert(dest.clone(), field_const);
                        keep_original_instruction = false; // Folded away
                    // Emit Move if needed during transform phase
                    } else {
                        current_state.remove(dest); // Field access failed or object not right type
                    }
                } else {
                    current_state.remove(dest); // Object not constant
                }
            }

            Instruction::ArrayGet { dest, array, index } => {
                let const_array = lookup_const(array, &current_state);
                let new_array_op = const_array.clone().map_or(array.clone(), Operand::Constant);
                let new_index_op =
                    lookup_const(index, &current_state).map_or(index.clone(), Operand::Constant);
                optimised_instruction = Instruction::ArrayGet {
                    dest: dest.clone(),
                    array: new_array_op,
                    index: new_index_op,
                };

                if let Some(c_array) = const_array {
                    // get the inner Vec of constants
                    if let Some(index_const) = lookup_const(index, &current_state) {
                        match c_array {
                            Constant::Array(_, c_array) => {
                                let i = match extract_number_from_operand(Operand::Constant(
                                    index_const,
                                )) {
                                    Some(i) => i as usize,
                                    None => {
                                        // Index not constant
                                        current_state.remove(dest);
                                        continue;
                                    }
                                };
                                if let Some(c) = c_array.get(i) {
                                    current_state.insert(dest.clone(), c.clone());
                                    keep_original_instruction = false; // Folded away
                                // Emit Move if needed during transform phase
                                } else {
                                    current_state.remove(dest); // Index out of bounds
                                }
                            }
                            _ => {
                                // Not an array type
                                // This shouldn't ever happen, indicates lower1 codegen error
                                panic!("Expected array constant, got {:?}", c_array);
                            }
                        }
                    } else {
                        current_state.remove(dest); // Index not constant
                    }
                } else {
                    current_state.remove(dest); // Array not constant
                }
            }

            Instruction::Length { dest, array } => {
                let const_array = lookup_const(array, &current_state);
                let new_array_op = const_array.clone().map_or(array.clone(), Operand::Constant);
                optimised_instruction = Instruction::Length {
                    dest: dest.clone(),
                    array: new_array_op,
                };

                if let Some(c_array) = const_array {
                    if let Some(length_const) = interpret::length_constant(c_array) {
                        current_state.insert(dest.clone(), length_const);
                        keep_original_instruction = false; // Folded away
                    } else {
                        current_state.remove(dest); // Length access failed or array not right type
                    }
                } else {
                    current_state.remove(dest); // Array not constant
                }
            }

            Instruction::Not { dest, src } => {
                let const_src = lookup_const(src, &current_state);
                let new_src_op = const_src.clone().map_or(src.clone(), Operand::Constant);
                optimised_instruction = Instruction::Not {
                    dest: dest.clone(),
                    src: new_src_op,
                };

                if let Some(c) = const_src {
                    if let Some(result_const) = interpret::not_constant(c) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::Neg { dest, src } => {
                let const_src = lookup_const(src, &current_state);
                let new_src_op = const_src.clone().map_or(src.clone(), Operand::Constant);
                optimised_instruction = Instruction::Neg {
                    dest: dest.clone(),
                    src: new_src_op,
                };

                if let Some(c) = const_src {
                    if let Some(result_const) = interpret::neg_constant(c) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::NewArray {
                dest,
                element_type,
                size,
            } => {
                let const_size = lookup_const(size, &current_state);
                let new_size_op = const_size.map_or(size.clone(), Operand::Constant); // Propagate constant size if available

                optimised_instruction = Instruction::NewArray {
                    dest: dest.clone(),
                    element_type: element_type.clone(),
                    size: new_size_op,
                };

                // Do NOT attempt to fold NewArray into a Constant::Array here.
                // This hides the operation and makes side-effect tracking harder.
                current_state.remove(dest);

                keep_original_instruction = true;
            }

            Instruction::ConstructObject { dest, class_name } => {
                // for now, we can make it constant using constant::Instance
                current_state.insert(
                    dest.clone(),
                    Constant::Instance {
                        class_name: class_name.clone(),
                        fields: HashMap::new(),
                        params: vec![],
                    },
                );
                keep_original_instruction = false; // Folded away
            }

            Instruction::InvokeVirtual {
                dest,
                class_name,
                method_name,
                method_ty,
                args,
                operand,
            } => {
                // can't do any pre-eval or folding - yet. will add this later
                // for now if args (Vec<Operand>) or Operand contain a variable that we've made constant, we need to update it to use the constant instead
                // cause the variable doesn't exisit anymore

                let new_args: Vec<Operand> = args
                    .iter()
                    .map(|arg| {
                        lookup_const(arg, &current_state).map_or(arg.clone(), Operand::Constant)
                    })
                    .collect();
                let new_operand = lookup_const(operand, &current_state)
                    .map_or(operand.clone(), Operand::Constant);
                optimised_instruction = Instruction::InvokeVirtual {
                    dest: dest.clone(),
                    class_name: class_name.clone(),
                    method_name: method_name.clone(),
                    method_ty: method_ty.clone(),
                    args: new_args,
                    operand: new_operand,
                };
                keep_original_instruction = true;
            }

            Instruction::InvokeStatic {
                dest,
                class_name,
                method_name,
                method_ty,
                args,
            } => {
                // Propagate constants in arguments
                let new_args: Vec<Operand> = args
                    .iter()
                    .map(|arg| {
                        lookup_const(arg, &current_state).map_or(arg.clone(), Operand::Constant)
                    })
                    .collect();
                optimised_instruction = Instruction::InvokeStatic {
                    dest: dest.clone(),
                    class_name: class_name.clone(),
                    method_name: method_name.clone(),
                    method_ty: method_ty.clone(),
                    args: new_args,
                };
                keep_original_instruction = true;
            }

            Instruction::InvokeInterface {
                class_name,
                method_name,
                method_ty,
                args,
                dest,
                operand,
            } => {
                // can't do any pre-eval or folding - yet. will add this later
                // for now if args (Vec<Operand>) or Operand contain a variable that we've made constant, we need to update it to use the constant instead
                // cause the variable doesn't exisit anymore

                let new_args: Vec<Operand> = args
                    .iter()
                    .map(|arg| {
                        lookup_const(arg, &current_state).map_or(arg.clone(), Operand::Constant)
                    })
                    .collect();
                let new_operand = lookup_const(operand, &current_state);
                optimised_instruction = Instruction::InvokeInterface {
                    class_name: class_name.clone(),
                    method_name: method_name.clone(),
                    method_ty: method_ty.clone(),
                    args: new_args,
                    dest: dest.clone(),
                    operand: new_operand.map_or(operand.clone(), Operand::Constant),
                };
                keep_original_instruction = true;
            }

            Instruction::Jump { .. } => {
                keep_original_instruction = true;
            }

            Instruction::Return { operand } => {
                if let Some(op) = operand {
                    // if the operand is a variable we made constant, we need to update it to use the constant instead
                    let new_operand: Operand = lookup_const(op, &current_state)
                        .map_or(op.clone(), |c| Operand::Constant(c));
                    optimised_instruction = Instruction::Return {
                        operand: Some(new_operand),
                    };
                } else {
                    optimised_instruction = Instruction::Return { operand: None };
                }
            }

            Instruction::Cast { op, ty, dest } => {
                let const_op = lookup_const(op, &current_state);
                let new_op = const_op.clone().map_or(op.clone(), Operand::Constant);
                optimised_instruction = Instruction::Cast {
                    op: new_op,
                    ty: ty.clone(),
                    dest: dest.clone(),
                };

                if let Some(c) = const_op {
                    if let Some(result_const) = interpret::cast_constant(c, ty.clone()) {
                        current_state.insert(dest.clone(), result_const);
                        keep_original_instruction = false; // Folded away
                    } else {
                        current_state.remove(dest);
                    }
                } else {
                    current_state.remove(dest);
                }
            }

            Instruction::ThrowNewWithMessage { .. } | Instruction::Label { .. } => {
                // nothing to do
                keep_original_instruction = true;
            }
        }

        if transform && keep_original_instruction {
            new_instructions.extend(pre_extra_instructions);
            new_instructions.push(optimised_instruction);
        }
    }

    (current_state, new_instructions)
}

// Helper specifically for the analysis phase to get output state
fn calculate_block_output_state(info: &BasicBlockInfo, in_state: &ConstantMap) -> ConstantMap {
    // For analysis, we only need the state, not the instructions.
    // Pass dummy data_types if not needed for state calculation itself.
    let dummy_data_types = HashMap::new();
    let (out_state, _) = process_block_instructions(info, in_state, false, &dummy_data_types);
    out_state
}

impl Instruction {
    fn get_set_field_name(&self) -> Option<&String> {
        if let Instruction::SetField { field_name, .. } = self {
            Some(field_name)
        } else {
            None
        }
    }
    fn get_set_field_ty(&self) -> Option<&Type> {
        if let Instruction::SetField { field_ty, .. } = self {
            Some(field_ty)
        } else {
            None
        }
    }
    fn get_set_field_owner(&self) -> Option<&String> {
        if let Instruction::SetField { owner_class, .. } = self {
            Some(owner_class)
        } else {
            None
        }
    }
    fn get_get_field_ty(&self) -> Option<&Type> {
        if let Instruction::GetField { field_ty, .. } = self {
            Some(field_ty)
        } else {
            None
        }
    }
    fn get_get_field_owner(&self) -> Option<&String> {
        if let Instruction::GetField { owner_class, .. } = self {
            Some(owner_class)
        } else {
            None
        }
    }
}

fn is_zero(op: &Operand) -> bool {
    matches!(op, Operand::Constant(c) if c.is_zero())
}

fn is_one(op: &Operand) -> bool {
    matches!(op, Operand::Constant(c) if c.is_one())
}

// Helper to update state when an instruction simplifies to assigning one operand to the destination
// E.g., a + 0 = a. The state of 'dest' should become the state of 'a'.
fn update_state_based_on_operand(
    state: &mut ConstantMap,
    dest: &String,
    src_op: &Operand,
    initial_state: &ConstantMap, // Pass the state *before* this instruction
) {
    match src_op {
        Operand::Constant(c) => {
            state.insert(dest.clone(), c.clone());
        }
        Operand::Variable { name, .. } => {
            // If the source variable was constant, propagate its value
            if let Some(const_val) = initial_state.get(name) {
                state.insert(dest.clone(), const_val.clone());
            } else {
                // Source variable is not constant, so dest isn't either
                state.remove(dest);
            }
        }
    }
}
