// Needed re-organisation of the instructions prior to optimisation, to make it easier

use super::*;

/// Checks if an OOMIR instruction terminates a basic block.
fn is_terminator(instruction: Option<&Instruction>) -> bool {
    match instruction {
        Some(Instruction::Jump { .. })
        | Some(Instruction::Branch { .. })
        | Some(Instruction::Return { .. })
        | Some(Instruction::ThrowNewWithMessage { .. })
        | Some(Instruction::Switch { .. }) => true,
        _ => false,
    }
}

/// Modifies a Function's CodeBlock in place to eliminate `Instruction::Label`
/// within basic blocks by splitting blocks and adding explicit jumps.
pub fn convert_labels_to_basic_blocks_in_function(function: &mut Function) {
    let mut needs_another_pass = true;

    while needs_another_pass {
        needs_another_pass = false;
        let mut new_blocks_this_pass: HashMap<String, BasicBlock> = HashMap::new();
        // Collect keys first to iterate over, allowing mutation of the map's values
        let block_names: HashSet<String> = function.body.basic_blocks.keys().cloned().collect();

        for block_name in block_names.clone() {
            // Re-fetch the block mutably in each iteration of the outer loop,
            // as `new_blocks_this_pass` might have been merged in the previous pass.
            // Check if the block still exists (it might have been replaced if split started at index 0)
            if let Some(block) = function.body.basic_blocks.get_mut(&block_name) {
                let original_instructions = std::mem::take(&mut block.instructions); // Temporarily take ownership
                let mut current_instructions: Vec<Instruction> = Vec::new();
                let mut current_block_label = block.label.clone(); // Label of the block segment we are currently building

                for (_, instruction) in original_instructions.into_iter().enumerate() {
                    match instruction {
                        Instruction::Label {
                            name: next_label_name,
                        } => {
                            // --- Found a label: Finalize the *current* segment ---
                            needs_another_pass = true; // Signal that we changed something

                            // 1. Add jump if the last instruction wasn't a terminator
                            if !is_terminator(current_instructions.last()) {
                                current_instructions.push(Instruction::Jump {
                                    target: next_label_name.clone(),
                                });
                            }

                            // 2. Update the *current* block (or the first segment)
                            // If i == 0, the original block becomes empty and should be removed later?
                            // No, we update the block associated with current_block_label.
                            if block.label == current_block_label {
                                // This is the first segment, update the original block
                                block.instructions = current_instructions;
                            } else {
                                // This is a subsequent segment, create/update in new_blocks
                                let segment_block = BasicBlock {
                                    label: current_block_label.clone(),
                                    instructions: current_instructions,
                                };
                                if let Some(existing) = new_blocks_this_pass
                                    .insert(current_block_label.clone(), segment_block)
                                {
                                    // This case should ideally not happen if labels are unique and splitting logic is correct
                                    eprintln!(
                                        "Warning: Overwriting newly created block segment '{}'",
                                        existing.label
                                    );
                                }
                            }

                            // 3. Prepare for the *next* segment starting with the label
                            current_instructions = Vec::new(); // Start fresh instructions
                            current_block_label = next_label_name.clone(); // The new segment gets the label's name

                            // 4. Check for conflicts *before* adding the new block placeholder
                            if block_names.contains(&current_block_label)
                                || new_blocks_this_pass.contains_key(&current_block_label)
                            {
                                panic!(
                                    "Label conflict: Label '{}' found inside block '{}' already exists as a block name.",
                                    current_block_label, block_name
                                );
                            }
                            // Add a placeholder so future conflicts are detected immediately
                            new_blocks_this_pass.insert(
                                current_block_label.clone(),
                                BasicBlock {
                                    label: current_block_label.clone(),
                                    instructions: vec![],
                                },
                            );

                            // Label instruction is consumed, don't add it to any block's list
                        }
                        _ => {
                            // Regular instruction, add it to the current segment
                            current_instructions.push(instruction);
                        }
                    }
                } // End loop through instructions

                // After iterating through all instructions of the original block:
                // Put the remaining instructions into the correct block
                if block.label == current_block_label {
                    // The last segment belongs to the original block
                    block.instructions = current_instructions;
                } else {
                    // The last segment is a new block (or updates a placeholder)
                    let final_segment_block = BasicBlock {
                        label: current_block_label.clone(),
                        instructions: current_instructions,
                    };
                    new_blocks_this_pass.insert(current_block_label.clone(), final_segment_block);
                }

                // If we split, we break from processing more blocks in this *inner* loop
                // and restart the pass, because the block_names list might be outdated.
                // However, processing all blocks listed in `block_names` initially is safer
                // to avoid issues with modifying the map while iterating indirectly.
                // The `while needs_another_pass` loop handles the reprocessing.
            } // end if let Some(block)
        } // End loop through block_names

        // Merge the newly created blocks from this pass into the main map
        function.body.basic_blocks.extend(new_blocks_this_pass);
    } // End while needs_another_pass
}

pub fn eliminate_duplicate_basic_blocks(func: &mut Function) {
    if func.body.basic_blocks.len() <= 1 {
        // Nothing to deduplicate if there's 0 or 1 block.
        return;
    }

    // 1. Group blocks by their instruction sequences.
    //    Key: The vector of instructions (cloned).
    //    Value: A vector of labels of blocks having this exact instruction sequence.
    let mut instruction_groups: HashMap<Vec<Instruction>, Vec<String>> = HashMap::new();
    for (label, block) in &func.body.basic_blocks {
        // Clone instructions to use as a key. Consider hashing if performance is critical
        // and instruction vectors are very large, but direct comparison is safer.
        instruction_groups
            .entry(block.instructions.clone())
            .or_default()
            .push(label.clone());
    }

    // 2. Identify duplicates and choose canonical blocks.
    //    Build a redirection map: duplicate_label -> canonical_label
    //    Keep track of labels to preserve (non-duplicates or canonical ones).
    let mut redirects: HashMap<String, String> = HashMap::new();
    let mut preserved_labels: HashSet<String> = HashSet::new();

    for labels in instruction_groups.values() {
        if labels.len() > 1 {
            // Found duplicates!
            // Choose the first label as the canonical one (arbitrary but consistent).
            let canonical_label = labels[0].clone();
            preserved_labels.insert(canonical_label.clone());

            // Map all other duplicate labels to the canonical one.
            for duplicate_label in labels.iter().skip(1) {
                redirects.insert(duplicate_label.clone(), canonical_label.clone());
                // Don't add duplicate_label to preserved_labels yet.
            }
        } else {
            // This block is unique, preserve its label.
            preserved_labels.insert(labels[0].clone());
        }
    }

    // If no redirects were created, no duplicates were found.
    if redirects.is_empty() {
        return;
    }

    // 3. Update the function's entry point if it points to a duplicate.
    if let Some(canonical_entry) = redirects.get(&func.body.entry) {
        func.body.entry = canonical_entry.clone();
    }

    // 4. Update jumps, branches, and switches in the *preserved* blocks.
    for label in &preserved_labels {
        // We must get a mutable reference *after* iterating immutably above.
        if let Some(block) = func.body.basic_blocks.get_mut(label) {
            for instruction in &mut block.instructions {
                match instruction {
                    Instruction::Jump { target } => {
                        if let Some(canonical_target) = redirects.get(target) {
                            *target = canonical_target.clone();
                        }
                    }
                    Instruction::Branch {
                        true_block,
                        false_block,
                        ..
                    } => {
                        if let Some(canonical_target) = redirects.get(true_block) {
                            *true_block = canonical_target.clone();
                        }
                        if let Some(canonical_target) = redirects.get(false_block) {
                            *false_block = canonical_target.clone();
                        }
                    }
                    Instruction::Switch {
                        targets, otherwise, ..
                    } => {
                        if let Some(canonical_target) = redirects.get(otherwise) {
                            *otherwise = canonical_target.clone();
                        }
                        for (_, target_label) in targets.iter_mut() {
                            if let Some(canonical_target) = redirects.get(target_label) {
                                *target_label = canonical_target.clone();
                            }
                        }
                    }
                    // Other instructions don't contain block labels as targets.
                    _ => {}
                }
            }
        }
    }

    // 5. Remove the duplicate basic blocks from the function body.
    func.body
        .basic_blocks
        .retain(|label, _| preserved_labels.contains(label));

    // Optional: Add a check or logging for removed blocks
    // println!("Removed {} duplicate blocks.", redirects.len());
}
