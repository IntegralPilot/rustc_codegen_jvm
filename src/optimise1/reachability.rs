use super::*;

// --- Reachability Analysis for DCE ---

// Helper to get potential successors from a single instruction
pub fn get_instruction_successors(instruction: &Instruction) -> Vec<String> {
    match instruction {
        Instruction::Jump { target } => vec![target.clone()],
        Instruction::Branch {
            true_block,
            false_block,
            ..
        } => vec![true_block.clone(), false_block.clone()],
        Instruction::Switch {
            targets, otherwise, ..
        } => {
            let mut succs: Vec<String> = targets.iter().map(|(_, target)| target.clone()).collect();
            succs.push(otherwise.clone());
            succs
        }
        Instruction::Return { .. } => vec![], // No successors within the function
        Instruction::ThrowNewWithMessage { .. } => vec![], // No successors within the function (usually)
        // Other instructions are not terminators
        _ => vec![],
    }
}

pub fn find_reachable_blocks(
    entry_label: &str,
    // Provides the successor edges for *all* potential blocks after optimization
    successors_map: &HashMap<String, HashSet<String>>,
    // Needed to know which block labels *exist* in the optimized set
    all_block_labels: &HashSet<String>,
) -> HashSet<String> {
    let mut reachable: HashSet<String> = HashSet::new();
    let mut worklist: VecDeque<String> = VecDeque::new();

    if all_block_labels.contains(entry_label) {
        reachable.insert(entry_label.to_string());
        worklist.push_back(entry_label.to_string());
    } else if !all_block_labels.is_empty() {
        println!(
            "Warning: Entry block '{}' not found in optimized block set.",
            entry_label
        );
    }

    while let Some(current_label) = worklist.pop_front() {
        // Get successors from the provided map, defaulting to empty if not found
        if let Some(successors) = successors_map.get(&current_label) {
            for successor_label in successors {
                // Ensure the successor actually exists in the set of blocks we generated
                if all_block_labels.contains(successor_label)
                    && reachable.insert(successor_label.clone())
                {
                    // If the insert was successful (i.e., it wasn't already reachable)
                    worklist.push_back(successor_label.clone());
                }
            }
        }
        // If a block isn't in successors_map, it has no successors (e.g., ends in Return)
    }

    reachable
}
