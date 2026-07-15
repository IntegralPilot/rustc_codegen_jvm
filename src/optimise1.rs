use crate::{lower1::operand::extract_number_from_operand, oomir::*};
use std::collections::{HashMap, HashSet, VecDeque};

mod copyprop;
mod dataflow;
mod reachability;
mod reorganisation;

use copyprop::propagate_copies_and_eliminate_dead_moves;
use dataflow::{analyze_constant_propagation, process_block_instructions};
use reachability::{find_reachable_blocks, get_instruction_successors};
use reorganisation::{
    convert_labels_to_basic_blocks_in_function, eliminate_duplicate_basic_blocks,
};

#[derive(Debug, Clone)]
struct BasicBlockInfo {
    original_block: BasicBlock,
    predecessors: HashSet<String>,
    successors: HashSet<String>,
}

type ConstantMap = HashMap<String, Constant>;

type DataflowResult = HashMap<String, ConstantMap>;

fn build_cfg(code_block: &CodeBlock) -> HashMap<String, BasicBlockInfo> {
    let mut cfg: HashMap<String, BasicBlockInfo> = code_block
        .basic_blocks
        .iter()
        .map(|(label, block)| {
            (
                label.clone(),
                BasicBlockInfo {
                    original_block: block.clone(),
                    predecessors: HashSet::new(),
                    successors: HashSet::new(),
                },
            )
        })
        .collect();

    if cfg.is_empty() {
        return cfg;
    }

    let mut all_successors: HashMap<String, Vec<String>> = HashMap::new();
    let cfg_keys: HashSet<String> = cfg.keys().cloned().collect();

    for (label, info) in &cfg {
        if let Some(terminator) = info.original_block.instructions.last() {
            let successors = get_instruction_successors(terminator);
            let valid_successors: Vec<String> = successors
                .into_iter()
                .filter(|succ_label| {
                    if cfg_keys.contains(succ_label) {
                        true
                    } else {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "optimisation",
                            format!(
                                "Warning: Block '{}' refers to non-existent successor '{}'",
                                label, succ_label
                            )
                        );
                        false
                    }
                })
                .collect();
            all_successors.insert(label.clone(), valid_successors);
        } else {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "optimisation",
                format!("Warning: Block '{}' has no instructions.", label)
            );
            all_successors.insert(label.clone(), vec![]);
        }
    }

    for (label, successors) in &all_successors {
        if let Some(info) = cfg.get_mut(label) {
            info.successors.extend(successors.iter().cloned());
        }
        for successor_label in successors {
            if let Some(successor_info) = cfg.get_mut(successor_label) {
                successor_info.predecessors.insert(label.clone());
            }
        }
    }

    cfg
}

fn transform_function(
    function: &mut Function,
    cfg: &HashMap<String, BasicBlockInfo>,
    analysis_result: &DataflowResult,
    data_types: &HashMap<String, DataType>,
) {
    let mut optimized_blocks_intermediate: HashMap<String, BasicBlock> = HashMap::new();
    let mut optimized_successors: HashMap<String, HashSet<String>> = HashMap::new();
    // Populate all labels from the original CFG before the loop
    let all_original_labels: HashSet<String> = cfg.keys().cloned().collect();

    for (label, info) in cfg {
        // Iterate using original CFG structure
        let block_entry_state = analysis_result
            .get(label)
            .expect("Analysis result missing for block");

        let (_, transformed_instructions) =
            process_block_instructions(info, block_entry_state, true, data_types);

        let optimized_block = BasicBlock {
            label: label.clone(),
            instructions: transformed_instructions,
        };
        // Store the potentially optimized block using its original label
        optimized_blocks_intermediate.insert(label.clone(), optimized_block);

        let mut current_successors = HashSet::new();
        // Get the block we just inserted to find its *new* terminator
        if let Some(opt_block) = optimized_blocks_intermediate.get(label) {
            if let Some(terminator) = opt_block.instructions.last() {
                let succ_labels = get_instruction_successors(terminator);
                // Filter successors against the set of original labels.
                // This ensures edges are kept even if the target block hasn't
                // been visited in this loop iteration yet.
                current_successors.extend(
                    succ_labels
                        .into_iter()
                        .filter(|s| all_original_labels.contains(s)),
                );
            }
        } else {
            // This case should likely not happen if we just inserted it
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "optimisation",
                format!(
                    "Internal Warning: optimized block {} not found immediately after insertion.",
                    label
                )
            );
        }
        optimized_successors.insert(label.clone(), current_successors);
    }

    let reachable_labels = find_reachable_blocks(
        &function.body.entry,
        &optimized_successors,
        &all_original_labels,
    );

    // (Keep the previous fix - don't remove empty reachable blocks)
    let mut final_basic_blocks = HashMap::new();
    for label in &reachable_labels {
        // Get the block from the intermediate results using the reachable label
        if let Some(block) = optimized_blocks_intermediate.get(label) {
            // Add the reachable block (including potentially empty ones)
            final_basic_blocks.insert(label.clone(), block.clone());
        } else {
            // This suggests reachable_labels contains a label not in intermediate map,
            // which would be an internal error (shouldn't happen if all_original_labels was used correctly).
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Error,
                "optimisation",
                format!(
                    "Internal Error: Reachable label '{}' not found in intermediate blocks.",
                    label
                )
            );
        }
    }

    // Check reachability against the original cfg's keyset size or existence check
    if !reachable_labels.contains(&function.body.entry) && !cfg.is_empty() {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Warn,
            "optimisation",
            format!(
                "Warning: Original entry block '{}' became unreachable in function '{}'.",
                function.body.entry, function.name
            )
        );
        if final_basic_blocks.is_empty() {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "optimisation",
                format!(
                    "Function '{}' appears fully optimized away or is empty.",
                    function.name
                )
            );
            function.body.basic_blocks.clear();
        } else {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Error,
                "optimisation",
                format!(
                    "ERROR: Function '{}' has reachable blocks but the original entry '{}' is not reachable. The resulting IR may be invalid.",
                    function.name, function.body.entry
                )
            );
            // Attempt to recover by picking a new entry point (arbitrarily)
            if let Some(new_entry_label) = final_basic_blocks.keys().next() {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "optimisation",
                    format!("Attempting to set new entry point to '{}'", new_entry_label)
                );
                function.body.entry = new_entry_label.clone();
            } else {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Error,
                    "optimisation",
                    "CRITICAL ERROR: final_basic_blocks is not empty but has no keys after entry removal."
                );
                // Maybe clear blocks if we can't even find a new entry?
                function.body.basic_blocks.clear();
            }
        }
    // Handle case where original entry existed but function optimized to empty
    } else if final_basic_blocks.is_empty() && cfg.contains_key(&function.body.entry) {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "optimisation",
            format!("Function '{}' optimized to be empty.", function.name)
        );
        function.body.basic_blocks.clear();
    }

    function.body.basic_blocks = final_basic_blocks;
}

pub fn optimise_function(
    mut function: Function,
    data_types: &HashMap<String, DataType>,
) -> Function {
    let instrumented_fn_name = function
        .owner_class
        .as_deref()
        .map(|owner| format!("{owner}::{}", function.name))
        .unwrap_or_else(|| function.name.clone());
    let _timer = crate::instrumentation::Timer::function("optimise1", None, &instrumented_fn_name);

    if function.body.basic_blocks.is_empty() {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "optimisation",
            format!(
                "Skipping optimization for empty function: {}",
                function.name
            )
        );
        return function;
    }
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "optimisation",
        format!("Optimizing function: {}", function.name)
    );

    // 0. Run needed reorganisation passes
    convert_labels_to_basic_blocks_in_function(&mut function);
    eliminate_duplicate_basic_blocks(&mut function);

    // 1. Build Initial CFG
    let cfg = build_cfg(&function.body);
    if cfg.is_empty() && !function.body.basic_blocks.is_empty() {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Warn,
            "optimisation",
            format!(
                "Warning: CFG construction failed for non-empty function {}",
                function.name
            )
        );
        return function; // Avoid panic if CFG fails
    }

    // 2. Perform Dataflow Analysis (Constant Propagation)
    // Ensure entry point exists in CFG before analysis
    if !cfg.contains_key(&function.body.entry) && !cfg.is_empty() {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Error,
            "optimisation",
            format!(
                "ERROR: Entry block '{}' not found in CFG for function {}. Skipping optimization.",
                function.body.entry, function.name
            )
        );
        // This might happen if the entry block itself has no instructions or references invalid blocks.
        return function;
    }
    let analysis_result = analyze_constant_propagation(&function.body.entry, &cfg);

    // 3. Transform & Perform Dead Code Elimination
    transform_function(&mut function, &cfg, &analysis_result, data_types);

    // 4. Clean up simple copies introduced by lowering and constant/algebraic rewrites.
    propagate_copies_and_eliminate_dead_moves(&mut function);

    // 5. Eliminate duplicate basic blocks (re-pass-through after transformation)
    eliminate_duplicate_basic_blocks(&mut function);

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "optimisation",
        format!("Finished optimizing function: {}", function.name)
    );
    function
}

pub fn optimise_module(module: Module) -> Module {
    let old_funcs = module.functions;
    let mut new_funcs = HashMap::new();
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "optimisation",
        format!("Optimizing module: {}", module.name)
    );
    for (name, func) in old_funcs {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "optimisation",
            format!("Optimizing function: {}", name)
        );
        // Pass data_types needed for analysis/transformation
        let new_func = optimise_function(func, &module.data_types);
        new_funcs.insert(name, new_func);
    }
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "optimisation",
        format!("Optimization complete for module: {}", module.name)
    );
    Module {
        name: module.name,
        source_file: module.source_file,
        functions: new_funcs,
        data_types: module.data_types, // Assume data_types are read-only for opts
        statics: module.statics,
    }
}
