use crate::{lower1::operand::extract_number_from_operand, oomir::*};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::collections::VecDeque;

pub(crate) mod copyprop;
mod dataflow;
mod reachability;
mod reorganisation;

use copyprop::propagate_copies_and_eliminate_dead_moves;
use dataflow::{analyze_constant_propagation, process_block_instructions};
use reachability::{find_reachable_blocks, get_block_successors};
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

fn build_cfg(code_block: &mut CodeBlock) -> HashMap<String, BasicBlockInfo> {
    let mut cfg: HashMap<String, BasicBlockInfo> = std::mem::take(&mut code_block.basic_blocks)
        .into_iter()
        .map(|(label, block)| {
            (
                label,
                BasicBlockInfo {
                    original_block: block,
                    predecessors: HashSet::default(),
                    successors: HashSet::default(),
                },
            )
        })
        .collect();

    if cfg.is_empty() {
        return cfg;
    }

    let mut all_successors: HashMap<String, Vec<String>> = HashMap::default();
    let cfg_keys: HashSet<String> = cfg.keys().cloned().collect();

    for (label, info) in &cfg {
        if !info.original_block.instructions.is_empty() {
            let successors = get_block_successors(&info.original_block);
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
    let mut optimized_blocks_intermediate: HashMap<String, BasicBlock> = HashMap::default();
    let mut optimized_successors: HashMap<String, HashSet<String>> = HashMap::default();
    let debug_locals = function
        .debug_variables
        .iter()
        .map(|variable| variable.oomir_name.clone())
        .collect::<HashSet<_>>();
    // Populate all labels from the original CFG before the loop
    let all_original_labels: HashSet<String> = cfg.keys().cloned().collect();

    for (label, info) in cfg {
        // Iterate using original CFG structure
        let block_entry_state = analysis_result
            .get(label)
            .expect("Analysis result missing for block");

        let (_, transformed_instructions) =
            process_block_instructions(info, block_entry_state, true, data_types, &debug_locals);

        let optimized_block = BasicBlock {
            label: label.clone(),
            instructions: transformed_instructions,
        };
        // Store the potentially optimized block using its original label
        optimized_blocks_intermediate.insert(label.clone(), optimized_block);

        let mut current_successors = HashSet::default();
        // Get the block we just inserted to find its *new* terminator
        if let Some(opt_block) = optimized_blocks_intermediate.get(label) {
            if !opt_block.instructions.is_empty() {
                let succ_labels = get_block_successors(opt_block);
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
    let mut final_basic_blocks = HashMap::default();
    for label in &reachable_labels {
        // Get the block from the intermediate results using the reachable label
        if let Some(block) = optimized_blocks_intermediate.remove(label) {
            // Add the reachable block (including potentially empty ones)
            final_basic_blocks.insert(label.clone(), block);
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

fn optimise_function_in_place(function: &mut Function, data_types: &HashMap<String, DataType>) {
    if function.body.basic_blocks.is_empty() {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "optimisation",
            format!(
                "Skipping optimization for empty function: {}",
                function.name
            )
        );
        return;
    }
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "optimisation",
        format!("Optimizing function: {}", function.name)
    );

    // 0. Run needed reorganisation passes
    convert_labels_to_basic_blocks_in_function(function);
    eliminate_duplicate_basic_blocks(function);

    // 1. Build Initial CFG
    let cfg = build_cfg(&mut function.body);
    if cfg.is_empty() {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Warn,
            "optimisation",
            format!(
                "Warning: CFG construction failed for non-empty function {}",
                function.name
            )
        );
        return;
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
        function.body.basic_blocks = cfg
            .into_iter()
            .map(|(label, info)| (label, info.original_block))
            .collect();
        return;
    }
    let analysis_result = analyze_constant_propagation(&function.body.entry, &cfg);

    // 3. Transform & Perform Dead Code Elimination
    transform_function(function, &cfg, &analysis_result, data_types);

    // 4. Clean up simple copies introduced by lowering and constant/algebraic rewrites.
    propagate_copies_and_eliminate_dead_moves(function);

    // 5. Eliminate duplicate basic blocks (re-pass-through after transformation)
    eliminate_duplicate_basic_blocks(function);

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "optimisation",
        format!("Finished optimizing function: {}", function.name)
    );
}

pub fn optimise_function(
    mut function: Function,
    data_types: &HashMap<String, DataType>,
) -> Function {
    optimise_function_in_place(&mut function, data_types);
    function
}

pub fn optimise_module(module: Module) -> Module {
    let old_funcs = module.functions;
    let mut new_funcs = HashMap::default();
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

    // Rust emits tiny generic helpers such as `needs_drop::<T>` as ordinary
    // functions even when their result is a compile-time constant. Fold calls
    // to side-effect-free constant-returning functions before the caller's
    // final optimisation pass. This lets constant branches and the pointer
    // temporaries contained exclusively in their dead arms disappear before
    // JVM lowering.
    let constant_returns = new_funcs
        .iter()
        .filter_map(|(key, function)| {
            simple_constant_return(function).map(|constant| (key.clone(), constant))
        })
        .collect::<HashMap<_, _>>();
    for function in new_funcs.values_mut() {
        if fold_constant_static_calls(function, &constant_returns) {
            optimise_function_in_place(function, &module.data_types);
        }
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
        suppressed_data_types: module.suppressed_data_types,
        shared_data_types: module.shared_data_types,
        relative_static_methods: module.relative_static_methods,
        external_interfaces: module.external_interfaces,
        statics: module.statics,
    }
}

fn simple_constant_return(function: &Function) -> Option<Constant> {
    let mut returned = None;
    for block in function.body.basic_blocks.values() {
        for instruction in &block.instructions {
            match instruction {
                Instruction::SourceLocation(_)
                | Instruction::LocalVariableScope(_)
                | Instruction::Label { .. }
                | Instruction::Jump { .. } => {}
                Instruction::Return {
                    operand: Some(Operand::Constant(constant)),
                } if constant.is_propagatable() => {
                    if returned
                        .as_ref()
                        .is_some_and(|previous| previous != constant)
                    {
                        return None;
                    }
                    returned = Some(constant.clone());
                }
                _ => return None,
            }
        }
    }
    returned
}

fn fold_constant_static_calls(
    function: &mut Function,
    constant_returns: &HashMap<FunctionKey, Constant>,
) -> bool {
    let mut changed = false;
    for block in function.body.basic_blocks.values_mut() {
        block.instructions.retain_mut(|instruction| {
            let Instruction::InvokeStatic {
                dest,
                class_name,
                method_name,
                method_ty,
                ..
            } = instruction
            else {
                return true;
            };
            let key = FunctionKey::new(class_name, method_name, method_ty);
            let Some(constant) = constant_returns.get(&key) else {
                return true;
            };

            changed = true;
            if let Some(dest) = dest.take() {
                *instruction = Instruction::Move {
                    dest,
                    src: Operand::Constant(constant.clone()),
                };
                true
            } else {
                false
            }
        });
    }
    changed
}

#[cfg(test)]
mod tests {
    use super::*;

    fn function(name: &str, instructions: Vec<Instruction>) -> Function {
        Function {
            name: name.to_string(),
            owner_class: Some("example/Test".to_string()),
            signature: Signature {
                params: Vec::new(),
                ret: Box::new(Type::Boolean),
                is_static: true,
            },
            debug_variables: Vec::new(),
            body: CodeBlock {
                entry: "entry".to_string(),
                basic_blocks: HashMap::from_iter([(
                    "entry".to_string(),
                    BasicBlock {
                        label: "entry".to_string(),
                        instructions,
                    },
                )]),
            },
        }
    }

    #[test]
    fn folds_same_module_constant_return_calls() {
        let callee = function(
            "constant",
            vec![Instruction::Return {
                operand: Some(Operand::Constant(Constant::Boolean(false))),
            }],
        );
        let mut caller = function(
            "caller",
            vec![Instruction::InvokeStatic {
                dest: Some("result".to_string()),
                class_name: "example/Test".to_string(),
                method_name: "constant".to_string(),
                method_ty: callee.signature.clone(),
                args: Vec::new(),
            }],
        );
        let constants = HashMap::from_iter([(
            FunctionKey::new("example/Test", "constant", &callee.signature),
            simple_constant_return(&callee).unwrap(),
        )]);

        assert!(fold_constant_static_calls(&mut caller, &constants));
        assert_eq!(
            caller.body.basic_blocks["entry"].instructions,
            vec![Instruction::Move {
                dest: "result".to_string(),
                src: Operand::Constant(Constant::Boolean(false)),
            }]
        );
    }
}
