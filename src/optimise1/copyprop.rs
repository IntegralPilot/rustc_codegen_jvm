use super::*;

type AliasMap = HashMap<String, Operand>;

pub fn propagate_copies_and_eliminate_dead_moves(function: &mut Function) {
    let debug_locals = function
        .debug_variables
        .iter()
        .map(|variable| variable.oomir_name.clone())
        .collect::<HashSet<_>>();
    propagate_copies(function, &debug_locals);
    eliminate_dead_moves(function, &debug_locals);
}

fn propagate_copies(function: &mut Function, debug_locals: &HashSet<String>) {
    let block_entry_aliases = analyze_copy_aliases(function, debug_locals);
    let mut labels: Vec<String> = function.body.basic_blocks.keys().cloned().collect();
    labels.sort();

    for label in labels {
        let Some(block) = function.body.basic_blocks.get_mut(&label) else {
            continue;
        };
        let mut aliases = block_entry_aliases.get(&label).cloned().unwrap_or_default();

        for instruction in &mut block.instructions {
            rewrite_instruction_uses(instruction, &aliases);
            transfer_aliases_through_instruction(instruction, &mut aliases, debug_locals);
        }
    }
}

fn analyze_copy_aliases(
    function: &Function,
    debug_locals: &HashSet<String>,
) -> HashMap<String, AliasMap> {
    let mut labels: Vec<String> = function.body.basic_blocks.keys().cloned().collect();
    labels.sort();

    let predecessors = block_predecessors(function);
    let mut entry_aliases: HashMap<String, AliasMap> = labels
        .iter()
        .map(|label| (label.clone(), AliasMap::new()))
        .collect();
    let mut exit_aliases = entry_aliases.clone();
    let mut changed = true;

    while changed {
        changed = false;

        for label in &labels {
            let next_entry = if label == &function.body.entry {
                AliasMap::new()
            } else {
                meet_predecessor_aliases(predecessors.get(label), &exit_aliases)
            };

            let next_exit = function
                .body
                .basic_blocks
                .get(label)
                .map(|block| {
                    transfer_aliases_through_block(block, next_entry.clone(), debug_locals)
                })
                .unwrap_or_default();

            if entry_aliases.get(label) != Some(&next_entry) {
                entry_aliases.insert(label.clone(), next_entry);
                changed = true;
            }
            if exit_aliases.get(label) != Some(&next_exit) {
                exit_aliases.insert(label.clone(), next_exit);
                changed = true;
            }
        }
    }

    entry_aliases
}

fn block_predecessors(function: &Function) -> HashMap<String, Vec<String>> {
    let mut predecessors: HashMap<String, Vec<String>> = function
        .body
        .basic_blocks
        .keys()
        .map(|label| (label.clone(), Vec::new()))
        .collect();

    for (label, block) in &function.body.basic_blocks {
        let successors = block
            .instructions
            .last()
            .map(super::reachability::get_instruction_successors)
            .unwrap_or_default();
        for successor in successors {
            if let Some(successor_predecessors) = predecessors.get_mut(&successor) {
                successor_predecessors.push(label.clone());
            }
        }
    }

    predecessors
}

fn meet_predecessor_aliases(
    predecessors: Option<&Vec<String>>,
    exit_aliases: &HashMap<String, AliasMap>,
) -> AliasMap {
    let Some(predecessors) = predecessors else {
        return AliasMap::new();
    };
    let mut predecessor_iter = predecessors.iter();
    let Some(first_predecessor) = predecessor_iter.next() else {
        return AliasMap::new();
    };

    let mut aliases = exit_aliases
        .get(first_predecessor)
        .cloned()
        .unwrap_or_default();
    for predecessor in predecessor_iter {
        let predecessor_aliases = exit_aliases.get(predecessor);
        aliases.retain(|dest, alias| {
            predecessor_aliases.is_some_and(|aliases| aliases.get(dest) == Some(alias))
        });
    }
    aliases
}

fn transfer_aliases_through_block(
    block: &BasicBlock,
    mut aliases: AliasMap,
    debug_locals: &HashSet<String>,
) -> AliasMap {
    for instruction in &block.instructions {
        transfer_aliases_through_instruction(instruction, &mut aliases, debug_locals);
    }
    aliases
}

fn transfer_aliases_through_instruction(
    instruction: &Instruction,
    aliases: &mut AliasMap,
    debug_locals: &HashSet<String>,
) {
    let rewritten_move_src = if let Instruction::Move { src, .. } = instruction {
        let mut src = src.clone();
        rewrite_operand(&mut src, aliases);
        Some(src)
    } else {
        None
    };

    let defs = instruction_defs(instruction);
    for def in &defs {
        kill_aliases_touching(def, aliases);
    }

    if let (Instruction::Move { dest, .. }, Some(Operand::Variable { name, ty })) =
        (instruction, rewritten_move_src)
        && dest != &name
        && !debug_locals.contains(dest)
    {
        aliases.insert(dest.clone(), Operand::Variable { name, ty });
    }
}

fn eliminate_dead_moves(function: &mut Function, debug_locals: &HashSet<String>) {
    loop {
        let live_out = block_live_out(function);
        let mut removed_any = false;

        let mut labels: Vec<String> = function.body.basic_blocks.keys().cloned().collect();
        labels.sort();
        for label in labels {
            let Some(block) = function.body.basic_blocks.get_mut(&label) else {
                continue;
            };

            let mut live = live_out.get(&label).cloned().unwrap_or_default();
            let mut keep = vec![true; block.instructions.len()];

            for (index, instruction) in block.instructions.iter().enumerate().rev() {
                let defs = instruction_defs(instruction);
                let uses = instruction_uses(instruction);

                if let Instruction::Move { dest, .. } = instruction
                    && !live.contains(dest)
                    && !debug_locals.contains(dest)
                {
                    keep[index] = false;
                    removed_any = true;
                    continue;
                }

                for def in defs {
                    live.remove(&def);
                }
                live.extend(uses);
            }

            if removed_any {
                let mut keep_iter = keep.into_iter();
                block
                    .instructions
                    .retain(|_| keep_iter.next().unwrap_or(true));
            }
        }

        if !removed_any {
            break;
        }
    }
}

fn block_live_out(function: &Function) -> HashMap<String, HashSet<String>> {
    let mut use_sets = HashMap::new();
    let mut def_sets = HashMap::new();
    let mut successors = HashMap::new();

    for (label, block) in &function.body.basic_blocks {
        let mut used_before_def = HashSet::new();
        let mut defined = HashSet::new();
        for instruction in &block.instructions {
            for used in instruction_uses(instruction) {
                if !defined.contains(&used) {
                    used_before_def.insert(used);
                }
            }
            defined.extend(instruction_defs(instruction));
        }

        let block_successors = block
            .instructions
            .last()
            .map(super::reachability::get_instruction_successors)
            .unwrap_or_default()
            .into_iter()
            .filter(|successor| function.body.basic_blocks.contains_key(successor))
            .collect::<Vec<_>>();

        use_sets.insert(label.clone(), used_before_def);
        def_sets.insert(label.clone(), defined);
        successors.insert(label.clone(), block_successors);
    }

    let mut live_in: HashMap<String, HashSet<String>> = function
        .body
        .basic_blocks
        .keys()
        .map(|label| (label.clone(), HashSet::new()))
        .collect();
    let mut live_out = live_in.clone();
    let mut changed = true;

    while changed {
        changed = false;
        let mut labels: Vec<String> = function.body.basic_blocks.keys().cloned().collect();
        labels.sort();
        labels.reverse();

        for label in labels {
            let mut next_out = HashSet::new();
            if let Some(block_successors) = successors.get(&label) {
                for successor in block_successors {
                    if let Some(successor_live_in) = live_in.get(successor) {
                        next_out.extend(successor_live_in.iter().cloned());
                    }
                }
            }

            let mut next_in = use_sets.get(&label).cloned().unwrap_or_default();
            let block_defs = def_sets.get(&label).cloned().unwrap_or_default();
            next_in.extend(
                next_out
                    .iter()
                    .filter(|name| !block_defs.contains(*name))
                    .cloned(),
            );

            if live_out.get(&label) != Some(&next_out) {
                live_out.insert(label.clone(), next_out);
                changed = true;
            }
            if live_in.get(&label) != Some(&next_in) {
                live_in.insert(label, next_in);
                changed = true;
            }
        }
    }

    live_out
}

fn rewrite_instruction_uses(instruction: &mut Instruction, aliases: &AliasMap) {
    match instruction {
        Instruction::Add { op1, op2, .. }
        | Instruction::Sub { op1, op2, .. }
        | Instruction::Mul { op1, op2, .. }
        | Instruction::Div { op1, op2, .. }
        | Instruction::Rem { op1, op2, .. }
        | Instruction::Eq { op1, op2, .. }
        | Instruction::Ne { op1, op2, .. }
        | Instruction::Lt { op1, op2, .. }
        | Instruction::Le { op1, op2, .. }
        | Instruction::Gt { op1, op2, .. }
        | Instruction::Ge { op1, op2, .. }
        | Instruction::BitAnd { op1, op2, .. }
        | Instruction::BitOr { op1, op2, .. }
        | Instruction::BitXor { op1, op2, .. }
        | Instruction::Shl { op1, op2, .. }
        | Instruction::Shr { op1, op2, .. } => {
            rewrite_operand(op1, aliases);
            rewrite_operand(op2, aliases);
        }
        Instruction::Not { src, .. }
        | Instruction::Neg { src, .. }
        | Instruction::Move { src, .. } => rewrite_operand(src, aliases),
        Instruction::Branch { condition, .. } => rewrite_operand(condition, aliases),
        Instruction::Return { operand } => {
            if let Some(operand) = operand {
                rewrite_operand(operand, aliases);
            }
        }
        Instruction::InvokeStatic { args, .. } => {
            rewrite_operands(args, aliases);
        }
        Instruction::CallIndirect {
            function_ptr, args, ..
        } => {
            rewrite_operand(function_ptr, aliases);
            rewrite_operands(args, aliases);
        }
        Instruction::InvokeInterface { operand, args, .. }
        | Instruction::InvokeVirtual { operand, args, .. } => {
            rewrite_operand(operand, aliases);
            rewrite_operands(args, aliases);
        }
        Instruction::Switch { discr, .. } => rewrite_operand(discr, aliases),
        Instruction::NewArray { size, .. } | Instruction::Length { array: size, .. } => {
            rewrite_operand(size, aliases);
        }
        Instruction::ArrayStore {
            array,
            index,
            value,
            ..
        } => {
            rewrite_variable_name(array, aliases);
            rewrite_operand(index, aliases);
            rewrite_operand(value, aliases);
        }
        Instruction::ArrayGet { array, index, .. } => {
            rewrite_operand(array, aliases);
            rewrite_operand(index, aliases);
        }
        Instruction::ConstructObject { args, .. } => {
            for (arg, _) in args {
                rewrite_operand(arg, aliases);
            }
        }
        Instruction::SetField { object, value, .. } => {
            rewrite_variable_name(object, aliases);
            rewrite_operand(value, aliases);
        }
        Instruction::GetField { object, .. } | Instruction::Cast { op: object, .. } => {
            rewrite_operand(object, aliases);
        }
        Instruction::SourceLocation(_)
        | Instruction::LocalVariableScope(_)
        | Instruction::CreateFunctionPointer { .. }
        | Instruction::Jump { .. }
        | Instruction::ThrowNewWithMessage { .. }
        | Instruction::Label { .. } => {}
    }
}

fn rewrite_operands(operands: &mut [Operand], aliases: &AliasMap) {
    for operand in operands {
        rewrite_operand(operand, aliases);
    }
}

fn rewrite_operand(operand: &mut Operand, aliases: &AliasMap) {
    let Operand::Variable {
        name,
        ty: expected_ty,
    } = operand
    else {
        return;
    };
    if let Some(alias) = resolve_alias(name, aliases)
        && alias.get_type().as_ref() == Some(expected_ty)
    {
        *operand = alias;
    }
}

fn rewrite_variable_name(name: &mut String, aliases: &AliasMap) {
    let Some(Operand::Variable {
        name: alias_name, ..
    }) = resolve_alias(name, aliases)
    else {
        return;
    };
    *name = alias_name;
}

fn resolve_alias(name: &str, aliases: &AliasMap) -> Option<Operand> {
    let mut current = name;
    let mut seen = HashSet::new();
    while let Some(Operand::Variable {
        name: next_name,
        ty,
    }) = aliases.get(current)
    {
        if !seen.insert(current.to_string()) {
            return None;
        }
        if !aliases.contains_key(next_name) {
            return Some(Operand::Variable {
                name: next_name.clone(),
                ty: ty.clone(),
            });
        }
        current = next_name;
    }
    aliases.get(name).cloned()
}

fn kill_aliases_touching(name: &str, aliases: &mut AliasMap) {
    let mut invalidated = HashSet::from([name.to_string()]);
    loop {
        let dependants = aliases
            .iter()
            .filter_map(|(alias_dest, alias_src)| {
                let touches_invalidated = invalidated.contains(alias_dest)
                    || matches!(
                        alias_src,
                        Operand::Variable { name: src_name, .. }
                            if invalidated.contains(src_name)
                    );
                touches_invalidated.then(|| alias_dest.clone())
            })
            .collect::<Vec<_>>();
        if dependants.is_empty() {
            break;
        }
        for dependant in dependants {
            aliases.remove(&dependant);
            invalidated.insert(dependant);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn redefining_alias_source_invalidates_transitive_dependants() {
        let slice_ty = Type::Slice(Box::new(Type::U8));
        let mut aliases = AliasMap::from([
            (
                "a".to_string(),
                Operand::Variable {
                    name: "b".to_string(),
                    ty: slice_ty.clone(),
                },
            ),
            (
                "b".to_string(),
                Operand::Variable {
                    name: "c".to_string(),
                    ty: slice_ty,
                },
            ),
        ]);

        kill_aliases_touching("c", &mut aliases);

        assert!(aliases.is_empty());
    }
}

fn instruction_uses(instruction: &Instruction) -> HashSet<String> {
    let mut uses = HashSet::new();
    collect_instruction_uses(instruction, &mut uses);
    uses
}

fn collect_instruction_uses(instruction: &Instruction, uses: &mut HashSet<String>) {
    match instruction {
        Instruction::Add { op1, op2, .. }
        | Instruction::Sub { op1, op2, .. }
        | Instruction::Mul { op1, op2, .. }
        | Instruction::Div { op1, op2, .. }
        | Instruction::Rem { op1, op2, .. }
        | Instruction::Eq { op1, op2, .. }
        | Instruction::Ne { op1, op2, .. }
        | Instruction::Lt { op1, op2, .. }
        | Instruction::Le { op1, op2, .. }
        | Instruction::Gt { op1, op2, .. }
        | Instruction::Ge { op1, op2, .. }
        | Instruction::BitAnd { op1, op2, .. }
        | Instruction::BitOr { op1, op2, .. }
        | Instruction::BitXor { op1, op2, .. }
        | Instruction::Shl { op1, op2, .. }
        | Instruction::Shr { op1, op2, .. } => {
            collect_operand_use(op1, uses);
            collect_operand_use(op2, uses);
        }
        Instruction::Not { src, .. }
        | Instruction::Neg { src, .. }
        | Instruction::Move { src, .. } => collect_operand_use(src, uses),
        Instruction::Branch { condition, .. } => collect_operand_use(condition, uses),
        Instruction::Return { operand } => {
            if let Some(operand) = operand {
                collect_operand_use(operand, uses);
            }
        }
        Instruction::InvokeStatic { args, .. } => {
            collect_operand_uses(args, uses);
        }
        Instruction::CallIndirect {
            function_ptr, args, ..
        } => {
            collect_operand_use(function_ptr, uses);
            collect_operand_uses(args, uses);
        }
        Instruction::InvokeInterface { operand, args, .. }
        | Instruction::InvokeVirtual { operand, args, .. } => {
            collect_operand_use(operand, uses);
            collect_operand_uses(args, uses);
        }
        Instruction::Switch { discr, .. } => collect_operand_use(discr, uses),
        Instruction::NewArray { size, .. } | Instruction::Length { array: size, .. } => {
            collect_operand_use(size, uses);
        }
        Instruction::ArrayStore {
            array,
            index,
            value,
            ..
        } => {
            uses.insert(array.clone());
            collect_operand_use(index, uses);
            collect_operand_use(value, uses);
        }
        Instruction::ArrayGet { array, index, .. } => {
            collect_operand_use(array, uses);
            collect_operand_use(index, uses);
        }
        Instruction::ConstructObject { args, .. } => {
            for (arg, _) in args {
                collect_operand_use(arg, uses);
            }
        }
        Instruction::SetField { object, value, .. } => {
            uses.insert(object.clone());
            collect_operand_use(value, uses);
        }
        Instruction::GetField { object, .. } | Instruction::Cast { op: object, .. } => {
            collect_operand_use(object, uses);
        }
        Instruction::SourceLocation(_)
        | Instruction::LocalVariableScope(_)
        | Instruction::CreateFunctionPointer { .. }
        | Instruction::Jump { .. }
        | Instruction::ThrowNewWithMessage { .. }
        | Instruction::Label { .. } => {}
    }
}

fn collect_operand_uses(operands: &[Operand], uses: &mut HashSet<String>) {
    for operand in operands {
        collect_operand_use(operand, uses);
    }
}

fn collect_operand_use(operand: &Operand, uses: &mut HashSet<String>) {
    if let Operand::Variable { name, .. } = operand {
        uses.insert(name.clone());
    }
}

fn instruction_defs(instruction: &Instruction) -> HashSet<String> {
    let mut defs = HashSet::new();
    match instruction {
        Instruction::Add { dest, .. }
        | Instruction::Sub { dest, .. }
        | Instruction::Mul { dest, .. }
        | Instruction::Div { dest, .. }
        | Instruction::Rem { dest, .. }
        | Instruction::Eq { dest, .. }
        | Instruction::Ne { dest, .. }
        | Instruction::Lt { dest, .. }
        | Instruction::Le { dest, .. }
        | Instruction::Gt { dest, .. }
        | Instruction::Ge { dest, .. }
        | Instruction::BitAnd { dest, .. }
        | Instruction::BitOr { dest, .. }
        | Instruction::BitXor { dest, .. }
        | Instruction::Shl { dest, .. }
        | Instruction::Shr { dest, .. }
        | Instruction::Not { dest, .. }
        | Instruction::Neg { dest, .. }
        | Instruction::Move { dest, .. }
        | Instruction::NewArray { dest, .. }
        | Instruction::ArrayGet { dest, .. }
        | Instruction::Length { dest, .. }
        | Instruction::ConstructObject { dest, .. }
        | Instruction::CreateFunctionPointer { dest, .. }
        | Instruction::GetField { dest, .. }
        | Instruction::Cast { dest, .. } => {
            defs.insert(dest.clone());
        }
        Instruction::CallIndirect { dest, .. }
        | Instruction::InvokeInterface { dest, .. }
        | Instruction::InvokeVirtual { dest, .. }
        | Instruction::InvokeStatic { dest, .. } => {
            if let Some(dest) = dest {
                defs.insert(dest.clone());
            }
        }
        Instruction::SourceLocation(_)
        | Instruction::LocalVariableScope(_)
        | Instruction::Jump { .. }
        | Instruction::Branch { .. }
        | Instruction::Return { .. }
        | Instruction::ThrowNewWithMessage { .. }
        | Instruction::Switch { .. }
        | Instruction::ArrayStore { .. }
        | Instruction::SetField { .. }
        | Instruction::Label { .. } => {}
    }
    defs
}
