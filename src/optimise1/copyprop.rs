use super::*;

#[derive(Clone, Debug, PartialEq, Eq)]
struct AliasValue {
    source: usize,
    ty: Type,
}

type AliasMap = HashMap<usize, AliasValue>;

#[derive(Debug)]
struct LocalInterner {
    names: Vec<String>,
    ids: HashMap<String, usize>,
}

impl LocalInterner {
    fn for_function(function: &Function) -> Self {
        let mut names = HashSet::new();
        for variable in &function.debug_variables {
            names.insert(variable.oomir_name.as_str());
        }
        for block in function.body.basic_blocks.values() {
            for instruction in &block.instructions {
                names.extend(instruction_uses(instruction));
                if let Some(def) = instruction_def(instruction) {
                    names.insert(def);
                }
            }
        }
        let mut names = names.into_iter().map(str::to_string).collect::<Vec<_>>();
        names.sort();
        let ids = names
            .iter()
            .enumerate()
            .map(|(id, name)| (name.clone(), id))
            .collect();
        Self { names, ids }
    }

    fn id(&self, name: &str) -> Option<usize> {
        self.ids.get(name).copied()
    }

    fn name(&self, id: usize) -> &str {
        &self.names[id]
    }

    fn len(&self) -> usize {
        self.names.len()
    }
}

pub fn propagate_copies_and_eliminate_dead_moves(function: &mut Function) {
    let locals = LocalInterner::for_function(function);
    let debug_locals = function
        .debug_variables
        .iter()
        .filter_map(|variable| locals.id(&variable.oomir_name))
        .collect::<HashSet<_>>();
    propagate_copies(function, &locals, &debug_locals);
    eliminate_dead_moves(function, &locals, &debug_locals);
}

fn propagate_copies(
    function: &mut Function,
    locals: &LocalInterner,
    debug_locals: &HashSet<usize>,
) {
    let (labels, block_entry_aliases) = analyze_copy_aliases(function, locals, debug_locals);
    for (block_index, label) in labels.into_iter().enumerate() {
        let Some(block) = function.body.basic_blocks.get_mut(&label) else {
            continue;
        };
        let mut aliases = block_entry_aliases[block_index].clone();

        for instruction in &mut block.instructions {
            rewrite_instruction_uses(instruction, &aliases, locals);
            transfer_aliases_through_instruction(instruction, &mut aliases, locals, debug_locals);
        }
    }
}

fn analyze_copy_aliases(
    function: &Function,
    locals: &LocalInterner,
    debug_locals: &HashSet<usize>,
) -> (Vec<String>, Vec<AliasMap>) {
    let mut labels: Vec<String> = function.body.basic_blocks.keys().cloned().collect();
    labels.sort();
    let label_ids = labels
        .iter()
        .enumerate()
        .map(|(index, label)| (label.as_str(), index))
        .collect::<HashMap<_, _>>();
    let entry = label_ids.get(function.body.entry.as_str()).copied();
    let mut successors = vec![Vec::new(); labels.len()];
    let mut predecessors = vec![Vec::new(); labels.len()];
    for (index, label) in labels.iter().enumerate() {
        let Some(block) = function.body.basic_blocks.get(label) else {
            continue;
        };
        for successor in super::reachability::get_block_successors(block) {
            let Some(successor) = label_ids.get(successor.as_str()).copied() else {
                continue;
            };
            successors[index].push(successor);
            predecessors[successor].push(index);
        }
    }

    let mut entry_aliases = vec![AliasMap::new(); labels.len()];
    let mut exit_aliases = entry_aliases.clone();
    let mut queue = (0..labels.len()).collect::<VecDeque<_>>();
    let mut queued = vec![true; labels.len()];
    while let Some(index) = queue.pop_front() {
        queued[index] = false;
        let next_entry = if Some(index) == entry {
            AliasMap::new()
        } else {
            meet_predecessor_aliases(&predecessors[index], &exit_aliases)
        };
        let next_exit = function
            .body
            .basic_blocks
            .get(&labels[index])
            .map(|block| {
                transfer_aliases_through_block(block, next_entry.clone(), locals, debug_locals)
            })
            .unwrap_or_default();
        entry_aliases[index] = next_entry;
        if exit_aliases[index] != next_exit {
            exit_aliases[index] = next_exit;
            for successor in successors[index].iter().copied() {
                if !queued[successor] {
                    queue.push_back(successor);
                    queued[successor] = true;
                }
            }
        }
    }

    (labels, entry_aliases)
}

fn meet_predecessor_aliases(predecessors: &[usize], exit_aliases: &[AliasMap]) -> AliasMap {
    let mut predecessor_iter = predecessors.iter();
    let Some(first_predecessor) = predecessor_iter.next() else {
        return AliasMap::new();
    };

    let mut aliases = exit_aliases[*first_predecessor].clone();
    for predecessor in predecessor_iter {
        let predecessor_aliases = &exit_aliases[*predecessor];
        aliases.retain(|dest, alias| predecessor_aliases.get(dest) == Some(alias));
    }
    aliases
}

fn transfer_aliases_through_block(
    block: &BasicBlock,
    mut aliases: AliasMap,
    locals: &LocalInterner,
    debug_locals: &HashSet<usize>,
) -> AliasMap {
    for instruction in &block.instructions {
        transfer_aliases_through_instruction(instruction, &mut aliases, locals, debug_locals);
    }
    aliases
}

fn transfer_aliases_through_instruction(
    instruction: &Instruction,
    aliases: &mut AliasMap,
    locals: &LocalInterner,
    debug_locals: &HashSet<usize>,
) {
    let rewritten_move_src = if let Instruction::Move { src, .. } = instruction {
        let mut src = src.clone();
        rewrite_operand(&mut src, aliases, locals);
        Some(src)
    } else {
        None
    };

    if let Some(def) = instruction_def(instruction).and_then(|name| locals.id(name)) {
        kill_aliases_touching(def, aliases);
    }

    if let (Instruction::Move { dest, .. }, Some(Operand::Variable { name, ty })) =
        (instruction, rewritten_move_src)
        && let (Some(dest), Some(source)) = (locals.id(dest), locals.id(&name))
        && dest != source
        && !debug_locals.contains(&dest)
    {
        aliases.insert(dest, AliasValue { source, ty });
    }
}

fn eliminate_dead_moves(
    function: &mut Function,
    locals: &LocalInterner,
    debug_locals: &HashSet<usize>,
) {
    loop {
        let (labels, live_out) = block_live_out(function, locals);
        let mut removed_any = false;

        for (block_index, label) in labels.into_iter().enumerate() {
            let Some(block) = function.body.basic_blocks.get_mut(&label) else {
                continue;
            };

            let mut live = live_out[block_index].clone();
            let mut keep = vec![true; block.instructions.len()];

            for (index, instruction) in block.instructions.iter().enumerate().rev() {
                let def = instruction_def(instruction).and_then(|name| locals.id(name));

                if let Instruction::Move { dest, .. } = instruction
                    && let Some(dest) = locals.id(dest)
                    && !bit_set_contains(&live, dest)
                    && !debug_locals.contains(&dest)
                {
                    keep[index] = false;
                    removed_any = true;
                    continue;
                }

                if let Some(def) = def {
                    bit_set_remove(&mut live, def);
                }
                for used in instruction_uses(instruction) {
                    if let Some(used) = locals.id(used) {
                        bit_set_insert(&mut live, used);
                    }
                }
            }

            if keep.iter().any(|keep| !keep) {
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

fn block_live_out(function: &Function, locals: &LocalInterner) -> (Vec<String>, Vec<Vec<u64>>) {
    let mut labels = function
        .body
        .basic_blocks
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    labels.sort();
    let label_ids = labels
        .iter()
        .enumerate()
        .map(|(index, label)| (label.as_str(), index))
        .collect::<HashMap<_, _>>();
    let word_count = locals.len().div_ceil(u64::BITS as usize);
    let mut use_sets = vec![vec![0; word_count]; labels.len()];
    let mut def_sets = vec![vec![0; word_count]; labels.len()];
    let mut successors = vec![Vec::new(); labels.len()];
    let mut predecessors = vec![Vec::new(); labels.len()];

    for (index, label) in labels.iter().enumerate() {
        let Some(block) = function.body.basic_blocks.get(label) else {
            continue;
        };
        for instruction in &block.instructions {
            for used in instruction_uses(instruction) {
                let Some(used) = locals.id(used) else {
                    continue;
                };
                if !bit_set_contains(&def_sets[index], used) {
                    bit_set_insert(&mut use_sets[index], used);
                }
            }
            if let Some(def) = instruction_def(instruction).and_then(|name| locals.id(name)) {
                bit_set_insert(&mut def_sets[index], def);
            }
        }

        for successor in super::reachability::get_block_successors(block) {
            if let Some(successor) = label_ids.get(successor.as_str()).copied() {
                successors[index].push(successor);
                predecessors[successor].push(index);
            }
        }
    }

    let mut live_in = vec![vec![0; word_count]; labels.len()];
    let mut live_out = live_in.clone();
    let mut queue = (0..labels.len()).rev().collect::<VecDeque<_>>();
    let mut queued = vec![true; labels.len()];
    let mut next_out = vec![0; word_count];
    let mut next_in = vec![0; word_count];
    while let Some(index) = queue.pop_front() {
        queued[index] = false;
        next_out.fill(0);
        for successor in successors[index].iter().copied() {
            for (word, successor_word) in next_out.iter_mut().zip(&live_in[successor]) {
                *word |= successor_word;
            }
        }
        for word in 0..word_count {
            next_in[word] = use_sets[index][word] | (next_out[word] & !def_sets[index][word]);
        }
        live_out[index].copy_from_slice(&next_out);
        if live_in[index] != next_in {
            live_in[index].copy_from_slice(&next_in);
            for predecessor in predecessors[index].iter().copied() {
                if !queued[predecessor] {
                    queue.push_back(predecessor);
                    queued[predecessor] = true;
                }
            }
        }
    }

    (labels, live_out)
}

fn bit_set_contains(set: &[u64], value: usize) -> bool {
    set.get(value / u64::BITS as usize)
        .is_some_and(|word| word & (1 << (value % u64::BITS as usize)) != 0)
}

fn bit_set_insert(set: &mut [u64], value: usize) {
    set[value / u64::BITS as usize] |= 1 << (value % u64::BITS as usize);
}

fn bit_set_remove(set: &mut [u64], value: usize) {
    set[value / u64::BITS as usize] &= !(1 << (value % u64::BITS as usize));
}

fn rewrite_instruction_uses(
    instruction: &mut Instruction,
    aliases: &AliasMap,
    locals: &LocalInterner,
) {
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
            rewrite_operand(op1, aliases, locals);
            rewrite_operand(op2, aliases, locals);
        }
        Instruction::Not { src, .. }
        | Instruction::Neg { src, .. }
        | Instruction::Move { src, .. } => rewrite_operand(src, aliases, locals),
        Instruction::Branch { condition, .. } => rewrite_operand(condition, aliases, locals),
        Instruction::Return { operand } => {
            if let Some(operand) = operand {
                rewrite_operand(operand, aliases, locals);
            }
        }
        Instruction::InvokeStatic { args, .. } => {
            rewrite_operands(args, aliases, locals);
        }
        Instruction::CallIndirect {
            function_ptr, args, ..
        } => {
            rewrite_operand(function_ptr, aliases, locals);
            rewrite_operands(args, aliases, locals);
        }
        Instruction::InvokeInterface { operand, args, .. }
        | Instruction::InvokeVirtual { operand, args, .. } => {
            rewrite_operand(operand, aliases, locals);
            rewrite_operands(args, aliases, locals);
        }
        Instruction::Switch { discr, .. } => rewrite_operand(discr, aliases, locals),
        Instruction::NewArray { size, .. } | Instruction::Length { array: size, .. } => {
            rewrite_operand(size, aliases, locals);
        }
        Instruction::ArrayStore {
            array,
            index,
            value,
            ..
        } => {
            rewrite_variable_name(array, aliases, locals);
            rewrite_operand(index, aliases, locals);
            rewrite_operand(value, aliases, locals);
        }
        Instruction::ArrayFill { array, value, .. } => {
            rewrite_variable_name(array, aliases, locals);
            rewrite_operand(value, aliases, locals);
        }
        Instruction::ArrayGet { array, index, .. } => {
            rewrite_operand(array, aliases, locals);
            rewrite_operand(index, aliases, locals);
        }
        Instruction::ConstructObject { args, .. } => {
            for (arg, _) in args {
                rewrite_operand(arg, aliases, locals);
            }
        }
        Instruction::SetField { object, value, .. } => {
            rewrite_variable_name(object, aliases, locals);
            rewrite_operand(value, aliases, locals);
        }
        Instruction::GetField { object, .. } | Instruction::Cast { op: object, .. } => {
            rewrite_operand(object, aliases, locals);
        }
        Instruction::SourceLocation(_)
        | Instruction::LocalVariableScope(_)
        | Instruction::UnwindStart { .. }
        | Instruction::UnwindEnd
        | Instruction::Rethrow
        | Instruction::CreateFunctionPointer { .. }
        | Instruction::Jump { .. }
        | Instruction::ThrowNewWithMessage { .. }
        | Instruction::Label { .. } => {}
    }
}

fn rewrite_operands(operands: &mut [Operand], aliases: &AliasMap, locals: &LocalInterner) {
    for operand in operands {
        rewrite_operand(operand, aliases, locals);
    }
}

fn rewrite_operand(operand: &mut Operand, aliases: &AliasMap, locals: &LocalInterner) {
    let Operand::Variable {
        name,
        ty: expected_ty,
    } = operand
    else {
        return;
    };
    if let Some(alias) = resolve_alias(name, aliases, locals)
        && alias.ty == *expected_ty
    {
        *operand = Operand::Variable {
            name: locals.name(alias.source).to_string(),
            ty: alias.ty,
        };
    }
}

fn rewrite_variable_name(name: &mut String, aliases: &AliasMap, locals: &LocalInterner) {
    let Some(alias) = resolve_alias(name, aliases, locals) else {
        return;
    };
    *name = locals.name(alias.source).to_string();
}

fn resolve_alias(name: &str, aliases: &AliasMap, locals: &LocalInterner) -> Option<AliasValue> {
    let mut current = locals.id(name)?;
    let mut seen = HashSet::new();
    let mut resolved = None;
    while let Some(alias) = aliases.get(&current) {
        if !seen.insert(current) {
            return None;
        }
        resolved = Some(alias.clone());
        current = alias.source;
    }
    resolved
}

fn kill_aliases_touching(local: usize, aliases: &mut AliasMap) {
    let mut invalidated = HashSet::from([local]);
    loop {
        let dependants = aliases
            .iter()
            .filter_map(|(alias_dest, alias_src)| {
                let touches_invalidated =
                    invalidated.contains(alias_dest) || invalidated.contains(&alias_src.source);
                touches_invalidated.then_some(*alias_dest)
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
                0,
                AliasValue {
                    source: 1,
                    ty: slice_ty.clone(),
                },
            ),
            (
                1,
                AliasValue {
                    source: 2,
                    ty: slice_ty,
                },
            ),
        ]);

        kill_aliases_touching(2, &mut aliases);

        assert!(aliases.is_empty());
    }
}

fn instruction_uses(instruction: &Instruction) -> HashSet<&str> {
    let mut uses = HashSet::new();
    collect_instruction_uses(instruction, &mut uses);
    uses
}

fn collect_instruction_uses<'a>(instruction: &'a Instruction, uses: &mut HashSet<&'a str>) {
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
            uses.insert(array);
            collect_operand_use(index, uses);
            collect_operand_use(value, uses);
        }
        Instruction::ArrayFill { array, value, .. } => {
            uses.insert(array);
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
            uses.insert(object);
            collect_operand_use(value, uses);
        }
        Instruction::GetField { object, .. } | Instruction::Cast { op: object, .. } => {
            collect_operand_use(object, uses);
        }
        Instruction::SourceLocation(_)
        | Instruction::LocalVariableScope(_)
        | Instruction::UnwindStart { .. }
        | Instruction::UnwindEnd
        | Instruction::Rethrow
        | Instruction::CreateFunctionPointer { .. }
        | Instruction::Jump { .. }
        | Instruction::ThrowNewWithMessage { .. }
        | Instruction::Label { .. } => {}
    }
}

fn collect_operand_uses<'a>(operands: &'a [Operand], uses: &mut HashSet<&'a str>) {
    for operand in operands {
        collect_operand_use(operand, uses);
    }
}

fn collect_operand_use<'a>(operand: &'a Operand, uses: &mut HashSet<&'a str>) {
    if let Operand::Variable { name, .. } = operand {
        uses.insert(name);
    }
}

fn instruction_def(instruction: &Instruction) -> Option<&str> {
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
        | Instruction::Cast { dest, .. } => Some(dest),
        Instruction::CallIndirect { dest, .. }
        | Instruction::InvokeInterface { dest, .. }
        | Instruction::InvokeVirtual { dest, .. }
        | Instruction::InvokeStatic { dest, .. } => dest.as_deref(),
        Instruction::SourceLocation(_)
        | Instruction::LocalVariableScope(_)
        | Instruction::UnwindStart { .. }
        | Instruction::UnwindEnd
        | Instruction::Rethrow
        | Instruction::Jump { .. }
        | Instruction::Branch { .. }
        | Instruction::Return { .. }
        | Instruction::ThrowNewWithMessage { .. }
        | Instruction::Switch { .. }
        | Instruction::ArrayStore { .. }
        | Instruction::ArrayFill { .. }
        | Instruction::SetField { .. }
        | Instruction::Label { .. } => None,
    }
}
