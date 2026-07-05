use super::stackmaps::FrameValue;
use ristretto_classfile::{self as jvm, attributes::Instruction};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug)]
pub(super) struct Optimise2Result {
    pub instructions: Vec<Instruction>,
    pub max_locals: u16,
    pub local_slot_map: BTreeMap<u16, u16>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LocalKind {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

#[derive(Clone, Copy, Debug)]
struct LocalRef {
    index: u16,
    width: u16,
}

#[derive(Clone, Debug)]
struct LiveRange {
    width: u16,
    first: usize,
    last: usize,
}

#[derive(Debug)]
struct LocalLiveness {
    widths: BTreeMap<u16, u16>,
    uses: Vec<BTreeSet<u16>>,
    defs: Vec<BTreeSet<u16>>,
    live_in: Vec<BTreeSet<u16>>,
    live_out: Vec<BTreeSet<u16>>,
}

pub(super) fn optimise(
    instructions: Vec<Instruction>,
    max_locals: u16,
    fixed_prefix_slots: u16,
) -> jvm::Result<Optimise2Result> {
    // Lower2 sees final JVM control flow, so it can safely do bytecode-level
    // peepholes and local-slot reuse before StackMapTable generation.
    let instructions = fold_boolean_branch_materialization(instructions)?;
    let instructions = remove_redundant_instructions(instructions)?;
    let local_slot_map = allocate_local_slots(&instructions, max_locals, fixed_prefix_slots);
    let (instructions, _) = rewrite_locals(instructions, &local_slot_map);
    let instructions = rewrite_store_load_pairs(instructions);
    let instructions = fold_boolean_zero_comparisons(instructions)?;
    let instructions = fold_stack_boolean_zero_comparisons(instructions)?;
    let instructions = remove_dead_duplicate_stores(instructions)?;
    let instructions = remove_redundant_instructions(instructions)?;
    let max_locals = compute_max_locals(&instructions);
    let max_locals = max_locals.max(fixed_prefix_slots);

    Ok(Optimise2Result {
        instructions,
        max_locals,
        local_slot_map,
    })
}

pub(super) fn remap_frame_values(
    values: &[FrameValue],
    slot_map: &BTreeMap<u16, u16>,
    max_locals: u16,
) -> Vec<FrameValue> {
    let mut remapped: Vec<Option<FrameValue>> = vec![None; max_locals as usize];
    let mut conflicts = vec![false; max_locals as usize];

    for (old_slot, new_slot) in slot_map {
        let Some(value) = values.get(usize::from(*old_slot)).cloned() else {
            continue;
        };
        if value == FrameValue::Top {
            continue;
        }

        let new_slot = usize::from(*new_slot);
        if new_slot >= remapped.len() {
            continue;
        }

        match &remapped[new_slot] {
            Some(existing) if existing != &value => conflicts[new_slot] = true,
            Some(_) => {}
            None => remapped[new_slot] = Some(value),
        }
    }

    let mut values = vec![FrameValue::Top; max_locals as usize];
    for (slot, value) in remapped.into_iter().enumerate() {
        if conflicts[slot] {
            continue;
        }
        let Some(value) = value else {
            continue;
        };
        let width = frame_value_width(&value);
        values[slot] = value;
        if width == 2 && slot + 1 < values.len() {
            values[slot + 1] = FrameValue::Top;
        }
    }
    values
}

fn fold_boolean_branch_materialization(
    mut instructions: Vec<Instruction>,
) -> jvm::Result<Vec<Instruction>> {
    if instructions.len() < 7 {
        return Ok(instructions);
    }

    let liveness = analyze_local_liveness(&instructions);
    let incoming = incoming_branch_sources(&instructions);
    let mut keep = vec![true; instructions.len()];
    let mut index = 0;

    while index + 6 < instructions.len() {
        if !keep[index..=index + 6].iter().all(|keep| *keep) {
            index += 1;
            continue;
        }

        let Some(true_target) = conditional_branch_target(&instructions[index]) else {
            index += 1;
            continue;
        };
        if usize::from(true_target) != index + 3
            || !matches!(instructions[index + 1], Instruction::Iconst_0)
            || !matches!(instructions[index + 3], Instruction::Iconst_1)
        {
            index += 1;
            continue;
        }

        let Instruction::Goto(end_target) = instructions[index + 2] else {
            index += 1;
            continue;
        };
        if usize::from(end_target) != index + 4 {
            index += 1;
            continue;
        }

        let Some((LocalKind::Int, stored_bool)) = local_store(&instructions[index + 4]) else {
            index += 1;
            continue;
        };
        let Some((LocalKind::Int, loaded_bool)) = local_load(&instructions[index + 5]) else {
            index += 1;
            continue;
        };
        if stored_bool.index != loaded_bool.index {
            index += 1;
            continue;
        }

        let Some((branch_on_true, final_target)) = bool_branch_target(&instructions[index + 6])
        else {
            index += 1;
            continue;
        };
        if (index + 1..=index + 6).contains(&usize::from(final_target))
            || liveness.live_out[index + 6].contains(&stored_bool.index)
            || !only_expected_incoming(&incoming, index)
        {
            index += 1;
            continue;
        }

        let replacement = if branch_on_true {
            set_conditional_branch_target(&instructions[index], final_target)
        } else {
            invert_conditional_branch(&instructions[index], final_target)
        }
        .ok_or_else(|| jvm::Error::VerificationError {
            context: "optimise2".to_string(),
            message: format!("Expected conditional branch at instruction {index}"),
        })?;

        instructions[index] = replacement;
        for keep_removed in keep.iter_mut().take(index + 7).skip(index + 1) {
            *keep_removed = false;
        }
        index += 7;
    }

    compact_instructions(instructions, &keep)
}

fn incoming_branch_sources(instructions: &[Instruction]) -> Vec<BTreeSet<usize>> {
    let mut incoming = vec![BTreeSet::new(); instructions.len()];
    for (source, instruction) in instructions.iter().enumerate() {
        for target in branch_targets(instruction) {
            if target >= 0
                && let Some(target_incoming) = incoming.get_mut(target as usize)
            {
                target_incoming.insert(source);
            }
        }
    }
    incoming
}

fn only_expected_incoming(incoming: &[BTreeSet<usize>], pattern_start: usize) -> bool {
    for offset in 1..=6 {
        let target = pattern_start + offset;
        let expected_source = match offset {
            3 => Some(pattern_start),
            4 => Some(pattern_start + 2),
            _ => None,
        };
        let Some(actual_sources) = incoming.get(target) else {
            return false;
        };
        match expected_source {
            Some(source) if actual_sources.len() == 1 && actual_sources.contains(&source) => {}
            None if actual_sources.is_empty() => {}
            _ => return false,
        }
    }
    true
}

fn fold_boolean_zero_comparisons(
    mut instructions: Vec<Instruction>,
) -> jvm::Result<Vec<Instruction>> {
    if instructions.len() < 6 {
        return Ok(instructions);
    }

    let liveness = analyze_local_liveness(&instructions);
    let incoming = incoming_branch_sources(&instructions);
    let mut keep = vec![true; instructions.len()];
    let mut index = 0;

    while index + 5 < instructions.len() {
        if !keep[index] {
            index += 1;
            continue;
        }

        let Some(true_value_target) = conditional_branch_target(&instructions[index]) else {
            index += 1;
            continue;
        };
        if usize::from(true_value_target) != index + 3
            || !matches!(instructions[index + 1], Instruction::Iconst_0)
            || !matches!(instructions[index + 3], Instruction::Iconst_1)
        {
            index += 1;
            continue;
        }

        let Instruction::Goto(join_target) = instructions[index + 2] else {
            index += 1;
            continue;
        };
        if usize::from(join_target) != index + 4 {
            index += 1;
            continue;
        }

        let mut cursor = index + 4;
        let mut stored_locals = BTreeSet::new();
        while cursor + 1 < instructions.len()
            && matches!(instructions[cursor], Instruction::Dup)
            && let Some((LocalKind::Int, local)) = local_store(&instructions[cursor + 1])
        {
            stored_locals.insert(local.index);
            cursor += 2;
        }

        if cursor + 1 >= instructions.len()
            || !matches!(instructions[cursor], Instruction::Iconst_0)
        {
            index += 1;
            continue;
        }

        let Some((branch_when_true, final_target)) =
            bool_zero_compare_target(&instructions[cursor + 1])
        else {
            index += 1;
            continue;
        };
        let branch_index = cursor + 1;
        if (index + 1..=branch_index).contains(&usize::from(final_target))
            || stored_locals
                .iter()
                .any(|local| liveness.live_out[branch_index].contains(local))
            || !only_expected_incoming_for_zero_compare(&incoming, index, branch_index)
        {
            index += 1;
            continue;
        }

        let replacement = if branch_when_true {
            set_conditional_branch_target(&instructions[index], final_target)
        } else {
            invert_conditional_branch(&instructions[index], final_target)
        }
        .ok_or_else(|| jvm::Error::VerificationError {
            context: "optimise2".to_string(),
            message: format!("Expected conditional branch at instruction {index}"),
        })?;

        instructions[index] = replacement;
        for keep_removed in keep.iter_mut().take(branch_index + 1).skip(index + 1) {
            *keep_removed = false;
        }
        index = branch_index + 1;
    }

    compact_instructions(instructions, &keep)
}

fn only_expected_incoming_for_zero_compare(
    incoming: &[BTreeSet<usize>],
    pattern_start: usize,
    pattern_end: usize,
) -> bool {
    for target in pattern_start + 1..=pattern_end {
        let expected_source = if target == pattern_start + 3 {
            Some(pattern_start)
        } else if target == pattern_start + 4 {
            Some(pattern_start + 2)
        } else {
            None
        };
        let Some(actual_sources) = incoming.get(target) else {
            return false;
        };
        match expected_source {
            Some(source) if actual_sources.len() == 1 && actual_sources.contains(&source) => {}
            None if actual_sources.is_empty() => {}
            _ => return false,
        }
    }
    true
}

fn fold_stack_boolean_zero_comparisons(
    mut instructions: Vec<Instruction>,
) -> jvm::Result<Vec<Instruction>> {
    if instructions.len() < 4 {
        return Ok(instructions);
    }

    let liveness = analyze_local_liveness(&instructions);
    let incoming = incoming_branch_sources(&instructions);
    let mut keep = vec![true; instructions.len()];
    let mut index = 0;

    while index + 3 < instructions.len() {
        if !keep[index] {
            index += 1;
            continue;
        }

        let mut cursor = index;
        let mut stores = Vec::new();
        while cursor + 1 < instructions.len()
            && matches!(instructions[cursor], Instruction::Dup)
            && let Some((LocalKind::Int, local)) = local_store(&instructions[cursor + 1])
        {
            stores.push((cursor, cursor + 1, local.index));
            cursor += 2;
        }

        if stores.is_empty()
            || cursor + 1 >= instructions.len()
            || !matches!(instructions[cursor], Instruction::Iconst_0)
        {
            index += 1;
            continue;
        }

        let Some((branch_when_true, final_target)) =
            bool_zero_compare_target(&instructions[cursor + 1])
        else {
            index += 1;
            continue;
        };
        let branch_index = cursor + 1;
        if (index..=branch_index).contains(&usize::from(final_target))
            || !range_has_no_incoming(&incoming, index, branch_index)
        {
            index += 1;
            continue;
        }

        let mut kept_live_stores = BTreeSet::new();
        for (dup_index, store_index, local) in stores.into_iter().rev() {
            if liveness.live_out[branch_index].contains(&local) && kept_live_stores.insert(local) {
                continue;
            }
            keep[dup_index] = false;
            keep[store_index] = false;
        }

        keep[cursor] = false;
        instructions[branch_index] = if branch_when_true {
            Instruction::Ifne(final_target)
        } else {
            Instruction::Ifeq(final_target)
        };
        index = branch_index + 1;
    }

    compact_instructions(instructions, &keep)
}

fn range_has_no_incoming(incoming: &[BTreeSet<usize>], start: usize, end: usize) -> bool {
    (start..=end).all(|target| {
        incoming
            .get(target)
            .is_some_and(|sources| sources.is_empty())
    })
}

fn remove_dead_duplicate_stores(instructions: Vec<Instruction>) -> jvm::Result<Vec<Instruction>> {
    if instructions.len() < 2 {
        return Ok(instructions);
    }

    let liveness = analyze_local_liveness(&instructions);
    let protected = protected_instruction_indices(&instructions);
    let mut keep = vec![true; instructions.len()];
    let mut index = 0;

    while index + 1 < instructions.len() {
        if protected.contains(&index) || protected.contains(&(index + 1)) {
            index += 1;
            continue;
        }

        let Some((store_kind, stored)) = local_store(&instructions[index + 1]) else {
            index += 1;
            continue;
        };
        let duplicate_matches_store = match &instructions[index] {
            Instruction::Dup => local_width(store_kind) == 1,
            Instruction::Dup2 => local_width(store_kind) == 2,
            _ => false,
        };
        if !duplicate_matches_store || liveness.live_out[index + 1].contains(&stored.index) {
            index += 1;
            continue;
        }

        keep[index] = false;
        keep[index + 1] = false;
        index += 2;
    }

    compact_instructions(instructions, &keep)
}

fn remove_redundant_instructions(instructions: Vec<Instruction>) -> jvm::Result<Vec<Instruction>> {
    if instructions.is_empty() {
        return Ok(instructions);
    }

    let protected = protected_instruction_indices(&instructions);
    let mut keep = vec![true; instructions.len()];

    for (index, instruction) in instructions.iter().enumerate() {
        if protected.contains(&index) {
            continue;
        }
        if matches!(instruction, Instruction::Nop)
            || matches!(instruction, Instruction::Goto(target) if usize::from(*target) == index + 1)
            || matches!(instruction, Instruction::Goto_w(target) if *target >= 0 && *target as usize == index + 1)
        {
            keep[index] = false;
        }
    }

    let mut index = 0;
    while index + 1 < instructions.len() {
        if !keep[index]
            || !keep[index + 1]
            || protected.contains(&index)
            || protected.contains(&(index + 1))
        {
            index += 1;
            continue;
        }

        if let Some((load_kind, load)) = local_load(&instructions[index])
            && let Some((store_kind, store)) = local_store(&instructions[index + 1])
            && load.index == store.index
            && load_kind == store_kind
        {
            keep[index] = false;
            keep[index + 1] = false;
            index += 2;
            continue;
        }

        index += 1;
    }

    compact_instructions(instructions, &keep)
}

fn rewrite_store_load_pairs(mut instructions: Vec<Instruction>) -> Vec<Instruction> {
    if instructions.len() < 2 {
        return instructions;
    }

    let protected = protected_instruction_indices(&instructions);
    for index in 0..instructions.len() - 1 {
        if protected.contains(&(index + 1)) {
            continue;
        }

        let Some((store_kind, stored)) = local_store(&instructions[index]) else {
            continue;
        };
        let Some((load_kind, loaded)) = local_load(&instructions[index + 1]) else {
            continue;
        };
        if store_kind != load_kind || stored.index != loaded.index {
            continue;
        }

        instructions[index] = if local_width(store_kind) == 2 {
            Instruction::Dup2
        } else {
            Instruction::Dup
        };
        instructions[index + 1] = make_store(store_kind, stored.index);
    }

    instructions
}

fn compute_max_locals(instructions: &[Instruction]) -> u16 {
    instructions
        .iter()
        .flat_map(|instruction| {
            local_reads(instruction)
                .into_iter()
                .chain(local_writes(instruction))
        })
        .map(|local| local.index + local.width)
        .max()
        .unwrap_or(0)
}

fn protected_instruction_indices(instructions: &[Instruction]) -> BTreeSet<usize> {
    let mut protected = BTreeSet::from([0usize]);
    for instruction in instructions {
        for target in branch_targets(instruction) {
            if target >= 0 {
                protected.insert(target as usize);
            }
        }
    }
    protected
}

fn compact_instructions(
    instructions: Vec<Instruction>,
    keep: &[bool],
) -> jvm::Result<Vec<Instruction>> {
    let mut old_to_new = vec![None; keep.len()];
    let mut next_index = 0usize;
    for (old_index, should_keep) in keep.iter().copied().enumerate() {
        if should_keep {
            old_to_new[old_index] = Some(next_index);
            next_index += 1;
        }
    }

    let mut compacted = Vec::with_capacity(next_index);
    for (old_index, instruction) in instructions.into_iter().enumerate() {
        if !keep[old_index] {
            continue;
        }
        compacted.push(retarget_branches(instruction, &old_to_new)?);
    }
    Ok(compacted)
}

fn retarget_branches(
    instruction: Instruction,
    old_to_new: &[Option<usize>],
) -> jvm::Result<Instruction> {
    use Instruction as I;

    let map_u16 = |target: u16| -> jvm::Result<u16> {
        let target = usize::from(target);
        old_to_new
            .get(target)
            .and_then(|mapped| *mapped)
            .and_then(|mapped| u16::try_from(mapped).ok())
            .ok_or_else(|| jvm::Error::VerificationError {
                context: "optimise2".to_string(),
                message: format!("Removed or invalid branch target {target}"),
            })
    };
    let map_i32 = |target: i32| -> jvm::Result<i32> {
        if target < 0 {
            return Ok(target);
        }
        let target = target as usize;
        old_to_new
            .get(target)
            .and_then(|mapped| *mapped)
            .and_then(|mapped| i32::try_from(mapped).ok())
            .ok_or_else(|| jvm::Error::VerificationError {
                context: "optimise2".to_string(),
                message: format!("Removed or invalid branch target {target}"),
            })
    };

    Ok(match instruction {
        I::Ifeq(target) => I::Ifeq(map_u16(target)?),
        I::Ifne(target) => I::Ifne(map_u16(target)?),
        I::Iflt(target) => I::Iflt(map_u16(target)?),
        I::Ifge(target) => I::Ifge(map_u16(target)?),
        I::Ifgt(target) => I::Ifgt(map_u16(target)?),
        I::Ifle(target) => I::Ifle(map_u16(target)?),
        I::If_icmpeq(target) => I::If_icmpeq(map_u16(target)?),
        I::If_icmpne(target) => I::If_icmpne(map_u16(target)?),
        I::If_icmplt(target) => I::If_icmplt(map_u16(target)?),
        I::If_icmpge(target) => I::If_icmpge(map_u16(target)?),
        I::If_icmpgt(target) => I::If_icmpgt(map_u16(target)?),
        I::If_icmple(target) => I::If_icmple(map_u16(target)?),
        I::If_acmpeq(target) => I::If_acmpeq(map_u16(target)?),
        I::If_acmpne(target) => I::If_acmpne(map_u16(target)?),
        I::Goto(target) => I::Goto(map_u16(target)?),
        I::Jsr(target) => I::Jsr(map_u16(target)?),
        I::Ifnull(target) => I::Ifnull(map_u16(target)?),
        I::Ifnonnull(target) => I::Ifnonnull(map_u16(target)?),
        I::Goto_w(target) => I::Goto_w(map_i32(target)?),
        I::Jsr_w(target) => I::Jsr_w(map_i32(target)?),
        I::Tableswitch {
            default,
            low,
            high,
            mut offsets,
        } => {
            let default = map_i32(default)?;
            for target in &mut offsets {
                *target = map_i32(*target)?;
            }
            I::Tableswitch {
                default,
                low,
                high,
                offsets,
            }
        }
        I::Lookupswitch { default, mut pairs } => {
            let default = map_i32(default)?;
            for target in pairs.values_mut() {
                *target = map_i32(*target)?;
            }
            I::Lookupswitch { default, pairs }
        }
        other => other,
    })
}

fn allocate_local_slots(
    instructions: &[Instruction],
    max_locals: u16,
    fixed_prefix_slots: u16,
) -> BTreeMap<u16, u16> {
    let live_ranges = compute_live_ranges(instructions);
    let mut slot_map = BTreeMap::new();

    for old_slot in 0..fixed_prefix_slots.min(max_locals) {
        slot_map.insert(old_slot, old_slot);
    }

    let mut intervals: Vec<(u16, LiveRange)> = live_ranges
        .into_iter()
        .filter(|(old_slot, _)| *old_slot >= fixed_prefix_slots)
        .collect();
    intervals.sort_by_key(|(old_slot, range)| (range.first, range.last, *old_slot));

    let mut active: Vec<(u16, u16, usize)> = Vec::new();
    for (old_slot, range) in intervals {
        active.retain(|(_, _, last)| *last >= range.first);
        let mut candidate = fixed_prefix_slots;
        while active.iter().any(|(physical_slot, width, _)| {
            ranges_overlap(candidate, range.width, *physical_slot, *width)
        }) {
            candidate += 1;
        }

        slot_map.insert(old_slot, candidate);
        active.push((candidate, range.width, range.last));
    }

    slot_map
}

fn compute_live_ranges(instructions: &[Instruction]) -> BTreeMap<u16, LiveRange> {
    let liveness = analyze_local_liveness(instructions);
    let mut ranges = BTreeMap::new();

    for index in 0..instructions.len() {
        for local in liveness.live_in[index]
            .iter()
            .chain(liveness.live_out[index].iter())
            .chain(liveness.uses[index].iter())
            .chain(liveness.defs[index].iter())
        {
            let width = liveness.widths.get(local).copied().unwrap_or(1);
            ranges
                .entry(*local)
                .and_modify(|range: &mut LiveRange| {
                    range.first = range.first.min(index);
                    range.last = range.last.max(index);
                    range.width = range.width.max(width);
                })
                .or_insert(LiveRange {
                    width,
                    first: index,
                    last: index,
                });
        }
    }

    ranges
}

fn analyze_local_liveness(instructions: &[Instruction]) -> LocalLiveness {
    let mut widths = BTreeMap::new();
    let mut uses = vec![BTreeSet::new(); instructions.len()];
    let mut defs = vec![BTreeSet::new(); instructions.len()];

    for (index, instruction) in instructions.iter().enumerate() {
        for local in local_reads(instruction) {
            widths
                .entry(local.index)
                .and_modify(|width: &mut u16| *width = (*width).max(local.width))
                .or_insert(local.width);
            uses[index].insert(local.index);
        }
        for local in local_writes(instruction) {
            widths
                .entry(local.index)
                .and_modify(|width: &mut u16| *width = (*width).max(local.width))
                .or_insert(local.width);
            defs[index].insert(local.index);
        }
    }

    let mut live_in = vec![BTreeSet::new(); instructions.len()];
    let mut live_out = vec![BTreeSet::new(); instructions.len()];
    let mut changed = true;

    while changed {
        changed = false;
        for index in (0..instructions.len()).rev() {
            let mut next_out = BTreeSet::new();
            for successor in instruction_successors(index, &instructions[index], instructions.len())
            {
                if let Some(successor_live_in) = live_in.get(successor) {
                    next_out.extend(successor_live_in.iter().copied());
                }
            }

            let mut next_in = next_out.clone();
            for local in &defs[index] {
                next_in.remove(local);
            }
            next_in.extend(uses[index].iter().copied());

            if next_in != live_in[index] || next_out != live_out[index] {
                live_in[index] = next_in;
                live_out[index] = next_out;
                changed = true;
            }
        }
    }

    LocalLiveness {
        widths,
        uses,
        defs,
        live_in,
        live_out,
    }
}

fn rewrite_locals(
    instructions: Vec<Instruction>,
    slot_map: &BTreeMap<u16, u16>,
) -> (Vec<Instruction>, u16) {
    let mut max_locals = 0u16;
    let instructions = instructions
        .into_iter()
        .map(|instruction| rewrite_local_instruction(instruction, slot_map, &mut max_locals))
        .collect();
    (instructions, max_locals)
}

fn rewrite_local_instruction(
    instruction: Instruction,
    slot_map: &BTreeMap<u16, u16>,
    max_locals: &mut u16,
) -> Instruction {
    use Instruction as I;

    let mut touch = |index: u16, width: u16| {
        *max_locals = (*max_locals).max(index + width);
    };
    let mapped = |index: u16| slot_map.get(&index).copied().unwrap_or(index);

    match instruction {
        I::Iload(index) => {
            let index = mapped(u16::from(index));
            touch(index, 1);
            make_load(LocalKind::Int, index)
        }
        I::Lload(index) => {
            let index = mapped(u16::from(index));
            touch(index, 2);
            make_load(LocalKind::Long, index)
        }
        I::Fload(index) => {
            let index = mapped(u16::from(index));
            touch(index, 1);
            make_load(LocalKind::Float, index)
        }
        I::Dload(index) => {
            let index = mapped(u16::from(index));
            touch(index, 2);
            make_load(LocalKind::Double, index)
        }
        I::Aload(index) => {
            let index = mapped(u16::from(index));
            touch(index, 1);
            make_load(LocalKind::Reference, index)
        }
        I::Iload_0 => rewrite_fixed_load(LocalKind::Int, 0, slot_map, &mut touch),
        I::Iload_1 => rewrite_fixed_load(LocalKind::Int, 1, slot_map, &mut touch),
        I::Iload_2 => rewrite_fixed_load(LocalKind::Int, 2, slot_map, &mut touch),
        I::Iload_3 => rewrite_fixed_load(LocalKind::Int, 3, slot_map, &mut touch),
        I::Lload_0 => rewrite_fixed_load(LocalKind::Long, 0, slot_map, &mut touch),
        I::Lload_1 => rewrite_fixed_load(LocalKind::Long, 1, slot_map, &mut touch),
        I::Lload_2 => rewrite_fixed_load(LocalKind::Long, 2, slot_map, &mut touch),
        I::Lload_3 => rewrite_fixed_load(LocalKind::Long, 3, slot_map, &mut touch),
        I::Fload_0 => rewrite_fixed_load(LocalKind::Float, 0, slot_map, &mut touch),
        I::Fload_1 => rewrite_fixed_load(LocalKind::Float, 1, slot_map, &mut touch),
        I::Fload_2 => rewrite_fixed_load(LocalKind::Float, 2, slot_map, &mut touch),
        I::Fload_3 => rewrite_fixed_load(LocalKind::Float, 3, slot_map, &mut touch),
        I::Dload_0 => rewrite_fixed_load(LocalKind::Double, 0, slot_map, &mut touch),
        I::Dload_1 => rewrite_fixed_load(LocalKind::Double, 1, slot_map, &mut touch),
        I::Dload_2 => rewrite_fixed_load(LocalKind::Double, 2, slot_map, &mut touch),
        I::Dload_3 => rewrite_fixed_load(LocalKind::Double, 3, slot_map, &mut touch),
        I::Aload_0 => rewrite_fixed_load(LocalKind::Reference, 0, slot_map, &mut touch),
        I::Aload_1 => rewrite_fixed_load(LocalKind::Reference, 1, slot_map, &mut touch),
        I::Aload_2 => rewrite_fixed_load(LocalKind::Reference, 2, slot_map, &mut touch),
        I::Aload_3 => rewrite_fixed_load(LocalKind::Reference, 3, slot_map, &mut touch),
        I::Iload_w(index) => {
            let index = mapped(index);
            touch(index, 1);
            make_load(LocalKind::Int, index)
        }
        I::Lload_w(index) => {
            let index = mapped(index);
            touch(index, 2);
            make_load(LocalKind::Long, index)
        }
        I::Fload_w(index) => {
            let index = mapped(index);
            touch(index, 1);
            make_load(LocalKind::Float, index)
        }
        I::Dload_w(index) => {
            let index = mapped(index);
            touch(index, 2);
            make_load(LocalKind::Double, index)
        }
        I::Aload_w(index) => {
            let index = mapped(index);
            touch(index, 1);
            make_load(LocalKind::Reference, index)
        }

        I::Istore(index) => {
            let index = mapped(u16::from(index));
            touch(index, 1);
            make_store(LocalKind::Int, index)
        }
        I::Lstore(index) => {
            let index = mapped(u16::from(index));
            touch(index, 2);
            make_store(LocalKind::Long, index)
        }
        I::Fstore(index) => {
            let index = mapped(u16::from(index));
            touch(index, 1);
            make_store(LocalKind::Float, index)
        }
        I::Dstore(index) => {
            let index = mapped(u16::from(index));
            touch(index, 2);
            make_store(LocalKind::Double, index)
        }
        I::Astore(index) => {
            let index = mapped(u16::from(index));
            touch(index, 1);
            make_store(LocalKind::Reference, index)
        }
        I::Istore_0 => rewrite_fixed_store(LocalKind::Int, 0, slot_map, &mut touch),
        I::Istore_1 => rewrite_fixed_store(LocalKind::Int, 1, slot_map, &mut touch),
        I::Istore_2 => rewrite_fixed_store(LocalKind::Int, 2, slot_map, &mut touch),
        I::Istore_3 => rewrite_fixed_store(LocalKind::Int, 3, slot_map, &mut touch),
        I::Lstore_0 => rewrite_fixed_store(LocalKind::Long, 0, slot_map, &mut touch),
        I::Lstore_1 => rewrite_fixed_store(LocalKind::Long, 1, slot_map, &mut touch),
        I::Lstore_2 => rewrite_fixed_store(LocalKind::Long, 2, slot_map, &mut touch),
        I::Lstore_3 => rewrite_fixed_store(LocalKind::Long, 3, slot_map, &mut touch),
        I::Fstore_0 => rewrite_fixed_store(LocalKind::Float, 0, slot_map, &mut touch),
        I::Fstore_1 => rewrite_fixed_store(LocalKind::Float, 1, slot_map, &mut touch),
        I::Fstore_2 => rewrite_fixed_store(LocalKind::Float, 2, slot_map, &mut touch),
        I::Fstore_3 => rewrite_fixed_store(LocalKind::Float, 3, slot_map, &mut touch),
        I::Dstore_0 => rewrite_fixed_store(LocalKind::Double, 0, slot_map, &mut touch),
        I::Dstore_1 => rewrite_fixed_store(LocalKind::Double, 1, slot_map, &mut touch),
        I::Dstore_2 => rewrite_fixed_store(LocalKind::Double, 2, slot_map, &mut touch),
        I::Dstore_3 => rewrite_fixed_store(LocalKind::Double, 3, slot_map, &mut touch),
        I::Astore_0 => rewrite_fixed_store(LocalKind::Reference, 0, slot_map, &mut touch),
        I::Astore_1 => rewrite_fixed_store(LocalKind::Reference, 1, slot_map, &mut touch),
        I::Astore_2 => rewrite_fixed_store(LocalKind::Reference, 2, slot_map, &mut touch),
        I::Astore_3 => rewrite_fixed_store(LocalKind::Reference, 3, slot_map, &mut touch),
        I::Istore_w(index) => {
            let index = mapped(index);
            touch(index, 1);
            make_store(LocalKind::Int, index)
        }
        I::Lstore_w(index) => {
            let index = mapped(index);
            touch(index, 2);
            make_store(LocalKind::Long, index)
        }
        I::Fstore_w(index) => {
            let index = mapped(index);
            touch(index, 1);
            make_store(LocalKind::Float, index)
        }
        I::Dstore_w(index) => {
            let index = mapped(index);
            touch(index, 2);
            make_store(LocalKind::Double, index)
        }
        I::Astore_w(index) => {
            let index = mapped(index);
            touch(index, 1);
            make_store(LocalKind::Reference, index)
        }
        I::Iinc(index, amount) => {
            let index = mapped(u16::from(index));
            touch(index, 1);
            make_iinc(index, i16::from(amount))
        }
        I::Iinc_w(index, amount) => {
            let index = mapped(index);
            touch(index, 1);
            make_iinc(index, amount)
        }
        I::Ret(index) => {
            let index = mapped(u16::from(index));
            touch(index, 1);
            make_ret(index)
        }
        I::Ret_w(index) => {
            let index = mapped(index);
            touch(index, 1);
            make_ret(index)
        }
        other => other,
    }
}

fn rewrite_fixed_load<F>(
    kind: LocalKind,
    old_index: u16,
    slot_map: &BTreeMap<u16, u16>,
    touch: &mut F,
) -> Instruction
where
    F: FnMut(u16, u16),
{
    let index = slot_map.get(&old_index).copied().unwrap_or(old_index);
    touch(index, local_width(kind));
    make_load(kind, index)
}

fn rewrite_fixed_store<F>(
    kind: LocalKind,
    old_index: u16,
    slot_map: &BTreeMap<u16, u16>,
    touch: &mut F,
) -> Instruction
where
    F: FnMut(u16, u16),
{
    let index = slot_map.get(&old_index).copied().unwrap_or(old_index);
    touch(index, local_width(kind));
    make_store(kind, index)
}

fn make_load(kind: LocalKind, index: u16) -> Instruction {
    use Instruction as I;

    match (kind, index) {
        (LocalKind::Int, 0) => I::Iload_0,
        (LocalKind::Int, 1) => I::Iload_1,
        (LocalKind::Int, 2) => I::Iload_2,
        (LocalKind::Int, 3) => I::Iload_3,
        (LocalKind::Int, index) if index <= u16::from(u8::MAX) => I::Iload(index as u8),
        (LocalKind::Int, index) => I::Iload_w(index),
        (LocalKind::Long, 0) => I::Lload_0,
        (LocalKind::Long, 1) => I::Lload_1,
        (LocalKind::Long, 2) => I::Lload_2,
        (LocalKind::Long, 3) => I::Lload_3,
        (LocalKind::Long, index) if index <= u16::from(u8::MAX) => I::Lload(index as u8),
        (LocalKind::Long, index) => I::Lload_w(index),
        (LocalKind::Float, 0) => I::Fload_0,
        (LocalKind::Float, 1) => I::Fload_1,
        (LocalKind::Float, 2) => I::Fload_2,
        (LocalKind::Float, 3) => I::Fload_3,
        (LocalKind::Float, index) if index <= u16::from(u8::MAX) => I::Fload(index as u8),
        (LocalKind::Float, index) => I::Fload_w(index),
        (LocalKind::Double, 0) => I::Dload_0,
        (LocalKind::Double, 1) => I::Dload_1,
        (LocalKind::Double, 2) => I::Dload_2,
        (LocalKind::Double, 3) => I::Dload_3,
        (LocalKind::Double, index) if index <= u16::from(u8::MAX) => I::Dload(index as u8),
        (LocalKind::Double, index) => I::Dload_w(index),
        (LocalKind::Reference, 0) => I::Aload_0,
        (LocalKind::Reference, 1) => I::Aload_1,
        (LocalKind::Reference, 2) => I::Aload_2,
        (LocalKind::Reference, 3) => I::Aload_3,
        (LocalKind::Reference, index) if index <= u16::from(u8::MAX) => I::Aload(index as u8),
        (LocalKind::Reference, index) => I::Aload_w(index),
    }
}

fn make_store(kind: LocalKind, index: u16) -> Instruction {
    use Instruction as I;

    match (kind, index) {
        (LocalKind::Int, 0) => I::Istore_0,
        (LocalKind::Int, 1) => I::Istore_1,
        (LocalKind::Int, 2) => I::Istore_2,
        (LocalKind::Int, 3) => I::Istore_3,
        (LocalKind::Int, index) if index <= u16::from(u8::MAX) => I::Istore(index as u8),
        (LocalKind::Int, index) => I::Istore_w(index),
        (LocalKind::Long, 0) => I::Lstore_0,
        (LocalKind::Long, 1) => I::Lstore_1,
        (LocalKind::Long, 2) => I::Lstore_2,
        (LocalKind::Long, 3) => I::Lstore_3,
        (LocalKind::Long, index) if index <= u16::from(u8::MAX) => I::Lstore(index as u8),
        (LocalKind::Long, index) => I::Lstore_w(index),
        (LocalKind::Float, 0) => I::Fstore_0,
        (LocalKind::Float, 1) => I::Fstore_1,
        (LocalKind::Float, 2) => I::Fstore_2,
        (LocalKind::Float, 3) => I::Fstore_3,
        (LocalKind::Float, index) if index <= u16::from(u8::MAX) => I::Fstore(index as u8),
        (LocalKind::Float, index) => I::Fstore_w(index),
        (LocalKind::Double, 0) => I::Dstore_0,
        (LocalKind::Double, 1) => I::Dstore_1,
        (LocalKind::Double, 2) => I::Dstore_2,
        (LocalKind::Double, 3) => I::Dstore_3,
        (LocalKind::Double, index) if index <= u16::from(u8::MAX) => I::Dstore(index as u8),
        (LocalKind::Double, index) => I::Dstore_w(index),
        (LocalKind::Reference, 0) => I::Astore_0,
        (LocalKind::Reference, 1) => I::Astore_1,
        (LocalKind::Reference, 2) => I::Astore_2,
        (LocalKind::Reference, 3) => I::Astore_3,
        (LocalKind::Reference, index) if index <= u16::from(u8::MAX) => I::Astore(index as u8),
        (LocalKind::Reference, index) => I::Astore_w(index),
    }
}

fn make_iinc(index: u16, amount: i16) -> Instruction {
    if index <= u16::from(u8::MAX) && amount >= i16::from(i8::MIN) && amount <= i16::from(i8::MAX) {
        Instruction::Iinc(index as u8, amount as i8)
    } else {
        Instruction::Iinc_w(index, amount)
    }
}

fn make_ret(index: u16) -> Instruction {
    if index <= u16::from(u8::MAX) {
        Instruction::Ret(index as u8)
    } else {
        Instruction::Ret_w(index)
    }
}

fn local_reads(instruction: &Instruction) -> Vec<LocalRef> {
    let mut reads = Vec::new();
    if let Some((_, local)) = local_load(instruction) {
        reads.push(local);
    }
    match instruction {
        Instruction::Iinc(index, _) | Instruction::Ret(index) => reads.push(LocalRef {
            index: u16::from(*index),
            width: 1,
        }),
        Instruction::Iinc_w(index, _) | Instruction::Ret_w(index) => reads.push(LocalRef {
            index: *index,
            width: 1,
        }),
        _ => {}
    }
    reads
}

fn local_writes(instruction: &Instruction) -> Vec<LocalRef> {
    let mut writes = Vec::new();
    if let Some((_, local)) = local_store(instruction) {
        writes.push(local);
    }
    match instruction {
        Instruction::Iinc(index, _) => writes.push(LocalRef {
            index: u16::from(*index),
            width: 1,
        }),
        Instruction::Iinc_w(index, _) => writes.push(LocalRef {
            index: *index,
            width: 1,
        }),
        _ => {}
    }
    writes
}

fn local_load(instruction: &Instruction) -> Option<(LocalKind, LocalRef)> {
    use Instruction as I;

    Some(match instruction {
        I::Iload(index) => local_access(LocalKind::Int, u16::from(*index)),
        I::Lload(index) => local_access(LocalKind::Long, u16::from(*index)),
        I::Fload(index) => local_access(LocalKind::Float, u16::from(*index)),
        I::Dload(index) => local_access(LocalKind::Double, u16::from(*index)),
        I::Aload(index) => local_access(LocalKind::Reference, u16::from(*index)),
        I::Iload_0 => local_access(LocalKind::Int, 0),
        I::Iload_1 => local_access(LocalKind::Int, 1),
        I::Iload_2 => local_access(LocalKind::Int, 2),
        I::Iload_3 => local_access(LocalKind::Int, 3),
        I::Lload_0 => local_access(LocalKind::Long, 0),
        I::Lload_1 => local_access(LocalKind::Long, 1),
        I::Lload_2 => local_access(LocalKind::Long, 2),
        I::Lload_3 => local_access(LocalKind::Long, 3),
        I::Fload_0 => local_access(LocalKind::Float, 0),
        I::Fload_1 => local_access(LocalKind::Float, 1),
        I::Fload_2 => local_access(LocalKind::Float, 2),
        I::Fload_3 => local_access(LocalKind::Float, 3),
        I::Dload_0 => local_access(LocalKind::Double, 0),
        I::Dload_1 => local_access(LocalKind::Double, 1),
        I::Dload_2 => local_access(LocalKind::Double, 2),
        I::Dload_3 => local_access(LocalKind::Double, 3),
        I::Aload_0 => local_access(LocalKind::Reference, 0),
        I::Aload_1 => local_access(LocalKind::Reference, 1),
        I::Aload_2 => local_access(LocalKind::Reference, 2),
        I::Aload_3 => local_access(LocalKind::Reference, 3),
        I::Iload_w(index) => local_access(LocalKind::Int, *index),
        I::Lload_w(index) => local_access(LocalKind::Long, *index),
        I::Fload_w(index) => local_access(LocalKind::Float, *index),
        I::Dload_w(index) => local_access(LocalKind::Double, *index),
        I::Aload_w(index) => local_access(LocalKind::Reference, *index),
        _ => return None,
    })
}

fn local_store(instruction: &Instruction) -> Option<(LocalKind, LocalRef)> {
    use Instruction as I;

    Some(match instruction {
        I::Istore(index) => local_access(LocalKind::Int, u16::from(*index)),
        I::Lstore(index) => local_access(LocalKind::Long, u16::from(*index)),
        I::Fstore(index) => local_access(LocalKind::Float, u16::from(*index)),
        I::Dstore(index) => local_access(LocalKind::Double, u16::from(*index)),
        I::Astore(index) => local_access(LocalKind::Reference, u16::from(*index)),
        I::Istore_0 => local_access(LocalKind::Int, 0),
        I::Istore_1 => local_access(LocalKind::Int, 1),
        I::Istore_2 => local_access(LocalKind::Int, 2),
        I::Istore_3 => local_access(LocalKind::Int, 3),
        I::Lstore_0 => local_access(LocalKind::Long, 0),
        I::Lstore_1 => local_access(LocalKind::Long, 1),
        I::Lstore_2 => local_access(LocalKind::Long, 2),
        I::Lstore_3 => local_access(LocalKind::Long, 3),
        I::Fstore_0 => local_access(LocalKind::Float, 0),
        I::Fstore_1 => local_access(LocalKind::Float, 1),
        I::Fstore_2 => local_access(LocalKind::Float, 2),
        I::Fstore_3 => local_access(LocalKind::Float, 3),
        I::Dstore_0 => local_access(LocalKind::Double, 0),
        I::Dstore_1 => local_access(LocalKind::Double, 1),
        I::Dstore_2 => local_access(LocalKind::Double, 2),
        I::Dstore_3 => local_access(LocalKind::Double, 3),
        I::Astore_0 => local_access(LocalKind::Reference, 0),
        I::Astore_1 => local_access(LocalKind::Reference, 1),
        I::Astore_2 => local_access(LocalKind::Reference, 2),
        I::Astore_3 => local_access(LocalKind::Reference, 3),
        I::Istore_w(index) => local_access(LocalKind::Int, *index),
        I::Lstore_w(index) => local_access(LocalKind::Long, *index),
        I::Fstore_w(index) => local_access(LocalKind::Float, *index),
        I::Dstore_w(index) => local_access(LocalKind::Double, *index),
        I::Astore_w(index) => local_access(LocalKind::Reference, *index),
        _ => return None,
    })
}

fn local_access(kind: LocalKind, index: u16) -> (LocalKind, LocalRef) {
    (
        kind,
        LocalRef {
            index,
            width: local_width(kind),
        },
    )
}

fn local_width(kind: LocalKind) -> u16 {
    match kind {
        LocalKind::Long | LocalKind::Double => 2,
        LocalKind::Int | LocalKind::Float | LocalKind::Reference => 1,
    }
}

fn frame_value_width(value: &FrameValue) -> u16 {
    match value {
        FrameValue::Long | FrameValue::Double => 2,
        _ => 1,
    }
}

fn ranges_overlap(left_start: u16, left_width: u16, right_start: u16, right_width: u16) -> bool {
    let left_start = u32::from(left_start);
    let left_end = left_start + u32::from(left_width);
    let right_start = u32::from(right_start);
    let right_end = right_start + u32::from(right_width);
    left_start < right_end && right_start < left_end
}

fn bool_branch_target(instruction: &Instruction) -> Option<(bool, u16)> {
    match instruction {
        Instruction::Ifne(target) => Some((true, *target)),
        Instruction::Ifeq(target) => Some((false, *target)),
        _ => None,
    }
}

fn bool_zero_compare_target(instruction: &Instruction) -> Option<(bool, u16)> {
    match instruction {
        Instruction::If_icmpne(target) => Some((true, *target)),
        Instruction::If_icmpeq(target) => Some((false, *target)),
        _ => None,
    }
}

fn conditional_branch_target(instruction: &Instruction) -> Option<u16> {
    match instruction {
        Instruction::Ifeq(target)
        | Instruction::Ifne(target)
        | Instruction::Iflt(target)
        | Instruction::Ifge(target)
        | Instruction::Ifgt(target)
        | Instruction::Ifle(target)
        | Instruction::If_icmpeq(target)
        | Instruction::If_icmpne(target)
        | Instruction::If_icmplt(target)
        | Instruction::If_icmpge(target)
        | Instruction::If_icmpgt(target)
        | Instruction::If_icmple(target)
        | Instruction::If_acmpeq(target)
        | Instruction::If_acmpne(target)
        | Instruction::Ifnull(target)
        | Instruction::Ifnonnull(target) => Some(*target),
        _ => None,
    }
}

fn set_conditional_branch_target(instruction: &Instruction, target: u16) -> Option<Instruction> {
    Some(match instruction {
        Instruction::Ifeq(_) => Instruction::Ifeq(target),
        Instruction::Ifne(_) => Instruction::Ifne(target),
        Instruction::Iflt(_) => Instruction::Iflt(target),
        Instruction::Ifge(_) => Instruction::Ifge(target),
        Instruction::Ifgt(_) => Instruction::Ifgt(target),
        Instruction::Ifle(_) => Instruction::Ifle(target),
        Instruction::If_icmpeq(_) => Instruction::If_icmpeq(target),
        Instruction::If_icmpne(_) => Instruction::If_icmpne(target),
        Instruction::If_icmplt(_) => Instruction::If_icmplt(target),
        Instruction::If_icmpge(_) => Instruction::If_icmpge(target),
        Instruction::If_icmpgt(_) => Instruction::If_icmpgt(target),
        Instruction::If_icmple(_) => Instruction::If_icmple(target),
        Instruction::If_acmpeq(_) => Instruction::If_acmpeq(target),
        Instruction::If_acmpne(_) => Instruction::If_acmpne(target),
        Instruction::Ifnull(_) => Instruction::Ifnull(target),
        Instruction::Ifnonnull(_) => Instruction::Ifnonnull(target),
        _ => return None,
    })
}

fn invert_conditional_branch(instruction: &Instruction, target: u16) -> Option<Instruction> {
    Some(match instruction {
        Instruction::Ifeq(_) => Instruction::Ifne(target),
        Instruction::Ifne(_) => Instruction::Ifeq(target),
        Instruction::Iflt(_) => Instruction::Ifge(target),
        Instruction::Ifge(_) => Instruction::Iflt(target),
        Instruction::Ifgt(_) => Instruction::Ifle(target),
        Instruction::Ifle(_) => Instruction::Ifgt(target),
        Instruction::If_icmpeq(_) => Instruction::If_icmpne(target),
        Instruction::If_icmpne(_) => Instruction::If_icmpeq(target),
        Instruction::If_icmplt(_) => Instruction::If_icmpge(target),
        Instruction::If_icmpge(_) => Instruction::If_icmplt(target),
        Instruction::If_icmpgt(_) => Instruction::If_icmple(target),
        Instruction::If_icmple(_) => Instruction::If_icmpgt(target),
        Instruction::If_acmpeq(_) => Instruction::If_acmpne(target),
        Instruction::If_acmpne(_) => Instruction::If_acmpeq(target),
        Instruction::Ifnull(_) => Instruction::Ifnonnull(target),
        Instruction::Ifnonnull(_) => Instruction::Ifnull(target),
        _ => return None,
    })
}

fn instruction_successors(
    index: usize,
    instruction: &Instruction,
    instruction_count: usize,
) -> Vec<usize> {
    use Instruction as I;

    let next = || {
        if index + 1 < instruction_count {
            vec![index + 1]
        } else {
            Vec::new()
        }
    };

    match instruction {
        I::Ifeq(target)
        | I::Ifne(target)
        | I::Iflt(target)
        | I::Ifge(target)
        | I::Ifgt(target)
        | I::Ifle(target)
        | I::If_icmpeq(target)
        | I::If_icmpne(target)
        | I::If_icmplt(target)
        | I::If_icmpge(target)
        | I::If_icmpgt(target)
        | I::If_icmple(target)
        | I::If_acmpeq(target)
        | I::If_acmpne(target)
        | I::Ifnull(target)
        | I::Ifnonnull(target) => {
            let mut successors = vec![usize::from(*target)];
            successors.extend(next());
            successors
        }
        I::Goto(target) => vec![usize::from(*target)],
        I::Goto_w(target) if *target >= 0 => vec![*target as usize],
        I::Tableswitch {
            default, offsets, ..
        } => {
            let mut successors = Vec::with_capacity(offsets.len() + 1);
            if *default >= 0 {
                successors.push(*default as usize);
            }
            for target in offsets {
                if *target >= 0 {
                    successors.push(*target as usize);
                }
            }
            successors
        }
        I::Lookupswitch { default, pairs } => {
            let mut successors = Vec::with_capacity(pairs.len() + 1);
            if *default >= 0 {
                successors.push(*default as usize);
            }
            for target in pairs.values() {
                if *target >= 0 {
                    successors.push(*target as usize);
                }
            }
            successors
        }
        I::Ireturn | I::Lreturn | I::Freturn | I::Dreturn | I::Areturn | I::Return | I::Athrow => {
            Vec::new()
        }
        _ => next(),
    }
}

fn branch_targets(instruction: &Instruction) -> Vec<i32> {
    use Instruction as I;

    match instruction {
        I::Ifeq(target)
        | I::Ifne(target)
        | I::Iflt(target)
        | I::Ifge(target)
        | I::Ifgt(target)
        | I::Ifle(target)
        | I::If_icmpeq(target)
        | I::If_icmpne(target)
        | I::If_icmplt(target)
        | I::If_icmpge(target)
        | I::If_icmpgt(target)
        | I::If_icmple(target)
        | I::If_acmpeq(target)
        | I::If_acmpne(target)
        | I::Goto(target)
        | I::Jsr(target)
        | I::Ifnull(target)
        | I::Ifnonnull(target) => vec![i32::from(*target)],
        I::Goto_w(target) | I::Jsr_w(target) => vec![*target],
        I::Tableswitch {
            default, offsets, ..
        } => {
            let mut targets = Vec::with_capacity(offsets.len() + 1);
            targets.push(*default);
            targets.extend(offsets.iter().copied());
            targets
        }
        I::Lookupswitch { default, pairs } => {
            let mut targets = Vec::with_capacity(pairs.len() + 1);
            targets.push(*default);
            targets.extend(pairs.values().copied());
            targets
        }
        _ => Vec::new(),
    }
}
