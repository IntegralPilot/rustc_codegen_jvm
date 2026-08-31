use super::jvm::{
    self,
    attributes::{ExceptionTableEntry, Instruction},
};
use super::stackmaps::FrameValue;
use crate::oomir::SourceLocation;
use std::{
    cmp::Reverse,
    collections::{BTreeMap, BTreeSet, BinaryHeap, VecDeque},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub(super) struct BytecodeMetadata {
    pub source_location: Option<Rc<SourceLocation>>,
    pub active_variables: Rc<Vec<usize>>,
}

impl Default for BytecodeMetadata {
    fn default() -> Self {
        Self {
            source_location: None,
            active_variables: Rc::new(Vec::new()),
        }
    }
}

#[derive(Debug)]
pub(super) struct Optimise2Result {
    pub instructions: Vec<Instruction>,
    pub metadata: Vec<BytecodeMetadata>,
    pub max_locals: u16,
    pub local_slot_map: BTreeMap<u16, u16>,
    pub metrics: Option<crate::metrics::Optimise2MethodMetrics>,
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
    widths: Vec<u16>,
    uses: Vec<Option<u16>>,
    defs: Vec<Option<u16>>,
    live_in: LocalBitMatrix,
    live_out: LocalBitMatrix,
}

#[derive(Debug, Default)]
enum CompactSuccessors {
    #[default]
    None,
    One(usize),
    Two(usize, usize),
    Many(Vec<usize>),
}

impl CompactSuccessors {
    fn push(&mut self, successor: usize) {
        *self = match std::mem::take(self) {
            Self::None => Self::One(successor),
            Self::One(first) => Self::Two(first, successor),
            Self::Two(first, second) => Self::Many(vec![first, second, successor]),
            Self::Many(mut successors) => {
                successors.push(successor);
                Self::Many(successors)
            }
        };
    }

    fn retain_below(&mut self, upper_bound: usize) {
        *self = match std::mem::take(self) {
            Self::None => Self::None,
            Self::One(first) if first < upper_bound => Self::One(first),
            Self::One(_) => Self::None,
            Self::Two(first, second) => match (first < upper_bound, second < upper_bound) {
                (true, true) => Self::Two(first, second),
                (true, false) => Self::One(first),
                (false, true) => Self::One(second),
                (false, false) => Self::None,
            },
            Self::Many(mut successors) => {
                successors.retain(|successor| *successor < upper_bound);
                match successors.len() {
                    0 => Self::None,
                    1 => Self::One(successors[0]),
                    2 => Self::Two(successors[0], successors[1]),
                    _ => Self::Many(successors),
                }
            }
        };
    }

    fn for_each(&self, mut visitor: impl FnMut(usize)) {
        match self {
            Self::None => {}
            Self::One(first) => visitor(*first),
            Self::Two(first, second) => {
                visitor(*first);
                visitor(*second);
            }
            Self::Many(successors) => successors.iter().copied().for_each(visitor),
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::None => 0,
            Self::One(_) => 1,
            Self::Two(_, _) => 2,
            Self::Many(successors) => successors.len(),
        }
    }
}

#[derive(Debug)]
struct LocalBitMatrix {
    words_per_row: usize,
    words: Vec<u64>,
}

impl LocalBitMatrix {
    fn new(rows: usize, local_count: usize) -> Self {
        let words_per_row = local_count.div_ceil(u64::BITS as usize);
        Self {
            words_per_row,
            words: vec![0; rows.saturating_mul(words_per_row)],
        }
    }

    fn row(&self, index: usize) -> &[u64] {
        let start = index * self.words_per_row;
        &self.words[start..start + self.words_per_row]
    }

    fn row_mut(&mut self, index: usize) -> &mut [u64] {
        let start = index * self.words_per_row;
        &mut self.words[start..start + self.words_per_row]
    }

    fn contains(&self, row: usize, local: u16) -> bool {
        let local = usize::from(local);
        self.row(row)
            .get(local / u64::BITS as usize)
            .is_some_and(|word| word & (1 << (local % u64::BITS as usize)) != 0)
    }

    fn iter(&self, row: usize) -> LocalBitIter<'_> {
        LocalBitIter {
            words: self.row(row),
            word_index: 0,
            remaining: self.row(row).first().copied().unwrap_or(0),
        }
    }
}

struct LocalBitIter<'a> {
    words: &'a [u64],
    word_index: usize,
    remaining: u64,
}

impl Iterator for LocalBitIter<'_> {
    type Item = u16;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.remaining != 0 {
                let bit = self.remaining.trailing_zeros() as usize;
                self.remaining &= self.remaining - 1;
                return u16::try_from(self.word_index * u64::BITS as usize + bit).ok();
            }
            self.word_index += 1;
            self.remaining = *self.words.get(self.word_index)?;
        }
    }
}

impl LocalLiveness {
    fn is_live_out(&self, instruction: usize, local: u16) -> bool {
        self.live_out.contains(instruction, local)
    }
}

type LocatedInstructions = (Vec<Instruction>, Vec<BytecodeMetadata>);

pub(super) fn optimise(
    instructions: Vec<Instruction>,
    source_locations: Vec<BytecodeMetadata>,
    max_locals: u16,
    fixed_prefix_slots: u16,
    pinned_local_slots: &BTreeSet<u16>,
    exception_table: &mut Vec<ExceptionTableEntry>,
) -> jvm::Result<Optimise2Result> {
    // Lower2 sees final JVM control flow, so it can safely do bytecode-level
    // peepholes and local-slot reuse before StackMapTable generation.
    if instructions.len() != source_locations.len() {
        return Err(jvm::Error::VerificationError {
            context: "optimise2".to_string(),
            message: "Instruction/source-location vectors have different lengths".to_string(),
        });
    }
    let mut metrics = crate::metrics::Optimise2MethodMetrics::new(instructions.len(), max_locals);
    let before = instructions.len();
    let (instructions, source_locations) = fold_boolean_branch_materialization(
        instructions,
        source_locations,
        pinned_local_slots,
        exception_table,
        metrics.as_mut(),
    )?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::BooleanMaterialisation,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let (instructions, source_locations) =
        remove_redundant_instructions(instructions, source_locations, exception_table)?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::RedundantInstructionsBefore,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let instructions = thread_jump_targets(instructions)?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::ThreadJumpsBefore,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let (instructions, source_locations) =
        fold_branch_over_goto(instructions, source_locations, exception_table)?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::BranchOverGotoBefore,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let (instructions, source_locations) =
        remove_unreachable_instructions(instructions, source_locations, exception_table)?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::UnreachableBefore,
            before,
            instructions.len(),
        );
    }
    let post_cleanup_baseline = instructions.len();
    let before = instructions.len();
    let (instructions, source_locations) =
        rewrite_store_load_pairs(instructions, source_locations, exception_table);
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::StoreLoadPairs,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let (instructions, source_locations) =
        fold_iinc_patterns(instructions, source_locations, exception_table)?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::IincPatterns,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let (instructions, source_locations) =
        fold_null_branch_comparisons(instructions, source_locations, exception_table)?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::NullComparisons,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let mut liveness_cache = None;
    let (instructions, source_locations) = fold_boolean_zero_comparisons(
        instructions,
        source_locations,
        pinned_local_slots,
        exception_table,
        metrics.as_mut(),
        &mut liveness_cache,
    )?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::BooleanZeroComparisons,
            before,
            instructions.len(),
        );
    }
    if instructions.len() != before {
        liveness_cache = None;
    }
    let before = instructions.len();
    let (instructions, source_locations) = fold_stack_boolean_zero_comparisons(
        instructions,
        source_locations,
        pinned_local_slots,
        exception_table,
        metrics.as_mut(),
        &mut liveness_cache,
    )?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::StackBooleanZeroComparisons,
            before,
            instructions.len(),
        );
    }
    if instructions.len() != before {
        liveness_cache = None;
    }
    let before = instructions.len();
    let (instructions, source_locations) = remove_dead_duplicate_stores(
        instructions,
        source_locations,
        pinned_local_slots,
        exception_table,
        metrics.as_mut(),
        &mut liveness_cache,
    )?;
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::DeadDuplicateStores,
            before,
            instructions.len(),
        );
    }
    if instructions.len() != before {
        liveness_cache = None;
    }
    let post_cleanup_changed = instructions.len() != post_cleanup_baseline;
    let before = instructions.len();
    let instructions = if post_cleanup_changed {
        thread_jump_targets(instructions)?
    } else {
        instructions
    };
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::ThreadJumpsAfter,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let (instructions, source_locations) = if post_cleanup_changed {
        fold_branch_over_goto(instructions, source_locations, exception_table)?
    } else {
        (instructions, source_locations)
    };
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::BranchOverGotoAfter,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let (instructions, source_locations) = if post_cleanup_changed {
        remove_unreachable_instructions(instructions, source_locations, exception_table)?
    } else {
        (instructions, source_locations)
    };
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::UnreachableAfter,
            before,
            instructions.len(),
        );
    }
    let before = instructions.len();
    let (instructions, source_locations) = if post_cleanup_changed {
        remove_redundant_instructions(instructions, source_locations, exception_table)?
    } else {
        (instructions, source_locations)
    };
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::RedundantInstructionsAfter,
            before,
            instructions.len(),
        );
    }
    if post_cleanup_changed {
        liveness_cache = None;
    }
    let local_slot_map = allocate_local_slots(
        &instructions,
        max_locals,
        fixed_prefix_slots,
        pinned_local_slots,
        exception_table,
        metrics.as_mut(),
        &mut liveness_cache,
    );
    let before = instructions.len();
    let (instructions, _) = rewrite_locals(instructions, &local_slot_map);
    if let Some(metrics) = metrics.as_mut() {
        metrics.observe_pass(
            crate::metrics::Optimise2Pass::RewriteLocals,
            before,
            instructions.len(),
        );
    }
    let max_locals = compute_max_locals(&instructions);
    let max_locals = max_locals.max(fixed_prefix_slots);
    if let Some(metrics) = metrics.as_mut() {
        metrics.finish(instructions.len(), max_locals);
    }

    Ok(Optimise2Result {
        instructions,
        metadata: source_locations,
        max_locals,
        local_slot_map,
        metrics,
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
    source_locations: Vec<BytecodeMetadata>,
    pinned_local_slots: &BTreeSet<u16>,
    exception_table: &mut Vec<ExceptionTableEntry>,
    metrics: Option<&mut crate::metrics::Optimise2MethodMetrics>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.len() < 7 || !has_boolean_branch_materialization(&instructions) {
        return Ok((instructions, source_locations));
    }

    let liveness = analyze_local_liveness(&instructions, exception_table, metrics);
    let incoming = incoming_branch_sources(&instructions);
    let protected = protected_instruction_indices(&instructions, exception_table);
    let mut keep = vec![true; instructions.len()];
    let mut index = 0;

    while index + 6 < instructions.len() {
        if !keep[index..=index + 6].iter().all(|keep| *keep) {
            index += 1;
            continue;
        }
        if (index..=index + 6).any(|index| protected.contains(&index)) {
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
        if stored_bool.index != loaded_bool.index || pinned_local_slots.contains(&stored_bool.index)
        {
            index += 1;
            continue;
        }

        let Some((branch_on_true, final_target)) = bool_branch_target(&instructions[index + 6])
        else {
            index += 1;
            continue;
        };
        if (index + 1..=index + 6).contains(&usize::from(final_target))
            || liveness.is_live_out(index + 6, stored_bool.index)
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

    compact_instructions(instructions, source_locations, &keep, exception_table)
}

fn has_boolean_branch_materialization(instructions: &[Instruction]) -> bool {
    instructions.windows(7).any(|window| {
        conditional_branch_target(&window[0]).is_some()
            && matches!(window[1], Instruction::Iconst_0)
            && matches!(window[2], Instruction::Goto(_))
            && matches!(window[3], Instruction::Iconst_1)
            && matches!(local_store(&window[4]), Some((LocalKind::Int, _)))
            && matches!(local_load(&window[5]), Some((LocalKind::Int, _)))
            && bool_branch_target(&window[6]).is_some()
    })
}

fn incoming_branch_sources(instructions: &[Instruction]) -> Vec<BTreeSet<usize>> {
    let mut incoming = vec![BTreeSet::new(); instructions.len()];
    for (source, instruction) in instructions.iter().enumerate() {
        visit_branch_targets(source, instruction, |target| {
            if target >= 0
                && let Some(target_incoming) = incoming.get_mut(target as usize)
            {
                target_incoming.insert(source);
            }
        });
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
    source_locations: Vec<BytecodeMetadata>,
    pinned_local_slots: &BTreeSet<u16>,
    exception_table: &mut Vec<ExceptionTableEntry>,
    metrics: Option<&mut crate::metrics::Optimise2MethodMetrics>,
    liveness_cache: &mut Option<LocalLiveness>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.len() < 6 || !has_boolean_zero_comparison(&instructions) {
        return Ok((instructions, source_locations));
    }

    let liveness = cached_local_liveness(liveness_cache, &instructions, exception_table, metrics);
    let incoming = incoming_branch_sources(&instructions);
    let protected = protected_instruction_indices(&instructions, exception_table);
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
            || stored_locals
                .iter()
                .any(|local| pinned_local_slots.contains(local))
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
        if (index..=branch_index).any(|index| protected.contains(&index))
            || (index + 1..=branch_index).contains(&usize::from(final_target))
            || stored_locals
                .iter()
                .any(|local| liveness.is_live_out(branch_index, *local))
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

    compact_instructions(instructions, source_locations, &keep, exception_table)
}

fn has_boolean_zero_comparison(instructions: &[Instruction]) -> bool {
    instructions.windows(2).any(|window| {
        matches!(window[0], Instruction::Iconst_0) && bool_zero_compare_target(&window[1]).is_some()
    }) && instructions
        .iter()
        .any(|instruction| matches!(instruction, Instruction::Iconst_1))
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
    source_locations: Vec<BytecodeMetadata>,
    pinned_local_slots: &BTreeSet<u16>,
    exception_table: &mut Vec<ExceptionTableEntry>,
    metrics: Option<&mut crate::metrics::Optimise2MethodMetrics>,
    liveness_cache: &mut Option<LocalLiveness>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.len() < 4 || !has_stack_boolean_zero_comparison(&instructions) {
        return Ok((instructions, source_locations));
    }

    let liveness = cached_local_liveness(liveness_cache, &instructions, exception_table, metrics);
    let incoming = incoming_branch_sources(&instructions);
    let protected = protected_instruction_indices(&instructions, exception_table);
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
        if (index..=branch_index).any(|index| protected.contains(&index))
            || (index..=branch_index).contains(&usize::from(final_target))
            || !range_has_no_incoming(&incoming, index, branch_index)
        {
            index += 1;
            continue;
        }

        let mut kept_live_stores = BTreeSet::new();
        for (dup_index, store_index, local) in stores.into_iter().rev() {
            if pinned_local_slots.contains(&local)
                || liveness.is_live_out(branch_index, local) && kept_live_stores.insert(local)
            {
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

    compact_instructions(instructions, source_locations, &keep, exception_table)
}

fn has_stack_boolean_zero_comparison(instructions: &[Instruction]) -> bool {
    instructions.windows(2).any(|window| {
        matches!(window[0], Instruction::Dup)
            && matches!(local_store(&window[1]), Some((LocalKind::Int, _)))
    }) && instructions.windows(2).any(|window| {
        matches!(window[0], Instruction::Iconst_0) && bool_zero_compare_target(&window[1]).is_some()
    })
}

fn range_has_no_incoming(incoming: &[BTreeSet<usize>], start: usize, end: usize) -> bool {
    (start..=end).all(|target| {
        incoming
            .get(target)
            .is_some_and(|sources| sources.is_empty())
    })
}

fn remove_dead_duplicate_stores(
    instructions: Vec<Instruction>,
    source_locations: Vec<BytecodeMetadata>,
    pinned_local_slots: &BTreeSet<u16>,
    exception_table: &mut Vec<ExceptionTableEntry>,
    metrics: Option<&mut crate::metrics::Optimise2MethodMetrics>,
    liveness_cache: &mut Option<LocalLiveness>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.len() < 2 || !has_duplicate_store(&instructions) {
        return Ok((instructions, source_locations));
    }

    let liveness = cached_local_liveness(liveness_cache, &instructions, exception_table, metrics);
    let protected = protected_instruction_indices(&instructions, exception_table);
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
        if !duplicate_matches_store
            || pinned_local_slots.contains(&stored.index)
            || liveness.is_live_out(index + 1, stored.index)
        {
            index += 1;
            continue;
        }

        keep[index] = false;
        keep[index + 1] = false;
        index += 2;
    }

    compact_instructions(instructions, source_locations, &keep, exception_table)
}

fn has_duplicate_store(instructions: &[Instruction]) -> bool {
    instructions.windows(2).any(|window| {
        matches!(window[0], Instruction::Dup | Instruction::Dup2)
            && local_store(&window[1]).is_some()
    })
}

fn remove_redundant_instructions(
    instructions: Vec<Instruction>,
    source_locations: Vec<BytecodeMetadata>,
    exception_table: &mut Vec<ExceptionTableEntry>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.is_empty() || !has_redundant_instruction(&instructions) {
        return Ok((instructions, source_locations));
    }

    let protected = protected_instruction_indices(&instructions, exception_table);
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

    compact_instructions(instructions, source_locations, &keep, exception_table)
}

fn has_redundant_instruction(instructions: &[Instruction]) -> bool {
    instructions.iter().enumerate().any(|(index, instruction)| {
        matches!(instruction, Instruction::Nop)
            || matches!(instruction, Instruction::Goto(target) if usize::from(*target) == index + 1)
            || matches!(instruction, Instruction::Goto_w(target) if *target >= 0 && *target as usize == index + 1)
    }) || instructions.windows(2).any(|window| {
        let Some((load_kind, load)) = local_load(&window[0]) else {
            return false;
        };
        let Some((store_kind, store)) = local_store(&window[1]) else {
            return false;
        };
        load.index == store.index && load_kind == store_kind
    })
}

fn remove_unreachable_instructions(
    instructions: Vec<Instruction>,
    source_locations: Vec<BytecodeMetadata>,
    exception_table: &mut Vec<ExceptionTableEntry>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.is_empty() {
        return Ok((instructions, source_locations));
    }
    // If every instruction before the last one can fall through, the entry's
    // linear path already reaches the complete method. Conditional branches
    // do not invalidate this proof because they retain their fallthrough edge.
    if instructions[..instructions.len() - 1]
        .iter()
        .all(instruction_can_fall_through)
    {
        return Ok((instructions, source_locations));
    }

    let mut reachable = vec![false; instructions.len()];
    let mut stack = vec![0usize];
    stack.extend(
        exception_table
            .iter()
            .map(|entry| usize::from(entry.handler_pc)),
    );
    while let Some(index) = stack.pop() {
        if index >= instructions.len() || reachable[index] {
            continue;
        }

        reachable[index] = true;
        for successor in instruction_successors(index, &instructions[index], instructions.len()) {
            if successor < instructions.len() && !reachable[successor] {
                stack.push(successor);
            }
        }
    }

    if reachable.iter().all(|reachable| *reachable) {
        return Ok((instructions, source_locations));
    }

    compact_instructions(instructions, source_locations, &reachable, exception_table)
}

fn instruction_can_fall_through(instruction: &Instruction) -> bool {
    !matches!(
        instruction,
        Instruction::Goto(_)
            | Instruction::Goto_w(_)
            | Instruction::Tableswitch(_)
            | Instruction::Lookupswitch(_)
            | Instruction::Ireturn
            | Instruction::Lreturn
            | Instruction::Freturn
            | Instruction::Dreturn
            | Instruction::Areturn
            | Instruction::Return
            | Instruction::Athrow
    )
}

fn thread_jump_targets(mut instructions: Vec<Instruction>) -> jvm::Result<Vec<Instruction>> {
    if !has_jump_chain(&instructions) {
        return Ok(instructions);
    }
    for index in 0..instructions.len() {
        let replacement = match &instructions[index] {
            Instruction::Ifeq(target) => Some(Instruction::Ifeq(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Ifne(target) => Some(Instruction::Ifne(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Iflt(target) => Some(Instruction::Iflt(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Ifge(target) => Some(Instruction::Ifge(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Ifgt(target) => Some(Instruction::Ifgt(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Ifle(target) => Some(Instruction::Ifle(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::If_icmpeq(target) => Some(Instruction::If_icmpeq(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::If_icmpne(target) => Some(Instruction::If_icmpne(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::If_icmplt(target) => Some(Instruction::If_icmplt(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::If_icmpge(target) => Some(Instruction::If_icmpge(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::If_icmpgt(target) => Some(Instruction::If_icmpgt(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::If_icmple(target) => Some(Instruction::If_icmple(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::If_acmpeq(target) => Some(Instruction::If_acmpeq(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::If_acmpne(target) => Some(Instruction::If_acmpne(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Goto(target) => Some(Instruction::Goto(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Ifnull(target) => Some(Instruction::Ifnull(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Ifnonnull(target) => Some(Instruction::Ifnonnull(thread_u16_target(
                &instructions,
                *target,
            )?)),
            Instruction::Goto_w(target) => Some(Instruction::Goto_w(thread_i32_target(
                &instructions,
                *target,
            )?)),
            Instruction::Tableswitch(table_switch) => {
                let mut table_switch = table_switch.clone();
                table_switch.default =
                    thread_switch_target(&instructions, index, table_switch.default)?;
                for target in &mut table_switch.offsets {
                    *target = thread_switch_target(&instructions, index, *target)?;
                }
                Some(Instruction::Tableswitch(table_switch))
            }
            Instruction::Lookupswitch(lookup_switch) => {
                let mut lookup_switch = lookup_switch.clone();
                lookup_switch.default =
                    thread_switch_target(&instructions, index, lookup_switch.default)?;
                for target in lookup_switch.pairs.values_mut() {
                    *target = thread_switch_target(&instructions, index, *target)?;
                }
                Some(Instruction::Lookupswitch(lookup_switch))
            }
            _ => None,
        };
        if let Some(replacement) = replacement {
            instructions[index] = replacement;
        }
    }

    Ok(instructions)
}

fn has_jump_chain(instructions: &[Instruction]) -> bool {
    instructions.iter().enumerate().any(|(index, instruction)| {
        let mut found = false;
        visit_branch_targets(index, instruction, |target| {
            found |= target >= 0
                && instructions
                    .get(target as usize)
                    .and_then(goto_target)
                    .is_some();
        });
        found
    })
}

fn fold_branch_over_goto(
    mut instructions: Vec<Instruction>,
    source_locations: Vec<BytecodeMetadata>,
    exception_table: &mut Vec<ExceptionTableEntry>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.len() < 3 || !has_branch_over_goto(&instructions) {
        return Ok((instructions, source_locations));
    }

    let incoming = incoming_branch_sources(&instructions);
    let protected = protected_instruction_indices(&instructions, exception_table);
    let mut keep = vec![true; instructions.len()];
    let mut index = 0;

    while index + 2 < instructions.len() {
        if !keep[index]
            || !keep[index + 1]
            || (index..=index + 2).any(|index| protected.contains(&index))
        {
            index += 1;
            continue;
        }

        let Some(branch_target) = conditional_branch_target(&instructions[index]) else {
            index += 1;
            continue;
        };
        if usize::from(branch_target) != index + 2
            || !incoming
                .get(index + 1)
                .is_some_and(|sources| sources.is_empty())
        {
            index += 1;
            continue;
        }

        let Some(goto_target) = goto_target(&instructions[index + 1]) else {
            index += 1;
            continue;
        };
        if goto_target == index + 2 {
            index += 1;
            continue;
        }
        let Ok(goto_target) = u16::try_from(goto_target) else {
            index += 1;
            continue;
        };

        let Some(replacement) = invert_conditional_branch(&instructions[index], goto_target) else {
            index += 1;
            continue;
        };
        instructions[index] = replacement;
        keep[index + 1] = false;
        index += 2;
    }

    compact_instructions(instructions, source_locations, &keep, exception_table)
}

fn has_branch_over_goto(instructions: &[Instruction]) -> bool {
    instructions.windows(2).enumerate().any(|(index, window)| {
        conditional_branch_target(&window[0]) == u16::try_from(index + 2).ok()
            && goto_target(&window[1]).is_some_and(|target| target != index + 2)
    })
}

fn rewrite_store_load_pairs(
    mut instructions: Vec<Instruction>,
    mut metadata: Vec<BytecodeMetadata>,
    exception_table: &[ExceptionTableEntry],
) -> LocatedInstructions {
    if instructions.len() < 2
        || !instructions.windows(2).any(|window| {
            matches!((local_store(&window[0]), local_load(&window[1])),
            (Some((store_kind, stored)), Some((load_kind, loaded)))
                if store_kind == load_kind && stored.index == loaded.index)
        })
    {
        return (instructions, metadata);
    }

    let protected = protected_instruction_indices(&instructions, exception_table);
    for index in 0..instructions.len() - 1 {
        if protected.contains(&index) || protected.contains(&(index + 1)) {
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
        // The store moves one instruction later, while the duplicate takes
        // the place of the original load. Keep source scopes and locations
        // attached to those semantic operations, not their old positions.
        metadata.swap(index, index + 1);
    }

    (instructions, metadata)
}

fn fold_iinc_patterns(
    mut instructions: Vec<Instruction>,
    source_locations: Vec<BytecodeMetadata>,
    exception_table: &mut Vec<ExceptionTableEntry>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.len() < 4
        || !instructions
            .windows(4)
            .enumerate()
            .any(|(index, _)| iinc_pattern(&instructions, index).is_some())
    {
        return Ok((instructions, source_locations));
    }

    let protected = protected_instruction_indices(&instructions, exception_table);
    let mut keep = vec![true; instructions.len()];
    let mut index = 0;

    while index + 3 < instructions.len() {
        if !keep[index..=index + 3].iter().all(|keep| *keep)
            || protected.contains(&(index + 1))
            || protected.contains(&(index + 2))
            || protected.contains(&(index + 3))
        {
            index += 1;
            continue;
        }

        let Some((local, amount)) = iinc_pattern(&instructions, index) else {
            index += 1;
            continue;
        };

        instructions[index] = if amount == 0 {
            Instruction::Nop
        } else {
            make_iinc(local, amount)
        };
        keep[index + 1] = false;
        keep[index + 2] = false;
        keep[index + 3] = false;
        index += 4;
    }

    compact_instructions(instructions, source_locations, &keep, exception_table)
}

fn fold_null_branch_comparisons(
    mut instructions: Vec<Instruction>,
    source_locations: Vec<BytecodeMetadata>,
    exception_table: &mut Vec<ExceptionTableEntry>,
) -> jvm::Result<LocatedInstructions> {
    if instructions.len() < 3
        || !instructions.windows(3).any(|window| {
            matches!(local_load(&window[0]), Some((LocalKind::Reference, _)))
                && matches!(window[1], Instruction::Aconst_null)
                && matches!(
                    window[2],
                    Instruction::If_acmpeq(_) | Instruction::If_acmpne(_)
                )
        })
    {
        return Ok((instructions, source_locations));
    }

    let protected = protected_instruction_indices(&instructions, exception_table);
    let mut keep = vec![true; instructions.len()];
    let mut index = 0;

    while index + 2 < instructions.len() {
        if !keep[index..=index + 2].iter().all(|keep| *keep)
            || (index..=index + 2).any(|index| protected.contains(&index))
        {
            index += 1;
            continue;
        }

        let is_reference_load = matches!(
            local_load(&instructions[index]),
            Some((LocalKind::Reference, _))
        );
        if !is_reference_load || !matches!(instructions[index + 1], Instruction::Aconst_null) {
            index += 1;
            continue;
        }

        let replacement = match instructions[index + 2] {
            Instruction::If_acmpeq(target) => Some(Instruction::Ifnull(target)),
            Instruction::If_acmpne(target) => Some(Instruction::Ifnonnull(target)),
            _ => None,
        };
        let Some(replacement) = replacement else {
            index += 1;
            continue;
        };

        instructions[index + 2] = replacement;
        keep[index + 1] = false;
        index += 3;
    }

    compact_instructions(instructions, source_locations, &keep, exception_table)
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

fn protected_instruction_indices(
    instructions: &[Instruction],
    exception_table: &[ExceptionTableEntry],
) -> BTreeSet<usize> {
    let mut protected = BTreeSet::from([0usize]);
    for (index, instruction) in instructions.iter().enumerate() {
        visit_branch_targets(index, instruction, |target| {
            if target >= 0 {
                protected.insert(target as usize);
            }
        });
    }
    for entry in exception_table {
        protected.insert(usize::from(entry.range_pc.start));
        if usize::from(entry.range_pc.end) < instructions.len() {
            protected.insert(usize::from(entry.range_pc.end));
        }
        protected.insert(usize::from(entry.handler_pc));
    }
    protected
}

fn compact_instructions(
    instructions: Vec<Instruction>,
    source_locations: Vec<BytecodeMetadata>,
    keep: &[bool],
    exception_table: &mut Vec<ExceptionTableEntry>,
) -> jvm::Result<LocatedInstructions> {
    if keep.iter().all(|keep| *keep) {
        return Ok((instructions, source_locations));
    }

    let mut old_to_new = vec![None; keep.len()];
    let mut next_index = 0usize;
    for (old_index, should_keep) in keep.iter().copied().enumerate() {
        if should_keep {
            old_to_new[old_index] = Some(next_index);
            next_index += 1;
        }
    }

    exception_table.retain(|entry| {
        (usize::from(entry.range_pc.start)..usize::from(entry.range_pc.end))
            .any(|index| keep.get(index).copied().unwrap_or(false))
    });

    let map_exact_boundary = |old: u16| -> jvm::Result<u16> {
        let old = usize::from(old);
        if old == keep.len() {
            return u16::try_from(next_index).map_err(|_| jvm::Error::VerificationError {
                context: "optimise2".to_string(),
                message: "Compacted exception boundary exceeds the JVM limit".to_string(),
            });
        }
        old_to_new
            .get(old)
            .and_then(|mapped| *mapped)
            .and_then(|mapped| u16::try_from(mapped).ok())
            .ok_or_else(|| jvm::Error::VerificationError {
                context: "optimise2".to_string(),
                message: format!("Removed or invalid exception boundary {old}"),
            })
    };
    let map_end_boundary = |old: u16| -> jvm::Result<u16> {
        let old = usize::from(old);
        let mapped = old_to_new
            .iter()
            .skip(old)
            .find_map(|mapped| *mapped)
            .unwrap_or(next_index);
        u16::try_from(mapped).map_err(|_| jvm::Error::VerificationError {
            context: "optimise2".to_string(),
            message: "Compacted exception range end exceeds the JVM limit".to_string(),
        })
    };
    for entry in exception_table.iter_mut() {
        entry.range_pc =
            map_exact_boundary(entry.range_pc.start)?..map_end_boundary(entry.range_pc.end)?;
        entry.handler_pc = map_exact_boundary(entry.handler_pc)?;
    }

    let mut compacted = Vec::with_capacity(next_index);
    let mut compacted_locations = Vec::with_capacity(next_index);
    for (old_index, (instruction, source_location)) in
        instructions.into_iter().zip(source_locations).enumerate()
    {
        if !keep[old_index] {
            continue;
        }
        let new_index = old_to_new[old_index].ok_or_else(|| jvm::Error::VerificationError {
            context: "optimise2".to_string(),
            message: format!("Kept instruction {old_index} has no compacted index"),
        })?;
        compacted.push(retarget_branches(
            instruction,
            old_index,
            new_index,
            &old_to_new,
        )?);
        compacted_locations.push(source_location);
    }
    Ok((compacted, compacted_locations))
}

fn retarget_branches(
    instruction: Instruction,
    old_index: usize,
    new_index: usize,
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
    let map_switch_i32 = |target: i32| -> jvm::Result<i32> {
        let absolute_target = old_index as i64 + i64::from(target);
        if absolute_target < 0 {
            return Err(jvm::Error::VerificationError {
                context: "optimise2".to_string(),
                message: format!("Invalid switch target {target} from instruction {old_index}"),
            });
        }
        let absolute_target = absolute_target as usize;
        let mapped = old_to_new
            .get(absolute_target)
            .and_then(|mapped| *mapped)
            .ok_or_else(|| jvm::Error::VerificationError {
                context: "optimise2".to_string(),
                message: format!("Removed or invalid switch target {absolute_target}"),
            })?;
        i32::try_from(mapped as i64 - new_index as i64).map_err(|_| jvm::Error::VerificationError {
            context: "optimise2".to_string(),
            message: format!("Switch target delta overflow for target {absolute_target}"),
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
        I::Tableswitch(mut table_switch) => {
            table_switch.default = map_switch_i32(table_switch.default)?;
            for target in &mut table_switch.offsets {
                *target = map_switch_i32(*target)?;
            }
            I::Tableswitch(table_switch)
        }
        I::Lookupswitch(mut lookup_switch) => {
            lookup_switch.default = map_switch_i32(lookup_switch.default)?;
            for target in lookup_switch.pairs.values_mut() {
                *target = map_switch_i32(*target)?;
            }
            I::Lookupswitch(lookup_switch)
        }
        other => other,
    })
}

fn allocate_local_slots(
    instructions: &[Instruction],
    max_locals: u16,
    fixed_prefix_slots: u16,
    pinned_local_slots: &BTreeSet<u16>,
    exception_table: &[ExceptionTableEntry],
    metrics: Option<&mut crate::metrics::Optimise2MethodMetrics>,
    liveness_cache: &mut Option<LocalLiveness>,
) -> BTreeMap<u16, u16> {
    let live_ranges = compute_live_ranges(instructions, exception_table, metrics, liveness_cache);
    let mut slot_map = BTreeMap::new();

    for old_slot in 0..fixed_prefix_slots.min(max_locals) {
        slot_map.insert(old_slot, old_slot);
    }

    let mut occupied = vec![false; usize::from(max_locals).saturating_add(2)];
    for slot in 0..fixed_prefix_slots.min(max_locals) {
        occupied[usize::from(slot)] = true;
    }
    for old_slot in pinned_local_slots
        .iter()
        .copied()
        .filter(|slot| *slot >= fixed_prefix_slots)
    {
        let Some(range) = live_ranges
            .get(usize::from(old_slot))
            .and_then(Option::as_ref)
        else {
            continue;
        };
        slot_map.insert(old_slot, old_slot);
        set_slot_occupancy(&mut occupied, old_slot, range.width, true);
    }

    let mut intervals: Vec<(u16, LiveRange)> = live_ranges
        .into_iter()
        .enumerate()
        .filter_map(|(old_slot, range)| {
            let old_slot = u16::try_from(old_slot).ok()?;
            range
                .filter(|_| {
                    old_slot >= fixed_prefix_slots && !pinned_local_slots.contains(&old_slot)
                })
                .map(|range| (old_slot, range))
        })
        .collect();
    intervals.sort_by_key(|(old_slot, range)| (range.first, range.last, *old_slot));

    let mut active = BinaryHeap::<Reverse<(usize, u16, u16)>>::new();
    for (old_slot, range) in intervals {
        while let Some(Reverse((last, physical_slot, width))) = active.peek().copied() {
            if last >= range.first {
                break;
            }
            active.pop();
            set_slot_occupancy(&mut occupied, physical_slot, width, false);
        }

        let candidate = first_available_slot(&mut occupied, fixed_prefix_slots, range.width);
        set_slot_occupancy(&mut occupied, candidate, range.width, true);
        slot_map.insert(old_slot, candidate);
        active.push(Reverse((range.last, candidate, range.width)));
    }

    slot_map
}

fn set_slot_occupancy(occupied: &mut Vec<bool>, slot: u16, width: u16, value: bool) {
    let start = usize::from(slot);
    let end = start + usize::from(width);
    if end > occupied.len() {
        occupied.resize(end, false);
    }
    occupied[start..end].fill(value);
}

fn first_available_slot(occupied: &mut Vec<bool>, first_slot: u16, width: u16) -> u16 {
    let width = usize::from(width);
    let mut candidate = usize::from(first_slot);
    loop {
        let end = candidate + width;
        if end > occupied.len() {
            occupied.resize(end.max(occupied.len().saturating_mul(2)), false);
        }
        match occupied[candidate..end]
            .iter()
            .position(|occupied| *occupied)
        {
            Some(offset) => candidate += offset + 1,
            None => {
                return u16::try_from(candidate)
                    .expect("JVM local-slot allocation exceeded the u16 slot range");
            }
        }
    }
}

fn compute_live_ranges(
    instructions: &[Instruction],
    exception_table: &[ExceptionTableEntry],
    metrics: Option<&mut crate::metrics::Optimise2MethodMetrics>,
    liveness_cache: &mut Option<LocalLiveness>,
) -> Vec<Option<LiveRange>> {
    if !has_backward_control_flow(instructions, exception_table) {
        return compute_linear_live_ranges(instructions);
    }

    let liveness = cached_local_liveness(liveness_cache, instructions, exception_table, metrics);
    let mut ranges: Vec<Option<LiveRange>> = vec![None; liveness.widths.len()];

    for index in 0..instructions.len() {
        for local in liveness
            .live_in
            .iter(index)
            .chain(liveness.live_out.iter(index))
            .chain(liveness.uses[index])
            .chain(liveness.defs[index])
        {
            let width = liveness
                .widths
                .get(usize::from(local))
                .copied()
                .unwrap_or(1);
            let range = &mut ranges[usize::from(local)];
            match range {
                Some(range) => {
                    range.first = range.first.min(index);
                    range.last = range.last.max(index);
                    range.width = range.width.max(width);
                }
                None => {
                    *range = Some(LiveRange {
                        width,
                        first: index,
                        last: index,
                    });
                }
            }
        }
    }

    ranges
}

fn has_backward_control_flow(
    instructions: &[Instruction],
    exception_table: &[ExceptionTableEntry],
) -> bool {
    for (index, instruction) in instructions.iter().enumerate() {
        let mut backward = false;
        visit_instruction_successors(index, instruction, instructions.len(), |successor| {
            backward |= successor <= index;
        });
        if backward {
            return true;
        }
    }
    exception_table.iter().any(|entry| {
        usize::from(entry.handler_pc) <= usize::from(entry.range_pc.end).min(instructions.len())
    })
}

fn compute_linear_live_ranges(instructions: &[Instruction]) -> Vec<Option<LiveRange>> {
    let mut ranges: Vec<Option<LiveRange>> = Vec::new();
    for (index, instruction) in instructions.iter().enumerate() {
        for local in local_reads(instruction)
            .into_iter()
            .chain(local_writes(instruction))
        {
            let slot = usize::from(local.index);
            ranges.resize(ranges.len().max(slot + 1), None);
            match &mut ranges[slot] {
                Some(range) => {
                    range.last = index;
                    range.width = range.width.max(local.width);
                }
                range @ None => {
                    *range = Some(LiveRange {
                        width: local.width,
                        first: index,
                        last: index,
                    });
                }
            }
        }
    }
    ranges
}

fn cached_local_liveness<'a>(
    cache: &'a mut Option<LocalLiveness>,
    instructions: &[Instruction],
    exception_table: &[ExceptionTableEntry],
    metrics: Option<&mut crate::metrics::Optimise2MethodMetrics>,
) -> &'a LocalLiveness {
    if cache.is_none() {
        *cache = Some(analyze_local_liveness(
            instructions,
            exception_table,
            metrics,
        ));
    }
    cache.as_ref().expect("liveness cache was initialized")
}

fn analyze_local_liveness(
    instructions: &[Instruction],
    exception_table: &[ExceptionTableEntry],
    metrics: Option<&mut crate::metrics::Optimise2MethodMetrics>,
) -> LocalLiveness {
    let mut widths = Vec::new();
    let mut uses = vec![None; instructions.len()];
    let mut defs = vec![None; instructions.len()];
    let mut highest_local = None;

    for (index, instruction) in instructions.iter().enumerate() {
        if let Some(local) = local_reads(instruction) {
            let required_len = usize::from(local.index) + 1;
            widths.resize(widths.len().max(required_len), 1);
            widths[usize::from(local.index)] = widths[usize::from(local.index)].max(local.width);
            uses[index] = Some(local.index);
            highest_local = Some(highest_local.unwrap_or(0).max(local.index));
        }
        if let Some(local) = local_writes(instruction) {
            let required_len = usize::from(local.index) + 1;
            widths.resize(widths.len().max(required_len), 1);
            widths[usize::from(local.index)] = widths[usize::from(local.index)].max(local.width);
            defs[index] = Some(local.index);
            highest_local = Some(highest_local.unwrap_or(0).max(local.index));
        }
    }

    let local_count = highest_local.map_or(0, |local| usize::from(local) + 1);
    let mut live_in = LocalBitMatrix::new(instructions.len(), local_count);
    let mut live_out = LocalBitMatrix::new(instructions.len(), local_count);
    let mut successors = instructions
        .iter()
        .enumerate()
        .map(|(index, instruction)| {
            let mut successors = CompactSuccessors::default();
            visit_instruction_successors(index, instruction, instructions.len(), |successor| {
                successors.push(successor);
            });
            successors
        })
        .collect::<Vec<_>>();
    for entry in exception_table {
        let handler = usize::from(entry.handler_pc);
        if handler >= instructions.len() {
            continue;
        }
        let end = usize::from(entry.range_pc.end).min(instructions.len());
        for instruction_successors in successors
            .iter_mut()
            .take(end)
            .skip(usize::from(entry.range_pc.start).min(end))
        {
            instruction_successors.push(handler);
        }
    }
    let mut predecessors = vec![Vec::new(); instructions.len()];
    let mut successor_edges = 0usize;
    for (index, instruction_successors) in successors.iter_mut().enumerate() {
        instruction_successors.retain_below(instructions.len());
        successor_edges += instruction_successors.len();
        instruction_successors.for_each(|successor| {
            predecessors[successor].push(index);
        });
    }

    let mut queue = (0..instructions.len()).rev().collect::<VecDeque<_>>();
    let mut queued = vec![true; instructions.len()];
    let mut next_out = vec![0; live_in.words_per_row];
    let mut next_in = vec![0; live_in.words_per_row];
    let mut worklist_pops = 0usize;
    while let Some(index) = queue.pop_front() {
        worklist_pops += 1;
        queued[index] = false;
        next_out.fill(0);
        successors[index].for_each(|successor| {
            for (word, successor_word) in next_out.iter_mut().zip(live_in.row(successor)) {
                *word |= successor_word;
            }
        });
        next_in.copy_from_slice(&next_out);
        if let Some(local) = defs[index] {
            let local = usize::from(local);
            next_in[local / u64::BITS as usize] &= !(1 << (local % u64::BITS as usize));
        }
        if let Some(local) = uses[index] {
            let local = usize::from(local);
            next_in[local / u64::BITS as usize] |= 1 << (local % u64::BITS as usize);
        }

        let in_changed = live_in.row(index) != next_in;
        if live_out.row(index) != next_out {
            live_out.row_mut(index).copy_from_slice(&next_out);
        }
        if in_changed {
            live_in.row_mut(index).copy_from_slice(&next_in);
            for predecessor in predecessors[index].iter().copied() {
                if !queued[predecessor] {
                    queue.push_back(predecessor);
                    queued[predecessor] = true;
                }
            }
        }
    }

    if let Some(metrics) = metrics {
        metrics.record_liveness(
            instructions.len(),
            local_count,
            live_in.words.len().saturating_add(live_out.words.len()),
            successor_edges,
            worklist_pops,
        );
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

fn iinc_pattern(instructions: &[Instruction], index: usize) -> Option<(u16, i16)> {
    let Some((LocalKind::Int, stored)) = local_store(&instructions[index + 3]) else {
        return None;
    };

    let (loaded, amount) = match instructions[index + 2] {
        Instruction::Iadd => {
            if let Some((LocalKind::Int, loaded)) = local_load(&instructions[index]) {
                (loaded, int_constant(&instructions[index + 1])?)
            } else if let Some((LocalKind::Int, loaded)) = local_load(&instructions[index + 1]) {
                (loaded, int_constant(&instructions[index])?)
            } else {
                return None;
            }
        }
        Instruction::Isub => {
            let Some((LocalKind::Int, loaded)) = local_load(&instructions[index]) else {
                return None;
            };
            let amount = int_constant(&instructions[index + 1])?.checked_neg()?;
            (loaded, amount)
        }
        _ => return None,
    };
    if loaded.index != stored.index {
        return None;
    }

    i16::try_from(amount)
        .ok()
        .map(|amount| (loaded.index, amount))
}

fn int_constant(instruction: &Instruction) -> Option<i32> {
    Some(match instruction {
        Instruction::Iconst_m1 => -1,
        Instruction::Iconst_0 => 0,
        Instruction::Iconst_1 => 1,
        Instruction::Iconst_2 => 2,
        Instruction::Iconst_3 => 3,
        Instruction::Iconst_4 => 4,
        Instruction::Iconst_5 => 5,
        Instruction::Bipush(value) => i32::from(*value),
        Instruction::Sipush(value) => i32::from(*value),
        _ => return None,
    })
}

fn goto_target(instruction: &Instruction) -> Option<usize> {
    match instruction {
        Instruction::Goto(target) => Some(usize::from(*target)),
        Instruction::Goto_w(target) if *target >= 0 => Some(*target as usize),
        _ => None,
    }
}

fn thread_u16_target(instructions: &[Instruction], target: u16) -> jvm::Result<u16> {
    let target = resolve_goto_chain(instructions, usize::from(target));
    u16::try_from(target).map_err(|_| jvm::Error::VerificationError {
        context: "optimise2".to_string(),
        message: format!("Threaded branch target {target} exceeds u16 range"),
    })
}

fn thread_i32_target(instructions: &[Instruction], target: i32) -> jvm::Result<i32> {
    if target < 0 {
        return Ok(target);
    }

    let target = resolve_goto_chain(instructions, target as usize);
    i32::try_from(target).map_err(|_| jvm::Error::VerificationError {
        context: "optimise2".to_string(),
        message: format!("Threaded wide branch target {target} exceeds i32 range"),
    })
}

fn thread_switch_target(
    instructions: &[Instruction],
    source_index: usize,
    target: i32,
) -> jvm::Result<i32> {
    let absolute_target = source_index as i64 + i64::from(target);
    if absolute_target < 0 {
        return Err(jvm::Error::VerificationError {
            context: "optimise2".to_string(),
            message: format!("Invalid switch target {target} from instruction {source_index}"),
        });
    }

    let threaded = resolve_goto_chain(instructions, absolute_target as usize);
    i32::try_from(threaded as i64 - source_index as i64).map_err(|_| {
        jvm::Error::VerificationError {
            context: "optimise2".to_string(),
            message: format!("Threaded switch target {threaded} exceeds i32 delta range"),
        }
    })
}

fn resolve_goto_chain(instructions: &[Instruction], target: usize) -> usize {
    let mut current = target;
    // A chain cannot visit more instructions without either ending or
    // cycling. Bounding the walk avoids allocating a set for every branch.
    for _ in 0..instructions.len() {
        let Some(next) = instructions.get(current).and_then(goto_target) else {
            break;
        };
        if next == current {
            break;
        }
        current = next;
    }

    current
}

fn local_reads(instruction: &Instruction) -> Option<LocalRef> {
    if let Some((_, local)) = local_load(instruction) {
        return Some(local);
    }
    match instruction {
        Instruction::Iinc(index, _) | Instruction::Ret(index) => Some(LocalRef {
            index: u16::from(*index),
            width: 1,
        }),
        Instruction::Iinc_w(index, _) | Instruction::Ret_w(index) => Some(LocalRef {
            index: *index,
            width: 1,
        }),
        _ => None,
    }
}

fn local_writes(instruction: &Instruction) -> Option<LocalRef> {
    if let Some((_, local)) = local_store(instruction) {
        return Some(local);
    }
    match instruction {
        Instruction::Iinc(index, _) => Some(LocalRef {
            index: u16::from(*index),
            width: 1,
        }),
        Instruction::Iinc_w(index, _) => Some(LocalRef {
            index: *index,
            width: 1,
        }),
        _ => None,
    }
}

pub(super) fn instruction_uses_local(instruction: &Instruction, index: u16) -> bool {
    local_reads(instruction)
        .into_iter()
        .chain(local_writes(instruction))
        .any(|local| local.index == index)
}

pub(super) fn instruction_writes_local(instruction: &Instruction, index: u16) -> bool {
    local_writes(instruction)
        .into_iter()
        .any(|local| local.index == index)
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

pub(super) fn instruction_successors(
    index: usize,
    instruction: &Instruction,
    instruction_count: usize,
) -> Vec<usize> {
    let mut successors = Vec::new();
    visit_instruction_successors(index, instruction, instruction_count, |successor| {
        successors.push(successor);
    });
    successors
}

fn visit_instruction_successors(
    index: usize,
    instruction: &Instruction,
    instruction_count: usize,
    mut visitor: impl FnMut(usize),
) {
    use Instruction as I;

    let next = (index + 1 < instruction_count).then_some(index + 1);

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
            visitor(usize::from(*target));
            if let Some(next) = next {
                visitor(next);
            }
        }
        I::Goto(target) => visitor(usize::from(*target)),
        I::Goto_w(target) if *target >= 0 => visitor(*target as usize),
        I::Tableswitch(table_switch) => {
            let default = index as i32 + table_switch.default;
            if default >= 0 {
                visitor(default as usize);
            }
            for target in &table_switch.offsets {
                let target = index as i32 + *target;
                if target >= 0 {
                    visitor(target as usize);
                }
            }
        }
        I::Lookupswitch(lookup_switch) => {
            let default = index as i32 + lookup_switch.default;
            if default >= 0 {
                visitor(default as usize);
            }
            for target in lookup_switch.pairs.values() {
                let target = index as i32 + *target;
                if target >= 0 {
                    visitor(target as usize);
                }
            }
        }
        I::Ireturn | I::Lreturn | I::Freturn | I::Dreturn | I::Areturn | I::Return | I::Athrow => {}
        _ => {
            if let Some(next) = next {
                visitor(next);
            }
        }
    }
}

fn visit_branch_targets(index: usize, instruction: &Instruction, mut visitor: impl FnMut(i32)) {
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
        | I::Ifnonnull(target) => visitor(i32::from(*target)),
        I::Goto_w(target) | I::Jsr_w(target) => visitor(*target),
        I::Tableswitch(table_switch) => {
            visitor(index as i32 + table_switch.default);
            for target in &table_switch.offsets {
                visitor(index as i32 + *target);
            }
        }
        I::Lookupswitch(lookup_switch) => {
            visitor(index as i32 + lookup_switch.default);
            for target in lookup_switch.pairs.values() {
                visitor(index as i32 + *target);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_bit_matrix_tracks_dense_and_sparse_slots() {
        let mut matrix = LocalBitMatrix::new(2, 130);
        matrix.row_mut(0)[0] |= 1;
        matrix.row_mut(0)[1] |= 1 << 6;
        matrix.row_mut(1)[2] |= 1 << 1;

        assert!(matrix.contains(0, 0));
        assert!(matrix.contains(0, 70));
        assert!(matrix.contains(1, 129));
        assert_eq!(matrix.iter(0).collect::<Vec<_>>(), vec![0, 70]);
        assert!(!matrix.contains(1, 70));
    }

    #[test]
    fn local_slot_allocator_reuses_only_non_overlapping_ranges() {
        let sequential = vec![
            Instruction::Istore(4),
            Instruction::Iload(4),
            Instruction::Pop,
            Instruction::Istore(5),
            Instruction::Iload(5),
            Instruction::Pop,
            Instruction::Return,
        ];
        let map = allocate_local_slots(&sequential, 6, 0, &BTreeSet::new(), &[], None, &mut None);
        assert_eq!(map.get(&4), Some(&0));
        assert_eq!(map.get(&5), Some(&0));

        let overlapping = vec![
            Instruction::Istore(4),
            Instruction::Istore(5),
            Instruction::Iload(4),
            Instruction::Pop,
            Instruction::Iload(5),
            Instruction::Pop,
            Instruction::Return,
        ];
        let map = allocate_local_slots(&overlapping, 6, 0, &BTreeSet::new(), &[], None, &mut None);
        assert_eq!(map.get(&4), Some(&0));
        assert_eq!(map.get(&5), Some(&1));
    }

    #[test]
    fn local_slot_allocator_respects_widths_and_pinned_slots() {
        let instructions = vec![
            Instruction::Lstore(10),
            Instruction::Istore(12),
            Instruction::Istore(14),
            Instruction::Lload(10),
            Instruction::Pop2,
            Instruction::Iload(12),
            Instruction::Pop,
            Instruction::Iload(14),
            Instruction::Pop,
            Instruction::Return,
        ];
        let pinned = BTreeSet::from([12]);
        let map = allocate_local_slots(&instructions, 15, 0, &pinned, &[], None, &mut None);

        assert_eq!(map.get(&10), Some(&0));
        assert_eq!(map.get(&12), Some(&12));
        assert_eq!(map.get(&14), Some(&2));
    }
}
