use super::*;

pub fn analyze(
    instructions: &[Instruction],
    initial_locals: &[FrameValue],
    local_hints: &[FrameValue],
    max_locals: usize,
    constant_pool: &ConstantPool,
    context: &str,
    exception_table: &[ExceptionTableEntry],
) -> jvm::Result<FrameAnalysis> {
    if instructions.is_empty() {
        return Ok(FrameAnalysis {
            max_stack: 0,
            block_starts: Vec::new(),
            entry_states: Vec::new(),
        });
    }

    let (block_starts, block_ends, block_by_instruction) =
        frame_blocks(instructions, exception_table);
    let mut handlers_by_block = vec![Vec::new(); block_starts.len()];
    for handler in exception_table {
        let start = usize::from(handler.range_pc.start).min(instructions.len());
        let end = usize::from(handler.range_pc.end).min(instructions.len());
        if start >= end {
            continue;
        }
        let first = block_by_instruction[start];
        let last = block_by_instruction[end - 1];
        for handlers in &mut handlers_by_block[first..=last] {
            let target = usize::from(handler.handler_pc);
            if !handlers.contains(&target) {
                handlers.push(target);
            }
        }
    }

    let mut max_stack = 0;
    let mut signatures = SignatureCache::default();
    let mut entry_states = vec![None; block_starts.len()];
    entry_states[0] = Some(FrameState::new(initial_locals.to_vec(), max_locals));
    let mut worklist = VecDeque::from([0usize]);
    let mut queued = vec![false; block_starts.len()];
    queued[0] = true;
    while let Some(block) = worklist.pop_front() {
        queued[block] = false;
        let Some(mut state) = entry_states[block].clone() else {
            continue;
        };

        max_stack = max_stack.max(state.stack_words);
        let mut last_handler_locals = HashMap::<usize, Arc<Vec<FrameValue>>>::default();
        for index in block_starts[block]..block_ends[block] {
            for &target in &handlers_by_block[block] {
                let unchanged = last_handler_locals.get(&target).is_some_and(|locals| {
                    Arc::ptr_eq(locals, &state.locals) || **locals == *state.locals
                });
                if unchanged || target >= instructions.len() {
                    continue;
                }
                last_handler_locals.insert(target, Arc::clone(&state.locals));
                let mut handler_state = state.clone();
                handler_state.stack.clear();
                handler_state.stack_words = 0;
                handler_state.push(FrameValue::Object("java/lang/Throwable".into()));
                merge_block_entry(
                    target,
                    handler_state,
                    &block_starts,
                    &block_by_instruction,
                    &mut entry_states,
                    &mut worklist,
                    &mut queued,
                    context,
                )?;
            }

            transfer_instruction(
                index,
                &instructions[index],
                &mut state,
                local_hints,
                constant_pool,
                context,
                &mut signatures,
            )
            .map_err(|error| jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Stack-map transfer failed at instruction {index} ({}): {error:?}\nInstruction window:\n{}",
                    describe_instruction(&instructions[index], constant_pool),
                    instruction_window(instructions, index, constant_pool),
                ),
            })?;

            max_stack = max_stack.max(state.stack_words);
            if index + 1 == block_ends[block] {
                for target in assignment_successors(index, &instructions[index], context)? {
                    if target >= instructions.len() {
                        continue;
                    }
                    merge_block_entry(
                        target,
                        state.clone(),
                        &block_starts,
                        &block_by_instruction,
                        &mut entry_states,
                        &mut worklist,
                        &mut queued,
                        context,
                    )?;
                }
            }
        }
    }

    Ok(FrameAnalysis {
        max_stack: u16::try_from(max_stack)?,
        block_starts,
        entry_states,
    })
}

pub(super) fn frame_blocks(
    instructions: &[Instruction],
    exception_table: &[ExceptionTableEntry],
) -> (Vec<usize>, Vec<usize>, Vec<usize>) {
    let mut starts = BTreeSet::from([0usize]);
    for target in branch_targets(instructions) {
        if usize::from(target) < instructions.len() {
            starts.insert(usize::from(target));
        }
    }
    for entry in exception_table {
        for boundary in [entry.range_pc.start, entry.range_pc.end, entry.handler_pc] {
            if usize::from(boundary) < instructions.len() {
                starts.insert(usize::from(boundary));
            }
        }
    }
    for (index, instruction) in instructions.iter().enumerate() {
        if instruction_ends_block(instruction) && index + 1 < instructions.len() {
            starts.insert(index + 1);
        }
    }

    let block_starts = starts.into_iter().collect::<Vec<_>>();
    let block_ends = block_starts
        .iter()
        .copied()
        .skip(1)
        .chain(std::iter::once(instructions.len()))
        .collect::<Vec<_>>();
    let mut block_by_instruction = vec![0usize; instructions.len()];
    for (block, (&start, &end)) in block_starts.iter().zip(&block_ends).enumerate() {
        block_by_instruction[start..end].fill(block);
    }
    (block_starts, block_ends, block_by_instruction)
}

pub(super) fn instruction_ends_block(instruction: &Instruction) -> bool {
    crate::jvm::flow::conditional_branch_target(instruction).is_some()
        || !crate::jvm::flow::instruction_can_fall_through(instruction)
}

pub(super) fn merge_block_entry(
    target: usize,
    incoming: FrameState,
    block_starts: &[usize],
    block_by_instruction: &[usize],
    entry_states: &mut [Option<FrameState>],
    worklist: &mut VecDeque<usize>,
    queued: &mut [bool],
    context: &str,
) -> jvm::Result<()> {
    let Some(&block) = block_by_instruction.get(target) else {
        return Ok(());
    };
    if block_starts[block] != target {
        return Err(jvm::Error::VerificationError {
            context: context.to_string(),
            message: format!("Control flow targets the middle of bytecode block at {target}"),
        });
    }
    let changed = match &mut entry_states[block] {
        Some(existing) => merge_state(existing, &incoming),
        slot @ None => {
            *slot = Some(incoming);
            true
        }
    };
    if changed && !queued[block] {
        queued[block] = true;
        worklist.push_back(block);
    }
    Ok(())
}

pub(super) fn merge_state(existing: &mut FrameState, incoming: &FrameState) -> bool {
    let mut changed = false;

    let local_len = existing.locals.len().max(incoming.locals.len());
    let locals = Arc::make_mut(&mut existing.locals);
    locals.resize(local_len, FrameValue::Top);
    for index in 0..local_len {
        let incoming_value = incoming.locals.get(index).unwrap_or(&FrameValue::Top);
        let merged = merge_value(&locals[index], incoming_value);
        if locals[index] != merged {
            locals[index] = merged;
            changed = true;
        }
    }

    if existing.stack.len() != incoming.stack.len() {
        let merged_len = existing.stack.len().min(incoming.stack.len());
        existing.stack.truncate(merged_len);
        changed = true;
    }
    for (existing_value, incoming_value) in existing.stack.iter_mut().zip(&incoming.stack) {
        let merged = merge_value(existing_value, incoming_value);
        if *existing_value != merged {
            *existing_value = merged;
            changed = true;
        }
    }

    existing.stack_words = existing
        .stack
        .iter()
        .map(|v| if v.is_category2() { 2 } else { 1 })
        .sum();
    changed
}

pub(super) fn merge_value(a: &FrameValue, b: &FrameValue) -> FrameValue {
    if a == b {
        return a.clone();
    }
    match (a, b) {
        (FrameValue::Top, _) | (_, FrameValue::Top) => FrameValue::Top,
        (FrameValue::Null, FrameValue::Object(class_name))
        | (FrameValue::Object(class_name), FrameValue::Null) => {
            FrameValue::Object(class_name.clone())
        }
        (FrameValue::Null, FrameValue::Null) => FrameValue::Null,
        (FrameValue::Object(a_class), FrameValue::Object(b_class)) => {
            FrameValue::Object(common_object_class(a_class, b_class).into())
        }
        _ => FrameValue::Top,
    }
}

pub(super) fn common_object_class(a: &str, b: &str) -> String {
    if a == b {
        return a.to_string();
    }
    if a == "java/lang/Object" || b == "java/lang/Object" {
        return "java/lang/Object".to_string();
    }
    if let Some(parent) = nested_parent_class(a) {
        if parent == b {
            return b.to_string();
        }
    }
    if let Some(parent) = nested_parent_class(b) {
        if parent == a {
            return a.to_string();
        }
    }
    if let (Some(a_parent), Some(b_parent)) = (nested_parent_class(a), nested_parent_class(b)) {
        if a_parent == b_parent {
            return a_parent.to_string();
        }
    }
    "java/lang/Object".to_string()
}

pub(super) fn nested_parent_class(class_name: &str) -> Option<&str> {
    if class_name.starts_with('[') {
        return None;
    }
    let (parent, _) = class_name.rsplit_once('$')?;
    if parent.is_empty() {
        None
    } else {
        Some(parent)
    }
}
