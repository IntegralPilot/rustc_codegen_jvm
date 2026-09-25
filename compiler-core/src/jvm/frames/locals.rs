use super::*;

pub fn initial_locals_for_descriptor(
    descriptor: &str,
    is_static: bool,
    this_class_name: Option<&str>,
    is_constructor: bool,
) -> jvm::Result<Vec<FrameValue>> {
    let mut locals = Vec::new();
    if !is_static {
        let this_value = if is_constructor {
            FrameValue::UninitializedThis
        } else {
            FrameValue::Object(
                normalize_class_name(this_class_name.unwrap_or("java/lang/Object")).into(),
            )
        };
        push_local_value(&mut locals, this_value);
    }

    let descriptor = jvm::JavaString::from(descriptor);
    let (params, _) = FieldType::parse_method_descriptor(&descriptor)?;
    for param in &params {
        push_local_value(&mut locals, frame_value_from_field_type(param));
    }
    Ok(locals)
}

/// Give verifier-visible defaults to control-flow-guarded locals, especially
/// drop values. Rust never observes them, but the JVM cannot correlate a drop
/// flag with its guarded load and requires a value on every incoming path.
pub fn initialize_locals_loaded_as_top(
    instructions: &mut Vec<Instruction>,
    initial_locals: &[FrameValue],
    local_hints: &[FrameValue],
    max_locals: u16,
    constant_pool: &ConstantPool,
    context: &str,
    exception_table: &mut [ExceptionTableEntry],
) -> jvm::Result<(usize, FrameAnalysis)> {
    if instructions.is_empty() {
        return Ok((
            0,
            FrameAnalysis {
                max_stack: 0,
                block_starts: Vec::new(),
                entry_states: Vec::new(),
            },
        ));
    }

    let locals = locals_loaded_before_definite_store(
        instructions,
        initial_locals,
        max_locals as usize,
        context,
        exception_table,
    )?;
    if locals.is_empty() {
        let analysis = analyze(
            instructions,
            initial_locals,
            local_hints,
            max_locals as usize,
            constant_pool,
            context,
            exception_table,
        )?;
        return Ok((0, analysis));
    }

    let mut prefix = Vec::with_capacity(locals.len() * 2);
    for (local, value) in locals {
        prefix.extend(default_local_initializer(local, &value));
    }
    let prefix_len = u16::try_from(prefix.len()).map_err(|_| jvm::Error::VerificationError {
        context: context.to_string(),
        message: "Verifier local-initialization prefix exceeds the JVM instruction limit"
            .to_string(),
    })?;
    shift_absolute_branch_targets(instructions, prefix_len, context)?;
    instructions.splice(0..0, prefix);
    shift_exception_table(exception_table, prefix_len, context)?;

    let remaining = locals_loaded_before_definite_store(
        instructions,
        initial_locals,
        max_locals as usize,
        context,
        exception_table,
    )?;
    if !remaining.is_empty() {
        return Err(jvm::Error::VerificationError {
            context: context.to_string(),
            message: format!(
                "Local initialization could not resolve verifier Top loads: {remaining:?}"
            ),
        });
    }

    let analysis = analyze(
        instructions,
        initial_locals,
        local_hints,
        max_locals as usize,
        constant_pool,
        context,
        exception_table,
    )?;

    Ok((usize::from(prefix_len), analysis))
}

pub(super) fn shift_exception_table(
    exception_table: &mut [ExceptionTableEntry],
    amount: u16,
    context: &str,
) -> jvm::Result<()> {
    for entry in exception_table {
        entry.range_pc.start = entry.range_pc.start.checked_add(amount).ok_or_else(|| {
            jvm::Error::VerificationError {
                context: context.to_string(),
                message: "exception range start overflowed while inserting a prefix".to_string(),
            }
        })?;
        entry.range_pc.end = entry.range_pc.end.checked_add(amount).ok_or_else(|| {
            jvm::Error::VerificationError {
                context: context.to_string(),
                message: "exception range end overflowed while inserting a prefix".to_string(),
            }
        })?;
        entry.handler_pc =
            entry
                .handler_pc
                .checked_add(amount)
                .ok_or_else(|| jvm::Error::VerificationError {
                    context: context.to_string(),
                    message: "exception handler overflowed while inserting a prefix".to_string(),
                })?;
    }
    Ok(())
}

pub(super) fn loaded_local(instruction: &Instruction) -> Option<(u16, FrameValue)> {
    use Instruction as I;

    let (local, value) = match instruction {
        I::Iload(local) => (u16::from(*local), FrameValue::Integer),
        I::Lload(local) => (u16::from(*local), FrameValue::Long),
        I::Fload(local) => (u16::from(*local), FrameValue::Float),
        I::Dload(local) => (u16::from(*local), FrameValue::Double),
        I::Aload(local) => (
            u16::from(*local),
            FrameValue::Object("java/lang/Object".into()),
        ),
        I::Iload_0 => (0, FrameValue::Integer),
        I::Iload_1 => (1, FrameValue::Integer),
        I::Iload_2 => (2, FrameValue::Integer),
        I::Iload_3 => (3, FrameValue::Integer),
        I::Lload_0 => (0, FrameValue::Long),
        I::Lload_1 => (1, FrameValue::Long),
        I::Lload_2 => (2, FrameValue::Long),
        I::Lload_3 => (3, FrameValue::Long),
        I::Fload_0 => (0, FrameValue::Float),
        I::Fload_1 => (1, FrameValue::Float),
        I::Fload_2 => (2, FrameValue::Float),
        I::Fload_3 => (3, FrameValue::Float),
        I::Dload_0 => (0, FrameValue::Double),
        I::Dload_1 => (1, FrameValue::Double),
        I::Dload_2 => (2, FrameValue::Double),
        I::Dload_3 => (3, FrameValue::Double),
        I::Aload_0 => (0, FrameValue::Object("java/lang/Object".into())),
        I::Aload_1 => (1, FrameValue::Object("java/lang/Object".into())),
        I::Aload_2 => (2, FrameValue::Object("java/lang/Object".into())),
        I::Aload_3 => (3, FrameValue::Object("java/lang/Object".into())),
        I::Iload_w(local) | I::Iinc_w(local, _) => (*local, FrameValue::Integer),
        I::Lload_w(local) => (*local, FrameValue::Long),
        I::Fload_w(local) => (*local, FrameValue::Float),
        I::Dload_w(local) => (*local, FrameValue::Double),
        I::Aload_w(local) => (*local, FrameValue::Object("java/lang/Object".into())),
        I::Iinc(local, _) => (u16::from(*local), FrameValue::Integer),
        _ => return None,
    };
    Some((local, value))
}

pub(super) fn stored_local(instruction: &Instruction) -> Option<u16> {
    use Instruction as I;

    Some(match instruction {
        I::Istore(local)
        | I::Lstore(local)
        | I::Fstore(local)
        | I::Dstore(local)
        | I::Astore(local) => u16::from(*local),
        I::Istore_0 | I::Lstore_0 | I::Fstore_0 | I::Dstore_0 | I::Astore_0 => 0,
        I::Istore_1 | I::Lstore_1 | I::Fstore_1 | I::Dstore_1 | I::Astore_1 => 1,
        I::Istore_2 | I::Lstore_2 | I::Fstore_2 | I::Dstore_2 | I::Astore_2 => 2,
        I::Istore_3 | I::Lstore_3 | I::Fstore_3 | I::Dstore_3 | I::Astore_3 => 3,
        I::Istore_w(local)
        | I::Lstore_w(local)
        | I::Fstore_w(local)
        | I::Dstore_w(local)
        | I::Astore_w(local)
        | I::Iinc_w(local, _) => *local,
        I::Iinc(local, _) => u16::from(*local),
        _ => return None,
    })
}

/// Find locals whose loads are not preceded by a store on every incoming path.
/// This only needs definite-assignment bits; exact verifier types and operand
/// stacks are left to the single typed analysis performed after prefixing.
pub(super) fn locals_loaded_before_definite_store(
    instructions: &[Instruction],
    initial_locals: &[FrameValue],
    max_locals: usize,
    context: &str,
    exception_table: &[ExceptionTableEntry],
) -> jvm::Result<BTreeMap<u16, FrameValue>> {
    if instructions.is_empty() {
        return Ok(BTreeMap::new());
    }
    let (block_starts, block_ends, block_by_instruction) =
        frame_blocks(instructions, exception_table);
    let word_count = max_locals.div_ceil(u64::BITS as usize);
    let mut initial = vec![0u64; word_count];
    for (slot, value) in initial_locals.iter().enumerate().take(max_locals) {
        if *value != FrameValue::Top {
            assignment_insert(&mut initial, slot);
        }
    }

    let mut handlers_by_instruction = vec![Vec::new(); instructions.len()];
    for handler in exception_table {
        let start = usize::from(handler.range_pc.start).min(instructions.len());
        let end = usize::from(handler.range_pc.end).min(instructions.len());
        for handlers in &mut handlers_by_instruction[start..end] {
            let target = usize::from(handler.handler_pc);
            if !handlers.contains(&target) {
                handlers.push(target);
            }
        }
    }

    let mut entries = vec![None; block_starts.len()];
    let mut block_loads = vec![BTreeMap::new(); block_starts.len()];
    entries[0] = Some(initial);
    let mut worklist = VecDeque::from([0usize]);
    let mut queued = vec![false; block_starts.len()];
    queued[0] = true;
    while let Some(block) = worklist.pop_front() {
        queued[block] = false;
        let Some(mut assigned) = entries[block].clone() else {
            continue;
        };
        let mut loads = BTreeMap::new();
        let mut last_handler_assignments = HashMap::<usize, Vec<u64>>::default();
        for index in block_starts[block]..block_ends[block] {
            for &target in &handlers_by_instruction[index] {
                if last_handler_assignments.get(&target) == Some(&assigned) {
                    continue;
                }
                last_handler_assignments.insert(target, assigned.clone());
                merge_assignment_entry(
                    target,
                    &assigned,
                    &block_starts,
                    &block_by_instruction,
                    &mut entries,
                    &mut worklist,
                    &mut queued,
                    context,
                )?;
            }
            if let Some((local, value)) = loaded_local(&instructions[index])
                && !assignment_contains(&assigned, usize::from(local))
                && let Some(existing) = loads.insert(local, value.clone())
                && existing != value
            {
                return Err(jvm::Error::VerificationError {
                    context: context.to_string(),
                    message: format!(
                        "Local {local} is loaded with incompatible types {existing:?} and {value:?}; latest load is instruction {index}"
                    ),
                });
            }
            if let Some(local) = stored_local(&instructions[index]) {
                assignment_insert(&mut assigned, usize::from(local));
            }
        }
        let last = block_ends[block] - 1;
        for target in assignment_successors(last, &instructions[last], context)? {
            if target < instructions.len() {
                merge_assignment_entry(
                    target,
                    &assigned,
                    &block_starts,
                    &block_by_instruction,
                    &mut entries,
                    &mut worklist,
                    &mut queued,
                    context,
                )?;
            }
        }
        block_loads[block] = loads;
    }

    let mut loads = BTreeMap::new();
    for local_loads in block_loads {
        for (local, value) in local_loads {
            if let Some(existing) = loads.insert(local, value.clone())
                && existing != value
            {
                return Err(jvm::Error::VerificationError {
                    context: context.to_string(),
                    message: format!(
                        "Local {local} is loaded with incompatible types {existing:?} and {value:?}"
                    ),
                });
            }
        }
    }
    Ok(loads)
}

pub(super) fn assignment_contains(assignments: &[u64], local: usize) -> bool {
    assignments
        .get(local / u64::BITS as usize)
        .is_some_and(|word| word & (1 << (local % u64::BITS as usize)) != 0)
}

pub(super) fn assignment_insert(assignments: &mut [u64], local: usize) {
    if let Some(word) = assignments.get_mut(local / u64::BITS as usize) {
        *word |= 1 << (local % u64::BITS as usize);
    }
}

pub(super) fn merge_assignment_entry(
    target: usize,
    incoming: &[u64],
    block_starts: &[usize],
    block_by_instruction: &[usize],
    entries: &mut [Option<Vec<u64>>],
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
    let changed = match &mut entries[block] {
        Some(existing) => {
            let previous = existing.clone();
            for (word, incoming) in existing.iter_mut().zip(incoming) {
                *word &= incoming;
            }
            *existing != previous
        }
        slot @ None => {
            *slot = Some(incoming.to_vec());
            true
        }
    };
    if changed && !queued[block] {
        queued[block] = true;
        worklist.push_back(block);
    }
    Ok(())
}

pub(super) fn default_local_initializer(local: u16, value: &FrameValue) -> [Instruction; 2] {
    use Instruction as I;

    let (constant, store) = match value {
        FrameValue::Integer => (I::Iconst_0, local_store(local, I::Istore, I::Istore_w)),
        FrameValue::Long => (I::Lconst_0, local_store(local, I::Lstore, I::Lstore_w)),
        FrameValue::Float => (I::Fconst_0, local_store(local, I::Fstore, I::Fstore_w)),
        FrameValue::Double => (I::Dconst_0, local_store(local, I::Dstore, I::Dstore_w)),
        FrameValue::Null
        | FrameValue::Object(_)
        | FrameValue::UninitializedThis
        | FrameValue::Uninitialized(_) => {
            (I::Aconst_null, local_store(local, I::Astore, I::Astore_w))
        }
        FrameValue::Top => unreachable!("a local load always supplies a concrete JVM type"),
    };
    [constant, store]
}

pub(super) fn local_store(
    local: u16,
    narrow: impl FnOnce(u8) -> Instruction,
    wide: impl FnOnce(u16) -> Instruction,
) -> Instruction {
    u8::try_from(local).map_or_else(|_| wide(local), narrow)
}

pub(super) fn shift_absolute_branch_targets(
    instructions: &mut [Instruction],
    amount: u16,
    context: &str,
) -> jvm::Result<()> {
    for instruction in instructions {
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
            | Instruction::Goto(target)
            | Instruction::Jsr(target)
            | Instruction::Ifnull(target)
            | Instruction::Ifnonnull(target) => {
                *target =
                    target
                        .checked_add(amount)
                        .ok_or_else(|| jvm::Error::VerificationError {
                            context: context.to_string(),
                            message: "Branch target overflow while inserting a method-entry prefix"
                                .to_string(),
                        })?;
            }
            Instruction::Goto_w(target) | Instruction::Jsr_w(target) => {
                *target = target.checked_add(i32::from(amount)).ok_or_else(|| {
                    jvm::Error::VerificationError {
                        context: context.to_string(),
                        message:
                            "Wide branch target overflow while inserting a method-entry prefix"
                                .to_string(),
                    }
                })?;
            }
            // Switch offsets are relative: both source and target move equally.
            _ => {}
        }
    }
    Ok(())
}

pub fn move_zero_branch_target(
    instructions: &mut Vec<Instruction>,
    context: &str,
) -> jvm::Result<bool> {
    if !branch_targets(instructions).contains(&0) {
        return Ok(false);
    }

    shift_absolute_branch_targets(instructions, 1, context)?;
    instructions.insert(0, Instruction::Nop);
    Ok(true)
}

pub fn push_local_value(locals: &mut Vec<FrameValue>, value: FrameValue) {
    let is_category2 = value.is_category2();
    locals.push(value);
    if is_category2 {
        locals.push(FrameValue::Top);
    }
}

pub fn set_slot_value(locals: &mut Vec<FrameValue>, local_index: u16, value: FrameValue) {
    let local_index = local_index as usize;
    let width = if value.is_category2() { 2 } else { 1 };
    if locals.len() < local_index + width {
        locals.resize(local_index + width, FrameValue::Top);
    }
    locals[local_index] = value;
    if width == 2 {
        locals[local_index + 1] = FrameValue::Top;
    }
}
