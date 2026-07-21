use super::constant_pool::InternedConstantPool;
use super::jvm::{
    self, BaseType, Constant, ConstantPool, FieldType,
    attributes::{
        ArrayType, Attribute, ExceptionTableEntry, Instruction, StackFrame, VerificationType,
    },
};
use crate::oomir::{self, Type};
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum FrameValue {
    Top,
    Integer,
    Float,
    Long,
    Double,
    Null,
    Object(Arc<str>),
    UninitializedThis,
    Uninitialized(u16),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FrameState {
    locals: Arc<Vec<FrameValue>>,
    stack: Vec<FrameValue>,
}

pub(super) struct FrameAnalysis {
    block_starts: Vec<usize>,
    entry_states: Vec<Option<FrameState>>,
}

impl FrameAnalysis {
    fn state_at(&self, instruction: usize) -> Option<&FrameState> {
        let block = self.block_starts.binary_search(&instruction).ok()?;
        self.entry_states.get(block)?.as_ref()
    }
}

impl FrameValue {
    fn is_category2(&self) -> bool {
        matches!(self, FrameValue::Long | FrameValue::Double)
    }

    fn is_reference_like(&self) -> bool {
        matches!(
            self,
            FrameValue::Null
                | FrameValue::Object(_)
                | FrameValue::UninitializedThis
                | FrameValue::Uninitialized(_)
        )
    }
}

impl FrameState {
    fn new(initial_locals: Vec<FrameValue>, max_locals: usize) -> Self {
        let mut locals = initial_locals;
        locals.resize(max_locals, FrameValue::Top);
        Self {
            locals: Arc::new(locals),
            stack: Vec::new(),
        }
    }

    fn pop(&mut self, context: &str, instruction_index: usize) -> jvm::Result<FrameValue> {
        self.stack
            .pop()
            .ok_or_else(|| jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!("Stack underflow at instruction {instruction_index}"),
            })
    }

    fn pop_category1(
        &mut self,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<FrameValue> {
        let value = self.pop(context, instruction_index)?;
        if value.is_category2() {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Expected category-1 value at instruction {instruction_index}, found {value:?}"
                ),
            });
        }
        Ok(value)
    }

    fn pop_reference(
        &mut self,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<FrameValue> {
        let value = self.pop_category1(context, instruction_index)?;
        if !value.is_reference_like() && value != FrameValue::Top {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Expected reference value at instruction {instruction_index}, found {value:?}"
                ),
            });
        }
        Ok(value)
    }

    fn load_local(
        &mut self,
        index: u16,
        local_hints: &[FrameValue],
        load_hint: FrameValue,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<()> {
        let mut value = self
            .locals
            .get(index as usize)
            .cloned()
            .unwrap_or(FrameValue::Top);
        if value == FrameValue::Top {
            value = local_hints
                .get(index as usize)
                .cloned()
                .unwrap_or(FrameValue::Top);
            if value == FrameValue::Top {
                value = load_hint;
                if value == FrameValue::Top {
                    return Err(jvm::Error::VerificationError {
                        context: context.to_string(),
                        message: format!(
                            "Loaded uninitialized local {index} at instruction {instruction_index}"
                        ),
                    });
                }
            }
            self.store_local(index, value.clone());
        }
        self.stack.push(value);
        Ok(())
    }

    fn store_local(&mut self, index: u16, value: FrameValue) {
        let index = index as usize;
        let width = if value.is_category2() { 2 } else { 1 };
        let locals = Arc::make_mut(&mut self.locals);
        if locals.len() < index + width {
            locals.resize(index + width, FrameValue::Top);
        }

        if index > 0 && locals[index - 1].is_category2() {
            locals[index - 1] = FrameValue::Top;
        }
        locals[index] = value;
        if width == 2 {
            locals[index + 1] = FrameValue::Top;
        }
    }

    fn initialize_object(&mut self, uninitialized: &FrameValue, class_name: &str) {
        let initialized = FrameValue::Object(normalize_class_name(class_name).into());
        for local in Arc::make_mut(&mut self.locals) {
            if local == uninitialized {
                *local = initialized.clone();
            }
        }
        for stack_value in &mut self.stack {
            if stack_value == uninitialized {
                *stack_value = initialized.clone();
            }
        }
    }
}

pub(super) fn initial_locals_for_oomir_function(
    function: &oomir::Function,
    is_static: bool,
    owner_class_name: Option<&str>,
) -> Vec<FrameValue> {
    let mut locals = Vec::new();
    if !is_static {
        let this_value = if function.name == "<init>" {
            FrameValue::UninitializedThis
        } else {
            FrameValue::Object(
                normalize_class_name(
                    owner_class_name
                        .or(function.owner_class.as_deref())
                        .unwrap_or("java/lang/Object"),
                )
                .into(),
            )
        };
        push_local_value(&mut locals, this_value);
    }

    let first_explicit_param = if is_static { 0 } else { 1 };
    for (_, param_ty) in function.signature.params.iter().skip(first_explicit_param) {
        if param_ty.has_jvm_value() {
            push_local_value(&mut locals, frame_value_from_oomir_type(param_ty));
        }
    }
    locals
}

pub(super) fn initial_locals_for_descriptor(
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

pub(super) fn local_hints_for_oomir_locals(
    local_var_map: &HashMap<(String, oomir::Type), u16>,
    max_locals: u16,
) -> Vec<FrameValue> {
    let mut hints = vec![FrameValue::Top; max_locals as usize];
    for ((_, ty), local_index) in local_var_map {
        set_slot_value(&mut hints, *local_index, frame_value_from_oomir_type(ty));
    }
    hints
}

pub(super) fn build_stack_map_attributes(
    instructions: &[Instruction],
    initial_locals: &[FrameValue],
    local_hints: &[FrameValue],
    max_locals: u16,
    constant_pool: &mut InternedConstantPool,
    context: &str,
    exception_table: &[ExceptionTableEntry],
) -> jvm::Result<Vec<Attribute>> {
    let analysis = solve_frame_states(
        instructions,
        initial_locals,
        local_hints,
        max_locals as usize,
        constant_pool,
        context,
        exception_table,
    )?;
    build_stack_map_attributes_from_analysis(
        instructions,
        initial_locals,
        constant_pool,
        exception_table,
        &analysis,
    )
}

pub(super) fn build_stack_map_attributes_from_analysis(
    instructions: &[Instruction],
    initial_locals: &[FrameValue],
    constant_pool: &mut InternedConstantPool,
    exception_table: &[ExceptionTableEntry],
    analysis: &FrameAnalysis,
) -> jvm::Result<Vec<Attribute>> {
    let mut target_offsets = branch_targets(instructions);
    target_offsets.extend(exception_table.iter().map(|entry| entry.handler_pc));
    if instructions.is_empty() || target_offsets.is_empty() {
        return Ok(Vec::new());
    }

    let name_index = constant_pool.add_utf8("StackMapTable")?;
    let mut previous_instruction_offset: Option<u16> = None;
    let mut frames = Vec::new();
    let mut verification_class_cache = HashMap::new();
    let mut previous_locals =
        locals_for_stack_map(initial_locals, constant_pool, &mut verification_class_cache)?;
    for target in target_offsets {
        if target == 0 {
            continue;
        }
        let Some(state) = analysis.state_at(target as usize) else {
            continue;
        };
        let instruction_delta = match previous_instruction_offset {
            Some(previous) => target.saturating_sub(previous).saturating_sub(1),
            None => target,
        };
        let locals =
            locals_for_stack_map(&state.locals, constant_pool, &mut verification_class_cache)?;
        let stack =
            stack_for_stack_map(&state.stack, constant_pool, &mut verification_class_cache)?;
        frames.push(compact_stack_frame(
            instruction_delta,
            &previous_locals,
            &locals,
            stack,
        ));
        previous_locals = locals;
        previous_instruction_offset = Some(target);
    }

    if frames.is_empty() {
        Ok(Vec::new())
    } else {
        Ok(vec![Attribute::StackMapTable { name_index, frames }])
    }
}

fn compact_stack_frame(
    offset_delta: u16,
    previous_locals: &[VerificationType],
    locals: &[VerificationType],
    stack: Vec<VerificationType>,
) -> StackFrame {
    if locals == previous_locals {
        return match stack.len() {
            0 => StackFrame::SameFrameExtended {
                frame_type: 251,
                offset_delta,
            },
            1 => StackFrame::SameLocals1StackItemFrameExtended {
                frame_type: 247,
                offset_delta,
                stack,
            },
            _ => StackFrame::FullFrame {
                frame_type: 255,
                offset_delta,
                locals: locals.to_vec(),
                stack,
            },
        };
    }

    if stack.is_empty() && locals.starts_with(previous_locals) {
        let appended = &locals[previous_locals.len()..];
        if (1..=3).contains(&appended.len()) {
            return StackFrame::AppendFrame {
                frame_type: 251 + appended.len() as u8,
                offset_delta,
                locals: appended.to_vec(),
            };
        }
    }
    if stack.is_empty() && previous_locals.starts_with(locals) {
        let removed = previous_locals.len() - locals.len();
        if (1..=3).contains(&removed) {
            return StackFrame::ChopFrame {
                frame_type: 251 - removed as u8,
                offset_delta,
            };
        }
    }

    StackFrame::FullFrame {
        frame_type: 255,
        offset_delta,
        locals: locals.to_vec(),
        stack,
    }
}

/// Give verifier-visible defaults to control-flow-guarded locals, especially
/// drop values. Rust never observes them, but the JVM cannot correlate a drop
/// flag with its guarded load and requires a value on every incoming path.
pub(super) fn initialize_locals_loaded_as_top(
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
        let analysis = solve_frame_states(
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

    let analysis = solve_frame_states(
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

fn shift_exception_table(
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

fn loaded_local(instruction: &Instruction) -> Option<(u16, FrameValue)> {
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

fn stored_local(instruction: &Instruction) -> Option<u16> {
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
fn locals_loaded_before_definite_store(
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
        let mut last_handler_assignments = HashMap::<usize, Vec<u64>>::new();
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

fn assignment_contains(assignments: &[u64], local: usize) -> bool {
    assignments
        .get(local / u64::BITS as usize)
        .is_some_and(|word| word & (1 << (local % u64::BITS as usize)) != 0)
}

fn assignment_insert(assignments: &mut [u64], local: usize) {
    if let Some(word) = assignments.get_mut(local / u64::BITS as usize) {
        *word |= 1 << (local % u64::BITS as usize);
    }
}

fn merge_assignment_entry(
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

fn assignment_successors(
    index: usize,
    instruction: &Instruction,
    context: &str,
) -> jvm::Result<Vec<usize>> {
    use Instruction as I;

    let fallthrough = || vec![index + 1];
    Ok(match instruction {
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
        | I::Ifnonnull(target) => vec![usize::from(*target), index + 1],
        I::Goto(target) | I::Jsr(target) => vec![usize::from(*target)],
        I::Goto_w(target) | I::Jsr_w(target) => usize::try_from(*target)
            .ok()
            .map(|target| vec![target])
            .unwrap_or_default(),
        I::Tableswitch(table) => std::iter::once(table.default)
            .chain(table.offsets.iter().copied())
            .map(|offset| relative_switch_target(index, offset, context))
            .collect::<jvm::Result<Vec<_>>>()?,
        I::Lookupswitch(table) => std::iter::once(table.default)
            .chain(table.pairs.iter().map(|(_, offset)| *offset))
            .map(|offset| relative_switch_target(index, offset, context))
            .collect::<jvm::Result<Vec<_>>>()?,
        I::Ret(_)
        | I::Ret_w(_)
        | I::Ireturn
        | I::Lreturn
        | I::Freturn
        | I::Dreturn
        | I::Areturn
        | I::Return
        | I::Athrow => Vec::new(),
        _ => fallthrough(),
    })
}

fn default_local_initializer(local: u16, value: &FrameValue) -> [Instruction; 2] {
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

fn local_store(
    local: u16,
    narrow: impl FnOnce(u8) -> Instruction,
    wide: impl FnOnce(u16) -> Instruction,
) -> Instruction {
    u8::try_from(local).map_or_else(|_| wide(local), narrow)
}

fn shift_absolute_branch_targets(
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

pub(super) fn move_zero_branch_target(
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

fn solve_frame_states(
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
            block_starts: Vec::new(),
            entry_states: Vec::new(),
        });
    }

    let (block_starts, block_ends, block_by_instruction) =
        frame_blocks(instructions, exception_table);
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

        let mut last_handler_locals = HashMap::<usize, Arc<Vec<FrameValue>>>::new();
        for index in block_starts[block]..block_ends[block] {
            for &target in &handlers_by_instruction[index] {
                let unchanged = last_handler_locals
                    .get(&target)
                    .is_some_and(|locals| **locals == *state.locals);
                if unchanged || target >= instructions.len() {
                    continue;
                }
                last_handler_locals.insert(target, Arc::clone(&state.locals));
                let mut handler_state = state.clone();
                handler_state.stack.clear();
                handler_state
                    .stack
                    .push(FrameValue::Object("java/lang/Throwable".into()));
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

            let mut successors = transfer_instruction(
                index,
                &instructions[index],
                state,
                local_hints,
                constant_pool,
                context,
            )
            .map_err(|error| jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Stack-map transfer failed at instruction {index} ({}): {error:?}\nInstruction window:\n{}",
                    describe_instruction(&instructions[index], constant_pool),
                    instruction_window(instructions, index, constant_pool),
                ),
            })?;

            if index + 1 < block_ends[block] {
                let Some(position) = successors
                    .iter()
                    .position(|(target, _)| *target == index + 1)
                else {
                    return Err(jvm::Error::VerificationError {
                        context: context.to_string(),
                        message: format!(
                            "Bytecode block ended unexpectedly after instruction {index}"
                        ),
                    });
                };
                state = successors.swap_remove(position).1;
                if !successors.is_empty() {
                    return Err(jvm::Error::VerificationError {
                        context: context.to_string(),
                        message: format!(
                            "Bytecode block contains an internal branch at instruction {index}"
                        ),
                    });
                }
            } else {
                for (target, successor_state) in successors {
                    if target >= instructions.len() {
                        continue;
                    }
                    merge_block_entry(
                        target,
                        successor_state,
                        &block_starts,
                        &block_by_instruction,
                        &mut entry_states,
                        &mut worklist,
                        &mut queued,
                        context,
                    )?;
                }
                break;
            }
        }
    }

    Ok(FrameAnalysis {
        block_starts,
        entry_states,
    })
}

fn frame_blocks(
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

fn instruction_ends_block(instruction: &Instruction) -> bool {
    use Instruction as I;
    matches!(
        instruction,
        I::Ifeq(_)
            | I::Ifne(_)
            | I::Iflt(_)
            | I::Ifge(_)
            | I::Ifgt(_)
            | I::Ifle(_)
            | I::If_icmpeq(_)
            | I::If_icmpne(_)
            | I::If_icmplt(_)
            | I::If_icmpge(_)
            | I::If_icmpgt(_)
            | I::If_icmple(_)
            | I::If_acmpeq(_)
            | I::If_acmpne(_)
            | I::Ifnull(_)
            | I::Ifnonnull(_)
            | I::Goto(_)
            | I::Goto_w(_)
            | I::Tableswitch(_)
            | I::Lookupswitch(_)
            | I::Jsr(_)
            | I::Jsr_w(_)
            | I::Ret(_)
            | I::Ret_w(_)
            | I::Ireturn
            | I::Lreturn
            | I::Freturn
            | I::Dreturn
            | I::Areturn
            | I::Return
            | I::Athrow
    )
}

fn merge_block_entry(
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

fn describe_instruction(instruction: &Instruction, constant_pool: &ConstantPool) -> String {
    let method = match instruction {
        Instruction::Invokevirtual(index) | Instruction::Invokespecial(index) => {
            method_ref_info(constant_pool, *index, false).ok()
        }
        Instruction::Invokestatic(index) => static_method_ref_info(constant_pool, *index).ok(),
        Instruction::Invokeinterface(index, _) => method_ref_info(constant_pool, *index, true).ok(),
        _ => None,
    };
    method.map_or_else(
        || format!("{instruction:?}"),
        |method| {
            format!(
                "{instruction:?} => {}.{}{}",
                method.class_name, method.method_name, method.descriptor
            )
        },
    )
}

fn instruction_window(
    instructions: &[Instruction],
    center: usize,
    constant_pool: &ConstantPool,
) -> String {
    let start = center.saturating_sub(8);
    let end = (center + 4).min(instructions.len().saturating_sub(1));
    (start..=end)
        .map(|index| {
            let marker = if index == center { ">" } else { " " };
            format!(
                "{marker} {index}: {}",
                describe_instruction(&instructions[index], constant_pool)
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn transfer_instruction(
    instruction_index: usize,
    instruction: &Instruction,
    mut state: FrameState,
    local_hints: &[FrameValue],
    constant_pool: &ConstantPool,
    context: &str,
) -> jvm::Result<Vec<(usize, FrameState)>> {
    use Instruction as I;

    match instruction {
        I::Nop => {}
        I::Aconst_null => state.stack.push(FrameValue::Null),
        I::Iconst_m1
        | I::Iconst_0
        | I::Iconst_1
        | I::Iconst_2
        | I::Iconst_3
        | I::Iconst_4
        | I::Iconst_5
        | I::Bipush(_)
        | I::Sipush(_) => state.stack.push(FrameValue::Integer),
        I::Lconst_0 | I::Lconst_1 => state.stack.push(FrameValue::Long),
        I::Fconst_0 | I::Fconst_1 | I::Fconst_2 => state.stack.push(FrameValue::Float),
        I::Dconst_0 | I::Dconst_1 => state.stack.push(FrameValue::Double),
        I::Ldc(index) => state
            .stack
            .push(frame_value_from_ldc(constant_pool, u16::from(*index))?),
        I::Ldc_w(index) => state
            .stack
            .push(frame_value_from_ldc(constant_pool, *index)?),
        I::Ldc2_w(index) => state
            .stack
            .push(frame_value_from_ldc2(constant_pool, *index)?),

        I::Iload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Lload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Fload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Dload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Aload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_0 | I::Lload_0 | I::Fload_0 | I::Dload_0 | I::Aload_0 => state.load_local(
            0,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_1 | I::Lload_1 | I::Fload_1 | I::Dload_1 | I::Aload_1 => state.load_local(
            1,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_2 | I::Lload_2 | I::Fload_2 | I::Dload_2 | I::Aload_2 => state.load_local(
            2,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_3 | I::Lload_3 | I::Fload_3 | I::Dload_3 | I::Aload_3 => state.load_local(
            3,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_w(index)
        | I::Lload_w(index)
        | I::Fload_w(index)
        | I::Dload_w(index)
        | I::Aload_w(index) => state.load_local(
            *index,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,

        I::Istore(index) => {
            state.pop(context, instruction_index)?;
            state.store_local(u16::from(*index), FrameValue::Integer);
        }
        I::Lstore(index) => {
            state.pop(context, instruction_index)?;
            state.store_local(u16::from(*index), FrameValue::Long);
        }
        I::Fstore(index) => {
            state.pop(context, instruction_index)?;
            state.store_local(u16::from(*index), FrameValue::Float);
        }
        I::Dstore(index) => {
            state.pop(context, instruction_index)?;
            state.store_local(u16::from(*index), FrameValue::Double);
        }
        I::Astore(index) => {
            let value = state.pop_reference(context, instruction_index)?;
            state.store_local(u16::from(*index), value);
        }
        I::Istore_0 => store_fixed(
            &mut state,
            0,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Istore_1 => store_fixed(
            &mut state,
            1,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Istore_2 => store_fixed(
            &mut state,
            2,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Istore_3 => store_fixed(
            &mut state,
            3,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Lstore_0 => store_fixed(&mut state, 0, FrameValue::Long, context, instruction_index)?,
        I::Lstore_1 => store_fixed(&mut state, 1, FrameValue::Long, context, instruction_index)?,
        I::Lstore_2 => store_fixed(&mut state, 2, FrameValue::Long, context, instruction_index)?,
        I::Lstore_3 => store_fixed(&mut state, 3, FrameValue::Long, context, instruction_index)?,
        I::Fstore_0 => store_fixed(&mut state, 0, FrameValue::Float, context, instruction_index)?,
        I::Fstore_1 => store_fixed(&mut state, 1, FrameValue::Float, context, instruction_index)?,
        I::Fstore_2 => store_fixed(&mut state, 2, FrameValue::Float, context, instruction_index)?,
        I::Fstore_3 => store_fixed(&mut state, 3, FrameValue::Float, context, instruction_index)?,
        I::Dstore_0 => store_fixed(
            &mut state,
            0,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Dstore_1 => store_fixed(
            &mut state,
            1,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Dstore_2 => store_fixed(
            &mut state,
            2,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Dstore_3 => store_fixed(
            &mut state,
            3,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Astore_0 => store_reference_fixed(&mut state, 0, context, instruction_index)?,
        I::Astore_1 => store_reference_fixed(&mut state, 1, context, instruction_index)?,
        I::Astore_2 => store_reference_fixed(&mut state, 2, context, instruction_index)?,
        I::Astore_3 => store_reference_fixed(&mut state, 3, context, instruction_index)?,
        I::Istore_w(local) => store_fixed(
            &mut state,
            *local,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Lstore_w(local) => store_fixed(
            &mut state,
            *local,
            FrameValue::Long,
            context,
            instruction_index,
        )?,
        I::Fstore_w(local) => store_fixed(
            &mut state,
            *local,
            FrameValue::Float,
            context,
            instruction_index,
        )?,
        I::Dstore_w(local) => store_fixed(
            &mut state,
            *local,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Astore_w(local) => {
            store_reference_fixed(&mut state, *local, context, instruction_index)?
        }

        I::Iaload | I::Baload | I::Caload | I::Saload => {
            state.pop(context, instruction_index)?;
            state.pop_reference(context, instruction_index)?;
            state.stack.push(FrameValue::Integer);
        }
        I::Laload => array_load(&mut state, FrameValue::Long, context, instruction_index)?,
        I::Faload => array_load(&mut state, FrameValue::Float, context, instruction_index)?,
        I::Daload => array_load(&mut state, FrameValue::Double, context, instruction_index)?,
        I::Aaload => {
            state.pop(context, instruction_index)?;
            let array = state.pop_reference(context, instruction_index)?;
            state.stack.push(array_component_value(&array));
        }
        I::Iastore
        | I::Lastore
        | I::Fastore
        | I::Dastore
        | I::Aastore
        | I::Bastore
        | I::Castore
        | I::Sastore => {
            state.pop(context, instruction_index)?;
            state.pop(context, instruction_index)?;
            state.pop_reference(context, instruction_index)?;
        }

        I::Pop => {
            state.pop(context, instruction_index)?;
        }
        I::Pop2 => {
            let value = state.pop(context, instruction_index)?;
            if !value.is_category2() {
                state.pop(context, instruction_index)?;
            }
        }
        I::Dup => {
            let value = state.pop_category1(context, instruction_index)?;
            state.stack.push(value.clone());
            state.stack.push(value);
        }
        I::Swap => {
            let first = state.pop_category1(context, instruction_index)?;
            let second = state.pop_category1(context, instruction_index)?;
            state.stack.push(first);
            state.stack.push(second);
        }
        I::Dup_x1 => {
            let value1 = state.pop_category1(context, instruction_index)?;
            let value2 = state.pop_category1(context, instruction_index)?;
            state.stack.push(value1.clone());
            state.stack.push(value2);
            state.stack.push(value1);
        }
        I::Dup2 => {
            let value1 = state.pop(context, instruction_index)?;
            if value1.is_category2() {
                state.stack.push(value1.clone());
                state.stack.push(value1);
            } else {
                let value2 = state.pop_category1(context, instruction_index)?;
                state.stack.push(value2.clone());
                state.stack.push(value1.clone());
                state.stack.push(value2);
                state.stack.push(value1);
            }
        }
        I::Dup_x2 | I::Dup2_x1 | I::Dup2_x2 => {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Stack-map builder does not yet support {:?} at instruction {instruction_index}",
                    instruction
                ),
            });
        }

        I::Iadd | I::Isub | I::Imul | I::Idiv | I::Irem | I::Iand | I::Ior | I::Ixor => {
            binary(&mut state, FrameValue::Integer, context, instruction_index)?
        }
        I::Ladd | I::Lsub | I::Lmul | I::Ldiv | I::Lrem | I::Land | I::Lor | I::Lxor => {
            binary(&mut state, FrameValue::Long, context, instruction_index)?
        }
        I::Fadd | I::Fsub | I::Fmul | I::Fdiv | I::Frem => {
            binary(&mut state, FrameValue::Float, context, instruction_index)?
        }
        I::Dadd | I::Dsub | I::Dmul | I::Ddiv | I::Drem => {
            binary(&mut state, FrameValue::Double, context, instruction_index)?
        }
        I::Ineg => unary(&mut state, FrameValue::Integer, context, instruction_index)?,
        I::Lneg => unary(&mut state, FrameValue::Long, context, instruction_index)?,
        I::Fneg => unary(&mut state, FrameValue::Float, context, instruction_index)?,
        I::Dneg => unary(&mut state, FrameValue::Double, context, instruction_index)?,
        I::Ishl | I::Ishr | I::Iushr => {
            shift(&mut state, FrameValue::Integer, context, instruction_index)?
        }
        I::Lshl | I::Lshr | I::Lushr => {
            shift(&mut state, FrameValue::Long, context, instruction_index)?
        }
        I::Iinc(local, _) => state.store_local(u16::from(*local), FrameValue::Integer),
        I::Iinc_w(local, _) => state.store_local(*local, FrameValue::Integer),

        I::I2l => convert(&mut state, FrameValue::Long, context, instruction_index)?,
        I::I2f => convert(&mut state, FrameValue::Float, context, instruction_index)?,
        I::I2d => convert(&mut state, FrameValue::Double, context, instruction_index)?,
        I::L2i | I::F2i | I::D2i | I::I2b | I::I2c | I::I2s => {
            convert(&mut state, FrameValue::Integer, context, instruction_index)?
        }
        I::L2f | I::D2f => convert(&mut state, FrameValue::Float, context, instruction_index)?,
        I::L2d | I::F2d => convert(&mut state, FrameValue::Double, context, instruction_index)?,
        I::F2l | I::D2l => convert(&mut state, FrameValue::Long, context, instruction_index)?,
        I::Lcmp | I::Fcmpl | I::Fcmpg | I::Dcmpl | I::Dcmpg => {
            state.pop(context, instruction_index)?;
            state.pop(context, instruction_index)?;
            state.stack.push(FrameValue::Integer);
        }

        I::Ifeq(target)
        | I::Ifne(target)
        | I::Iflt(target)
        | I::Ifge(target)
        | I::Ifgt(target)
        | I::Ifle(target) => {
            state.pop(context, instruction_index)?;
            return Ok(branch_successors(instruction_index, *target, state));
        }
        I::If_icmpeq(target)
        | I::If_icmpne(target)
        | I::If_icmplt(target)
        | I::If_icmpge(target)
        | I::If_icmpgt(target)
        | I::If_icmple(target)
        | I::If_acmpeq(target)
        | I::If_acmpne(target) => {
            state.pop(context, instruction_index)?;
            state.pop(context, instruction_index)?;
            return Ok(branch_successors(instruction_index, *target, state));
        }
        I::Ifnull(target) | I::Ifnonnull(target) => {
            state.pop_reference(context, instruction_index)?;
            return Ok(branch_successors(instruction_index, *target, state));
        }
        I::Goto(target) => return Ok(vec![(*target as usize, state)]),
        I::Goto_w(target) => return Ok(vec![(*target as usize, state)]),
        I::Tableswitch(table_switch) => {
            state.pop(context, instruction_index)?;
            let mut successors = Vec::with_capacity(table_switch.offsets.len() + 1);
            successors.push((
                relative_switch_target(instruction_index, table_switch.default, context)?,
                state.clone(),
            ));
            for target in &table_switch.offsets {
                successors.push((
                    relative_switch_target(instruction_index, *target, context)?,
                    state.clone(),
                ));
            }
            return Ok(successors);
        }
        I::Lookupswitch(lookup_switch) => {
            state.pop(context, instruction_index)?;
            let mut successors = Vec::with_capacity(lookup_switch.pairs.len() + 1);
            successors.push((
                relative_switch_target(instruction_index, lookup_switch.default, context)?,
                state.clone(),
            ));
            for target in lookup_switch.pairs.values() {
                successors.push((
                    relative_switch_target(instruction_index, *target, context)?,
                    state.clone(),
                ));
            }
            return Ok(successors);
        }
        I::Jsr(_) | I::Ret(_) | I::Jsr_w(_) | I::Ret_w(_) => {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Legacy subroutine instruction unsupported at {instruction_index}"
                ),
            });
        }

        I::Ireturn | I::Lreturn | I::Freturn | I::Dreturn | I::Areturn => {
            state.pop(context, instruction_index)?;
            return Ok(Vec::new());
        }
        I::Return => return Ok(Vec::new()),

        I::Getstatic(field_ref) => {
            let field_type = field_type_for_ref(constant_pool, *field_ref)?;
            state.stack.push(frame_value_from_field_type(&field_type));
        }
        I::Putstatic(_) => {
            state.pop(context, instruction_index)?;
        }
        I::Getfield(field_ref) => {
            state.pop_reference(context, instruction_index)?;
            let field_type = field_type_for_ref(constant_pool, *field_ref)?;
            state.stack.push(frame_value_from_field_type(&field_type));
        }
        I::Putfield(_) => {
            state.pop(context, instruction_index)?;
            state.pop_reference(context, instruction_index)?;
        }
        I::Invokevirtual(method_ref) | I::Invokespecial(method_ref) => {
            let method = method_ref_info(constant_pool, *method_ref, false)?;
            apply_invoke(
                &mut state,
                &method,
                false,
                matches!(instruction, I::Invokespecial(_)),
                context,
                instruction_index,
            )?;
        }
        I::Invokestatic(method_ref) => {
            let method = static_method_ref_info(constant_pool, *method_ref)?;
            apply_invoke(&mut state, &method, true, false, context, instruction_index)?;
        }
        I::Invokedynamic(invoke_dynamic_ref) => {
            let method = invoke_dynamic_info(constant_pool, *invoke_dynamic_ref)?;
            apply_invoke(&mut state, &method, true, false, context, instruction_index)?;
        }
        I::Invokeinterface(method_ref, _) => {
            let method = method_ref_info(constant_pool, *method_ref, true)?;
            apply_invoke(
                &mut state,
                &method,
                false,
                false,
                context,
                instruction_index,
            )?;
        }

        I::New(class_index) => {
            let _ = constant_pool.try_get_class(*class_index)?;
            state
                .stack
                .push(FrameValue::Uninitialized(instruction_index as u16));
        }
        I::Newarray(array_type) => {
            state.pop(context, instruction_index)?;
            state.stack.push(FrameValue::Object(
                array_descriptor_from_type(array_type).into(),
            ));
        }
        I::Anewarray(class_index) => {
            state.pop(context, instruction_index)?;
            let class_name = constant_pool.try_get_class(*class_index)?.to_string();
            let array_name = if class_name.starts_with('[') {
                format!("[{class_name}")
            } else {
                format!("[L{};", normalize_class_name(&class_name))
            };
            state.stack.push(FrameValue::Object(array_name.into()));
        }
        I::Arraylength => {
            state.pop_reference(context, instruction_index)?;
            state.stack.push(FrameValue::Integer);
        }
        I::Athrow => {
            state.pop_reference(context, instruction_index)?;
            return Ok(Vec::new());
        }
        I::Checkcast(class_index) => {
            state.pop_reference(context, instruction_index)?;
            let class_name = constant_pool.try_get_class(*class_index)?.to_string();
            state
                .stack
                .push(FrameValue::Object(normalize_class_name(&class_name).into()));
        }
        I::Instanceof(_) => {
            state.pop_reference(context, instruction_index)?;
            state.stack.push(FrameValue::Integer);
        }
        I::Monitorenter | I::Monitorexit => {
            state.pop_reference(context, instruction_index)?;
        }
        I::Multianewarray(class_index, dimensions) => {
            for _ in 0..*dimensions {
                state.pop(context, instruction_index)?;
            }
            let class_name = constant_pool.try_get_class(*class_index)?.to_string();
            state
                .stack
                .push(FrameValue::Object(normalize_class_name(&class_name).into()));
        }
        I::Wide | I::Breakpoint | I::Impdep1 | I::Impdep2 => {}
    }

    Ok(next_successor(instruction_index, state))
}

struct MethodRefInfo {
    class_name: String,
    method_name: String,
    descriptor: String,
}

fn apply_invoke(
    state: &mut FrameState,
    method: &MethodRefInfo,
    is_static: bool,
    is_special: bool,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    let descriptor = jvm::JavaString::from(method.descriptor.as_str());
    let (params, return_type) = FieldType::parse_method_descriptor(&descriptor)?;
    for _ in params.iter().rev() {
        state.pop(context, instruction_index)?;
    }

    let receiver = if is_static {
        None
    } else {
        Some(state.pop_reference(context, instruction_index)?)
    };

    if is_special && method.method_name == "<init>" {
        if let Some(receiver) = receiver {
            match receiver {
                FrameValue::Uninitialized(_) | FrameValue::UninitializedThis => {
                    state.initialize_object(&receiver, &method.class_name);
                }
                _ => {}
            }
        }
    }

    if let Some(return_type) = return_type {
        state.stack.push(frame_value_from_field_type(&return_type));
    }
    Ok(())
}

fn branch_successors(index: usize, target: u16, state: FrameState) -> Vec<(usize, FrameState)> {
    let mut successors = vec![(target as usize, state.clone())];
    successors.extend(next_successor(index, state));
    successors
}

fn next_successor(index: usize, state: FrameState) -> Vec<(usize, FrameState)> {
    vec![(index + 1, state)]
}

fn store_fixed(
    state: &mut FrameState,
    local: u16,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.store_local(local, value);
    Ok(())
}

fn store_reference_fixed(
    state: &mut FrameState,
    local: u16,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    let value = state.pop_reference(context, instruction_index)?;
    state.store_local(local, value);
    Ok(())
}

fn array_load(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop_reference(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn unary(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn binary(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn shift(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn convert(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn merge_state(existing: &mut FrameState, incoming: &FrameState) -> bool {
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

    changed
}

fn merge_value(a: &FrameValue, b: &FrameValue) -> FrameValue {
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

fn common_object_class(a: &str, b: &str) -> String {
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

fn nested_parent_class(class_name: &str) -> Option<&str> {
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

fn relative_switch_target(
    instruction_index: usize,
    offset: i32,
    context: &str,
) -> jvm::Result<usize> {
    relative_switch_target_opt(instruction_index, offset).ok_or_else(|| {
        jvm::Error::VerificationError {
            context: context.to_string(),
            message: format!(
                "Invalid switch target offset {offset} at instruction {instruction_index}"
            ),
        }
    })
}

fn relative_switch_target_opt(instruction_index: usize, offset: i32) -> Option<usize> {
    let target = instruction_index as i64 + i64::from(offset);
    (target >= 0).then_some(target as usize)
}

fn branch_targets(instructions: &[Instruction]) -> BTreeSet<u16> {
    let mut targets = BTreeSet::new();
    for (instruction_index, instruction) in instructions.iter().enumerate() {
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
            | Instruction::Ifnull(target)
            | Instruction::Ifnonnull(target) => {
                targets.insert(*target);
            }
            Instruction::Goto_w(target) => {
                if *target >= 0 {
                    targets.insert(*target as u16);
                }
            }
            Instruction::Tableswitch(table_switch) => {
                if let Some(default) =
                    relative_switch_target_opt(instruction_index, table_switch.default)
                {
                    targets.insert(default as u16);
                }
                for offset in &table_switch.offsets {
                    if let Some(target) = relative_switch_target_opt(instruction_index, *offset) {
                        targets.insert(target as u16);
                    }
                }
            }
            Instruction::Lookupswitch(lookup_switch) => {
                if let Some(default) =
                    relative_switch_target_opt(instruction_index, lookup_switch.default)
                {
                    targets.insert(default as u16);
                }
                for offset in lookup_switch.pairs.values() {
                    if let Some(target) = relative_switch_target_opt(instruction_index, *offset) {
                        targets.insert(target as u16);
                    }
                }
            }
            _ => {}
        }
    }
    targets
}

fn load_hint_for_instruction(instruction: &Instruction) -> FrameValue {
    match instruction {
        Instruction::Iload(_)
        | Instruction::Iload_0
        | Instruction::Iload_1
        | Instruction::Iload_2
        | Instruction::Iload_3
        | Instruction::Iload_w(_) => FrameValue::Integer,
        Instruction::Lload(_)
        | Instruction::Lload_0
        | Instruction::Lload_1
        | Instruction::Lload_2
        | Instruction::Lload_3
        | Instruction::Lload_w(_) => FrameValue::Long,
        Instruction::Fload(_)
        | Instruction::Fload_0
        | Instruction::Fload_1
        | Instruction::Fload_2
        | Instruction::Fload_3
        | Instruction::Fload_w(_) => FrameValue::Float,
        Instruction::Dload(_)
        | Instruction::Dload_0
        | Instruction::Dload_1
        | Instruction::Dload_2
        | Instruction::Dload_3
        | Instruction::Dload_w(_) => FrameValue::Double,
        Instruction::Aload(_)
        | Instruction::Aload_0
        | Instruction::Aload_1
        | Instruction::Aload_2
        | Instruction::Aload_3
        | Instruction::Aload_w(_) => FrameValue::Object("java/lang/Object".into()),
        _ => FrameValue::Top,
    }
}

fn frame_value_from_oomir_type(ty: &Type) -> FrameValue {
    match ty {
        Type::Unit | Type::Void => FrameValue::Top,
        Type::Boolean
        | Type::Char
        | Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::F16
        | Type::I32
        | Type::U32 => FrameValue::Integer,
        Type::I64 | Type::U64 => FrameValue::Long,
        Type::F32 => FrameValue::Float,
        Type::F64 => FrameValue::Double,
        Type::Str => FrameValue::Object(oomir::UTF8_VIEW_CLASS.into()),
        Type::Class(name) | Type::Interface(name) => {
            FrameValue::Object(normalize_class_name(name).into())
        }
        Type::Array(_) | Type::MutableReference(_) => {
            FrameValue::Object(ty.to_jvm_descriptor().into())
        }
        Type::Pointer(_) => FrameValue::Object(oomir::POINTER_CLASS.into()),
        Type::Slice(_) => FrameValue::Object(oomir::SLICE_VIEW_CLASS.into()),
        Type::Reference(inner) => {
            if inner.is_jvm_reference_type() {
                frame_value_from_oomir_type(inner)
            } else {
                FrameValue::Object("java/lang/Object".into())
            }
        }
    }
}

fn frame_value_from_field_type(field_type: &FieldType) -> FrameValue {
    match field_type {
        FieldType::Base(BaseType::Long) => FrameValue::Long,
        FieldType::Base(BaseType::Float) => FrameValue::Float,
        FieldType::Base(BaseType::Double) => FrameValue::Double,
        FieldType::Base(_) => FrameValue::Integer,
        FieldType::Object(class_name) => {
            FrameValue::Object(normalize_class_name(&class_name.to_string()).into())
        }
        FieldType::Array(_) => FrameValue::Object(field_type.class_name().into()),
    }
}

fn frame_value_from_ldc(constant_pool: &ConstantPool, index: u16) -> jvm::Result<FrameValue> {
    let value = match constant_pool.try_get(index)? {
        Constant::Integer(_) => FrameValue::Integer,
        Constant::Float(_) => FrameValue::Float,
        Constant::String(_) => FrameValue::Object("java/lang/String".into()),
        Constant::Class(_) => FrameValue::Object("java/lang/Class".into()),
        Constant::MethodType(_) => FrameValue::Object("java/lang/invoke/MethodType".into()),
        Constant::MethodHandle { .. } => FrameValue::Object("java/lang/invoke/MethodHandle".into()),
        Constant::Dynamic { .. } => FrameValue::Object("java/lang/Object".into()),
        _ => FrameValue::Top,
    };
    Ok(value)
}

fn frame_value_from_ldc2(constant_pool: &ConstantPool, index: u16) -> jvm::Result<FrameValue> {
    let value = match constant_pool.try_get(index)? {
        Constant::Long(_) => FrameValue::Long,
        Constant::Double(_) => FrameValue::Double,
        _ => FrameValue::Top,
    };
    Ok(value)
}

fn field_type_for_ref(constant_pool: &ConstantPool, field_ref: u16) -> jvm::Result<FieldType> {
    let (_, name_and_type_index) = constant_pool.try_get_field_ref(field_ref)?;
    let (_, descriptor_index) = constant_pool.try_get_name_and_type(*name_and_type_index)?;
    let descriptor = constant_pool.try_get_utf8(*descriptor_index)?;
    Ok(FieldType::parse(&descriptor.to_string())?)
}

fn method_ref_info(
    constant_pool: &ConstantPool,
    method_ref: u16,
    is_interface: bool,
) -> jvm::Result<MethodRefInfo> {
    let (class_index, name_and_type_index) = if is_interface {
        constant_pool.try_get_interface_method_ref(method_ref)?
    } else {
        constant_pool.try_get_method_ref(method_ref)?
    };
    let class_name = constant_pool.try_get_class(*class_index)?.to_string();
    let (name_index, descriptor_index) =
        constant_pool.try_get_name_and_type(*name_and_type_index)?;
    let method_name = constant_pool.try_get_utf8(*name_index)?.to_string();
    let descriptor = constant_pool.try_get_utf8(*descriptor_index)?.to_string();
    Ok(MethodRefInfo {
        class_name,
        method_name,
        descriptor,
    })
}

fn static_method_ref_info(
    constant_pool: &ConstantPool,
    method_ref: u16,
) -> jvm::Result<MethodRefInfo> {
    let is_interface = matches!(
        constant_pool.try_get(method_ref)?,
        Constant::InterfaceMethodRef { .. }
    );
    method_ref_info(constant_pool, method_ref, is_interface)
}

fn invoke_dynamic_info(
    constant_pool: &ConstantPool,
    invoke_dynamic_ref: u16,
) -> jvm::Result<MethodRefInfo> {
    let Constant::InvokeDynamic {
        name_and_type_index,
        ..
    } = constant_pool.try_get(invoke_dynamic_ref)?
    else {
        return Err(jvm::Error::VerificationError {
            context: "invokedynamic stack-map transfer".to_string(),
            message: format!(
                "constant-pool entry #{invoke_dynamic_ref} is not an InvokeDynamic constant"
            ),
        });
    };
    let (name_index, descriptor_index) =
        constant_pool.try_get_name_and_type(*name_and_type_index)?;
    Ok(MethodRefInfo {
        class_name: "java/lang/Object".to_string(),
        method_name: constant_pool.try_get_utf8(*name_index)?.to_string(),
        descriptor: constant_pool.try_get_utf8(*descriptor_index)?.to_string(),
    })
}

fn push_local_value(locals: &mut Vec<FrameValue>, value: FrameValue) {
    let is_category2 = value.is_category2();
    locals.push(value);
    if is_category2 {
        locals.push(FrameValue::Top);
    }
}

fn set_slot_value(locals: &mut Vec<FrameValue>, local_index: u16, value: FrameValue) {
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

fn locals_for_stack_map(
    locals: &[FrameValue],
    constant_pool: &mut InternedConstantPool,
    verification_class_cache: &mut HashMap<String, u16>,
) -> jvm::Result<Vec<VerificationType>> {
    let mut end = locals.len();
    while end > 0 && locals[end - 1] == FrameValue::Top {
        end -= 1;
    }

    let mut result = Vec::new();
    let mut index = 0;
    while index < end {
        let value = &locals[index];
        result.push(to_verification_type(
            value,
            constant_pool,
            verification_class_cache,
        )?);
        index += if value.is_category2() { 2 } else { 1 };
    }
    Ok(result)
}

fn stack_for_stack_map(
    stack: &[FrameValue],
    constant_pool: &mut InternedConstantPool,
    verification_class_cache: &mut HashMap<String, u16>,
) -> jvm::Result<Vec<VerificationType>> {
    stack
        .iter()
        .map(|value| to_verification_type(value, constant_pool, verification_class_cache))
        .collect()
}

fn to_verification_type(
    value: &FrameValue,
    constant_pool: &mut InternedConstantPool,
    verification_class_cache: &mut HashMap<String, u16>,
) -> jvm::Result<VerificationType> {
    Ok(match value {
        FrameValue::Top => VerificationType::Top,
        FrameValue::Integer => VerificationType::Integer,
        FrameValue::Float => VerificationType::Float,
        FrameValue::Long => VerificationType::Long,
        FrameValue::Double => VerificationType::Double,
        FrameValue::Null => VerificationType::Null,
        FrameValue::Object(class_name) => {
            let cpool_index = match verification_class_cache.get(class_name.as_ref()) {
                Some(cpool_index) => *cpool_index,
                None => {
                    let cpool_index = constant_pool.add_class(class_name)?;
                    verification_class_cache.insert(class_name.to_string(), cpool_index);
                    cpool_index
                }
            };
            VerificationType::Object { cpool_index }
        }
        FrameValue::UninitializedThis => VerificationType::UninitializedThis,
        FrameValue::Uninitialized(offset) => VerificationType::Uninitialized { offset: *offset },
    })
}

fn array_component_value(array: &FrameValue) -> FrameValue {
    let FrameValue::Object(class_name) = array else {
        return FrameValue::Object("java/lang/Object".into());
    };
    let Some(component_descriptor) = class_name.strip_prefix('[') else {
        return FrameValue::Object("java/lang/Object".into());
    };
    if component_descriptor.starts_with('[') {
        return FrameValue::Object(component_descriptor.into());
    }
    if component_descriptor.starts_with('L') && component_descriptor.ends_with(';') {
        return FrameValue::Object(component_descriptor[1..component_descriptor.len() - 1].into());
    }
    match component_descriptor.chars().next() {
        Some('J') => FrameValue::Long,
        Some('F') => FrameValue::Float,
        Some('D') => FrameValue::Double,
        Some('Z' | 'B' | 'C' | 'S' | 'I') => FrameValue::Integer,
        _ => FrameValue::Object("java/lang/Object".into()),
    }
}

fn array_descriptor_from_type(array_type: &ArrayType) -> String {
    let descriptor = match array_type {
        ArrayType::Boolean => "Z",
        ArrayType::Char => "C",
        ArrayType::Float => "F",
        ArrayType::Double => "D",
        ArrayType::Byte => "B",
        ArrayType::Short => "S",
        ArrayType::Int => "I",
        ArrayType::Long => "J",
    };
    format!("[{descriptor}")
}

fn normalize_class_name(class_name: &str) -> String {
    class_name.replace('.', "/")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compact_frames_reuse_or_extend_previous_locals() {
        let previous = vec![VerificationType::Integer];
        assert!(matches!(
            compact_stack_frame(4, &previous, &previous, Vec::new()),
            StackFrame::SameFrameExtended {
                frame_type: 251,
                offset_delta: 4
            }
        ));
        assert!(matches!(
            compact_stack_frame(
                7,
                &previous,
                &[VerificationType::Integer, VerificationType::Float],
                Vec::new()
            ),
            StackFrame::AppendFrame {
                frame_type: 252,
                offset_delta: 7,
                ..
            }
        ));
        assert!(matches!(
            compact_stack_frame(
                2,
                &[VerificationType::Integer, VerificationType::Float],
                &previous,
                Vec::new()
            ),
            StackFrame::ChopFrame {
                frame_type: 250,
                offset_delta: 2
            }
        ));
    }

    #[test]
    fn compact_frames_fall_back_when_stack_or_locals_require_it() {
        let previous = vec![VerificationType::Integer];
        assert!(matches!(
            compact_stack_frame(3, &previous, &previous, vec![VerificationType::Integer]),
            StackFrame::SameLocals1StackItemFrameExtended { .. }
        ));
        assert!(matches!(
            compact_stack_frame(
                3,
                &previous,
                &[VerificationType::Float],
                vec![VerificationType::Integer, VerificationType::Integer]
            ),
            StackFrame::FullFrame { .. }
        ));
    }

    #[test]
    fn definite_assignment_finds_control_flow_guarded_loads() {
        let instructions = vec![
            Instruction::Iconst_0,
            Instruction::Ifeq(4),
            Instruction::Iconst_1,
            Instruction::Istore_1,
            Instruction::Iload_1,
            Instruction::Pop,
            Instruction::Return,
        ];
        let loads =
            locals_loaded_before_definite_store(&instructions, &[], 2, "test", &[]).unwrap();
        assert_eq!(loads, BTreeMap::from([(1, FrameValue::Integer)]));
    }

    #[test]
    fn definite_assignment_accepts_a_store_on_every_path() {
        let instructions = vec![
            Instruction::Iconst_0,
            Instruction::Ifeq(5),
            Instruction::Iconst_1,
            Instruction::Istore_1,
            Instruction::Goto(7),
            Instruction::Iconst_0,
            Instruction::Istore_1,
            Instruction::Iload_1,
            Instruction::Pop,
            Instruction::Return,
        ];
        let loads =
            locals_loaded_before_definite_store(&instructions, &[], 2, "test", &[]).unwrap();
        assert!(loads.is_empty());
    }
}
