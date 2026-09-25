use super::*;

pub fn build_stack_map_attributes(
    instructions: &[Instruction],
    initial_locals: &[FrameValue],
    local_hints: &[FrameValue],
    max_locals: u16,
    constant_pool: &mut InternedConstantPool,
    context: &str,
    exception_table: &[ExceptionTableEntry],
) -> jvm::Result<Vec<Attribute>> {
    let analysis = analyze(
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

pub fn build_stack_map_attributes_from_analysis(
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
    let mut verification_class_cache = HashMap::default();
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

pub(super) fn compact_stack_frame(
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

pub(super) fn locals_for_stack_map(
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

pub(super) fn stack_for_stack_map(
    stack: &[FrameValue],
    constant_pool: &mut InternedConstantPool,
    verification_class_cache: &mut HashMap<String, u16>,
) -> jvm::Result<Vec<VerificationType>> {
    stack
        .iter()
        .map(|value| to_verification_type(value, constant_pool, verification_class_cache))
        .collect()
}

pub(super) fn to_verification_type(
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
