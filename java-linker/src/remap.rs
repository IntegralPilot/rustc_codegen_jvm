use crate::*;

pub(crate) fn constant_pool_error(context: &str, error: impl std::fmt::Display) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, format!("{context}: {error}"))
}

pub(crate) fn constant_pool_index(constants: &ConstantPool<'_>) -> HashMap<ConstantKey, u16> {
    let mut target_constants = HashMap::default();
    target_constants.reserve(constants.len());
    for raw_index in 1..=constants.len() {
        let Ok(index) = u16::try_from(raw_index) else {
            continue;
        };
        if let Ok(constant) = constants.try_get(index) {
            target_constants
                .entry(ConstantKey::from(constant))
                .or_insert(index);
        }
    }
    target_constants
}

pub(crate) trait ConstantIndexes {
    fn remap(&self, index: u16) -> io::Result<u16>;
}

impl ConstantIndexes for HashMap<u16, u16> {
    fn remap(&self, index: u16) -> io::Result<u16> {
        self.get(&index).copied().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("missing imported constant-pool index for #{index}"),
            )
        })
    }
}

pub(crate) fn remapped_constant_index(
    index: u16,
    indexes: &impl ConstantIndexes,
) -> io::Result<u16> {
    indexes.remap(index)
}

pub(crate) fn remap_verification_type(
    verification_type: &mut VerificationType,
    indexes: &impl ConstantIndexes,
) -> io::Result<()> {
    if let VerificationType::Object { cpool_index } = verification_type {
        *cpool_index = remapped_constant_index(*cpool_index, indexes)?;
    }
    Ok(())
}

pub(crate) fn remap_stack_frame(
    frame: &mut StackFrame,
    indexes: &impl ConstantIndexes,
) -> io::Result<()> {
    match frame {
        StackFrame::SameLocals1StackItemFrame { stack, .. }
        | StackFrame::SameLocals1StackItemFrameExtended { stack, .. } => {
            for item in stack {
                remap_verification_type(item, indexes)?;
            }
        }
        StackFrame::AppendFrame { locals, .. } => {
            for item in locals {
                remap_verification_type(item, indexes)?;
            }
        }
        StackFrame::FullFrame { locals, stack, .. } => {
            for item in locals.iter_mut().chain(stack.iter_mut()) {
                remap_verification_type(item, indexes)?;
            }
        }
        StackFrame::SameFrame { .. }
        | StackFrame::ChopFrame { .. }
        | StackFrame::SameFrameExtended { .. } => {}
    }
    Ok(())
}

pub(crate) fn remap_instruction(
    instruction: &mut Instruction,
    indexes: &impl ConstantIndexes,
) -> io::Result<()> {
    match instruction {
        Instruction::Ldc(index) => {
            let shifted = remapped_constant_index(u16::from(*index), indexes)?;
            if let Ok(short_index) = u8::try_from(shifted) {
                *index = short_index;
            } else {
                *instruction = Instruction::Ldc_w(shifted);
            }
        }
        Instruction::Ldc_w(index)
        | Instruction::Ldc2_w(index)
        | Instruction::Getstatic(index)
        | Instruction::Putstatic(index)
        | Instruction::Getfield(index)
        | Instruction::Putfield(index)
        | Instruction::Invokevirtual(index)
        | Instruction::Invokespecial(index)
        | Instruction::Invokestatic(index)
        | Instruction::Invokedynamic(index)
        | Instruction::New(index)
        | Instruction::Anewarray(index)
        | Instruction::Checkcast(index)
        | Instruction::Instanceof(index) => *index = remapped_constant_index(*index, indexes)?,
        Instruction::Invokeinterface(index, _) | Instruction::Multianewarray(index, _) => {
            *index = remapped_constant_index(*index, indexes)?;
        }
        _ => {}
    }
    Ok(())
}

pub(crate) fn instruction_byte_offsets(instructions: &[Instruction]) -> io::Result<Vec<u16>> {
    let mut bytes = Cursor::new(Vec::new());
    let mut offsets = Vec::with_capacity(instructions.len() + 1);
    for instruction in instructions {
        offsets.push(u16::try_from(bytes.position()).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "JVM method exceeds the bytecode offset limit",
            )
        })?);
        instruction.to_bytes(&mut bytes).map_err(|error| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("could not measure JVM instruction: {error}"),
            )
        })?;
    }
    offsets.push(u16::try_from(bytes.position()).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "JVM method exceeds the bytecode offset limit",
        )
    })?);
    Ok(offsets)
}

pub(crate) fn remap_code_offset(
    pc: u16,
    old_offsets: &[u16],
    new_offsets: &[u16],
) -> io::Result<u16> {
    let instruction_index = old_offsets.binary_search(&pc).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("local-variable PC {pc} is not an instruction boundary"),
        )
    })?;
    new_offsets.get(instruction_index).copied().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "local-variable PC has no remapped instruction boundary",
        )
    })
}

pub(crate) fn remap_local_variable_ranges(
    attributes: &mut [Attribute],
    old_offsets: &[u16],
    new_offsets: &[u16],
) -> io::Result<()> {
    for attribute in attributes {
        if let Attribute::LocalVariableTable { variables, .. } = attribute {
            for variable in variables {
                let old_end = variable
                    .start_pc
                    .checked_add(variable.length)
                    .ok_or_else(|| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            "local-variable bytecode range overflows u16",
                        )
                    })?;
                let new_start = remap_code_offset(variable.start_pc, old_offsets, new_offsets)?;
                let new_end = remap_code_offset(old_end, old_offsets, new_offsets)?;
                variable.start_pc = new_start;
                variable.length = new_end.checked_sub(new_start).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "remapped local-variable bytecode range is reversed",
                    )
                })?;
            }
        }
    }
    Ok(())
}

pub(crate) fn remap_attribute(
    attribute: &mut Attribute,
    indexes: &impl ConstantIndexes,
) -> io::Result<()> {
    match attribute {
        Attribute::Code {
            name_index,
            code,
            exception_table,
            attributes,
            ..
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            let old_byte_offsets = instruction_byte_offsets(code)?;
            for instruction in code.iter_mut() {
                remap_instruction(instruction, indexes)?;
            }
            let new_byte_offsets = instruction_byte_offsets(code)?;
            remap_local_variable_ranges(attributes, &old_byte_offsets, &new_byte_offsets)?;
            for exception in exception_table {
                if exception.catch_type != 0 {
                    exception.catch_type = remapped_constant_index(exception.catch_type, indexes)?;
                }
            }
            for nested in attributes {
                remap_attribute(nested, indexes)?;
            }
        }
        Attribute::StackMapTable { name_index, frames } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            for frame in frames {
                remap_stack_frame(frame, indexes)?;
            }
        }
        Attribute::Exceptions {
            name_index,
            exception_indexes,
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            for index in exception_indexes {
                *index = remapped_constant_index(*index, indexes)?;
            }
        }
        Attribute::Signature {
            name_index,
            signature_index,
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            *signature_index = remapped_constant_index(*signature_index, indexes)?;
        }
        Attribute::LineNumberTable { name_index, .. }
        | Attribute::Synthetic { name_index }
        | Attribute::Deprecated { name_index } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
        }
        Attribute::LocalVariableTable {
            name_index,
            variables,
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            for variable in variables {
                variable.name_index = remapped_constant_index(variable.name_index, indexes)?;
                variable.descriptor_index =
                    remapped_constant_index(variable.descriptor_index, indexes)?;
            }
        }
        Attribute::MethodParameters {
            name_index,
            parameters,
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            for parameter in parameters {
                if parameter.name_index != 0 {
                    parameter.name_index = remapped_constant_index(parameter.name_index, indexes)?;
                }
            }
        }
        other => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "cannot merge generated JVM method with unsupported {} attribute",
                    other.name()
                ),
            ));
        }
    }
    Ok(())
}
