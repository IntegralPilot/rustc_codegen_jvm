//! Exact encoded instruction sizes, including switch alignment.
use crate::classfile::{self as jvm, attributes::Instruction};
use std::io::Cursor;

pub fn instruction_byte_offsets(instructions: &[Instruction]) -> Result<Vec<usize>, jvm::Error> {
    let mut offsets = Vec::with_capacity(instructions.len() + 1);
    let mut byte_offset = 0usize;
    let mut scratch = Cursor::new(Vec::with_capacity(16));
    for instruction in instructions {
        offsets.push(byte_offset);
        byte_offset += instruction_size_at(instruction, byte_offset, &mut scratch)?;
    }
    offsets.push(byte_offset);
    Ok(offsets)
}

pub fn instruction_size_at(
    instruction: &Instruction,
    byte_offset: usize,
    scratch: &mut Cursor<Vec<u8>>,
) -> Result<usize, jvm::Error> {
    match instruction {
        Instruction::Ifeq(_)
        | Instruction::Ifne(_)
        | Instruction::Iflt(_)
        | Instruction::Ifge(_)
        | Instruction::Ifgt(_)
        | Instruction::Ifle(_)
        | Instruction::If_icmpeq(_)
        | Instruction::If_icmpne(_)
        | Instruction::If_icmplt(_)
        | Instruction::If_icmpge(_)
        | Instruction::If_icmpgt(_)
        | Instruction::If_icmple(_)
        | Instruction::If_acmpeq(_)
        | Instruction::If_acmpne(_)
        | Instruction::Goto(_)
        | Instruction::Jsr(_)
        | Instruction::Ifnull(_)
        | Instruction::Ifnonnull(_) => Ok(3),
        Instruction::Goto_w(_) | Instruction::Jsr_w(_) => Ok(5),
        Instruction::Tableswitch(table_switch) => {
            let position_after_opcode = byte_offset + 1;
            let padding = (4 - (position_after_opcode % 4)) % 4;
            Ok(1 + padding + 12 + table_switch.offsets.len() * 4)
        }
        Instruction::Lookupswitch(lookup_switch) => {
            let position_after_opcode = byte_offset + 1;
            let padding = (4 - (position_after_opcode % 4)) % 4;
            Ok(1 + padding + 8 + lookup_switch.pairs.len() * 8)
        }
        _ => {
            scratch.get_mut().clear();
            scratch.set_position(0);
            instruction.to_bytes(scratch)?;
            Ok(scratch.get_ref().len())
        }
    }
}
