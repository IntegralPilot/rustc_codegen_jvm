//! Exact encoded instruction sizes, including switch alignment.
use crate::classfile::{self as jvm, attributes::Instruction};

pub fn instruction_byte_offsets(instructions: &[Instruction]) -> Result<Vec<usize>, jvm::Error> {
    let mut offsets = Vec::with_capacity(instructions.len() + 1);
    let mut byte_offset = 0usize;
    for instruction in instructions {
        offsets.push(byte_offset);
        byte_offset += instruction_size_at(instruction, byte_offset);
    }
    offsets.push(byte_offset);
    Ok(offsets)
}

pub fn instruction_size_at(instruction: &Instruction, byte_offset: usize) -> usize {
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
        | Instruction::Ifnonnull(_) => 3,
        Instruction::Goto_w(_) | Instruction::Jsr_w(_) => 5,
        Instruction::Tableswitch(table_switch) => {
            let position_after_opcode = byte_offset + 1;
            let padding = (4 - (position_after_opcode % 4)) % 4;
            1 + padding + 12 + table_switch.offsets.len() * 4
        }
        Instruction::Lookupswitch(lookup_switch) => {
            let position_after_opcode = byte_offset + 1;
            let padding = (4 - (position_after_opcode % 4)) % 4;
            1 + padding + 8 + lookup_switch.pairs.len() * 8
        }
        Instruction::Iinc_w(..) => 6,
        _ => match instruction.code() {
            0x10 | 0x12 | 0x15..=0x19 | 0x36..=0x3a | 0xa9 | 0xbc => 2,
            0x11 | 0x13 | 0x14 | 0x84 | 0xb2..=0xb8 | 0xbb | 0xbd | 0xc0 | 0xc1 => 3,
            0xc4 | 0xc5 => 4,
            0xb9 | 0xba => 5,
            _ => 1,
        },
    }
}
