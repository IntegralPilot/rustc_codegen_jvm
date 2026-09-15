use super::*;
use crate::jvm::encoding::instruction_size_at;
use ristretto_classfile::attributes::Instruction;
use std::io::Cursor;

pub(super) struct Offsets(Vec<u16>);
impl Offsets {
    pub(super) fn at(&self, index: usize) -> Result<u16> {
        self.0.get(index).copied().ok_or_else(|| {
            Error::InvalidInstructionOffset(u32::try_from(index).unwrap_or(u32::MAX))
        })
    }
    fn relative(&self, index: usize, delta: i32) -> Result<i32> {
        let target = usize::try_from(index as i64 + i64::from(delta))?;
        Ok(i32::from(self.at(target)?) - i32::from(self.at(index)?))
    }
}

pub(super) fn instructions(code: &[Instruction], bytes: &mut Vec<u8>) -> Result<Offsets> {
    let mut offsets = Vec::with_capacity(code.len());
    let mut size = 0;
    for instruction in code {
        offsets.push(u16::try_from(size)?);
        size += instruction_size_at(instruction, size);
    }
    // The JVM Code attribute is limited to 65535 bytes.
    u16::try_from(size)?;
    let offsets = Offsets(offsets);
    bytes.extend_from_slice(&u32::try_from(size)?.to_be_bytes());
    bytes.reserve(size);
    // Ordinary instructions can use Ristretto's encoder directly in the final
    // class buffer. Only control flow needs a code-relative position.
    let mut writer = Cursor::new(std::mem::take(bytes));
    writer.set_position(writer.get_ref().len() as u64);
    let result = (|| {
        for (index, instruction) in code.iter().enumerate() {
            let output = writer.get_mut();
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
                    output.push(instruction.code());
                    let delta =
                        i32::from(offsets.at(*target as usize)?) - i32::from(offsets.at(index)?);
                    output.extend_from_slice(&i16::try_from(delta)?.to_be_bytes());
                }
                Instruction::Goto_w(target) | Instruction::Jsr_w(target) => {
                    output.push(instruction.code());
                    let delta = i32::from(offsets.at(usize::try_from(*target)?)?)
                        - i32::from(offsets.at(index)?);
                    output.extend_from_slice(&delta.to_be_bytes());
                }
                Instruction::Tableswitch(table) => {
                    switch_header(output, instruction, offsets.at(index)?);
                    output
                        .extend_from_slice(&offsets.relative(index, table.default)?.to_be_bytes());
                    output.extend_from_slice(&table.low.to_be_bytes());
                    output.extend_from_slice(&table.high.to_be_bytes());
                    for &delta in &table.offsets {
                        output.extend_from_slice(&offsets.relative(index, delta)?.to_be_bytes());
                    }
                }
                Instruction::Lookupswitch(table) => {
                    switch_header(output, instruction, offsets.at(index)?);
                    output
                        .extend_from_slice(&offsets.relative(index, table.default)?.to_be_bytes());
                    output.extend_from_slice(&i32::try_from(table.pairs.len())?.to_be_bytes());
                    for (key, &delta) in &table.pairs {
                        output.extend_from_slice(&key.to_be_bytes());
                        output.extend_from_slice(&offsets.relative(index, delta)?.to_be_bytes());
                    }
                }
                _ => {
                    instruction.to_bytes(&mut writer)?;
                    continue;
                }
            }
            writer.set_position(writer.get_ref().len() as u64);
        }
        Ok(())
    })();
    *bytes = writer.into_inner();
    result.map(|()| offsets)
}

fn switch_header(bytes: &mut Vec<u8>, instruction: &Instruction, position: u16) {
    bytes.push(instruction.code());
    let padding = (4 - (usize::from(position) + 1) % 4) % 4;
    bytes.resize(bytes.len() + padding, 0);
}
