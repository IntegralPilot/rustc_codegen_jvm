use super::*;
use ristretto_classfile::attributes::{StackFrame, VerificationType};

pub(super) fn encode(
    frames: &[StackFrame],
    offsets: &code::Offsets,
    bytes: &mut Vec<u8>,
) -> Result<()> {
    count(bytes, frames.len())?;
    let mut previous = None;
    for frame in frames {
        let index = previous.map_or(usize::from(frame.offset_delta()), |(index, _)| {
            index + usize::from(frame.offset_delta()) + 1
        });
        let position = offsets.at(index)?;
        let delta = previous.map_or(position, |(_, previous)| position - previous - 1);
        match frame {
            StackFrame::SameFrame { .. } => bytes.push(u8::try_from(delta)?),
            StackFrame::SameLocals1StackItemFrame { stack, .. } => {
                bytes.push(u8::try_from(delta + 64)?);
                item(stack, 64, bytes)?;
            }
            StackFrame::SameLocals1StackItemFrameExtended {
                frame_type, stack, ..
            } => {
                bytes.push(*frame_type);
                u16(bytes, delta);
                item(stack, *frame_type, bytes)?;
            }
            StackFrame::ChopFrame { frame_type, .. }
            | StackFrame::SameFrameExtended { frame_type, .. } => {
                bytes.push(*frame_type);
                u16(bytes, delta);
            }
            StackFrame::AppendFrame {
                frame_type, locals, ..
            } => {
                bytes.push(*frame_type);
                u16(bytes, delta);
                values(locals, bytes)?;
            }
            StackFrame::FullFrame {
                frame_type,
                locals,
                stack,
                ..
            } => {
                bytes.push(*frame_type);
                u16(bytes, delta);
                count(bytes, locals.len())?;
                values(locals, bytes)?;
                count(bytes, stack.len())?;
                values(stack, bytes)?;
            }
        }
        previous = Some((index, position));
    }
    Ok(())
}

fn item(stack: &[VerificationType], frame_type: u8, bytes: &mut Vec<u8>) -> Result<()> {
    stack
        .first()
        .ok_or(Error::InvalidStackFrameType(frame_type))?
        .to_bytes(bytes)
}
fn values(values: &[VerificationType], bytes: &mut Vec<u8>) -> Result<()> {
    for value in values {
        value.to_bytes(bytes)?;
    }
    Ok(())
}
