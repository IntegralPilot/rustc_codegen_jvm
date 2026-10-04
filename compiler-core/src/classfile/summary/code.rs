//! Visit constant indexes without retaining an instruction or stack-map graph.
use super::*;
use crate::classfile::{ByteReader, attributes::Instruction};

pub(super) fn constants(
    bytes: &[u8],
    mut visit: impl FnMut(u16) -> io::Result<()>,
) -> io::Result<()> {
    let mut r = Reader { bytes };
    r.take(4)?; // stack/local bounds
    let length = r.u32()? as usize;
    let mut code = ByteReader::new(r.take(length)?);
    while code.remaining() != 0 {
        use Instruction::*;
        let instruction = Instruction::from_bytes(&mut code).map_err(|_| invalid())?;
        let index = match instruction {
            Ldc(i) => u16::from(i),
            Ldc_w(i)
            | Ldc2_w(i)
            | Getstatic(i)
            | Putstatic(i)
            | Getfield(i)
            | Putfield(i)
            | Invokevirtual(i)
            | Invokespecial(i)
            | Invokestatic(i)
            | Invokeinterface(i, _)
            | Invokedynamic(i)
            | New(i)
            | Anewarray(i)
            | Checkcast(i)
            | Instanceof(i)
            | Multianewarray(i, _) => i,
            _ => continue,
        };
        visit(index)?;
    }
    for _ in 0..r.u16()? {
        r.take(6)?;
        let catch = r.u16()?;
        if catch != 0 {
            visit(catch)?;
        }
    }
    Ok(())
}

pub(super) fn stack_maps<'a>(
    bytes: &[u8],
    utf8: impl Fn(u16) -> io::Result<&'a [u8]>,
    mut visit: impl FnMut(u16) -> io::Result<()>,
) -> io::Result<()> {
    let mut r = Reader { bytes };
    r.take(4)?;
    let length = r.u32()? as usize;
    r.take(length)?;
    let catches = r.u16()? as usize;
    r.take(catches * 8)?;
    for _ in 0..r.u16()? {
        let name = utf8(r.u16()?)?;
        let length = r.u32()? as usize;
        let data = r.take(length)?;
        if name != b"StackMapTable" {
            continue;
        }
        let mut frames = Reader { bytes: data };
        fn ty(r: &mut Reader<'_>, visit: &mut impl FnMut(u16) -> io::Result<()>) -> io::Result<()> {
            match r.u8()? {
                0..=6 => {}
                7 => visit(r.u16()?)?,
                8 => {
                    r.u16()?;
                }
                _ => return Err(invalid()),
            }
            Ok(())
        }
        for _ in 0..frames.u16()? {
            match frames.u8()? {
                0..=63 => {}
                64..=127 => ty(&mut frames, &mut visit)?,
                247 => {
                    frames.u16()?;
                    ty(&mut frames, &mut visit)?;
                }
                248..=251 => {
                    frames.u16()?;
                }
                frame @ 252..=254 => {
                    frames.u16()?;
                    for _ in 0..frame - 251 {
                        ty(&mut frames, &mut visit)?;
                    }
                }
                255 => {
                    frames.u16()?;
                    for _ in 0..frames.u16()? {
                        ty(&mut frames, &mut visit)?;
                    }
                    for _ in 0..frames.u16()? {
                        ty(&mut frames, &mut visit)?;
                    }
                }
                _ => return Err(invalid()),
            }
        }
        if !frames.bytes.is_empty() {
            return Err(invalid());
        }
    }
    Ok(())
}
