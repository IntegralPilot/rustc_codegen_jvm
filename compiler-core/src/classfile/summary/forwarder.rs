//! Prove exact static forwarding without a bytecode graph.
//! Permit local spills. Reject casts, branches, handlers, effects and reordered
//! arguments. Require identical caller and target descriptors.
use super::*;
use crate::classfile::{ByteReader, attributes::Instruction};

#[derive(Clone, Copy, PartialEq, Eq)]
struct Value {
    source: u16,
    kind: u8,
}

fn signature(bytes: &[u8]) -> Option<(Vec<(usize, Value)>, u8)> {
    let mut cursor = bytes.strip_prefix(b"(")?;
    let mut params = Vec::new();
    let mut slot = 0;
    while !cursor.starts_with(b")") {
        let (&first, rest) = cursor.split_first()?;
        cursor = rest;
        let kind = match first {
            b'Z' | b'B' | b'C' | b'S' | b'I' => 0,
            b'J' => 1,
            b'F' => 2,
            b'D' => 3,
            b'L' | b'[' => {
                let mut element = first;
                while element == b'[' {
                    let (&next, rest) = cursor.split_first()?;
                    element = next;
                    cursor = rest;
                }
                if element == b'L' {
                    let end = cursor.iter().position(|&b| b == b';')?;
                    cursor = &cursor[end + 1..];
                } else if !b"ZBCSIJFD".contains(&element) {
                    return None;
                }
                4
            }
            _ => return None,
        };
        params.push((
            slot,
            Value {
                source: params.len() as u16,
                kind,
            },
        ));
        slot += if kind == 1 || kind == 3 { 2 } else { 1 };
        if slot > 64 {
            return None;
        }
    }
    let result = match cursor.get(1)? {
        b'Z' | b'B' | b'C' | b'S' | b'I' => 0,
        b'J' => 1,
        b'F' => 2,
        b'D' => 3,
        b'L' | b'[' => 4,
        b'V' => 5,
        _ => return None,
    };
    Some((params, result))
}

pub(super) fn target<'a>(
    bytes: &[u8],
    descriptor: &[u8],
    pool: &Pool<'a>,
) -> Option<MethodKey<'a>> {
    let (params, result) = signature(descriptor)?;
    let mut raw = Reader { bytes };
    raw.take(4).ok()?;
    let length = raw.u32().ok()? as usize;
    if length > 512 {
        return None;
    }
    let bytes = raw.take(length).ok()?;
    if raw.u16().ok()? != 0 {
        return None;
    }
    let mut code = ByteReader::new(bytes);
    let mut locals = [None; 64];
    for &(slot, value) in &params {
        locals[slot] = Some(value);
    }
    let mut stack = Vec::with_capacity(params.len() + 1);
    let mut target = None;
    while code.remaining() != 0 {
        use Instruction::*;
        let instruction = Instruction::from_bytes(&mut code).ok()?;
        let load = match instruction {
            Iload(i) => Some((i as usize, 0)),
            Lload(i) => Some((i as usize, 1)),
            Fload(i) => Some((i as usize, 2)),
            Dload(i) => Some((i as usize, 3)),
            Aload(i) => Some((i as usize, 4)),
            Iload_0 => Some((0, 0)),
            Iload_1 => Some((1, 0)),
            Iload_2 => Some((2, 0)),
            Iload_3 => Some((3, 0)),
            Lload_0 => Some((0, 1)),
            Lload_1 => Some((1, 1)),
            Lload_2 => Some((2, 1)),
            Lload_3 => Some((3, 1)),
            Fload_0 => Some((0, 2)),
            Fload_1 => Some((1, 2)),
            Fload_2 => Some((2, 2)),
            Fload_3 => Some((3, 2)),
            Dload_0 => Some((0, 3)),
            Dload_1 => Some((1, 3)),
            Dload_2 => Some((2, 3)),
            Dload_3 => Some((3, 3)),
            Aload_0 => Some((0, 4)),
            Aload_1 => Some((1, 4)),
            Aload_2 => Some((2, 4)),
            Aload_3 => Some((3, 4)),
            _ => None,
        };
        if let Some((slot, kind)) = load {
            let value = (*locals.get(slot)?)?;
            if value.kind != kind {
                return None;
            }
            stack.push(value);
            continue;
        }
        let store = match instruction {
            Istore(i) => Some((i as usize, 0)),
            Lstore(i) => Some((i as usize, 1)),
            Fstore(i) => Some((i as usize, 2)),
            Dstore(i) => Some((i as usize, 3)),
            Astore(i) => Some((i as usize, 4)),
            Istore_0 => Some((0, 0)),
            Istore_1 => Some((1, 0)),
            Istore_2 => Some((2, 0)),
            Istore_3 => Some((3, 0)),
            Lstore_0 => Some((0, 1)),
            Lstore_1 => Some((1, 1)),
            Lstore_2 => Some((2, 1)),
            Lstore_3 => Some((3, 1)),
            Fstore_0 => Some((0, 2)),
            Fstore_1 => Some((1, 2)),
            Fstore_2 => Some((2, 2)),
            Fstore_3 => Some((3, 2)),
            Dstore_0 => Some((0, 3)),
            Dstore_1 => Some((1, 3)),
            Dstore_2 => Some((2, 3)),
            Dstore_3 => Some((3, 3)),
            Astore_0 => Some((0, 4)),
            Astore_1 => Some((1, 4)),
            Astore_2 => Some((2, 4)),
            Astore_3 => Some((3, 4)),
            _ => None,
        };
        if let Some((slot, kind)) = store {
            let value = stack.pop()?;
            if value.kind != kind {
                return None;
            }
            *locals.get_mut(slot)? = Some(value);
            continue;
        }
        match instruction {
            Nop => {}
            Goto(target) if usize::from(target) == code.position() => {}
            Goto_w(target) if usize::try_from(target).ok() == Some(code.position()) => {}
            Invokestatic(index) if target.is_none() => {
                if !stack.iter().eq(params.iter().map(|(_, value)| value)) {
                    return None;
                }
                let Constant::Member(owner, member, true) = *pool.constants.get(index as usize)?
                else {
                    return None;
                };
                let callee = pool.member(owner, member).ok()?;
                if callee.descriptor != descriptor {
                    return None;
                }
                target = Some(callee);
                stack.clear();
                if result != 5 {
                    stack.push(Value {
                        source: u16::MAX,
                        kind: result,
                    });
                }
            }
            Ireturn | Lreturn | Freturn | Dreturn | Areturn | Return => {
                let kind = match instruction {
                    Ireturn => 0,
                    Lreturn => 1,
                    Freturn => 2,
                    Dreturn => 3,
                    Areturn => 4,
                    _ => 5,
                };
                if kind != result || code.remaining() != 0 {
                    return None;
                }
                if result != 5
                    && stack.pop()?
                        != (Value {
                            source: u16::MAX,
                            kind: result,
                        })
                {
                    return None;
                }
                return if stack.is_empty() { target } else { None };
            }
            _ => return None,
        }
    }
    None
}
