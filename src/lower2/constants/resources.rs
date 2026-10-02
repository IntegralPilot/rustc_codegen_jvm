//! Large primitive constants are data, never a program of element stores.
use super::*;

const MIN_BYTES: usize = 1024;

pub(super) fn element(ty: &oomir::Type, value: &oomir::Constant) -> Option<(u64, usize)> {
    use oomir::{Constant as C, Type as T};
    Some(match (ty, value) {
        (T::Boolean, C::Boolean(v)) => (u64::from(*v), 1),
        (T::I8 | T::U8, C::I8(v)) => (*v as u8 as u64, 1),
        (T::I8 | T::U8, C::U8(v)) => (u64::from(*v), 1),
        (T::I16, C::I16(v)) => (*v as u16 as u64, 2),
        (T::U16, C::U16(v)) | (T::F16, C::F16(v)) => (u64::from(*v), 2),
        (T::Char, C::Char(v)) => (u64::from(*v as u16), 2),
        (T::I32, C::I32(v)) => (*v as u32 as u64, 4),
        (T::U32, C::U32(v)) => (u64::from(*v), 4),
        (T::I64, C::I64(v)) => (*v as u64, 8),
        (T::U64, C::U64(v)) => (*v, 8),
        (T::F32, C::F32(v)) => (u64::from(v.to_bits()), 4),
        (T::F64, C::F64(v)) => (v.to_bits(), 8),
        _ => return None,
    })
}

pub(super) fn eligible(
    cp: &InternedConstantPool,
    ty: &oomir::Type,
    values: &[oomir::Constant],
) -> bool {
    cp.resource_anchor().is_some()
        && values
            .first()
            .and_then(|v| element(ty, v))
            .is_some_and(|(_, size)| {
                values.len().saturating_mul(size) >= MIN_BYTES
                    && values.iter().all(|v| element(ty, v).is_some())
            })
}

pub(super) fn fill(
    code: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    ty: &oomir::Type,
    values: &[oomir::Constant],
) -> jvm::Result<()> {
    let (_, size) = element(ty, &values[0]).expect("validated primitive constant");
    let per_block = jvm::resources::BLOCK_BYTES / size;
    for (block, values) in values.chunks(per_block).enumerate() {
        let mut bytes = Vec::with_capacity(values.len() * size);
        for value in values {
            let (bits, _) = element(ty, value).expect("validated primitive constant");
            bytes.extend_from_slice(&bits.to_le_bytes()[..size]);
        }
        fill_block(code, cp, block * per_block, values.len(), bytes)?;
    }
    Ok(())
}

pub(super) fn fill_bytes(
    code: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    bytes: &[u8],
) -> jvm::Result<bool> {
    if bytes.len() < MIN_BYTES || cp.resource_anchor().is_none() {
        return Ok(false);
    }
    for (block, bytes) in bytes.chunks(jvm::resources::BLOCK_BYTES).enumerate() {
        fill_block(
            code,
            cp,
            block * jvm::resources::BLOCK_BYTES,
            bytes.len(),
            bytes.to_vec(),
        )?;
    }
    Ok(true)
}

fn fill_block(
    code: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    start: usize,
    count: usize,
    bytes: Vec<u8>,
) -> jvm::Result<()> {
    let anchor = cp.resource_anchor().expect("resource owner");
    let name = cp.add_resource(bytes)?;
    let owner = cp.add_class("org/rustlang/runtime/ConstantData")?;
    let fill = cp.add_method_ref(
        owner,
        "fill",
        "(Ljava/lang/Object;IILjava/lang/String;Ljava/lang/Class;)V",
    )?;
    code.extend([
        Instruction::Dup,
        get_int_const_instr(cp, i32::try_from(start)?),
        get_int_const_instr(cp, i32::try_from(count)?),
        Instruction::Ldc_w(name),
        Instruction::Ldc_w(anchor),
        Instruction::Invokestatic(fill),
    ]);
    Ok(())
}
