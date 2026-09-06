//! Keep exact-layout scalar memory operations compact until the runtime call.
use super::*;

pub(super) const MEMORY_BYTES_CLASS: &str = "org/rustlang/runtime/MemoryBytes";

pub(super) fn emit_bits_to_union_bytes(
    bits: oomir::Operand,
    size: usize,
    storage: &JvmUnionStorage,
    offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    counter: &mut usize,
) -> Result<(), String> {
    if size > 8 {
        return Err(format!("scalar memory word exceeds 8 bytes: {size}"));
    }
    if size == 0 {
        return Ok(());
    }
    let offset = storage.byte_index(offset, instructions, counter);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: None,
        class_name: MEMORY_BYTES_CLASS.into(),
        method_name: "write".into(),
        method_ty: oomir::Signature {
            params: vec![
                ("bytes".into(), byte_array_type()),
                ("offset".into(), oomir::Type::I32),
                ("size".into(), oomir::Type::I32),
                ("value".into(), oomir::Type::I64),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args: vec![
            operand_var(storage.bytes_var.clone(), byte_array_type()),
            offset,
            oomir::Operand::Constant(oomir::Constant::I32(size as i32)),
            bits,
        ],
    });
    Ok(())
}

pub(super) fn emit_bits_from_union_bytes(
    bits_ty: oomir::Type,
    size: usize,
    storage: &JvmUnionStorage,
    offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    counter: &mut usize,
) -> oomir::Operand {
    assert!(size <= 8, "scalar memory word exceeds 8 bytes");
    if size == 0 {
        return oomir::Operand::Constant(int_constant_for_type(0, &bits_ty));
    }
    let offset = storage.byte_index(offset, instructions, counter);
    let dest = next_union_temp("memory_word", counter);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest.clone()),
        class_name: MEMORY_BYTES_CLASS.into(),
        method_name: "read".into(),
        method_ty: oomir::Signature {
            params: vec![
                ("bytes".into(), byte_array_type()),
                ("offset".into(), oomir::Type::I32),
                ("size".into(), oomir::Type::I32),
            ],
            ret: Box::new(oomir::Type::I64),
            is_static: true,
        },
        args: vec![
            operand_var(storage.bytes_var.clone(), byte_array_type()),
            offset,
            oomir::Operand::Constant(oomir::Constant::I32(size as i32)),
        ],
    });
    let value = operand_var(dest, oomir::Type::I64);
    if bits_ty == oomir::Type::I64 {
        return value;
    }
    let dest = next_union_temp("memory_bits", counter);
    instructions.push(oomir::Instruction::Cast {
        op: value,
        ty: bits_ty.clone(),
        dest: dest.clone(),
    });
    operand_var(dest, bits_ty)
}
