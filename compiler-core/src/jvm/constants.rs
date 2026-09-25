use crate::classfile::{attributes::Instruction, constant_pool::InternedConstantPool};

fn immediate_int_const_instr(val: i32) -> Option<Instruction> {
    match val {
        -1 => Some(Instruction::Iconst_m1),
        0 => Some(Instruction::Iconst_0),
        1 => Some(Instruction::Iconst_1),
        2 => Some(Instruction::Iconst_2),
        3 => Some(Instruction::Iconst_3),
        4 => Some(Instruction::Iconst_4),
        5 => Some(Instruction::Iconst_5),
        v @ -128..=-2 | v @ 6..=127 => Some(Instruction::Bipush(v as i8)),
        v @ -32768..=-129 | v @ 128..=32767 => Some(Instruction::Sipush(v as i16)),
        _ => None,
    }
}

pub fn get_int_const_instr(cp: &mut InternedConstantPool, val: i32) -> Instruction {
    immediate_int_const_instr(val).unwrap_or_else(|| {
        let index = cp
            .add_integer(val)
            .expect("Failed to add integer to constant pool");
        if let Ok(idx8) = u8::try_from(index) {
            Instruction::Ldc(idx8)
        } else {
            Instruction::Ldc_w(index)
        }
    })
}

/// Appends an integer without consuming a constant-pool entry. Generated array
/// indices have high cardinality, so pooled indices can exhaust large classes.
pub fn append_unpooled_int_const(instructions: &mut Vec<Instruction>, val: i32) {
    if let Some(instruction) = immediate_int_const_instr(val) {
        instructions.push(instruction);
        return;
    }

    instructions.push(Instruction::Bipush((val >> 24) as i8));
    for shift in [16, 8, 0] {
        instructions.push(Instruction::Bipush(8));
        instructions.push(Instruction::Ishl);
        let byte = ((val >> shift) & 0xff) as i16;
        instructions.push(if byte <= i16::from(i8::MAX) {
            Instruction::Bipush(byte as i8)
        } else {
            Instruction::Sipush(byte)
        });
        instructions.push(Instruction::Ior);
    }
}

pub fn get_long_const_instr(cp: &mut InternedConstantPool, val: i64) -> Instruction {
    match val {
        0 => Instruction::Lconst_0,
        1 => Instruction::Lconst_1,
        _ => {
            // Add the long value to the constant pool.
            let index = cp
                .add_long(val)
                .expect("Failed to add long to constant pool");
            // Ldc2_w is used for long/double constants and always takes a u16 index.
            Instruction::Ldc2_w(index)
        }
    }
}

pub fn get_float_const_instr(cp: &mut InternedConstantPool, val: f32) -> Instruction {
    if val.to_bits() == 0.0f32.to_bits() {
        Instruction::Fconst_0
    } else if val == 1.0 {
        Instruction::Fconst_1
    } else if val == 2.0 {
        Instruction::Fconst_2
    } else {
        // Add the float value to the constant pool.
        let index = cp
            .add_float(val)
            .expect("Failed to add float to constant pool");
        // Ldc2_w is used for long/double constants and always takes a u16 index.
        Instruction::Ldc_w(index)
    }
}

pub fn get_double_const_instr(cp: &mut InternedConstantPool, val: f64) -> Instruction {
    // Using bit representation for exact zero comparison is more robust
    if val.to_bits() == 0.0f64.to_bits() {
        Instruction::Dconst_0
    } else if val == 1.0 {
        Instruction::Dconst_1
    } else {
        // Add the double value to the constant pool.
        let index = cp
            .add_double(val)
            .expect("Failed to add double to constant pool");
        // Ldc2_w is used for long/double constants and always takes a u16 index.
        Instruction::Ldc2_w(index)
    }
}
