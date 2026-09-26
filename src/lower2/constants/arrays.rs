//! Borrowed array constants append directly into the enclosing method.
use super::*;
use crate::lower2::helpers::are_types_jvm_compatible;

pub(super) fn append_empty_array(
    instructions: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    element_type: &oomir::Type,
    length: usize,
) -> jvm::Result<()> {
    let length = i32::try_from(length).map_err(|_| jvm::Error::VerificationError {
        context: "constant array allocation".to_string(),
        message: "Constant array length exceeds the JVM address space".to_string(),
    })?;
    instructions.push(get_int_const_instr(cp, length));
    if !element_type.has_jvm_value() {
        instructions.push(Instruction::Anewarray(cp.add_class("java/lang/Object")?));
    } else if let Some(code) = element_type.to_jvm_primitive_array_type_code() {
        let array_type = ArrayType::from_bytes(&mut jvm::ByteReader::new(&[code]))?;
        instructions.push(Instruction::Newarray(array_type));
    } else if let Some(internal_name) = element_type.to_jvm_internal_name() {
        instructions.push(Instruction::Anewarray(cp.add_class(&internal_name)?));
    } else {
        return Err(jvm::Error::VerificationError {
            context: "constant array allocation".to_string(),
            message: format!("Cannot create a JVM array for element type {element_type:?}"),
        });
    }
    Ok(())
}

fn load_array_with(
    instructions: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    element_type: &oomir::Type,
    length: usize,
    mut element: impl FnMut(usize, &mut Vec<Instruction>, &mut InternedConstantPool) -> jvm::Result<()>,
) -> jvm::Result<()> {
    append_empty_array(instructions, cp, element_type, length)?;
    if !element_type.has_jvm_value() {
        return Ok(());
    }
    let store = element_type
        .get_jvm_array_store_instruction()
        .ok_or_else(|| jvm::Error::VerificationError {
            context: "constant array".into(),
            message: format!("Cannot store array elements of type {element_type:?}"),
        })?;
    for index in 0..length {
        instructions.push(Instruction::Dup);
        instructions.push(get_int_const_instr(cp, index as i32));
        element(index, instructions, cp)?;
        instructions.push(store.clone());
    }
    Ok(())
}

pub(super) fn load_array(
    instructions: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    element_type: &oomir::Type,
    elements: &[oomir::Constant],
) -> jvm::Result<()> {
    if oomir::is_packed_byte_array(element_type, elements) {
        append_empty_array(instructions, cp, element_type, elements.len())?;
        return fill_packed_bytes(instructions, cp, 0, elements.len(), |index| {
            oomir::constant_byte(&elements[index]).unwrap()
        });
    }
    load_array_with(
        instructions,
        cp,
        element_type,
        elements.len(),
        |index, instructions, cp| {
            let value = &elements[index];
            let value_type = oomir::Type::from_constant(value);
            if &value_type != element_type && !are_types_jvm_compatible(&value_type, element_type) {
                return Err(jvm::Error::VerificationError {
                    context: "constant array".into(),
                    message: format!(
                        "Expected {element_type:?}, found {value_type:?} at element {index}"
                    ),
                });
            }
            load_constant(instructions, cp, value)
        },
    )
}

pub(super) fn load_bytes(
    instructions: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    bytes: &[u8],
) -> jvm::Result<()> {
    if bytes.len() >= 32 {
        append_empty_array(instructions, cp, &oomir::Type::U8, bytes.len())?;
        return fill_packed_bytes(instructions, cp, 0, bytes.len(), |index| bytes[index]);
    }
    load_array_with(
        instructions,
        cp,
        &oomir::Type::U8,
        bytes.len(),
        |index, instructions, cp| {
            instructions.push(get_int_const_instr(cp, i32::from(bytes[index] as i8)));
            Ok(())
        },
    )
}

/// Fill ranges of the array on the stack, retaining that array as the result.
pub(super) fn fill_packed_bytes(
    instructions: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    start: usize,
    length: usize,
    byte: impl Fn(usize) -> u8,
) -> jvm::Result<()> {
    let owner = cp.add_class("org/rustlang/runtime/MemoryBytes")?;
    let fill = cp.add_method_ref(owner, "fillConstant", "([BILjava/lang/String;)V")?;
    for offset in (0..length).step_by(oomir::PACKED_BYTE_CHUNK) {
        let end = length.min(offset + oomir::PACKED_BYTE_CHUNK);
        let packed: String = (offset..end).map(|index| char::from(byte(index))).collect();
        instructions.push(Instruction::Dup);
        instructions.push(get_int_const_instr(cp, i32::try_from(start + offset)?));
        instructions.push(Instruction::Ldc_w(cp.add_string(packed)?));
        instructions.push(Instruction::Invokestatic(fill));
    }
    Ok(())
}
