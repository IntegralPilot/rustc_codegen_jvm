use super::oomir;

use ristretto_classfile::{self as jvm, attributes::Instruction};

/// Returns the number of JVM local variable slots a type occupies (1 or 2).
pub fn get_type_size(ty: &oomir::Type) -> u16 {
    match ty {
        oomir::Type::I64 | oomir::Type::F64 => 2,
        _ => 1,
    }
}

/// Gets the appropriate type-specific load instruction.
pub fn get_load_instruction(ty: &oomir::Type, index: u16) -> Result<Instruction, jvm::Error> {
    let index_u8: u8 = index.try_into().unwrap();
    Ok(match ty {
        oomir::Type::I8
        | oomir::Type::I16
        | oomir::Type::I32
        | oomir::Type::Boolean
        | oomir::Type::Char => match index {
            0 => Instruction::Iload_0,
            1 => Instruction::Iload_1,
            2 => Instruction::Iload_2,
            3 => Instruction::Iload_3,
            _ => Instruction::Iload(index_u8),
        },
        oomir::Type::I64 => match index {
            0 => Instruction::Lload_0,
            1 => Instruction::Lload_1,
            2 => Instruction::Lload_2,
            3 => Instruction::Lload_3,
            _ => Instruction::Lload(index_u8),
        },
        oomir::Type::F32 => match index {
            0 => Instruction::Fload_0,
            1 => Instruction::Fload_1,
            2 => Instruction::Fload_2,
            3 => Instruction::Fload_3,
            _ => Instruction::Fload(index_u8),
        },
        oomir::Type::F64 => match index {
            0 => Instruction::Dload_0,
            1 => Instruction::Dload_1,
            2 => Instruction::Dload_2,
            3 => Instruction::Dload_3,
            _ => Instruction::Dload(index_u8),
        },
        oomir::Type::Reference(_)
        | oomir::Type::Array(_)
        | oomir::Type::String
        | oomir::Type::Class(_) => match index {
            0 => Instruction::Aload_0,
            1 => Instruction::Aload_1,
            2 => Instruction::Aload_2,
            3 => Instruction::Aload_3,
            _ => Instruction::Aload(index_u8),
        },
        oomir::Type::Void => {
            return Err(jvm::Error::VerificationError {
                context: "get_load_instruction".to_string(),
                message: "Cannot load void type".to_string(),
            });
        }
    })
}

/// Gets the appropriate type-specific store instruction.
pub fn get_store_instruction(ty: &oomir::Type, index: u16) -> Result<Instruction, jvm::Error> {
    let index_u8: u8 = index.try_into().unwrap(); // TODO: Handle wide instructions if index > 255
    Ok(match ty {
        oomir::Type::I8
        | oomir::Type::I16
        | oomir::Type::I32
        | oomir::Type::Boolean
        | oomir::Type::Char => match index {
            0 => Instruction::Istore_0,
            1 => Instruction::Istore_1,
            2 => Instruction::Istore_2,
            3 => Instruction::Istore_3,
            _ => Instruction::Istore(index_u8),
        },
        oomir::Type::I64 => match index {
            0 => Instruction::Lstore_0,
            1 => Instruction::Lstore_1,
            2 => Instruction::Lstore_2,
            3 => Instruction::Lstore_3,
            _ => Instruction::Lstore(index_u8),
        },
        oomir::Type::F32 => match index {
            0 => Instruction::Fstore_0,
            1 => Instruction::Fstore_1,
            2 => Instruction::Fstore_2,
            3 => Instruction::Fstore_3,
            _ => Instruction::Fstore(index_u8),
        },
        oomir::Type::F64 => match index {
            0 => Instruction::Dstore_0,
            1 => Instruction::Dstore_1,
            2 => Instruction::Dstore_2,
            3 => Instruction::Dstore_3,
            _ => Instruction::Dstore(index_u8),
        },
        oomir::Type::Reference(_)
        | oomir::Type::Array(_)
        | oomir::Type::String
        | oomir::Type::Class(_) => match index {
            0 => Instruction::Astore_0,
            1 => Instruction::Astore_1,
            2 => Instruction::Astore_2,
            3 => Instruction::Astore_3,
            _ => Instruction::Astore(index_u8),
        },
        oomir::Type::Void => {
            return Err(jvm::Error::VerificationError {
                context: "get_store_instruction".to_string(),
                message: "Cannot store void type".to_string(),
            });
        }
    })
}

// Helper to check if types are compatible enough for JVM assignments (e.g., U8 -> I32)
pub fn are_types_jvm_compatible(src: &oomir::Type, dest: &oomir::Type) -> bool {
    if src == dest {
        return true;
    }
    match (src, dest) {
        // Allow storing smaller ints into I32 array slots if that's the JVM target type
        (
            oomir::Type::I8 | oomir::Type::I16 | oomir::Type::Boolean | oomir::Type::Char,
            oomir::Type::I32,
        ) => true,
        // TODO: Add more other compatibility rules (e.g., Reference vs Class)
        _ => false,
    }
}
