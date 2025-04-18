use super::oomir;

use oomir::Type;
use ristretto_classfile::{self as jvm, attributes::Instruction};
use std::collections::{HashMap, HashSet, VecDeque};

/// Returns the number of JVM local variable slots a type occupies (1 or 2).
pub fn get_type_size(ty: &Type) -> u16 {
    match ty {
        Type::I64 | Type::F64 => 2,
        _ => 1,
    }
}

/// Gets the appropriate type-specific load instruction.
pub fn get_load_instruction(ty: &Type, index: u16) -> Result<Instruction, jvm::Error> {
    Ok(match ty {
        // Integer-like types
        Type::I8 | Type::I16 | Type::I32 | Type::Boolean | Type::Char => {
            match index {
                0 => Instruction::Iload_0,
                1 => Instruction::Iload_1,
                2 => Instruction::Iload_2,
                3 => Instruction::Iload_3,
                // For indices that can fit into u8, use Iload, otherwise use Iload_w.
                _ if index <= u8::MAX as u16 => Instruction::Iload(index as u8),
                _ => Instruction::Iload_w(index),
            }
        }
        // Long type
        Type::I64 => match index {
            0 => Instruction::Lload_0,
            1 => Instruction::Lload_1,
            2 => Instruction::Lload_2,
            3 => Instruction::Lload_3,
            _ if index <= u8::MAX as u16 => Instruction::Lload(index as u8),
            _ => Instruction::Lload_w(index),
        },
        // Float type
        Type::F32 => match index {
            0 => Instruction::Fload_0,
            1 => Instruction::Fload_1,
            2 => Instruction::Fload_2,
            3 => Instruction::Fload_3,
            _ if index <= u8::MAX as u16 => Instruction::Fload(index as u8),
            _ => Instruction::Fload_w(index),
        },
        // Double type
        Type::F64 => match index {
            0 => Instruction::Dload_0,
            1 => Instruction::Dload_1,
            2 => Instruction::Dload_2,
            3 => Instruction::Dload_3,
            _ if index <= u8::MAX as u16 => Instruction::Dload(index as u8),
            _ => Instruction::Dload_w(index),
        },
        // Reference-like types
        Type::Reference(_) | Type::Array(_) | Type::String | Type::Class(_) => match index {
            0 => Instruction::Aload_0,
            1 => Instruction::Aload_1,
            2 => Instruction::Aload_2,
            3 => Instruction::Aload_3,
            _ if index <= u8::MAX as u16 => Instruction::Aload(index as u8),
            _ => Instruction::Aload_w(index),
        },
        // For void, return an error
        Type::Void => {
            return Err(jvm::Error::VerificationError {
                context: "get_load_instruction".to_string(),
                message: "Cannot load void type".to_string(),
            });
        }
    })
}

/// Gets the appropriate type-specific store instruction.
pub fn get_store_instruction(ty: &Type, index: u16) -> Result<Instruction, jvm::Error> {
    use Type::*;
    let instr = match ty {
        I8 | I16 | I32 | Boolean | Char => {
            if index <= 3 {
                match index {
                    0 => Instruction::Istore_0,
                    1 => Instruction::Istore_1,
                    2 => Instruction::Istore_2,
                    3 => Instruction::Istore_3,
                    _ => unreachable!(),
                }
            } else if index <= u8::MAX as u16 {
                Instruction::Istore(index as u8)
            } else {
                Instruction::Istore_w(index)
            }
        }
        I64 => {
            if index <= 3 {
                match index {
                    0 => Instruction::Lstore_0,
                    1 => Instruction::Lstore_1,
                    2 => Instruction::Lstore_2,
                    3 => Instruction::Lstore_3,
                    _ => unreachable!(),
                }
            } else if index <= u8::MAX as u16 {
                Instruction::Lstore(index as u8)
            } else {
                Instruction::Lstore_w(index)
            }
        }
        F32 => {
            if index <= 3 {
                match index {
                    0 => Instruction::Fstore_0,
                    1 => Instruction::Fstore_1,
                    2 => Instruction::Fstore_2,
                    3 => Instruction::Fstore_3,
                    _ => unreachable!(),
                }
            } else if index <= u8::MAX as u16 {
                Instruction::Fstore(index as u8)
            } else {
                Instruction::Fstore_w(index)
            }
        }
        F64 => {
            if index <= 3 {
                match index {
                    0 => Instruction::Dstore_0,
                    1 => Instruction::Dstore_1,
                    2 => Instruction::Dstore_2,
                    3 => Instruction::Dstore_3,
                    _ => unreachable!(),
                }
            } else if index <= u8::MAX as u16 {
                Instruction::Dstore(index as u8)
            } else {
                Instruction::Dstore_w(index)
            }
        }
        Reference(_) | Array(_) | String | Class(_) => {
            if index <= 3 {
                match index {
                    0 => Instruction::Astore_0,
                    1 => Instruction::Astore_1,
                    2 => Instruction::Astore_2,
                    3 => Instruction::Astore_3,
                    _ => unreachable!(),
                }
            } else if index <= u8::MAX as u16 {
                Instruction::Astore(index as u8)
            } else {
                Instruction::Astore_w(index)
            }
        }
        Void => {
            return Err(jvm::Error::VerificationError {
                context: "get_store_instructions".to_string(),
                message: "Cannot store void type".to_string(),
            });
        }
    };

    Ok(instr)
}

/// Returns a sequence of instructions to cast a primitive type from `src` to `dest`.
pub fn get_cast_instructions(src: &Type, dest: &Type) -> Result<Vec<Instruction>, jvm::Error> {
    // If the two types are equal, no conversion is needed.
    if src == dest {
        return Ok(vec![]);
    }

    // Only support numeric primitive conversions.
    // Supported types: I8, I16, I32, I64, F32, F64, and Char.
    fn is_supported(ty: &Type) -> bool {
        matches!(
            ty,
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Char
        )
    }
    if !(is_supported(src) && is_supported(dest)) {
        return Err(jvm::Error::VerificationError {
            context: "get_cast_instructions".to_string(),
            message: format!("Unsupported type conversion from {:?} to {:?}", src, dest),
        });
    }

    // Normalize types: in the JVM, I8, I16, and Char are treated as I32 on the operand stack.
    fn normalize(ty: &Type) -> Type {
        match ty {
            Type::I8 | Type::I16 | Type::Char => Type::I32,
            _ => ty.clone(),
        }
    }

    let norm_src = normalize(src);
    let norm_dest = normalize(dest);

    // Final conversion: when the destination is one of the narrow types, use a final opcode.
    fn final_conversion(orig_dest: &Type) -> Option<Instruction> {
        match orig_dest {
            Type::I8 => Some(Instruction::I2b),
            Type::Char => Some(Instruction::I2c),
            Type::I16 => Some(Instruction::I2s),
            _ => None,
        }
    }

    let mut instructions = vec![];

    // If the normalized types are already equal, only perform a final conversion
    // if the original destination is a narrow type.
    if norm_src == norm_dest {
        if let Some(final_inst) = final_conversion(dest) {
            instructions.push(final_inst);
        }
        return Ok(instructions);
    }

    // Build the conversion graph between normalized types.
    // The nodes we care about are: I32, I64, F32, and F64.
    // Each edge is a pair (target_type, conversion_instruction).
    let mut graph: HashMap<Type, Vec<(Type, Instruction)>> = HashMap::new();

    // From I32:
    graph.insert(
        Type::I32,
        vec![
            (Type::I64, Instruction::I2l),
            (Type::F64, Instruction::I2d),
            (Type::F32, Instruction::I2f),
        ],
    );
    // From I64:
    graph.insert(
        Type::I64,
        vec![
            (Type::I32, Instruction::L2i),
            (Type::F64, Instruction::L2d),
            (Type::F32, Instruction::L2f),
        ],
    );
    // From F64:
    graph.insert(
        Type::F64,
        vec![
            (Type::I32, Instruction::D2i),
            (Type::I64, Instruction::D2l),
            (Type::F32, Instruction::D2f),
        ],
    );
    // From F32:
    graph.insert(
        Type::F32,
        vec![
            (Type::I32, Instruction::F2i),
            (Type::I64, Instruction::F2l),
            (Type::F64, Instruction::F2d),
        ],
    );

    // Use breadth-first search (BFS) to find a conversion path from norm_src to norm_dest.
    #[derive(Clone)]
    struct Node {
        ty: Type,
        path: Vec<Instruction>,
    }

    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();

    queue.push_back(Node {
        ty: norm_src.clone(),
        path: vec![],
    });
    visited.insert(norm_src.clone());

    let mut found_path: Option<Vec<Instruction>> = None;

    while let Some(current) = queue.pop_front() {
        if current.ty == norm_dest {
            found_path = Some(current.path);
            break;
        }
        if let Some(neighbors) = graph.get(&current.ty) {
            for (next_ty, opcode) in neighbors {
                if !visited.contains(next_ty) {
                    let mut new_path = current.path.clone();
                    new_path.push(opcode.clone());
                    queue.push_back(Node {
                        ty: next_ty.clone(),
                        path: new_path,
                    });
                    visited.insert(next_ty.clone());
                }
            }
        }
    }

    let conversion_chain = found_path.ok_or_else(|| jvm::Error::VerificationError {
        context: "get_cast_instructions".to_string(),
        message: format!("No conversion path found from {:?} to {:?}", src, dest),
    })?;

    // Append the conversion chain instructions.
    instructions.extend(conversion_chain);

    // If a final conversion from I32 to a narrow type is required, append it.
    if let Some(final_inst) = final_conversion(dest) {
        instructions.push(final_inst);
    }

    Ok(instructions)
}

pub fn get_operand_type(operand: &oomir::Operand) -> Type {
    match operand {
        oomir::Operand::Variable { ty, .. } => ty.clone(),
        oomir::Operand::Constant(c) => Type::from_constant(c),
    }
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

/// Parses a JVM method descriptor and returns the parameter types as a vector of strings.
pub fn parse_jvm_descriptor_params(descriptor: &str) -> Result<Vec<String>, String> {
    // 1. Find the '(' and the closing ')'
    let start = descriptor
        .find('(')
        .ok_or_else(|| "Descriptor must start with '('".to_string())?;
    let end = descriptor[start + 1..]
        .find(')')
        .ok_or_else(|| "Descriptor missing ')'".to_string())?
        + start
        + 1;
    let params = &descriptor[start + 1..end];

    let bytes = params.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();

    while i < bytes.len() {
        let tok_start = i;
        match bytes[i] as char {
            // 2a. Primitive
            'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => {
                i += 1;
            }

            // 2b. Object type
            'L' => {
                i += 1;
                // scan until ';'
                while i < bytes.len() && bytes[i] as char != ';' {
                    i += 1;
                }
                if i == bytes.len() {
                    return Err("Unterminated object type (missing `;`)".into());
                }
                i += 1; // include ';'
            }

            // 2c. Array: one or more '[' then a component descriptor
            '[' => {
                i += 1;
                // multi-dimensional arrays
                while i < bytes.len() && bytes[i] as char == '[' {
                    i += 1;
                }
                if i == bytes.len() {
                    return Err("Array type without component descriptor".into());
                }
                match bytes[i] as char {
                    // primitive component
                    'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => {
                        i += 1;
                    }
                    // object component
                    'L' => {
                        i += 1;
                        while i < bytes.len() && bytes[i] as char != ';' {
                            i += 1;
                        }
                        if i == bytes.len() {
                            return Err("Unterminated object type in array".into());
                        }
                        i += 1;
                    }
                    other => {
                        return Err(format!("Invalid array component descriptor '{}'", other));
                    }
                }
            }

            // anything else is invalid
            other => {
                return Err(format!("Invalid descriptor character '{}'", other));
            }
        }

        // slice out the full token and push
        out.push(params[tok_start..i].to_string());
    }

    Ok(out)
}
