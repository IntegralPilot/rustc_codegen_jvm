use super::oomir;

use oomir::Type;
use ristretto_classfile::{self as jvm, ConstantPool, attributes::Instruction};

use super::{BIG_DECIMAL_CLASS, BIG_INTEGER_CLASS};

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
        Type::Reference(_)
        | Type::MutableReference(_)
        | Type::Array(_)
        | Type::String
        | Type::Class(_) => match index {
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
        Reference(_) | MutableReference(_) | Array(_) | String | Class(_) => {
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

/// Returns a sequence of instructions to cast a value of type `src` on the stack
/// to type `dest`. Requires the constant pool for class/method references.
pub fn get_cast_instructions(
    src: &Type,
    dest: &Type,
    cp: &mut ConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;

    // 0. Identity cast
    if src == dest {
        return Ok(vec![]);
    }

    // 1. Primitive <-> Primitive
    if src.is_jvm_primitive_like() && dest.is_jvm_primitive_like() {
        return primitive_to_primitive(src, dest);
    }

    // 2. Primitive -> BigInteger / BigDecimal
    if src.is_jvm_primitive_like() {
        if let Type::Class(cn) = dest {
            if cn == BIG_INTEGER_CLASS {
                return prim_to_bigint(src, cp);
            }
            if cn == BIG_DECIMAL_CLASS {
                return prim_to_bigdec(src, cp);
            }
        }
    }

    // 3. BigInteger / BigDecimal -> Primitive
    if let Type::Class(cn) = src {
        if dest.is_jvm_primitive_like() {
            if cn == BIG_INTEGER_CLASS {
                return bigint_to_prim(dest, cp);
            }
            if cn == BIG_DECIMAL_CLASS {
                return bigdec_to_prim(dest, cp);
            }
        }
    }

    // 4. Reference -> Reference (including String, BigInteger, BigDecimal interop)
    if src.is_jvm_reference_type() && dest.is_jvm_reference_type() {
        // String -> BigInteger
        if src == &Type::String && dest == &Type::Class(BIG_INTEGER_CLASS.into()) {
            let bi_idx = cp.add_class(BIG_INTEGER_CLASS)?;
            let ctor = cp.add_method_ref(bi_idx, "<init>", "(Ljava/lang/String;)V")?;
            return Ok(vec![JI::New(bi_idx), JI::Swap, JI::Invokespecial(ctor)]);
        }

        // BigInteger -> String
        if src == &Type::Class(BIG_INTEGER_CLASS.into()) && dest == &Type::String {
            let bi_idx = cp.add_class(BIG_INTEGER_CLASS)?;
            let mref = cp.add_method_ref(bi_idx, "toString", "()Ljava/lang/String;")?;
            return Ok(vec![JI::Invokevirtual(mref)]);
        }

        // String -> BigDecimal
        if src == &Type::String && dest == &Type::Class(BIG_DECIMAL_CLASS.into()) {
            let bd_idx = cp.add_class(BIG_DECIMAL_CLASS)?;
            let ctor = cp.add_method_ref(bd_idx, "<init>", "(Ljava/lang/String;)V")?;
            return Ok(vec![JI::New(bd_idx), JI::Swap, JI::Invokespecial(ctor)]);
        }

        // BigDecimal -> String
        if src == &Type::Class(BIG_DECIMAL_CLASS.into()) && dest == &Type::String {
            let bd_idx = cp.add_class(BIG_DECIMAL_CLASS)?;
            let mref = cp.add_method_ref(bd_idx, "toString", "()Ljava/lang/String;")?;
            return Ok(vec![JI::Invokevirtual(mref)]);
        }

        // BigInteger -> BigDecimal
        if src == &Type::Class(BIG_INTEGER_CLASS.into())
            && dest == &Type::Class(BIG_DECIMAL_CLASS.into())
        {
            let bd_idx = cp.add_class(BIG_DECIMAL_CLASS)?;
            let ctor = cp.add_method_ref(bd_idx, "<init>", "(Ljava/math/BigInteger;)V")?;
            return Ok(vec![JI::New(bd_idx), JI::Swap, JI::Invokespecial(ctor)]);
        }

        // BigDecimal -> BigInteger
        if src == &Type::Class(BIG_DECIMAL_CLASS.into())
            && dest == &Type::Class(BIG_INTEGER_CLASS.into())
        {
            let bd_idx = cp.add_class(BIG_DECIMAL_CLASS)?;
            let mref =
                cp.add_method_ref(bd_idx, "toBigIntegerExact", "()Ljava/math/BigInteger;")?;
            return Ok(vec![JI::Invokevirtual(mref)]);
        }

        // Generic checkcast for all other reference-to-reference
        // Check if both are reference types AND have valid internal names/descriptors
        if let (Some(_), Some(dest_name)) = (
            src.to_jvm_descriptor_or_internal_name(),
            dest.to_jvm_descriptor_or_internal_name(),
        ) {
            let dest_idx = cp.add_class(&dest_name)?;

            return Ok(vec![JI::Checkcast(dest_idx)]);
        }
    }

    // 5. Fallback: unsupported
    Err(jvm::Error::VerificationError {
        context: "get_casting_instructions".into(),
        message: format!("Unsupported cast: {:?} → {:?}", src, dest),
    })
}

/// Helper for primitive→primitive casts via a small JVM‐opcode graph + narrowing.
fn primitive_to_primitive(src: &Type, dest: &Type) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;
    use std::collections::{HashMap, HashSet, VecDeque};

    // Normalize smaller ints → I32
    fn normalize(ty: &Type) -> Type {
        match ty {
            Type::I8 | Type::I16 | Type::Char | Type::Boolean => Type::I32,
            _ => ty.clone(),
        }
    }
    // Final narrowing instr for byte/char/short
    fn narrow(dest: &Type) -> Option<Instruction> {
        Some(match dest {
            Type::I8 => JI::I2b,
            Type::Char => JI::I2c,
            Type::I16 => JI::I2s,
            _ => return None,
        })
    }

    let ns = normalize(src);
    let nd = normalize(dest);

    // If same, just maybe narrow
    if ns == nd {
        return Ok(narrow(dest).into_iter().collect());
    }

    // Build graph
    let graph: HashMap<_, _> = [
        (
            Type::I32,
            vec![
                (Type::I64, JI::I2l),
                (Type::F64, JI::I2d),
                (Type::F32, JI::I2f),
            ],
        ),
        (
            Type::I64,
            vec![
                (Type::I32, JI::L2i),
                (Type::F64, JI::L2d),
                (Type::F32, JI::L2f),
            ],
        ),
        (
            Type::F64,
            vec![
                (Type::I32, JI::D2i),
                (Type::I64, JI::D2l),
                (Type::F32, JI::D2f),
            ],
        ),
        (
            Type::F32,
            vec![
                (Type::I32, JI::F2i),
                (Type::I64, JI::F2l),
                (Type::F64, JI::F2d),
            ],
        ),
    ]
    .into_iter()
    .collect();

    // BFS
    #[derive(Clone)]
    struct Node {
        ty: Type,
        path: Vec<Instruction>,
    }
    let mut q = VecDeque::new();
    let mut seen = HashSet::new();
    q.push_back(Node {
        ty: ns.clone(),
        path: vec![],
    });
    seen.insert(ns.clone());

    while let Some(Node { ty, path }) = q.pop_front() {
        if ty == nd {
            let mut p = path;
            if let Some(n) = narrow(dest) {
                p.push(n);
            }
            return Ok(p);
        }
        if let Some(neigh) = graph.get(&ty) {
            for (nty, op) in neigh {
                if seen.insert(nty.clone()) {
                    let mut p2 = path.clone();
                    p2.push(op.clone());
                    q.push_back(Node {
                        ty: nty.clone(),
                        path: p2,
                    });
                }
            }
        }
    }

    Err(jvm::Error::VerificationError {
        context: "primitive_to_primitive".into(),
        message: format!("No path {:?}→{:?}", src, dest),
    })
}

/// primitive → BigInteger via BigInteger.valueOf(long)
fn prim_to_bigint(src: &Type, cp: &mut ConstantPool) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;
    let bi_idx = cp.add_class(BIG_INTEGER_CLASS)?;
    let mref = cp.add_method_ref(bi_idx, "valueOf", "(J)Ljava/math/BigInteger;")?;
    let mut ins = Vec::new();
    match src {
        Type::I64            => ins.push(JI::Invokestatic(mref)),
        _ /* smaller ints */ => {
            ins.push(JI::I2l);
            ins.push(JI::Invokestatic(mref));
        }
    }
    Ok(ins)
}

/// primitive → BigDecimal via BigDecimal.valueOf(long|double)
fn prim_to_bigdec(src: &Type, cp: &mut ConstantPool) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;
    let bd_idx = cp.add_class(BIG_DECIMAL_CLASS)?;
    let (cast, sig) = match src {
        Type::F32           => (Some(JI::F2d), "(D)Ljava/math/BigDecimal;"),
        Type::F64           => (None,          "(D)Ljava/math/BigDecimal;"),
        _ /* ints */        => (Some(JI::I2l), "(J)Ljava/math/BigDecimal;"),
    };
    let mref = cp.add_method_ref(bd_idx, "valueOf", sig)?;
    let mut ins = Vec::new();
    if let Some(c) = cast {
        ins.push(c)
    }
    ins.push(JI::Invokestatic(mref));
    Ok(ins)
}

/// BigInteger → primitive via intValue/longValue/doubleValue(...)
fn bigint_to_prim(dest: &Type, cp: &mut ConstantPool) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;
    let bi_idx = cp.add_class(BIG_INTEGER_CLASS)?;
    let mut ins = Vec::new();
    match dest {
        Type::I32 | Type::I8 | Type::I16 | Type::Char | Type::Boolean => {
            let m = cp.add_method_ref(bi_idx, "intValue", "()I")?;
            ins.push(JI::Invokevirtual(m));
            if let Some(n) = final_conversion(dest) {
                ins.push(n)
            }
        }
        Type::I64 => {
            let m = cp.add_method_ref(bi_idx, "longValue", "()J")?;
            ins.push(JI::Invokevirtual(m));
        }
        Type::F32 => {
            let m = cp.add_method_ref(bi_idx, "doubleValue", "()D")?;
            ins.push(JI::Invokevirtual(m));
            ins.push(JI::D2f);
        }
        Type::F64 => {
            let m = cp.add_method_ref(bi_idx, "doubleValue", "()D")?;
            ins.push(JI::Invokevirtual(m));
        }
        _ => {
            return Err(jvm::Error::VerificationError {
                context: "bigint_to_prim".into(),
                message: format!("Cannot unbox BigInteger → {:?}", dest),
            });
        }
    }
    Ok(ins)
}

/// BigDecimal → primitive via intValue/longValue/floatValue/doubleValue(...)
fn bigdec_to_prim(dest: &Type, cp: &mut ConstantPool) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;
    let bd_idx = cp.add_class(BIG_DECIMAL_CLASS)?;
    let mut ins = Vec::new();
    let (name, sig) = match dest {
        Type::I32 | Type::I8 | Type::I16 | Type::Char | Type::Boolean => ("intValue", "()I"),
        Type::I64 => ("longValue", "()J"),
        Type::F32 => ("floatValue", "()F"),
        Type::F64 => ("doubleValue", "()D"),
        _ => {
            return Err(jvm::Error::VerificationError {
                context: "bigdec_to_prim".into(),
                message: format!("Cannot unbox BigDecimal → {:?}", dest),
            });
        }
    };
    let m = cp.add_method_ref(bd_idx, name, sig)?;
    ins.push(JI::Invokevirtual(m));
    if matches!(dest, Type::I8 | Type::I16 | Type::Char) {
        if let Some(n) = final_conversion(dest) {
            ins.push(n)
        }
    }
    Ok(ins)
}

/// Final narrowing for byte/short/char
fn final_conversion(dest: &Type) -> Option<Instruction> {
    use Instruction::*;
    match dest {
        Type::I8 => Some(I2b),
        Type::Char => Some(I2c),
        Type::I16 => Some(I2s),
        _ => None,
    }
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
