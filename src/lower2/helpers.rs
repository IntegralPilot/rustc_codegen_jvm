use super::{constant_pool::InternedConstantPool, oomir};

use super::jvm::{self, attributes::Instruction};
use oomir::Type;

use super::consts::{get_int_const_instr, get_long_const_instr};
use super::{BIG_DECIMAL_CLASS, BIG_INTEGER_CLASS, F128_CLASS, I128_CLASS, U128_CLASS};

/// Returns the number of JVM local variable slots a type occupies (0, 1, or 2).
pub fn get_type_size(ty: &Type) -> u16 {
    match ty {
        Type::Unit | Type::Void => 0,
        Type::I64 | Type::U64 | Type::F64 => 2,
        _ => 1,
    }
}

fn constant_load_stack_floor(constant: &oomir::Constant) -> u16 {
    use oomir::Constant;
    match constant {
        Constant::Unit => 0,
        Constant::I64(_) | Constant::U64(_) | Constant::F64(_) => 2,
        Constant::Array(_, elements) => elements
            .iter()
            .map(|element| 3 + constant_load_stack_floor(element))
            .max()
            .unwrap_or(1),
        Constant::Slice(element_type, elements) => {
            let backing = Constant::Array(element_type.clone(), elements.clone());
            (2 + constant_load_stack_floor(&backing)).max(4)
        }
        Constant::SliceRef { backing, .. } => (2 + constant_load_stack_floor(backing)).max(4),
        Constant::Instance { params, .. } => {
            let mut stack = 2;
            let mut peak = stack;
            for param in params {
                peak = peak.max(stack + constant_load_stack_floor(param));
                stack += get_type_size(&Type::from_constant(param));
            }
            peak
        }
        Constant::StaticRef { ty, .. } | Constant::FactoryCall { ty, .. } => get_type_size(ty),
        _ => 1,
    }
}

fn operand_load_stack_floor(operand: &oomir::Operand) -> u16 {
    match operand {
        oomir::Operand::Constant(constant) => constant_load_stack_floor(constant),
        oomir::Operand::Variable { ty, .. } => get_type_size(ty),
    }
}

fn operand_sequence_stack_floor<'a>(
    operands: impl IntoIterator<Item = &'a oomir::Operand>,
    initial_stack: u16,
) -> u16 {
    let mut stack = initial_stack;
    let mut peak = stack;
    for operand in operands {
        peak = peak.max(stack + operand_load_stack_floor(operand));
        stack += operand.get_type().as_ref().map(get_type_size).unwrap_or(0);
    }
    peak
}

pub fn oomir_function_stack_floor(function: &oomir::Function) -> u16 {
    let mut floor = 0;
    for block in function.body.basic_blocks.values() {
        for instruction in &block.instructions {
            match instruction {
                oomir::Instruction::Add { op1, op2, .. }
                | oomir::Instruction::Sub { op1, op2, .. }
                | oomir::Instruction::Mul { op1, op2, .. }
                | oomir::Instruction::Div { op1, op2, .. }
                | oomir::Instruction::Rem { op1, op2, .. }
                | oomir::Instruction::Eq { op1, op2, .. }
                | oomir::Instruction::Ne { op1, op2, .. }
                | oomir::Instruction::Lt { op1, op2, .. }
                | oomir::Instruction::Le { op1, op2, .. }
                | oomir::Instruction::Gt { op1, op2, .. }
                | oomir::Instruction::Ge { op1, op2, .. }
                | oomir::Instruction::BitAnd { op1, op2, .. }
                | oomir::Instruction::BitOr { op1, op2, .. }
                | oomir::Instruction::BitXor { op1, op2, .. }
                | oomir::Instruction::Shl { op1, op2, .. }
                | oomir::Instruction::Shr { op1, op2, .. } => {
                    floor = floor.max(operand_sequence_stack_floor([op1, op2], 0));
                }
                oomir::Instruction::Not { src, .. }
                | oomir::Instruction::Neg { src, .. }
                | oomir::Instruction::Move { src, .. } => {
                    floor = floor.max(operand_load_stack_floor(src));
                }
                oomir::Instruction::Branch { condition, .. } => {
                    floor = floor.max(operand_load_stack_floor(condition));
                }
                oomir::Instruction::Return {
                    operand: Some(operand),
                } => {
                    floor = floor.max(operand_load_stack_floor(operand));
                }
                oomir::Instruction::ConstructObject { args, .. } => {
                    floor = floor.max(operand_sequence_stack_floor(
                        args.iter().map(|(operand, _)| operand),
                        2,
                    ));
                }
                oomir::Instruction::InvokeStatic { args, .. } => {
                    floor = floor.max(operand_sequence_stack_floor(args, 0));
                }
                oomir::Instruction::CallIndirect {
                    function_ptr, args, ..
                } => {
                    floor = floor.max(operand_load_stack_floor(function_ptr));
                    floor = floor.max(operand_sequence_stack_floor(args, 1));
                }
                oomir::Instruction::InvokeInterface { operand, args, .. }
                | oomir::Instruction::InvokeVirtual { operand, args, .. } => {
                    floor = floor.max(operand_load_stack_floor(operand));
                    floor = floor.max(operand_sequence_stack_floor(args, 1));
                }
                oomir::Instruction::ThrowNewWithMessage { .. } => {
                    floor = floor.max(3);
                }
                oomir::Instruction::Switch { discr, .. } => {
                    floor = floor.max(operand_load_stack_floor(discr));
                }
                oomir::Instruction::NewArray { size, .. } => {
                    floor = floor.max(operand_load_stack_floor(size));
                }
                oomir::Instruction::ArrayStore { index, value, .. } => {
                    floor = floor.max(operand_sequence_stack_floor([index, value], 1));
                }
                oomir::Instruction::ArrayGet { array, index, .. } => {
                    floor = floor.max(operand_sequence_stack_floor([array, index], 0));
                }
                oomir::Instruction::Length { array, .. }
                | oomir::Instruction::GetField { object: array, .. }
                | oomir::Instruction::Cast { op: array, .. } => {
                    floor = floor.max(operand_load_stack_floor(array));
                }
                oomir::Instruction::SetField { value, .. } => {
                    floor = floor.max(1 + operand_load_stack_floor(value));
                }
                _ => {}
            }
        }
    }
    floor
}

/// Gets the appropriate type-specific load instruction.
pub fn get_load_instruction(ty: &Type, index: u16) -> Result<Instruction, jvm::Error> {
    Ok(match ty {
        // Integer-like types
        Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::F16
        | Type::I32
        | Type::U32
        | Type::Boolean
        | Type::Char => {
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
        Type::I64 | Type::U64 => match index {
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
        | Type::Pointer(_)
        | Type::MutableReference(_)
        | Type::Array(_)
        | Type::Slice(_)
        | Type::Str
        | Type::String
        | Type::Class(_)
        | Type::Interface(_) => match index {
            0 => Instruction::Aload_0,
            1 => Instruction::Aload_1,
            2 => Instruction::Aload_2,
            3 => Instruction::Aload_3,
            _ if index <= u8::MAX as u16 => Instruction::Aload(index as u8),
            _ => Instruction::Aload_w(index),
        },
        // For void, return an error
        Type::Unit | Type::Void => {
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
        I8 | U8 | I16 | U16 | F16 | I32 | U32 | Boolean | Char => {
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
        I64 | U64 => {
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
        Reference(_) | Pointer(_) | MutableReference(_) | Array(_) | Slice(_) | Str | String
        | Class(_) | Interface(_) => {
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
        Unit | Void => {
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
    fn_name: &str,
    src: &Type,
    dest: &Type,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;

    // 0. Identity cast
    if src == dest {
        return Ok(vec![]);
    }

    // 1. Primitive <-> Primitive
    if src.is_jvm_primitive_like() && dest.is_jvm_primitive_like() {
        return primitive_to_primitive(src, dest, cp);
    }

    // 2. Primitive -> BigInteger / BigDecimal
    if src.is_jvm_primitive_like() {
        if let Type::Class(cn) = dest {
            if cn == F128_CLASS {
                return prim_to_f128(src, cp);
            }
            if cn == I128_CLASS || cn == U128_CLASS {
                return prim_to_int128(src, cn, cp);
            }
            if cn == BIG_INTEGER_CLASS {
                return prim_to_bigint(src, cp);
            }
            if cn == BIG_DECIMAL_CLASS {
                return prim_to_bigdec(src, cp);
            }
            if cn == "java/lang/Object" {
                let wrapper_method = match src {
                    Type::Boolean => Some(("java/lang/Boolean", "(Z)Ljava/lang/Boolean;")),
                    Type::Char => Some(("java/lang/Character", "(C)Ljava/lang/Character;")),
                    Type::I8 | Type::U8 => Some(("java/lang/Byte", "(B)Ljava/lang/Byte;")),
                    Type::I16 | Type::F16 => Some(("java/lang/Short", "(S)Ljava/lang/Short;")),
                    Type::U16 => Some(("java/lang/Character", "(C)Ljava/lang/Character;")),
                    Type::I32 | Type::U32 => Some(("java/lang/Integer", "(I)Ljava/lang/Integer;")),
                    Type::I64 | Type::U64 => Some(("java/lang/Long", "(J)Ljava/lang/Long;")),
                    Type::F32 => Some(("java/lang/Float", "(F)Ljava/lang/Float;")),
                    Type::F64 => Some(("java/lang/Double", "(D)Ljava/lang/Double;")),
                    _ => None,
                };

                if let Some((wrapper, descriptor)) = wrapper_method {
                    let wrapper_idx = cp.add_class(wrapper)?;
                    let mref = cp.add_method_ref(wrapper_idx, "valueOf", descriptor)?;
                    return Ok(vec![JI::Invokestatic(mref)]);
                }
            }
            if oomir::is_non_null_class_name(cn) {
                let class_idx = cp.add_class(cn)?;
                let init = cp.add_method_ref(class_idx, "<init>", "()V")?;
                let mut instrs = Vec::new();
                match get_type_size(src) {
                    2 => instrs.push(JI::Pop2),
                    _ => instrs.push(JI::Pop),
                }
                instrs.push(JI::New(class_idx));
                instrs.push(JI::Dup);
                instrs.push(JI::Invokespecial(init));
                return Ok(instrs);
            }
        }
    }

    // 3. BigInteger / BigDecimal -> Primitive
    if let Type::Class(cn) = src {
        if dest.is_jvm_primitive_like() {
            if cn == F128_CLASS {
                return f128_to_prim(dest, cp);
            }
            if cn == I128_CLASS || cn == U128_CLASS {
                return int128_to_prim(cn, dest, cp);
            }
            if cn == BIG_INTEGER_CLASS {
                return bigint_to_prim(dest, cp);
            }
            if cn == BIG_DECIMAL_CLASS {
                return bigdec_to_prim(dest, cp);
            }
        }
    }

    if src == &Type::Class("java/lang/Object".to_string()) && dest.is_jvm_primitive_like() {
        if dest == &Type::Boolean {
            let class = cp.add_class("java/lang/Boolean")?;
            let unbox = cp.add_method_ref(class, "booleanValue", "()Z")?;
            return Ok(vec![JI::Checkcast(class), JI::Invokevirtual(unbox)]);
        }
        let (method, descriptor) = match dest {
            Type::I8 | Type::U8 => ("objectToI8", "(Ljava/lang/Object;)B"),
            Type::I16 | Type::F16 => ("objectToI16", "(Ljava/lang/Object;)S"),
            Type::U16 | Type::Char => ("objectToU16", "(Ljava/lang/Object;)C"),
            Type::I32 | Type::U32 => ("objectToI32", "(Ljava/lang/Object;)I"),
            Type::I64 | Type::U64 => ("objectToI64", "(Ljava/lang/Object;)J"),
            Type::F32 => ("objectToF32", "(Ljava/lang/Object;)F"),
            Type::F64 => ("objectToF64", "(Ljava/lang/Object;)D"),
            _ => unreachable!(),
        };
        let class = cp.add_class("org/rustlang/runtime/Numbers")?;
        let unbox = cp.add_method_ref(class, method, descriptor)?;
        return Ok(vec![JI::Invokestatic(unbox)]);
    }

    // 4. Reference -> Reference (including String, BigInteger, BigDecimal interop)
    if src.is_jvm_reference_type() && dest.is_jvm_reference_type() {
        if let (Type::Class(src_class), Type::Class(dest_class)) = (src, dest) {
            if (src_class == I128_CLASS || src_class == U128_CLASS) && dest_class == F128_CLASS {
                let class = cp.add_class(F128_CLASS)?;
                let method_name = if src_class == I128_CLASS {
                    "fromI128Value"
                } else {
                    "fromU128Value"
                };
                let method = cp.add_method_ref(
                    class,
                    method_name,
                    &format!("(L{src_class};)L{F128_CLASS};"),
                )?;
                return Ok(vec![JI::Invokestatic(method)]);
            }
            if src_class == F128_CLASS && (dest_class == I128_CLASS || dest_class == U128_CLASS) {
                let class = cp.add_class(F128_CLASS)?;
                let method_name = if dest_class == I128_CLASS {
                    "castToI128"
                } else {
                    "castToU128"
                };
                let method = cp.add_method_ref(class, method_name, &format!("()L{dest_class};"))?;
                return Ok(vec![JI::Invokevirtual(method)]);
            }
        }

        if let (Type::Class(src_class), Type::Class(dest_class)) = (src, dest)
            && (src_class == I128_CLASS || src_class == U128_CLASS)
            && (dest_class == I128_CLASS || dest_class == U128_CLASS)
        {
            let owner = cp.add_class(src_class)?;
            let method_name = if dest_class == I128_CLASS {
                "toI128"
            } else {
                "toU128"
            };
            let method = cp.add_method_ref(owner, method_name, &format!("()L{dest_class};"))?;
            return Ok(vec![JI::Invokevirtual(method)]);
        }

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

        if let Type::Class(cn) = dest {
            if oomir::is_non_null_class_name(cn) {
                let class_idx = cp.add_class(cn)?;
                let init = cp.add_method_ref(class_idx, "<init>", "()V")?;
                return Ok(vec![
                    JI::Pop,
                    JI::New(class_idx),
                    JI::Dup,
                    JI::Invokespecial(init),
                ]);
            }
        }

        // String → short[] (also used for raw/slice pointer stand-ins).
        if src == &Type::String
            && (dest == &Type::Array(Box::new(Type::I16))
                || dest == &Type::MutableReference(Box::new(Type::I16)))
        {
            let core_idx = cp.add_class("org/rustlang/core/Core")?;
            let mref = cp.add_method_ref(core_idx, "toShortArray", "(Ljava/lang/String;)[S")?;
            return Ok(vec![Instruction::Invokestatic(mref)]);
        }

        // short[] → String
        if (src == &Type::Array(Box::new(Type::I16))
            || src == &Type::MutableReference(Box::new(Type::I16)))
            && dest == &Type::String
        {
            let core_idx = cp.add_class("org/rustlang/core/Core")?;
            let mref = cp.add_method_ref(core_idx, "fromShortArray", "([S)Ljava/lang/String;")?;
            return Ok(vec![Instruction::Invokestatic(mref)]);
        }

        if let Type::MutableReference(box inner) = src {
            let mut instrs = vec![JI::Iconst_0, JI::Aaload];
            if dest != inner {
                if let Some(dest_name) = dest.to_jvm_descriptor_or_internal_name() {
                    let dest_idx = cp.add_class(&dest_name)?;
                    instrs.push(JI::Checkcast(dest_idx));
                }
            }
            return Ok(instrs);
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

    // Keep code generation moving after a representation mismatch. The replacement is
    // verifier-correct but deliberately has no semantic meaning: discard the source and
    // synthesize the JVM default value for the requested destination type.
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Warn,
        "bytecode-gen",
        format!(
            "Unsupported cast: {:?} → {:?}; replacing it with the destination's default value. In function: {}",
            src, dest, fn_name
        )
    );

    let mut instructions = match get_type_size(src) {
        0 => Vec::new(),
        2 => vec![JI::Pop2],
        _ => vec![JI::Pop],
    };
    instructions.extend(match dest {
        Type::Unit | Type::Void => Vec::new(),
        Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::F16
        | Type::I32
        | Type::U32
        | Type::Boolean
        | Type::Char => vec![JI::Iconst_0],
        Type::I64 | Type::U64 => vec![JI::Lconst_0],
        Type::F32 => vec![JI::Fconst_0],
        Type::F64 => vec![JI::Dconst_0],
        Type::MutableReference(_)
        | Type::Pointer(_)
        | Type::Reference(_)
        | Type::Array(_)
        | Type::Slice(_)
        | Type::Str
        | Type::String
        | Type::Class(_)
        | Type::Interface(_) => vec![JI::Aconst_null],
    });
    Ok(instructions)
}

/// Semantic Rust primitive casts.  The JVM descriptor alone is insufficient here:
/// `u32` is carried in an `int`, `u64` in a `long`, and `f16` in a `short` bit-pattern.
fn primitive_to_primitive(
    src: &Type,
    dest: &Type,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;

    fn int_width(ty: &Type) -> Option<u32> {
        match ty {
            Type::Boolean => Some(1),
            Type::I8 | Type::U8 => Some(8),
            Type::I16 | Type::U16 | Type::Char => Some(16),
            Type::I32 | Type::U32 => Some(32),
            Type::I64 | Type::U64 => Some(64),
            _ => None,
        }
    }

    fn is_unsigned(ty: &Type) -> bool {
        matches!(
            ty,
            Type::Boolean | Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::Char
        )
    }

    fn narrow(ty: &Type) -> Option<Instruction> {
        match ty {
            Type::I8 | Type::U8 => Some(JI::I2b),
            Type::I16 => Some(JI::I2s),
            Type::U16 | Type::Char => Some(JI::I2c),
            _ => None,
        }
    }

    fn numbers_call(
        cp: &mut InternedConstantPool,
        name: &str,
        descriptor: &str,
    ) -> Result<Instruction, jvm::Error> {
        let class = cp.add_class("org/rustlang/runtime/Numbers")?;
        let method = cp.add_method_ref(class, name, descriptor)?;
        Ok(JI::Invokestatic(method))
    }

    // binary16 is stored as raw bits. Decode before a cast out, and round once on a cast in.
    if src == &Type::F16 {
        let mut result = vec![numbers_call(cp, "f16ToF32", "(S)F")?];
        result.extend(primitive_to_primitive(&Type::F32, dest, cp)?);
        return Ok(result);
    }
    if dest == &Type::F16 {
        return match src {
            Type::F32 => Ok(vec![numbers_call(cp, "f32ToF16", "(F)S")?]),
            Type::F64 => Ok(vec![numbers_call(cp, "f64ToF16", "(D)S")?]),
            _ if int_width(src).is_some() => {
                let mut result = primitive_to_primitive(src, &Type::F64, cp)?;
                result.push(numbers_call(cp, "f64ToF16", "(D)S")?);
                Ok(result)
            }
            _ => Err(jvm::Error::VerificationError {
                context: "primitive_to_primitive".into(),
                message: format!("No path {src:?}→F16"),
            }),
        };
    }

    if matches!(src, Type::F32 | Type::F64) && matches!(dest, Type::F32 | Type::F64) {
        return Ok(match (src, dest) {
            (Type::F32, Type::F64) => vec![JI::F2d],
            (Type::F64, Type::F32) => vec![JI::D2f],
            _ => Vec::new(),
        });
    }

    if matches!(src, Type::F32 | Type::F64) && int_width(dest).is_some() {
        let prefix = if src == &Type::F32 { "f32" } else { "f64" };
        let source_descriptor = if src == &Type::F32 { "F" } else { "D" };
        let (suffix, return_descriptor, direct) = match dest {
            Type::I8 => ("ToI8", "B", None),
            Type::I16 => ("ToI16", "S", None),
            Type::I32 => (
                "",
                "",
                Some(if src == &Type::F32 { JI::F2i } else { JI::D2i }),
            ),
            Type::I64 => (
                "",
                "",
                Some(if src == &Type::F32 { JI::F2l } else { JI::D2l }),
            ),
            Type::U8 | Type::Boolean => ("ToU8", "B", None),
            Type::U16 | Type::Char => ("ToU16", "C", None),
            Type::U32 => ("ToU32", "I", None),
            Type::U64 => ("ToU64", "J", None),
            _ => unreachable!(),
        };
        if let Some(op) = direct {
            return Ok(vec![op]);
        }
        return Ok(vec![numbers_call(
            cp,
            &format!("{prefix}{suffix}"),
            &format!("({source_descriptor}){return_descriptor}"),
        )?]);
    }

    if int_width(src).is_some() && matches!(dest, Type::F32 | Type::F64) {
        let to_f32 = dest == &Type::F32;
        return Ok(match src {
            Type::U32 => vec![numbers_call(
                cp,
                if to_f32 { "u32ToF32" } else { "u32ToF64" },
                if to_f32 { "(I)F" } else { "(I)D" },
            )?],
            Type::U64 => vec![numbers_call(
                cp,
                if to_f32 { "u64ToF32" } else { "u64ToF64" },
                if to_f32 { "(J)F" } else { "(J)D" },
            )?],
            Type::I64 => vec![if to_f32 { JI::L2f } else { JI::L2d }],
            Type::U8 => vec![
                get_int_const_instr(cp, 0xff),
                JI::Iand,
                if to_f32 { JI::I2f } else { JI::I2d },
            ],
            _ => vec![if to_f32 { JI::I2f } else { JI::I2d }],
        });
    }

    if let (Some(src_width), Some(dest_width)) = (int_width(src), int_width(dest)) {
        let mut result = Vec::new();
        if dest_width <= 32 {
            if src_width == 64 {
                result.push(JI::L2i);
            }
            if let Some(op) = narrow(dest) {
                result.push(op);
            }
            return Ok(result);
        }

        if src_width < 64 {
            match src {
                Type::U8 => {
                    result.push(get_int_const_instr(cp, 0xff));
                    result.push(JI::Iand);
                    result.push(JI::I2l);
                }
                Type::U32 => {
                    result.push(JI::I2l);
                    result.push(get_long_const_instr(cp, 0xffff_ffff));
                    result.push(JI::Land);
                }
                _ if is_unsigned(src) => result.push(JI::I2l),
                _ => result.push(JI::I2l),
            }
        }
        return Ok(result);
    }

    Err(jvm::Error::VerificationError {
        context: "primitive_to_primitive".into(),
        message: format!("No path {src:?}→{dest:?}"),
    })
}

fn prim_to_int128(
    src: &Type,
    dest_class: &str,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;

    let class = cp.add_class(dest_class)?;
    let return_descriptor = format!("L{dest_class};");
    match src {
        Type::F16 => {
            let numbers = cp.add_class("org/rustlang/runtime/Numbers")?;
            let decode = cp.add_method_ref(numbers, "f16ToF32", "(S)F")?;
            let convert =
                cp.add_method_ref(class, "fromF32", &format!("(F){return_descriptor}"))?;
            Ok(vec![JI::Invokestatic(decode), JI::Invokestatic(convert)])
        }
        Type::F32 => {
            let convert =
                cp.add_method_ref(class, "fromF32", &format!("(F){return_descriptor}"))?;
            Ok(vec![JI::Invokestatic(convert)])
        }
        Type::F64 => {
            let convert =
                cp.add_method_ref(class, "fromF64", &format!("(D){return_descriptor}"))?;
            Ok(vec![JI::Invokestatic(convert)])
        }
        _ => {
            let unsigned = matches!(
                src,
                Type::Boolean | Type::Char | Type::U8 | Type::U16 | Type::U32 | Type::U64
            );
            let carrier_type = if unsigned { Type::U64 } else { Type::I64 };
            let mut instructions = primitive_to_primitive(src, &carrier_type, cp)?;
            let method_name = if unsigned { "fromU64" } else { "fromI64" };
            let convert =
                cp.add_method_ref(class, method_name, &format!("(J){return_descriptor}"))?;
            instructions.push(JI::Invokestatic(convert));
            Ok(instructions)
        }
    }
}

fn prim_to_f128(src: &Type, cp: &mut InternedConstantPool) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;

    let class = cp.add_class(F128_CLASS)?;
    match src {
        Type::F16 => {
            let numbers = cp.add_class("org/rustlang/runtime/Numbers")?;
            let decode = cp.add_method_ref(numbers, "f16ToF32", "(S)F")?;
            let convert = cp.add_method_ref(class, "fromF32", &format!("(F)L{F128_CLASS};"))?;
            Ok(vec![JI::Invokestatic(decode), JI::Invokestatic(convert)])
        }
        Type::F32 => {
            let convert = cp.add_method_ref(class, "fromF32", &format!("(F)L{F128_CLASS};"))?;
            Ok(vec![JI::Invokestatic(convert)])
        }
        Type::F64 => {
            let convert = cp.add_method_ref(class, "fromF64", &format!("(D)L{F128_CLASS};"))?;
            Ok(vec![JI::Invokestatic(convert)])
        }
        _ => {
            let unsigned = matches!(
                src,
                Type::Boolean | Type::Char | Type::U8 | Type::U16 | Type::U32 | Type::U64
            );
            let carrier_type = if unsigned { Type::U64 } else { Type::I64 };
            let mut instructions = primitive_to_primitive(src, &carrier_type, cp)?;
            let method_name = if unsigned { "fromU64" } else { "fromI64" };
            let convert = cp.add_method_ref(class, method_name, &format!("(J)L{F128_CLASS};"))?;
            instructions.push(JI::Invokestatic(convert));
            Ok(instructions)
        }
    }
}

fn f128_to_prim(
    dest: &Type,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;

    let class = cp.add_class(F128_CLASS)?;
    let (method_name, descriptor) = match dest {
        Type::I8 => ("castToI8", "()B"),
        Type::U8 | Type::Boolean => ("castToU8", "()B"),
        Type::I16 => ("castToI16", "()S"),
        Type::U16 | Type::Char => ("castToU16", "()C"),
        Type::I32 => ("castToI32", "()I"),
        Type::U32 => ("castToU32", "()I"),
        Type::I64 => ("castToI64", "()J"),
        Type::U64 => ("castToU64", "()J"),
        Type::F32 => ("castToF32", "()F"),
        Type::F64 => ("castToF64", "()D"),
        Type::F16 => ("castToF16", "()S"),
        _ => {
            return Err(jvm::Error::VerificationError {
                context: "f128_to_prim".into(),
                message: format!("Cannot cast F128 to {dest:?}"),
            });
        }
    };
    let method = cp.add_method_ref(class, method_name, descriptor)?;
    Ok(vec![JI::Invokevirtual(method)])
}

fn int128_to_prim(
    src_class: &str,
    dest: &Type,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;

    let class = cp.add_class(src_class)?;
    match dest {
        Type::F16 => {
            let to_double = cp.add_method_ref(class, "doubleValue", "()D")?;
            let numbers = cp.add_class("org/rustlang/runtime/Numbers")?;
            let to_half = cp.add_method_ref(numbers, "f64ToF16", "(D)S")?;
            Ok(vec![
                JI::Invokevirtual(to_double),
                JI::Invokestatic(to_half),
            ])
        }
        Type::F32 => {
            let method = cp.add_method_ref(class, "floatValue", "()F")?;
            Ok(vec![JI::Invokevirtual(method)])
        }
        Type::F64 => {
            let method = cp.add_method_ref(class, "doubleValue", "()D")?;
            Ok(vec![JI::Invokevirtual(method)])
        }
        _ => {
            let method = cp.add_method_ref(class, "longValue", "()J")?;
            let mut instructions = vec![JI::Invokevirtual(method)];
            instructions.extend(primitive_to_primitive(&Type::I64, dest, cp)?);
            Ok(instructions)
        }
    }
}

/// primitive → BigInteger via BigInteger.valueOf(long)
fn prim_to_bigint(
    src: &Type,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
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
fn prim_to_bigdec(
    src: &Type,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
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
fn bigint_to_prim(
    dest: &Type,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
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
fn bigdec_to_prim(
    dest: &Type,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
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
        // Rust enum values are instances of generated variant subclasses. Their class
        // names are derived from the base enum as `Base$Variant`, and lower1 declares
        // `Base` as their JVM superclass.
        (oomir::Type::Class(source), oomir::Type::Class(target)) => source
            .strip_prefix(target)
            .is_some_and(|suffix| suffix.starts_with('$') && suffix.len() > 1),
        // TODO: Add more other compatibility rules (e.g., Interface implementations).
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
