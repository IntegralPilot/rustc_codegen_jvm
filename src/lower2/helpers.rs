use super::{constant_pool::InternedConstantPool, oomir};

use super::jvm::{self, attributes::Instruction};
use oomir::Type;

use super::{F128_CLASS, I128_CLASS, U128_CLASS};

/// Returns the number of JVM local variable slots a type occupies (0, 1, or 2).
pub fn get_type_size(ty: &Type) -> u16 {
    match ty {
        Type::Unit | Type::Void => 0,
        Type::I64 | Type::U64 | Type::F64 => 2,
        _ => 1,
    }
}

pub fn local_kind(ty: &Type) -> Option<jvm_compiler_core::jvm::locals::LocalKind> {
    use jvm_compiler_core::jvm::locals::LocalKind as K;
    Some(match ty {
        Type::Unit | Type::Void => return None,
        Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::F16
        | Type::I32
        | Type::U32
        | Type::Boolean
        | Type::Char => K::Int,
        Type::I64 | Type::U64 => K::Long,
        Type::F32 => K::Float,
        Type::F64 => K::Double,
        Type::Reference(_)
        | Type::Pointer(_)
        | Type::MutableReference(_)
        | Type::Array(_)
        | Type::Slice(_)
        | Type::Str
        | Type::Class(_)
        | Type::Interface(_) => K::Reference,
    })
}

pub fn get_load_instruction(ty: &Type, index: u16) -> jvm::Result<Instruction> {
    local_kind(ty)
        .map(|kind| kind.load(index))
        .ok_or_else(|| jvm::Error::VerificationError {
            context: "local load".into(),
            message: "Cannot load a type without a JVM value".into(),
        })
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

    if !src.has_jvm_value() && dest.is_jvm_reference_type() {
        return Ok(vec![JI::Aconst_null]);
    }

    // 1. Primitive <-> Primitive
    if src.is_jvm_primitive_like() && dest.is_jvm_primitive_like() {
        return primitive_to_primitive(src, dest, cp);
    }

    // 2. Primitive -> runtime numeric wrapper
    if src.is_jvm_primitive_like() {
        if let Type::Class(cn) = dest {
            if cn == F128_CLASS {
                return prim_to_f128(src, cp);
            }
            if cn == I128_CLASS || cn == U128_CLASS {
                return prim_to_int128(src, cn, cp);
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
        }
    }

    // 3. Runtime numeric wrapper -> Primitive
    if let Type::Class(cn) = src {
        if dest.is_jvm_primitive_like() {
            if cn == F128_CLASS {
                return f128_to_prim(dest, cp);
            }
            if cn == I128_CLASS || cn == U128_CLASS {
                return int128_to_prim(cn, dest, cp);
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

    // 4. Reference -> Reference
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

        if let Type::MutableReference(inner) = src {
            let inner = inner.as_ref();
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

    Err(jvm::Error::VerificationError {
        context: format!("Function {fn_name}"),
        message: format!(
            "unsupported representation cast {src:?} → {dest:?}; refusing to synthesize a default value"
        ),
    })
}

fn primitive_to_primitive(
    src: &Type,
    dest: &Type,
    cp: &mut InternedConstantPool,
) -> jvm::Result<Vec<Instruction>> {
    let scalar = |ty: &Type| {
        oomir::scalar::scalar_type(ty).ok_or_else(|| jvm::Error::VerificationError {
            context: "primitive cast".into(),
            message: format!("non-scalar primitive cast type {ty:?}"),
        })
    };
    jvm_compiler_core::jvm::casts::primitive(&scalar(src)?, &scalar(dest)?, cp)
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
        // Pointer constants are materialized as ordinary Pointer constructor
        // instances, while typed operands retain their Rust pointee metadata.
        // Both have the exact same JVM reference descriptor.
        (oomir::Type::Class(source), oomir::Type::Pointer(_))
        | (oomir::Type::Pointer(_), oomir::Type::Class(source)) => source == oomir::POINTER_CLASS,
        // TODO: Add more other compatibility rules (e.g., Interface implementations).
        _ => false,
    }
}

pub(super) fn return_instruction_for_type(ty: &oomir::Type) -> Instruction {
    match ty {
        oomir::Type::I8
        | oomir::Type::U8
        | oomir::Type::I16
        | oomir::Type::U16
        | oomir::Type::F16
        | oomir::Type::I32
        | oomir::Type::U32
        | oomir::Type::Boolean
        | oomir::Type::Char => Instruction::Ireturn,
        oomir::Type::I64 | oomir::Type::U64 => Instruction::Lreturn,
        oomir::Type::F32 => Instruction::Freturn,
        oomir::Type::F64 => Instruction::Dreturn,
        oomir::Type::Str
        | oomir::Type::Class(_)
        | oomir::Type::Array(_)
        | oomir::Type::Slice(_)
        | oomir::Type::Reference(_)
        | oomir::Type::Pointer(_)
        | oomir::Type::MutableReference(_)
        | oomir::Type::Interface(_) => Instruction::Areturn,
        oomir::Type::Void | oomir::Type::Unit => Instruction::Return,
    }
}
