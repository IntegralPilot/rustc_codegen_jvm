//! Boundary between compiler scalar bits and the current JVM constant carriers.
use super::{Constant, Type};
pub use jvm_compiler_core::scalar::{Scalar, ScalarType};

impl Constant {
    pub fn is_integer_like(&self) -> bool {
        from_constant(self).is_some_and(|value| {
            value.ty().integer().is_some()
                || matches!(value.ty(), ScalarType::Bool | ScalarType::Char)
        })
    }
}

pub fn scalar_type(ty: &Type) -> Option<ScalarType> {
    Some(match ty {
        Type::Boolean => ScalarType::Bool,
        Type::Char => ScalarType::Char,
        Type::I8 => ScalarType::I8,
        Type::U8 => ScalarType::U8,
        Type::I16 => ScalarType::I16,
        Type::U16 => ScalarType::U16,
        Type::I32 => ScalarType::I32,
        Type::U32 => ScalarType::U32,
        Type::I64 => ScalarType::I64,
        Type::U64 => ScalarType::U64,
        Type::F16 => ScalarType::F16,
        Type::F32 => ScalarType::F32,
        Type::F64 => ScalarType::F64,
        Type::Class(name) if name == "org/rustlang/runtime/I128" => ScalarType::I128,
        Type::Class(name) if name == "org/rustlang/runtime/U128" => ScalarType::U128,
        _ => return None,
    })
}

pub fn from_constant(value: &Constant) -> Option<Scalar> {
    let (ty, bits) = match value {
        Constant::Boolean(x) => return Some(Scalar::boolean(*x)),
        Constant::Char(x) => (ScalarType::Char, *x as u128),
        Constant::I8(x) => (ScalarType::I8, *x as u8 as u128),
        Constant::U8(x) => (ScalarType::U8, *x as u128),
        Constant::I16(x) => (ScalarType::I16, *x as u16 as u128),
        Constant::U16(x) => (ScalarType::U16, *x as u128),
        Constant::I32(x) => (ScalarType::I32, *x as u32 as u128),
        Constant::U32(x) => (ScalarType::U32, *x as u128),
        Constant::I64(x) => (ScalarType::I64, *x as u64 as u128),
        Constant::U64(x) => (ScalarType::U64, *x as u128),
        Constant::F16(x) => (ScalarType::F16, *x as u128),
        Constant::F32(x) => return Some(Scalar::f32(*x)),
        Constant::F64(x) => return Some(Scalar::f64(*x)),
        Constant::Instance {
            class_name, params, ..
        } => {
            let [Constant::String(text)] = params.as_slice() else {
                return None;
            };
            match class_name.as_str() {
                "org/rustlang/runtime/I128" => {
                    (ScalarType::I128, text.parse::<i128>().ok()? as u128)
                }
                "org/rustlang/runtime/U128" => (ScalarType::U128, text.parse::<u128>().ok()?),
                _ => return None,
            }
        }
        _ => return None,
    };
    Scalar::from_bits(ty, bits)
}
