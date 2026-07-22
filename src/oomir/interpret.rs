use super::{Constant, Type};
use num_bigint::{BigInt, ParseBigIntError};
use num_traits::{ToPrimitive, Zero};
use std::convert::{TryFrom, TryInto};
use std::str::FromStr; // Needed for parsing

// Helps distinguish parsing errors from unsupported operations
enum InterpretError {
    ParseBigInt(ParseBigIntError),
    UnsupportedOperation,
    DivisionByZero,
}

impl std::fmt::Debug for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpretError::ParseBigInt(e) => write!(f, "ParseBigIntError: {:?}", e),
            InterpretError::UnsupportedOperation => write!(f, "UnsupportedOperation"),
            InterpretError::DivisionByZero => write!(f, "DivisionByZero"),
        }
    }
}

impl From<ParseBigIntError> for InterpretError {
    fn from(e: ParseBigIntError) -> Self {
        InterpretError::ParseBigInt(e)
    }
}

fn parse_constant_to_bigint(c: &Constant) -> Result<BigInt, InterpretError> {
    match c {
        Constant::I8(v) => Ok(BigInt::from(*v)),
        Constant::I16(v) => Ok(BigInt::from(*v)),
        Constant::I32(v) => Ok(BigInt::from(*v)),
        Constant::I64(v) => Ok(BigInt::from(*v)),
        Constant::U8(v) => Ok(BigInt::from(*v)),
        Constant::U16(v) => Ok(BigInt::from(*v)),
        Constant::U32(v) => Ok(BigInt::from(*v)),
        Constant::U64(v) => Ok(BigInt::from(*v)),
        Constant::Boolean(v) => Ok(BigInt::from(*v as i8)), // true -> 1, false -> 0
        Constant::Char(v) => Ok(BigInt::from(*v as u32)),   // Use Unicode scalar value
        Constant::Instance {
            class_name, params, ..
        } if matches!(
            class_name.as_str(),
            "org/rustlang/runtime/I128" | "org/rustlang/runtime/U128"
        ) =>
        {
            if params.len() == 1 {
                if let Constant::String(ref s) = params[0] {
                    Ok(BigInt::from_str(&s)?)
                } else {
                    Err(InterpretError::UnsupportedOperation) // Expected string param
                }
            } else {
                Err(InterpretError::UnsupportedOperation) // Wrong number of params
            }
        }
        _ => Err(InterpretError::UnsupportedOperation), // Cannot parse other types to BigInt directly
    }
}

fn is_i128_class(class_name: &str) -> bool {
    class_name == "org/rustlang/runtime/I128"
}

fn is_u128_class(class_name: &str) -> bool {
    class_name == "org/rustlang/runtime/U128"
}

/// Converts an arbitrary-precision intermediate back to the exact fixed-width
/// representation of `template`, applying Rust's two's-complement wrapping.
fn bigint_to_integer_constant_like(value: BigInt, template: &Constant) -> Option<Constant> {
    let (width, signed): (usize, bool) = match template {
        Constant::I8(_) => (8, true),
        Constant::U8(_) => (8, false),
        Constant::I16(_) => (16, true),
        Constant::U16(_) => (16, false),
        Constant::I32(_) => (32, true),
        Constant::U32(_) => (32, false),
        Constant::I64(_) => (64, true),
        Constant::U64(_) => (64, false),
        Constant::Instance { class_name, .. } if is_i128_class(class_name) => (128, true),
        Constant::Instance { class_name, .. } if is_u128_class(class_name) => (128, false),
        _ => return None,
    };

    let modulus = BigInt::from(1u8) << width;
    let mut wrapped = value % &modulus;
    if wrapped < BigInt::zero() {
        wrapped += &modulus;
    }
    let signed_value = if signed && wrapped >= (BigInt::from(1u8) << (width - 1)) {
        wrapped.clone() - &modulus
    } else {
        wrapped.clone()
    };

    match template {
        Constant::I8(_) => signed_value.to_i8().map(Constant::I8),
        Constant::U8(_) => wrapped.to_u8().map(Constant::U8),
        Constant::I16(_) => signed_value.to_i16().map(Constant::I16),
        Constant::U16(_) => wrapped.to_u16().map(Constant::U16),
        Constant::I32(_) => signed_value.to_i32().map(Constant::I32),
        Constant::U32(_) => wrapped.to_u32().map(Constant::U32),
        Constant::I64(_) => signed_value.to_i64().map(Constant::I64),
        Constant::U64(_) => wrapped.to_u64().map(Constant::U64),
        Constant::Instance {
            class_name, fields, ..
        } if is_i128_class(class_name) || is_u128_class(class_name) => Some(Constant::Instance {
            class_name: class_name.clone(),
            fields: fields.clone(),
            params: vec![Constant::String(if signed {
                signed_value.to_string()
            } else {
                wrapped.to_string()
            })],
            param_types: Vec::new(),
        }),
        _ => None,
    }
}

pub fn cast_constant(c: Constant, ty: Type) -> Option<Constant> {
    match (&ty, &c) {
        // Identity casts
        _ if Type::from_constant(&c) == ty => Some(c),

        (_, Constant::I8(val)) => match ty {
            Type::I8 => Some(Constant::I8(*val)),
            Type::I16 => Some(Constant::I16(*val as i16)),
            Type::I32 => Some(Constant::I32(*val as i32)),
            Type::I64 => Some(Constant::I64(*val as i64)),
            Type::F32 => Some(Constant::F32(*val as f32)),
            Type::F64 => Some(Constant::F64(*val as f64)),
            Type::Char => std::char::from_u32(*val as u32).map(Constant::Char),
            Type::Boolean => Some(Constant::Boolean(*val != 0)),
            _ => None,
        },
        (_, Constant::I16(val)) => match ty {
            Type::I8 => Some(Constant::I8(*val as i8)),
            Type::I16 => Some(Constant::I16(*val)),
            Type::I32 => Some(Constant::I32(*val as i32)),
            Type::I64 => Some(Constant::I64(*val as i64)),
            Type::F32 => Some(Constant::F32(*val as f32)),
            Type::F64 => Some(Constant::F64(*val as f64)),
            Type::Char => std::char::from_u32(*val as u32).map(Constant::Char),
            Type::Boolean => Some(Constant::Boolean(*val != 0)),
            _ => None,
        },
        (_, Constant::I32(val)) => match ty {
            Type::I8 => Some(Constant::I8(*val as i8)),
            Type::I16 => Some(Constant::I16(*val as i16)),
            Type::I32 => Some(Constant::I32(*val)),
            Type::I64 => Some(Constant::I64(*val as i64)),
            Type::F32 => Some(Constant::F32(*val as f32)),
            Type::F64 => Some(Constant::F64(*val as f64)),
            Type::Char => (*val)
                .try_into()
                .ok()
                .and_then(std::char::from_u32)
                .map(Constant::Char),
            Type::Boolean => Some(Constant::Boolean(*val != 0)),
            _ => None,
        },
        (_, Constant::I64(val)) => match ty {
            Type::I8 => Some(Constant::I8(*val as i8)),
            Type::I16 => Some(Constant::I16(*val as i16)),
            Type::I32 => Some(Constant::I32(*val as i32)),
            Type::I64 => Some(Constant::I64(*val)),
            Type::F32 => Some(Constant::F32(*val as f32)),
            Type::F64 => Some(Constant::F64(*val as f64)),
            Type::Char => (*val)
                .try_into()
                .ok()
                .and_then(std::char::from_u32)
                .map(Constant::Char),
            Type::Boolean => Some(Constant::Boolean(*val != 0)),
            _ => None,
        },
        (_, Constant::F32(val)) => match ty {
            Type::I8 => Some(Constant::I8(*val as i8)),
            Type::I16 => Some(Constant::I16(*val as i16)),
            Type::I32 => Some(Constant::I32(*val as i32)),
            Type::I64 => Some(Constant::I64(*val as i64)),
            Type::F32 => Some(Constant::F32(*val)),
            Type::F64 => Some(Constant::F64(*val as f64)),
            _ => None,
        },
        (_, Constant::F64(val)) => match ty {
            Type::I8 => Some(Constant::I8(*val as i8)),
            Type::I16 => Some(Constant::I16(*val as i16)),
            Type::I32 => Some(Constant::I32(*val as i32)),
            Type::I64 => Some(Constant::I64(*val as i64)),
            Type::F32 => Some(Constant::F32(*val as f32)),
            Type::F64 => Some(Constant::F64(*val)),
            _ => None,
        },
        (_, Constant::Boolean(val)) => match ty {
            Type::I8 => Some(Constant::I8(*val as i8)),
            Type::I16 => Some(Constant::I16(*val as i16)),
            Type::I32 => Some(Constant::I32(*val as i32)),
            Type::I64 => Some(Constant::I64(*val as i64)),
            Type::Boolean => Some(Constant::Boolean(*val)),
            _ => None,
        },
        (_, Constant::Char(val)) => match ty {
            Type::I8 => Some(Constant::I8(*val as i8)),
            Type::I16 => Some(Constant::I16(*val as i16)),
            Type::I32 => Some(Constant::I32(*val as i32)),
            Type::I64 => Some(Constant::I64(*val as i64)),
            Type::Char => Some(Constant::Char(*val)),
            _ => None,
        },

        // Non-primitive identity casts (already handled at the top) or invalid casts
        (_, Constant::String(_)) | (_, Constant::Array(_, _)) => {
            if Type::from_constant(&c) == ty {
                Some(c)
            } else {
                None
            }
        }

        // Unsupported cast
        _ => None,
    }
}

pub fn unify_ops_type(op1: Constant, op2: Constant) -> Option<(Constant, Constant)> {
    let type1 = Type::from_constant(&op1);
    let type2 = Type::from_constant(&op2);

    if type1 == type2 {
        return Some((op1, op2));
    }

    // Determine the target type based on promotion rules
    let target_type = match (&type1, &type2) {
        (Type::Class(cn1), _) if is_i128_class(cn1) || is_u128_class(cn1) => type1.clone(),
        (_, Type::Class(cn2)) if is_i128_class(cn2) || is_u128_class(cn2) => type2.clone(),

        (Type::F64, _) | (_, Type::F64) => Type::F64,
        (Type::F32, _) | (_, Type::F32) => Type::F32,
        (Type::I64, _) | (_, Type::I64) => Type::I64,
        (Type::I32, _) | (_, Type::I32) => Type::I32,
        (Type::I16, _) | (_, Type::I16) => Type::I16,
        (Type::I8, _) | (_, Type::I8) => Type::I8,
        // Bool/Char promotions if needed (often handled directly in ops)
        (Type::Boolean, Type::Boolean) => Type::Boolean, // Or promote to I32? Depends on op.
        (Type::Char, Type::Char) => Type::Char,          // Or promote to I32? Depends on op.

        // Non-numeric/compatible types - unification fails
        _ => return None,
    };

    // Cast both operands to the target type
    let casted_op1 = cast_constant(op1, target_type.clone())?;
    let casted_op2 = cast_constant(op2, target_type)?;

    Some((casted_op1, casted_op2))
}

// Helper macro for arithmetic primitives (reduces boilerplate slightly)
macro_rules! primitive_arithmetic {
    ($op1:ident, $op2:ident, $op:tt, $wrapping_op:ident) => {
        match $op1 {
            Constant::I8(a) => { let b = $op2.as_i8()?; Some(Constant::I8(a.$wrapping_op(b))) },
            Constant::I16(a) => { let b = $op2.as_i16()?; Some(Constant::I16(a.$wrapping_op(b))) },
            Constant::I32(a) => { let b = $op2.as_i32()?; Some(Constant::I32(a.$wrapping_op(b))) },
            Constant::I64(a) => { let b = $op2.as_i64()?; Some(Constant::I64(a.$wrapping_op(b))) },
            Constant::U8(a) => { let b = $op2.as_u8()?; Some(Constant::U8(a.$wrapping_op(b))) },
            Constant::U16(a) => { let b = $op2.as_u16()?; Some(Constant::U16(a.$wrapping_op(b))) },
            Constant::U32(a) => { let b = $op2.as_u32()?; Some(Constant::U32(a.$wrapping_op(b))) },
            Constant::U64(a) => { let b = $op2.as_u64()?; Some(Constant::U64(a.$wrapping_op(b))) },
            Constant::F32(a) => { let b = $op2.as_f32()?; Some(Constant::F32(a $op b)) },
            Constant::F64(a) => { let b = $op2.as_f64()?; Some(Constant::F64(a $op b)) },
            _ => None,
        }
    };
     ($op1:ident, $op2:ident, $op:tt, $checked_op:ident, $err:expr) => {
        match $op1 {
             Constant::I8(a) => { let b = $op2.as_i8()?; a.$checked_op(b).map(Constant::I8).ok_or($err).ok() },
             Constant::I16(a) => { let b = $op2.as_i16()?; a.$checked_op(b).map(Constant::I16).ok_or($err).ok() },
             Constant::I32(a) => { let b = $op2.as_i32()?; a.$checked_op(b).map(Constant::I32).ok_or($err).ok() },
             Constant::I64(a) => { let b = $op2.as_i64()?; a.$checked_op(b).map(Constant::I64).ok_or($err).ok() },
             Constant::U8(a) => { let b = $op2.as_u8()?; a.$checked_op(b).map(Constant::U8).ok_or($err).ok() },
             Constant::U16(a) => { let b = $op2.as_u16()?; a.$checked_op(b).map(Constant::U16).ok_or($err).ok() },
             Constant::U32(a) => { let b = $op2.as_u32()?; a.$checked_op(b).map(Constant::U32).ok_or($err).ok() },
             Constant::U64(a) => { let b = $op2.as_u64()?; a.$checked_op(b).map(Constant::U64).ok_or($err).ok() },
             Constant::F32(a) => { let b = $op2.as_f32()?; if b == 0.0 { Err($err).ok() } else { Some(Constant::F32(a $op b)) } },
             Constant::F64(a) => { let b = $op2.as_f64()?; if b == 0.0 { Err($err).ok() } else { Some(Constant::F64(a $op b)) } },
            _ => None,
        }
    };
}

pub fn add_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. }
            if is_i128_class(class_name) || is_u128_class(class_name) =>
        {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            bigint_to_integer_constant_like(bi1 + bi2, &unified_op1)
        }
        _ => primitive_arithmetic!(unified_op1, unified_op2, +, wrapping_add),
    }
}

pub fn subtract_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. }
            if is_i128_class(class_name) || is_u128_class(class_name) =>
        {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            bigint_to_integer_constant_like(bi1 - bi2, &unified_op1)
        }
        _ => primitive_arithmetic!(unified_op1, unified_op2, -, wrapping_sub),
    }
}

pub fn multiply_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. }
            if is_i128_class(class_name) || is_u128_class(class_name) =>
        {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            bigint_to_integer_constant_like(bi1 * bi2, &unified_op1)
        }
        _ => primitive_arithmetic!(unified_op1, unified_op2, *, wrapping_mul), // Note: F32/F64 have '-' in the template, correct is '*'
    }
}

pub fn divide_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. }
            if is_i128_class(class_name) || is_u128_class(class_name) =>
        {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            if bi2.is_zero() {
                return None;
            } // Division by zero
            // Use checked_div for integer division semantics (truncates)
            bi1.checked_div(&bi2)
                .and_then(|value| bigint_to_integer_constant_like(value, &unified_op1))
        }
        _ => {
            primitive_arithmetic!(unified_op1, unified_op2, /, checked_div, InterpretError::DivisionByZero)
        }
    }
}

pub fn rem_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. }
            if is_i128_class(class_name) || is_u128_class(class_name) =>
        {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            if bi2.is_zero() {
                return None;
            } // Division by zero
            // Use checked_rem
            bigint_to_integer_constant_like(bi1 % bi2, &unified_op1)
        }
        _ => {
            primitive_arithmetic!(unified_op1, unified_op2, %, checked_rem, InterpretError::DivisionByZero)
        }
    }
}

// Helper for comparisons
fn compare_constants(op1: Constant, op2: Constant) -> Option<std::cmp::Ordering> {
    if let (Ok(bi1), Ok(bi2)) = (
        parse_constant_to_bigint(&op1),
        parse_constant_to_bigint(&op2),
    ) {
        return Some(bi1.cmp(&bi2));
    }

    // Fallback to primitive comparisons if wide-integer comparison was not applicable.
    match (op1, op2) {
        (Constant::F64(a), Constant::F64(b)) => a.partial_cmp(&b),
        (Constant::F32(a), Constant::F32(b)) => a.partial_cmp(&b),
        (Constant::I64(a), Constant::I64(b)) => Some(a.cmp(&b)),
        (Constant::U64(a), Constant::U64(b)) => Some(a.cmp(&b)),
        (Constant::U32(a), Constant::U32(b)) => Some(a.cmp(&b)),
        (Constant::U16(a), Constant::U16(b)) => Some(a.cmp(&b)),
        (Constant::U8(a), Constant::U8(b)) => Some(a.cmp(&b)),
        (Constant::I32(a), Constant::I32(b)) => Some(a.cmp(&b)),
        (Constant::I16(a), Constant::I16(b)) => Some(a.cmp(&b)),
        (Constant::I8(a), Constant::I8(b)) => Some(a.cmp(&b)),
        (Constant::Boolean(a), Constant::Boolean(b)) => Some(a.cmp(&b)),
        (Constant::Char(a), Constant::Char(b)) => Some(a.cmp(&b)),
        (Constant::String(a), Constant::String(b)) => Some(a.cmp(&b)),
        (Constant::Unit, Constant::Unit) => Some(std::cmp::Ordering::Equal),

        // Complex types (basic equality was needed for recursion, maybe add cmp?)
        (Constant::Array(ty1, elems1), Constant::Array(ty2, elems2)) => {
            if ty1 != ty2 || elems1.len() != elems2.len() {
                // Decide if different types/lengths are comparable (e.g., Less/Greater) or just None
                return None;
            }
            // Lexicographical comparison
            for (e1, e2) in elems1.iter().zip(elems2.iter()) {
                match compare_constants(e1.clone(), e2.clone())? {
                    std::cmp::Ordering::Equal => continue,
                    other => return Some(other),
                }
            }
            Some(std::cmp::Ordering::Equal)
        }

        (
            Constant::Instance {
                class_name: cn1,
                fields: f1,
                params: p1,
                ..
            },
            Constant::Instance {
                class_name: cn2,
                fields: f2,
                params: p2,
                ..
            },
        ) => {
            // Must be same class, field count, param count to be potentially equal or comparable
            if cn1 != cn2 || f1.len() != f2.len() || p1.len() != p2.len() {
                return None; // Not comparable in this context
            }

            // Compare parameters lexicographically
            for i in 0..p1.len() {
                // Recursively call compare_constants!
                match compare_constants(p1[i].clone(), p2[i].clone())? {
                    std::cmp::Ordering::Equal => continue, // Check next param
                    other_ordering => return Some(other_ordering), // Found difference
                }
            }

            // Compare fields lexicographically (ensure consistent order, e.g., by name)
            // Note: HashMap iteration order isn't guaranteed, so sort keys for deterministic comparison.
            let mut f1_keys: Vec<_> = f1.keys().collect();
            let mut f2_keys: Vec<_> = f2.keys().collect();
            f1_keys.sort();
            f2_keys.sort();

            if f1_keys != f2_keys {
                return None; // Different field names, treat as incomparable or unequal
                // Or potentially define an ordering based on keys? Simpler to return None.
            }

            for key in f1_keys {
                let val1 = f1.get(key).unwrap(); // Key must exist based on checks
                let val2 = f2.get(key).unwrap();
                // Recursively call compare_constants!
                match compare_constants(val1.clone(), val2.clone())? {
                    std::cmp::Ordering::Equal => continue, // Check next field
                    other_ordering => return Some(other_ordering), // Found difference
                }
            }

            // If all params and fields are equal
            Some(std::cmp::Ordering::Equal)
        }

        _ => None, // Incompatible types for comparison
    }
}

// Internal equality check, handling recursion carefully
fn eq_constants_internal(op1: Constant, op2: Constant) -> Option<bool> {
    match compare_constants(op1, op2) {
        // No clone needed here if compare_constants handles it
        Some(std::cmp::Ordering::Equal) => Some(true),
        Some(_) => Some(false), // Comparable but not equal
        None => None,           // Not comparable
    }
}

pub fn eq_constants(op1: Constant, op2: Constant) -> Option<bool> {
    eq_constants_internal(op1.clone(), op2.clone())
}

pub fn gt_constants(op1: Constant, op2: Constant) -> Option<bool> {
    compare_constants(op1.clone(), op2.clone()).map(|ord| ord == std::cmp::Ordering::Greater)
}

pub fn lt_constants(op1: Constant, op2: Constant) -> Option<bool> {
    compare_constants(op1, op2).map(|ord| ord == std::cmp::Ordering::Less)
}

pub fn ge_constants(op1: Constant, op2: Constant) -> Option<bool> {
    compare_constants(op1, op2).map(|ord| ord != std::cmp::Ordering::Less)
}

pub fn le_constants(op1: Constant, op2: Constant) -> Option<bool> {
    compare_constants(op1, op2).map(|ord| ord != std::cmp::Ordering::Greater)
}

fn do_bitwise_op<F>(op1: Constant, op2: Constant, func: F) -> Option<Constant>
where
    F: FnOnce(BigInt, BigInt) -> Result<BigInt, InterpretError>, // Assume BigInt for bitwise
{
    // Promote both to BigInt if possible, otherwise fallback to primitives
    if let (Ok(bi1), Ok(bi2)) = (
        parse_constant_to_bigint(&op1),
        parse_constant_to_bigint(&op2),
    ) {
        // BigInt bitwise ops might need care with negative numbers (two's complement)
        // num_bigint handles this correctly.
        return func(bi1, bi2)
            .ok()
            .and_then(|value| bigint_to_integer_constant_like(value, &op1));
    }

    // Fallback to primitive integer bitwise ops
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?; // Unify to largest primitive int type
    match &unified_op1 {
        Constant::I8(a) => {
            let b = unified_op2.as_i8()?;
            func(BigInt::from(*a), BigInt::from(b))
                .ok()
                .and_then(|res| res.to_i8())
                .map(Constant::I8)
        }
        Constant::I16(a) => {
            let b = unified_op2.as_i16()?;
            func(BigInt::from(*a), BigInt::from(b))
                .ok()
                .and_then(|res| res.to_i16())
                .map(Constant::I16)
        }
        Constant::I32(a) => {
            let b = unified_op2.as_i32()?;
            func(BigInt::from(*a), BigInt::from(b))
                .ok()
                .and_then(|res| res.to_i32())
                .map(Constant::I32)
        }
        Constant::I64(a) => {
            let b = unified_op2.as_i64()?;
            func(BigInt::from(*a), BigInt::from(b))
                .ok()
                .and_then(|res| res.to_i64())
                .map(Constant::I64)
        }
        // Bitwise on floats, bools, etc. is not meaningful/allowed
        _ => None,
    }
}

// Helper for shift amount conversion (must be usize for BigInt shifts)
fn get_shift_amount(op2: &Constant) -> Option<usize> {
    match op2 {
        Constant::I8(v) => usize::try_from(*v).ok(),
        Constant::I16(v) => usize::try_from(*v).ok(),
        Constant::I32(v) => usize::try_from(*v).ok(),
        Constant::I64(v) => usize::try_from(*v).ok(),
        Constant::U8(v) => Some(usize::from(*v)),
        Constant::U16(v) => Some(usize::from(*v)),
        Constant::U32(v) => usize::try_from(*v).ok(),
        Constant::U64(v) => usize::try_from(*v).ok(),
        _ => None, // Invalid shift amount type
    }
}

pub fn bit_and_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    do_bitwise_op(op1, op2, |a, b| Ok(a & b))
}

pub fn bit_or_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    do_bitwise_op(op1, op2, |a, b| Ok(a | b))
}

pub fn bit_xor_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    do_bitwise_op(op1, op2, |a, b| Ok(a ^ b))
}

// Note: Rust/num_bigint << is SHL, >> is SHR (arithmetic/sign-extending for signed)
pub fn shl_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let bi1 = parse_constant_to_bigint(&op1).ok()?;
    let amount = get_shift_amount(&op2)?;
    bigint_to_integer_constant_like(bi1 << amount, &op1)
}

pub fn shr_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let bi1 = parse_constant_to_bigint(&op1).ok()?;
    let amount = get_shift_amount(&op2)?;
    bigint_to_integer_constant_like(bi1 >> amount, &op1)
}

pub fn not_constant(op1: Constant) -> Option<Constant> {
    match op1 {
        Constant::Boolean(a) => Some(Constant::Boolean(!a)),
        // Bitwise NOT on integers
        c if parse_constant_to_bigint(&c).is_ok() => {
            let bi = parse_constant_to_bigint(&c).ok()?;
            // Bitwise NOT for BigInt is `!`, corresponds to two's complement
            bigint_to_integer_constant_like(!bi, &c)
        }
        // Primitives (already handled by BigInt path, but keep for clarity/if BigInt fails?)
        Constant::I8(a) => Some(Constant::I8(!a)),
        Constant::I16(a) => Some(Constant::I16(!a)),
        Constant::I32(a) => Some(Constant::I32(!a)),
        Constant::I64(a) => Some(Constant::I64(!a)),
        _ => None, // NOT doesn't apply to floats, strings, etc.
    }
}

pub fn neg_constant(op1: Constant) -> Option<Constant> {
    match op1 {
        // 128-bit integer negation
        Constant::Instance { ref class_name, .. }
            if is_i128_class(class_name) || is_u128_class(class_name) =>
        {
            let value = parse_constant_to_bigint(&op1).ok()?;
            bigint_to_integer_constant_like(-value, &op1)
        }
        // Primitive Negation
        Constant::I8(a) => Some(Constant::I8(a.wrapping_neg())), // Use wrapping_neg for primitives
        Constant::I16(a) => Some(Constant::I16(a.wrapping_neg())),
        Constant::I32(a) => Some(Constant::I32(a.wrapping_neg())),
        Constant::I64(a) => Some(Constant::I64(a.wrapping_neg())),
        Constant::F32(a) => Some(Constant::F32(-a)),
        Constant::F64(a) => Some(Constant::F64(-a)),
        _ => None, // Cannot negate bool, char, string etc.
    }
}

pub fn switch_constants(op: Constant, targets: Vec<(Constant, String)>) -> Option<String> {
    // Use the updated eq_constants
    for (target_val, label) in targets {
        match eq_constants(op.clone(), target_val) {
            // Use updated eq_constants
            Some(true) => return Some(label),
            Some(false) => continue,
            None => { /* Constants might be incomparable, treat as no match */ }
        }
    }
    None // No match found
}

pub fn length_constant(array: Constant) -> Option<Constant> {
    match array {
        Constant::Array(_, elems) => Some(Constant::I32(
            elems.len().try_into().ok().unwrap_or(i32::MAX), // Handle potential overflow
        )),
        _ => None, // Not an array
    }
}

pub fn get_field_constant(instance: Constant, field_name: String) -> Option<Constant> {
    match instance {
        Constant::Instance { fields, .. } => fields.get(&field_name).cloned(),
        _ => None, // Not an instance or doesn't have fields map
    }
}

impl Constant {
    fn as_i8(&self) -> Option<i8> {
        if let Constant::I8(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_i16(&self) -> Option<i16> {
        if let Constant::I16(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_i32(&self) -> Option<i32> {
        if let Constant::I32(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_i64(&self) -> Option<i64> {
        if let Constant::I64(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_u8(&self) -> Option<u8> {
        if let Constant::U8(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_u16(&self) -> Option<u16> {
        if let Constant::U16(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_u32(&self) -> Option<u32> {
        if let Constant::U32(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_u64(&self) -> Option<u64> {
        if let Constant::U64(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_f32(&self) -> Option<f32> {
        if let Constant::F32(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    fn as_f64(&self) -> Option<f64> {
        if let Constant::F64(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}
