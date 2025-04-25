use super::{Constant, Type};
use bigdecimal::{BigDecimal, FromPrimitive, ParseBigDecimalError, RoundingMode, ToPrimitive};
use num_bigint::{BigInt, ParseBigIntError};
use num_traits::Zero;
use std::convert::{TryFrom, TryInto};
use std::ops::Div;
use std::str::FromStr; // Needed for parsing

// --- Error Enum (Optional but Recommended) ---
// Helps distinguish parsing errors from unsupported operations
#[derive(Debug)]
enum InterpretError {
    ParseBigInt(ParseBigIntError),
    ParseBigDecimal(ParseBigDecimalError),
    UnsupportedOperation,
    DivisionByZero,
    Overflow, // For lossy casts back to primitives if needed
}

impl From<ParseBigIntError> for InterpretError {
    fn from(e: ParseBigIntError) -> Self {
        InterpretError::ParseBigInt(e)
    }
}
impl From<ParseBigDecimalError> for InterpretError {
    fn from(e: ParseBigDecimalError) -> Self {
        InterpretError::ParseBigDecimal(e)
    }
}

// --- Helper Functions ---

fn parse_constant_to_bigint(c: &Constant) -> Result<BigInt, InterpretError> {
    match c {
        Constant::I8(v) => Ok(BigInt::from(*v)),
        Constant::I16(v) => Ok(BigInt::from(*v)),
        Constant::I32(v) => Ok(BigInt::from(*v)),
        Constant::I64(v) => Ok(BigInt::from(*v)),
        Constant::Boolean(v) => Ok(BigInt::from(*v as i8)), // true -> 1, false -> 0
        Constant::Char(v) => Ok(BigInt::from(*v as u32)),   // Use Unicode scalar value
        Constant::Instance {
            class_name, params, ..
        } if class_name == "java/math/BigInteger" => {
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

fn parse_constant_to_bigdecimal(c: &Constant) -> Result<BigDecimal, InterpretError> {
    match c {
        Constant::I8(v) => BigDecimal::from_i8(*v).ok_or(InterpretError::Overflow),
        Constant::I16(v) => BigDecimal::from_i16(*v).ok_or(InterpretError::Overflow),
        Constant::I32(v) => BigDecimal::from_i32(*v).ok_or(InterpretError::Overflow),
        Constant::I64(v) => BigDecimal::from_i64(*v).ok_or(InterpretError::Overflow),
        Constant::F32(v) => BigDecimal::from_f32(*v).ok_or(InterpretError::UnsupportedOperation), // May lose precision
        Constant::F64(v) => BigDecimal::from_f64(*v).ok_or(InterpretError::UnsupportedOperation), // May lose precision
        Constant::Boolean(v) => BigDecimal::from_i8(*v as i8).ok_or(InterpretError::Overflow),
        Constant::Char(v) => BigDecimal::from_u32(*v as u32).ok_or(InterpretError::Overflow),
        Constant::Instance { class_name, .. } if class_name == "java/math/BigInteger" => {
            // Convert BigInt instance to BigDecimal
            let bigint = parse_constant_to_bigint(c)?;
            Ok(BigDecimal::from(bigint))
        }
        Constant::Instance {
            class_name, params, ..
        } if class_name == "java/math/BigDecimal" => {
            if params.len() == 1 {
                if let Constant::String(ref s) = params[0] {
                    // Handle "0.0" vs "-0.0" string parsing explicitly if library doesn't normalize
                    // BigDecimal::from_str usually handles this fine.
                    Ok(BigDecimal::from_str(&s)?)
                } else {
                    Err(InterpretError::UnsupportedOperation) // Expected string param
                }
            } else {
                Err(InterpretError::UnsupportedOperation) // Wrong number of params
            }
        }
        _ => Err(InterpretError::UnsupportedOperation), // Cannot parse other types
    }
}

fn bigint_to_constant(bi: BigInt) -> Constant {
    Constant::Instance {
        class_name: "java/math/BigInteger".to_string(),
        fields: Default::default(), // Assuming no fields needed for constant representation
        params: vec![Constant::String(bi.to_string())],
    }
}

fn bigdecimal_to_constant(bd: BigDecimal) -> Constant {
    // Ensure consistent string representation (e.g., avoid trailing zeros if possible, match Java?)
    // bd.to_string() is usually sufficient. Use `.normalized()` if needed.
    Constant::Instance {
        class_name: "java/math/BigDecimal".to_string(),
        fields: Default::default(),
        params: vec![Constant::String(bd.to_string())],
    }
}

// Default scale for BigDecimal division (adjust as needed)
const BIGDECIMAL_DIVISION_SCALE: i64 = 50;

// --- Updated cast_constant ---
pub fn cast_constant(c: Constant, ty: Type) -> Option<Constant> {
    match (&ty, &c) {
        // Identity casts
        _ if Type::from_constant(&c) == ty => Some(c),

        // To BigInt
        (Type::Class(cn), _) if cn == "java/math/BigInteger" => {
            parse_constant_to_bigint(&c).ok().map(bigint_to_constant)
        }
        // To BigDecimal
        (Type::Class(cn), _) if cn == "java/math/BigDecimal" => parse_constant_to_bigdecimal(&c)
            .ok()
            .map(bigdecimal_to_constant),

        // From BigInt
        (_, Constant::Instance { class_name, .. }) if class_name == "java/math/BigInteger" => {
            let bigint = parse_constant_to_bigint(&c).ok()?;
            match ty {
                Type::I8 => bigint.to_i8().map(Constant::I8),
                Type::I16 => bigint.to_i16().map(Constant::I16),
                Type::I32 => bigint.to_i32().map(Constant::I32),
                Type::I64 => bigint.to_i64().map(Constant::I64),
                Type::F32 => bigint.to_f32().map(Constant::F32), // Precision loss possible
                Type::F64 => bigint.to_f64().map(Constant::F64), // Precision loss possible
                Type::Boolean => Some(Constant::Boolean(!bigint.is_zero())),
                Type::Char => bigint
                    .to_u32()
                    .and_then(std::char::from_u32)
                    .map(Constant::Char),
                // Cannot cast BigInt to String, Array, other Class constantly
                _ => None,
            }
        }
        // From BigDecimal
        (_, Constant::Instance { class_name, .. }) if class_name == "java/math/BigDecimal" => {
            let bigdec = parse_constant_to_bigdecimal(&c).ok()?;
            match ty {
                // Truncates towards zero for float -> int
                Type::I8 => bigdec.to_i8().map(Constant::I8),
                Type::I16 => bigdec.to_i16().map(Constant::I16),
                Type::I32 => bigdec.to_i32().map(Constant::I32),
                Type::I64 => bigdec.to_i64().map(Constant::I64),
                Type::F32 => bigdec.to_f32().map(Constant::F32), // Precision loss possible
                Type::F64 => bigdec.to_f64().map(Constant::F64), // Precision loss possible
                Type::Boolean => Some(Constant::Boolean(!bigdec.is_zero())),
                // Cannot cast BigDecimal to Char, String, Array, other Class constantly
                _ => None,
            }
        }

        // --- Existing Primitive Casts (keep as they were) ---
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
        (_, Constant::String(_)) | (_, Constant::Class(_)) | (_, Constant::Array(_, _)) => {
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

// --- Updated unify_ops_type ---
pub fn unify_ops_type(op1: Constant, op2: Constant) -> Option<(Constant, Constant)> {
    let type1 = Type::from_constant(&op1);
    let type2 = Type::from_constant(&op2);

    // Determine the target type based on promotion rules
    let target_type = match (&type1, &type2) {
        // --- Big Decimal involved ---
        (Type::Class(cn1), _) if cn1 == "java/math/BigDecimal" => type1.clone(),
        (_, Type::Class(cn2)) if cn2 == "java/math/BigDecimal" => type2.clone(),

        // --- Big Integer involved (and no BigDecimal) ---
        (Type::Class(cn1), _) if cn1 == "java/math/BigInteger" => match type2 {
            Type::F32 | Type::F64 => Type::Class("java/math/BigDecimal".to_string()), // Promote BigInt to Dec
            _ => type1.clone(), // Promote Int/Bool/Char to BigInt
        },
        (_, Type::Class(cn2)) if cn2 == "java/math/BigInteger" => match type1 {
            Type::F32 | Type::F64 => Type::Class("java/math/BigDecimal".to_string()), // Promote BigInt to Dec
            _ => type2.clone(), // Promote Int/Bool/Char to BigInt
        },

        // --- Standard Primitive Promotions ---
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
             Constant::F32(a) => { let b = $op2.as_f32()?; if b == 0.0 { Err($err).ok() } else { Some(Constant::F32(a $op b)) } },
             Constant::F64(a) => { let b = $op2.as_f64()?; if b == 0.0 { Err($err).ok() } else { Some(Constant::F64(a $op b)) } },
            _ => None,
        }
    };
}

pub fn add_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. } if class_name == "java/math/BigInteger" => {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            Some(bigint_to_constant(bi1 + bi2))
        }
        Constant::Instance { class_name, .. } if class_name == "java/math/BigDecimal" => {
            let bd1 = parse_constant_to_bigdecimal(&unified_op1).ok()?;
            let bd2 = parse_constant_to_bigdecimal(&unified_op2).ok()?;
            Some(bigdecimal_to_constant(bd1 + bd2))
        }
        _ => primitive_arithmetic!(unified_op1, unified_op2, +, wrapping_add),
    }
}

pub fn subtract_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. } if class_name == "java/math/BigInteger" => {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            Some(bigint_to_constant(bi1 - bi2))
        }
        Constant::Instance { class_name, .. } if class_name == "java/math/BigDecimal" => {
            let bd1 = parse_constant_to_bigdecimal(&unified_op1).ok()?;
            let bd2 = parse_constant_to_bigdecimal(&unified_op2).ok()?;
            Some(bigdecimal_to_constant(bd1 - bd2))
        }
        _ => primitive_arithmetic!(unified_op1, unified_op2, -, wrapping_sub),
    }
}

pub fn multiply_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. } if class_name == "java/math/BigInteger" => {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            Some(bigint_to_constant(bi1 * bi2))
        }
        Constant::Instance { class_name, .. } if class_name == "java/math/BigDecimal" => {
            let bd1 = parse_constant_to_bigdecimal(&unified_op1).ok()?;
            let bd2 = parse_constant_to_bigdecimal(&unified_op2).ok()?;
            Some(bigdecimal_to_constant(bd1 * bd2))
        }
        _ => primitive_arithmetic!(unified_op1, unified_op2, *, wrapping_mul), // Note: F32/F64 have '-' in the template, correct is '*'
    }
}

pub fn divide_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. } if class_name == "java/math/BigInteger" => {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            if bi2.is_zero() {
                return None;
            } // Division by zero
            // Use checked_div for integer division semantics (truncates)
            bi1.checked_div(&bi2).map(bigint_to_constant)
        }
        Constant::Instance { class_name, .. } if class_name == "java/math/BigDecimal" => {
            let bd1 = parse_constant_to_bigdecimal(&unified_op1).ok()?;
            let bd2 = parse_constant_to_bigdecimal(&unified_op2).ok()?;
            if bd2.is_zero() {
                return None;
            } // Division by zero
            // Perform division with a specified scale and rounding mode
            // Java's default is often complex; HALF_UP is common. Adjust scale as needed.
            if bd2.is_zero() {
                None
            } else {
                let result = bd1
                    .div(&bd2)
                    .with_scale_round(BIGDECIMAL_DIVISION_SCALE, RoundingMode::HalfUp);
                Some(bigdecimal_to_constant(result))
            }
        }
        _ => {
            primitive_arithmetic!(unified_op1, unified_op2, /, checked_div, InterpretError::DivisionByZero)
        }
    }
}

pub fn rem_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let (unified_op1, unified_op2) = unify_ops_type(op1, op2)?;
    match &unified_op1 {
        Constant::Instance { class_name, .. } if class_name == "java/math/BigInteger" => {
            let bi1 = parse_constant_to_bigint(&unified_op1).ok()?;
            let bi2 = parse_constant_to_bigint(&unified_op2).ok()?;
            if bi2.is_zero() {
                return None;
            } // Division by zero
            // Use checked_rem
            Some(bigint_to_constant(bi1 % bi2))
        }
        Constant::Instance { class_name, .. } if class_name == "java/math/BigDecimal" => {
            let bd1 = parse_constant_to_bigdecimal(&unified_op1).ok()?;
            let bd2 = parse_constant_to_bigdecimal(&unified_op2).ok()?;
            if bd2.is_zero() {
                return None;
            } // Division by zero
            // BigDecimal's remainder might differ slightly from % on floats.
            // Java's BigDecimal remainder: a.remainder(b) = a - a.divideToIntegralValue(b).multiply(b)
            // This matches num_bigint's `rem` behaviour more closely than floats `%`.
            Some(bigdecimal_to_constant(bd1 % bd2)) // Use the '%' operator defined for BigDecimal
        }
        _ => {
            primitive_arithmetic!(unified_op1, unified_op2, %, checked_rem, InterpretError::DivisionByZero)
        }
    }
}

// Helper for comparisons
fn compare_constants(op1: Constant, op2: Constant) -> Option<std::cmp::Ordering> {
    // Try comparing as BigDecimals first (most general numeric type here)
    if let (Ok(bd1), Ok(bd2)) = (
        parse_constant_to_bigdecimal(&op1),
        parse_constant_to_bigdecimal(&op2),
    ) {
        return Some(bd1.cmp(&bd2));
    }

    // If BigDecimal fails (e.g., non-numeric involved), try BigInt
    if let (Ok(bi1), Ok(bi2)) = (
        parse_constant_to_bigint(&op1),
        parse_constant_to_bigint(&op2),
    ) {
        return Some(bi1.cmp(&bi2));
    }

    // Fallback to primitive comparisons if big nums failed or weren't applicable
    // (This part is largely the same as your original eq_constants logic,
    // but simplified as cross-type numeric is handled above by promoting to BigDecimal)
    match (op1, op2) {
        // --- Primitives (like types) ---
        (Constant::F64(a), Constant::F64(b)) => a.partial_cmp(&b),
        (Constant::F32(a), Constant::F32(b)) => a.partial_cmp(&b),
        (Constant::I64(a), Constant::I64(b)) => Some(a.cmp(&b)),
        (Constant::I32(a), Constant::I32(b)) => Some(a.cmp(&b)),
        (Constant::I16(a), Constant::I16(b)) => Some(a.cmp(&b)),
        (Constant::I8(a), Constant::I8(b)) => Some(a.cmp(&b)),
        (Constant::Boolean(a), Constant::Boolean(b)) => Some(a.cmp(&b)),
        (Constant::Char(a), Constant::Char(b)) => Some(a.cmp(&b)),
        (Constant::String(a), Constant::String(b)) => Some(a.cmp(&b)),

        // Complex types (basic equality was needed for recursion, maybe add cmp?)
        (Constant::Class(a), Constant::Class(b)) => {
            if a == b {
                Some(std::cmp::Ordering::Equal)
            } else {
                None
            }
        } // Only equality makes sense

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
            },
            Constant::Instance {
                class_name: cn2,
                fields: f2,
                params: p2,
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

// --- Bitwise/Shift Operations (Generally only apply to Integers) ---

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
        return func(bi1, bi2).ok().map(bigint_to_constant);
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
        Constant::Instance { class_name, .. } if class_name == "java/math/BigInteger" => {
            parse_constant_to_bigint(op2).ok()?.to_usize()
        }
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
    Some(bigint_to_constant(bi1 << amount))
}

pub fn shr_constants(op1: Constant, op2: Constant) -> Option<Constant> {
    let bi1 = parse_constant_to_bigint(&op1).ok()?;
    let amount = get_shift_amount(&op2)?;
    Some(bigint_to_constant(bi1 >> amount))
}

pub fn not_constant(op1: Constant) -> Option<Constant> {
    match op1 {
        Constant::Boolean(a) => Some(Constant::Boolean(!a)),
        // Bitwise NOT on integers
        c if parse_constant_to_bigint(&c).is_ok() => {
            let bi = parse_constant_to_bigint(&c).ok()?;
            // Bitwise NOT for BigInt is `!`, corresponds to two's complement
            Some(bigint_to_constant(!bi))
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
        // BigInt Negation
        Constant::Instance { ref class_name, .. } if class_name == "java/math/BigInteger" => {
            parse_constant_to_bigint(&op1)
                .ok()
                .map(|bi| bigint_to_constant(-bi))
        }
        // BigDecimal Negation
        Constant::Instance { ref class_name, .. } if class_name == "java/math/BigDecimal" => {
            parse_constant_to_bigdecimal(&op1)
                .ok()
                .map(|bd| bigdecimal_to_constant(-bd))
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

// new_array_constant, length_constant, get_field_constant remain the same
// as they don't involve arithmetic/comparison logic directly.

pub fn new_array_constant(ty: Type, size: usize) -> Option<Constant> {
    // Create a new array constant of the specified type and size
    let default_val = match ty {
        Type::I8 => Constant::I8(0),
        Type::I16 => Constant::I16(0),
        Type::I32 => Constant::I32(0),
        Type::I64 => Constant::I64(0),
        Type::F32 => Constant::F32(0.0),
        Type::F64 => Constant::F64(0.0),
        Type::Boolean => Constant::Boolean(false),
        Type::Char => Constant::Char('\u{0000}'),
        // TODO: How to represent default BigInt/BigDecimal? Null or zero?
        // Let's use zero for now, assuming non-null elements
        Type::Class(ref cn) if cn == "java/math/BigInteger" => bigint_to_constant(BigInt::zero()),
        Type::Class(ref cn) if cn == "java/math/BigDecimal" => {
            bigdecimal_to_constant(BigDecimal::zero())
        }
        // Other class types? Null or default constructor? Needs definition.
        // For now, let's prevent array creation for unhandled types.
        _ => return None, // Or handle null/default instances
    };
    Some(Constant::Array(Box::new(ty), vec![default_val; size]))
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
