//! Allocation-free scalar semantics. Bit identity is distinct from numeric equality.
mod bits;
mod cast;
pub use bits::BitOp;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl BinaryOp {
    pub fn is_comparison(self) -> bool {
        matches!(
            self,
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum ScalarType {
    Bool,
    Char,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    F16,
    F32,
    F64,
}

impl ScalarType {
    pub fn integer(self) -> Option<(u32, bool)> {
        use ScalarType::*;
        Some(match self {
            I8 => (8, true),
            U8 => (8, false),
            I16 => (16, true),
            U16 => (16, false),
            I32 => (32, true),
            U32 => (32, false),
            I64 => (64, true),
            U64 => (64, false),
            I128 => (128, true),
            U128 => (128, false),
            _ => return None,
        })
    }

    fn mask(self) -> u128 {
        let width = self.integer().map_or(128, |(width, _)| width);
        u128::MAX >> (128 - width)
    }
}

/// Constants are pool entries, not embedded in every operand. Two words avoid
/// u128's 16-byte alignment inflating this record from 24 to 32 bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Scalar {
    words: [u64; 2],
    ty: ScalarType,
}

impl Scalar {
    pub fn from_bits(ty: ScalarType, bits: u128) -> Option<Self> {
        use ScalarType::*;
        let valid = match ty {
            Bool => bits <= 1,
            Char => u32::try_from(bits).ok().and_then(char::from_u32).is_some(),
            F16 => bits <= u16::MAX.into(),
            F32 => bits <= u32::MAX.into(),
            F64 => bits <= u64::MAX.into(),
            _ => bits & !ty.mask() == 0,
        };
        valid.then_some(Self {
            words: [bits as u64, (bits >> 64) as u64],
            ty,
        })
    }

    pub fn integer(ty: ScalarType, bits: u128) -> Option<Self> {
        ty.integer()?;
        Self::from_bits(ty, bits & ty.mask())
    }

    pub fn boolean(value: bool) -> Self {
        Self {
            words: [u64::from(value), 0],
            ty: ScalarType::Bool,
        }
    }

    pub fn f32(value: f32) -> Self {
        Self {
            words: [value.to_bits().into(), 0],
            ty: ScalarType::F32,
        }
    }

    pub fn f64(value: f64) -> Self {
        Self {
            words: [value.to_bits(), 0],
            ty: ScalarType::F64,
        }
    }

    pub fn ty(self) -> ScalarType {
        self.ty
    }
    pub fn bits(self) -> u128 {
        u128::from(self.words[0]) | (u128::from(self.words[1]) << 64)
    }

    pub fn signed(self) -> Option<i128> {
        let (width, signed) = self.ty.integer()?;
        signed.then(|| ((self.bits() << (128 - width)) as i128) >> (128 - width))
    }

    pub fn overflows(self, op: BinaryOp, rhs: Self) -> Option<bool> {
        if self.ty != rhs.ty {
            return None;
        }
        let (width, signed) = self.ty.integer()?;
        Some(if signed {
            let a = self.signed()?;
            let b = rhs.signed()?;
            let result = match op {
                BinaryOp::Add => a.checked_add(b),
                BinaryOp::Sub => a.checked_sub(b),
                BinaryOp::Mul => a.checked_mul(b),
                _ => return None,
            };
            let min = i128::MIN >> (128 - width);
            let max = i128::MAX >> (128 - width);
            result.is_none_or(|result| result < min || result > max)
        } else {
            let result = match op {
                BinaryOp::Add => self.bits().checked_add(rhs.bits()),
                BinaryOp::Sub => self.bits().checked_sub(rhs.bits()),
                BinaryOp::Mul => self.bits().checked_mul(rhs.bits()),
                _ => return None,
            };
            result.is_none_or(|result| result > self.ty.mask())
        })
    }

    pub fn binary(self, op: BinaryOp, rhs: Self) -> Option<Self> {
        use BinaryOp::*;
        if matches!(op, Shl | Shr) {
            let (width, signed) = self.ty.integer()?;
            rhs.ty.integer()?;
            let amount = u32::try_from(rhs.bits()).ok()?;
            if amount >= width {
                return None;
            }
            let value = match op {
                Shl => self.bits() << amount,
                Shr if signed => (self.signed()? >> amount) as u128,
                Shr => self.bits() >> amount,
                _ => unreachable!(),
            };
            return Self::integer(self.ty, value);
        }
        if self.ty != rhs.ty {
            return None;
        }

        macro_rules! float {
            ($a:expr, $b:expr, $constructor:ident) => {{
                let (a, b) = ($a, $b);
                return Some(match op {
                    Add => Self::$constructor(a + b),
                    Sub => Self::$constructor(a - b),
                    Mul => Self::$constructor(a * b),
                    Div => Self::$constructor(a / b),
                    Rem => Self::$constructor(a % b),
                    Eq => Self::boolean(a == b),
                    Ne => Self::boolean(a != b),
                    Lt => Self::boolean(a < b),
                    Le => Self::boolean(a <= b),
                    Gt => Self::boolean(a > b),
                    Ge => Self::boolean(a >= b),
                    _ => return None,
                });
            }};
        }
        match self.ty {
            ScalarType::F32 => float!(
                f32::from_bits(self.words[0] as u32),
                f32::from_bits(rhs.words[0] as u32),
                f32
            ),
            ScalarType::F64 => float!(
                f64::from_bits(self.words[0]),
                f64::from_bits(rhs.words[0]),
                f64
            ),
            ScalarType::F16 => return None,
            _ => {}
        }
        let (a, b) = (self.bits(), rhs.bits());
        if op.is_comparison() {
            let ordering = if let (Some(a), Some(b)) = (self.signed(), rhs.signed()) {
                a.cmp(&b)
            } else {
                a.cmp(&b)
            };
            return Some(Self::boolean(match op {
                Eq => ordering.is_eq(),
                Ne => !ordering.is_eq(),
                Lt => ordering.is_lt(),
                Le => !ordering.is_gt(),
                Gt => ordering.is_gt(),
                Ge => !ordering.is_lt(),
                _ => unreachable!(),
            }));
        }
        if self.ty == ScalarType::Bool {
            return Some(Self::boolean(match op {
                BitAnd => a & b != 0,
                BitOr => a | b != 0,
                BitXor => a ^ b != 0,
                _ => return None,
            }));
        }
        let (width, signed) = self.ty.integer()?;
        let value = match op {
            Add => a.wrapping_add(b),
            Sub => a.wrapping_sub(b),
            Mul => a.wrapping_mul(b),
            BitAnd => a & b,
            BitOr => a | b,
            BitXor => a ^ b,
            Div | Rem if signed => {
                let (a, b) = (self.signed()?, rhs.signed()?);
                // Preserve the overflow/zero checks even for narrow integers.
                let min = i128::MIN >> (128 - width);
                if b == 0 || (a == min && b == -1) {
                    return None;
                }
                if op == Div {
                    (a / b) as u128
                } else {
                    (a % b) as u128
                }
            }
            Div => a.checked_div(b)?,
            Rem => a.checked_rem(b)?,
            _ => return None,
        };
        Self::integer(self.ty, value)
    }

    pub fn not(self) -> Option<Self> {
        if self.ty == ScalarType::Bool {
            Some(Self::boolean(self.bits() == 0))
        } else {
            Self::integer(self.ty, !self.bits())
        }
    }

    pub fn neg(self) -> Option<Self> {
        match self.ty {
            ScalarType::F32 => Self::from_bits(self.ty, self.bits() ^ (1 << 31)),
            ScalarType::F64 => Self::from_bits(self.ty, self.bits() ^ (1 << 63)),
            _ => Self::integer(self.ty, self.bits().wrapping_neg()),
        }
    }
}

mod fold;
#[cfg(test)]
mod tests;
pub use fold::{BinaryFold, fold_binary};
