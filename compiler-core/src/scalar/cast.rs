use super::*;

impl Scalar {
    pub fn cast(self, target: ScalarType) -> Option<Self> {
        use ScalarType::*;
        if self.ty == target {
            return Some(self);
        }
        macro_rules! float {
            ($value:expr) => {{
                let value = $value;
                return match target {
                    F32 => Some(Self::f32(value as f32)),
                    F64 => Some(Self::f64(value as f64)),
                    I8 => Self::integer(target, (value as i8) as u128),
                    U8 => Self::integer(target, (value as u8) as u128),
                    I16 => Self::integer(target, (value as i16) as u128),
                    U16 => Self::integer(target, (value as u16) as u128),
                    I32 => Self::integer(target, (value as i32) as u128),
                    U32 => Self::integer(target, (value as u32) as u128),
                    I64 => Self::integer(target, (value as i64) as u128),
                    U64 => Self::integer(target, (value as u64) as u128),
                    I128 => Self::integer(target, (value as i128) as u128),
                    U128 => Self::integer(target, value as u128),
                    _ => None,
                };
            }};
        }
        match self.ty {
            F16 => return None,
            F32 => float!(f32::from_bits(self.words[0] as u32)),
            F64 => float!(f64::from_bits(self.words[0])),
            _ => {}
        }
        let signed = self.signed();
        let bits = signed.map_or(self.bits(), |value| value as u128);
        match target {
            F32 => Some(Self::f32(
                signed.map_or_else(|| self.bits() as f32, |value| value as f32),
            )),
            F64 => Some(Self::f64(
                signed.map_or_else(|| self.bits() as f64, |value| value as f64),
            )),
            Bool => Some(Self::boolean(bits != 0)),
            Char => Self::from_bits(Char, bits),
            F16 => None,
            _ => Self::integer(target, bits),
        }
    }
}
