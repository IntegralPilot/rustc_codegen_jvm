use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BitOp {
    Count,
    LeadingZeros,
    TrailingZeros,
    Reverse,
    SwapBytes,
}

impl BitOp {
    pub fn is_count(self) -> bool {
        matches!(self, Self::Count | Self::LeadingZeros | Self::TrailingZeros)
    }
}

impl Scalar {
    pub fn bit(self, op: BitOp) -> Option<Self> {
        let (width, _) = self.ty.integer()?;
        let bits = self.bits();
        Some(match op {
            BitOp::Count => Self::integer(ScalarType::U32, bits.count_ones() as u128)?,
            BitOp::LeadingZeros => Self::integer(
                ScalarType::U32,
                (bits.leading_zeros() - (128 - width)) as u128,
            )?,
            BitOp::TrailingZeros => {
                Self::integer(ScalarType::U32, bits.trailing_zeros().min(width) as u128)?
            }
            BitOp::Reverse => Self::integer(self.ty, bits.reverse_bits() >> (128 - width))?,
            BitOp::SwapBytes => Self::integer(self.ty, bits.swap_bytes() >> (128 - width))?,
        })
    }
}
