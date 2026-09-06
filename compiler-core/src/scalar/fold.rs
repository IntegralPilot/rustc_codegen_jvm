use super::{BinaryOp, Scalar, ScalarType};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryFold {
    Left,
    Right,
    Constant(Scalar),
}

/// Integer identities and constant evaluation shared by construction and dataflow.
/// Operands have already been evaluated; identities never remove their effects.
pub fn fold_binary(
    op: BinaryOp,
    left_ty: ScalarType,
    right_ty: ScalarType,
    left: Option<Scalar>,
    right: Option<Scalar>,
    same_value: impl FnOnce() -> bool,
) -> Option<BinaryFold> {
    use BinaryFold::*;
    use BinaryOp::*;
    if let (Some(left), Some(right)) = (left, right) {
        return left.binary(op, right).map(Constant);
    }
    left_ty.integer()?;
    if !matches!(op, Shl | Shr) && left_ty != right_ty {
        return None;
    }
    let (a, b) = (left.map(Scalar::bits), right.map(Scalar::bits));
    let same_value = matches!(op, Sub | BitXor | BitAnd | BitOr) && same_value();
    let zero = || Constant(Scalar::integer(left_ty, 0).unwrap());
    Some(match op {
        Add | BitOr | BitXor if b == Some(0) => Left,
        Add | BitOr | BitXor if a == Some(0) => Right,
        Sub | Shl | Shr if b == Some(0) => Left,
        Mul | BitAnd if a == Some(0) || b == Some(0) => zero(),
        Mul | Div if b == Some(1) => Left,
        Mul if a == Some(1) => Right,
        Rem if b == Some(1) => zero(),
        Sub | BitXor if same_value => zero(),
        BitAnd | BitOr if same_value => Left,
        // 0/x and x/x retain possible division by zero. Floating-point
        // identities would change NaN or signed-zero behavior.
        _ => return None,
    })
}
