use super::*;
use std::collections::HashSet;

#[test]
fn casts_extend_truncate_saturate_and_preserve_float_width() {
    let negative = Scalar::integer(ScalarType::I8, 0xff).unwrap();
    assert_eq!(
        negative.cast(ScalarType::U64).unwrap().bits(),
        u64::MAX.into()
    );
    assert_eq!(negative.cast(ScalarType::I128).unwrap().signed(), Some(-1));
    assert_eq!(
        Scalar::f64(f64::NAN).cast(ScalarType::I32).unwrap().bits(),
        0
    );
    assert_eq!(
        Scalar::f32(f32::INFINITY)
            .cast(ScalarType::I8)
            .unwrap()
            .signed(),
        Some(127)
    );
    assert_eq!(
        Scalar::f64(f64::NEG_INFINITY)
            .cast(ScalarType::U128)
            .unwrap()
            .bits(),
        0
    );
    assert_eq!(
        Scalar::f64(0.0).neg().unwrap().cast(ScalarType::F32),
        Some(Scalar::f32(-0.0))
    );
    assert!(negative.cast(ScalarType::Char).is_none());
    assert_eq!(
        Scalar::integer(ScalarType::U32, 0x1f980)
            .unwrap()
            .cast(ScalarType::Char)
            .unwrap()
            .bits(),
        0x1f980
    );
    assert!(
        Scalar::integer(ScalarType::U32, 0xd800)
            .unwrap()
            .cast(ScalarType::Char)
            .is_none()
    );
}

#[test]
fn float_identity_is_bitwise_but_comparisons_are_numeric() {
    for zero in [Scalar::f32(0.0), Scalar::f64(0.0)] {
        let negative = zero.neg().unwrap();
        assert_ne!(zero, negative);
        assert_eq!(
            zero.binary(BinaryOp::Eq, negative),
            Some(Scalar::boolean(true))
        );
    }
    let nan = Scalar::f64(f64::from_bits(0x7ff8_0000_0000_0042));
    assert_eq!(nan, nan);
    assert_eq!(HashSet::from([nan, nan]).len(), 1);
    assert_eq!(nan.binary(BinaryOp::Eq, nan), Some(Scalar::boolean(false)));
    assert_eq!(nan.binary(BinaryOp::Ne, nan), Some(Scalar::boolean(true)));
    assert_eq!(nan.binary(BinaryOp::Le, nan), Some(Scalar::boolean(false)));
    assert!(
        f64::from_bits(Scalar::f64(0.0).binary(BinaryOp::Mul, nan).unwrap().bits() as u64).is_nan()
    );
}

#[test]
fn integer_widths_overflow_and_traps_are_preserved() {
    for ty in [
        ScalarType::I8,
        ScalarType::U8,
        ScalarType::I16,
        ScalarType::U16,
        ScalarType::I32,
        ScalarType::U32,
        ScalarType::I64,
        ScalarType::U64,
        ScalarType::I128,
        ScalarType::U128,
    ] {
        let (width, signed) = ty.integer().unwrap();
        let one = Scalar::integer(ty, 1).unwrap();
        let zero = Scalar::integer(ty, 0).unwrap();
        let all = Scalar::integer(ty, u128::MAX).unwrap();
        assert_eq!(all.binary(BinaryOp::Add, one), Some(zero));
        assert_eq!(zero.binary(BinaryOp::Div, zero), None);
        assert_eq!(one.binary(BinaryOp::Rem, zero), None);
        assert_eq!(
            one.binary(
                BinaryOp::Shl,
                Scalar::integer(ScalarType::U32, width.into()).unwrap()
            ),
            None
        );
        if signed {
            let min = Scalar::integer(ty, 1 << (width - 1)).unwrap();
            assert_eq!(min.binary(BinaryOp::Div, all), None);
            assert_eq!(min.binary(BinaryOp::Rem, all), None);
            assert_eq!(all.binary(BinaryOp::Shr, one), Some(all));
        }
    }
}

#[test]
fn exhaustive_byte_arithmetic_matches_rust() {
    for a in u8::MIN..=u8::MAX {
        for b in u8::MIN..=u8::MAX {
            let x = Scalar::integer(ScalarType::U8, a.into()).unwrap();
            let y = Scalar::integer(ScalarType::U8, b.into()).unwrap();
            for (op, expected) in [
                (BinaryOp::Add, a.wrapping_add(b)),
                (BinaryOp::Sub, a.wrapping_sub(b)),
                (BinaryOp::Mul, a.wrapping_mul(b)),
                (BinaryOp::BitAnd, a & b),
                (BinaryOp::BitOr, a | b),
                (BinaryOp::BitXor, a ^ b),
            ] {
                assert_eq!(x.binary(op, y).unwrap().bits(), u128::from(expected));
            }
        }
    }
}
