#![feature(f16)]
#![feature(f128)]

macro_rules! test_comparisons {
    ($type:ty, $a:expr, $b:expr, $c:expr, $d:expr, $zero:expr, $nzero:expr) => {
        assert!($a == $b, concat!(stringify!($type), ": a == b"));
        assert!($a != $c, concat!(stringify!($type), ": a != c"));
        assert!($a < $c, concat!(stringify!($type), ": a < c"));
        assert!($a <= $b, concat!(stringify!($type), ": a <= b"));
        assert!($c > $a, concat!(stringify!($type), ": c > a"));
        assert!($a >= $b, concat!(stringify!($type), ": a >= b"));
        assert!($d < $a, concat!(stringify!($type), ": d < a"));
    };
}

macro_rules! test_comparisons_float {
    ($type:ty, $a:expr, $b:expr, $c:expr, $d:expr, $zero:expr, $nzero:expr, $nan:expr) => {
        assert!($a == $b, concat!(stringify!($type), ": a == b"));
        assert!($a != $c, concat!(stringify!($type), ": a != c"));
        assert!($a < $c, concat!(stringify!($type), ": a < c"));
        assert!($a <= $b, concat!(stringify!($type), ": a <= b"));
        assert!($c > $a, concat!(stringify!($type), ": c > a"));
        assert!($a >= $b, concat!(stringify!($type), ": a >= b"));
        assert!($d < $a, concat!(stringify!($type), ": d < a"));
        assert!($zero == $nzero, concat!(stringify!($type), ": 0.0 == -0.0"));
        assert!(!$nan.eq(&$nan), concat!(stringify!($type), ": !(nan == nan)"));
        assert!($nan != $nan, concat!(stringify!($type), ": nan != nan"));
    };
}

macro_rules! test_binary_ops {
    ($type:ty, $a:expr, $b:expr, $zero:expr, $and:expr, $or:expr, $xor:expr) => {
        assert!(($a & $b) == $and, concat!(stringify!($type), ": a & b"));
        assert!(($a | $b) == $or, concat!(stringify!($type), ": a | b"));
        assert!(($a ^ $b) == $xor, concat!(stringify!($type), ": a ^ b"));
        assert!(($a & $zero) == $zero, concat!(stringify!($type), ": a & 0"));
        assert!(($a | $zero) == $a, concat!(stringify!($type), ": a | 0"));
        assert!(($a ^ $zero) == $a, concat!(stringify!($type), ": a ^ 0"));
    };
}

macro_rules! test_ops {
    ($type:ty, $a:expr, $b:expr, $zero:expr, $add:expr, $sub:expr, $mul:expr, $div:expr) => {
        assert!($a + $b == $add, concat!(stringify!($type), ": a + b"));
        assert!($a - $b == $sub, concat!(stringify!($type), ": a - b"));
        assert!($a * $b == $mul, concat!(stringify!($type), ": a * b"));
        assert!($a / $b == $div, concat!(stringify!($type), ": a / b"));
        assert!($a + $zero == $a, concat!(stringify!($type), ": a + 0"));
        assert!($a - $zero == $a, concat!(stringify!($type), ": a - 0"));
        assert!($a * $zero == $zero, concat!(stringify!($type), ": a * 0"));
    };
}

#[inline(never)]
fn sparse_switch(value: i32) -> i32 {
    match value {
        -1000 => 10,
        0 => 20,
        1000 => 30,
        _ => 40,
    }
}

#[inline(never)]
fn opaque_f128(value: f128) -> f128 {
    value
}

macro_rules! opaque_integer {
    ($name:ident, $type:ty) => {
        #[inline(never)]
        fn $name(value: $type) -> $type {
            value
        }
    };
}

opaque_integer!(opaque_i8, i8);
opaque_integer!(opaque_u8, u8);
opaque_integer!(opaque_i16, i16);
opaque_integer!(opaque_u16, u16);
opaque_integer!(opaque_i32, i32);
opaque_integer!(opaque_u32, u32);
opaque_integer!(opaque_i64, i64);
opaque_integer!(opaque_u64, u64);
opaque_integer!(opaque_i128, i128);
opaque_integer!(opaque_u128, u128);

#[inline(never)]
fn opaque_f16(value: f16) -> f16 { value }

#[inline(never)]
fn opaque_f32(value: f32) -> f32 { value }

#[inline(never)]
fn opaque_f64(value: f64) -> f64 { value }

fn runtime_integer_ops() {
    assert!(opaque_u8(u8::MAX).wrapping_add(opaque_u8(1)) == 0, "u8 wrapping add");
    assert!(opaque_u8(0).wrapping_sub(opaque_u8(1)) == u8::MAX, "u8 wrapping sub");
    assert!(opaque_u8(200).wrapping_mul(opaque_u8(2)) == 144, "u8 wrapping mul");
    assert!(opaque_u8(250) / opaque_u8(3) == 83, "u8 division");
    assert!(opaque_u8(250) % opaque_u8(3) == 1, "u8 remainder");
    assert!((opaque_u8(1) << opaque_u32(7)) == 128, "u8 shift left");
    assert!((opaque_u8(128) >> opaque_u32(7)) == 1, "u8 logical shift right");
    assert!(!opaque_u8(0) == u8::MAX, "u8 not");
    assert!(opaque_u8(u8::MAX) > opaque_u8(1), "u8 unsigned ordering");

    assert!(opaque_i8(i8::MAX).wrapping_add(opaque_i8(1)) == i8::MIN, "i8 wrapping add");
    assert!(opaque_i8(i8::MIN).wrapping_sub(opaque_i8(1)) == i8::MAX, "i8 wrapping sub");
    assert!(opaque_i8(100).wrapping_mul(opaque_i8(3)) == 44, "i8 wrapping mul");
    assert!(opaque_i8(-100) / opaque_i8(3) == -33, "i8 division");
    assert!(opaque_i8(-100) % opaque_i8(3) == -1, "i8 remainder");
    assert!((opaque_i8(-128) >> opaque_u32(7)) == -1, "i8 arithmetic shift right");

    assert!(opaque_u16(u16::MAX).wrapping_add(opaque_u16(1)) == 0, "u16 wrapping add");
    assert!(opaque_u16(50_000).wrapping_mul(opaque_u16(2)) == 34_464, "u16 wrapping mul");
    assert!(opaque_u16(u16::MAX) / opaque_u16(257) == 255, "u16 division");
    assert!(opaque_u16(u16::MAX) > opaque_u16(1), "u16 unsigned ordering");
    assert!(opaque_i16(i16::MAX).wrapping_add(opaque_i16(1)) == i16::MIN, "i16 wrapping add");
    assert!(opaque_i16(-30_000) / opaque_i16(7) == -4_285, "i16 division");
    assert!(opaque_i16(-30_000) % opaque_i16(7) == -5, "i16 remainder");

    assert!(opaque_u32(u32::MAX).wrapping_add(opaque_u32(1)) == 0, "u32 wrapping add");
    assert!(opaque_u32(0).wrapping_sub(opaque_u32(1)) == u32::MAX, "u32 wrapping sub");
    assert!(opaque_u32(0x8000_0000) / opaque_u32(2) == 0x4000_0000, "u32 unsigned division");
    assert!(opaque_u32(u32::MAX) % opaque_u32(10) == 5, "u32 unsigned remainder");
    assert!(opaque_u32(u32::MAX) > opaque_u32(i32::MAX as u32), "u32 unsigned ordering");
    assert!((opaque_u32(0x8000_0000) >> opaque_u32(31)) == 1, "u32 logical shift right");
    assert!(opaque_i32(i32::MAX).wrapping_add(opaque_i32(1)) == i32::MIN, "i32 wrapping add");
    assert!(opaque_i32(i32::MIN) / opaque_i32(2) == -1_073_741_824, "i32 division");

    assert!(opaque_u64(u64::MAX).wrapping_add(opaque_u64(1)) == 0, "u64 wrapping add");
    assert!(opaque_u64(0).wrapping_sub(opaque_u64(1)) == u64::MAX, "u64 wrapping sub");
    assert!(opaque_u64(1u64 << 63) / opaque_u64(2) == 1u64 << 62, "u64 unsigned division");
    assert!(opaque_u64(u64::MAX) % opaque_u64(10) == 5, "u64 unsigned remainder");
    assert!(opaque_u64(u64::MAX) > opaque_u64(i64::MAX as u64), "u64 unsigned ordering");
    assert!((opaque_u64(1u64 << 63) >> opaque_u32(63)) == 1, "u64 logical shift right");
    assert!(opaque_i64(i64::MAX).wrapping_add(opaque_i64(1)) == i64::MIN, "i64 wrapping add");

    let u128_high = opaque_u128((1u128 << 127) + 17);
    assert!(u128_high > opaque_u128(i128::MAX as u128), "u128 unsigned ordering");
    assert!(u128_high / opaque_u128(3) == ((1u128 << 127) + 17) / 3, "u128 division");
    assert!(u128_high % opaque_u128(3) == ((1u128 << 127) + 17) % 3, "u128 remainder");
    assert!(opaque_u128(u128::MAX).wrapping_add(opaque_u128(1)) == 0, "u128 wrapping add");
    assert!(opaque_u128(1u128 << 100).wrapping_mul(opaque_u128(1u128 << 40)) == 0, "u128 wrapping mul");
    assert!(
        opaque_u128(0x1234_5678_9abc_def0_1357_9bdf_2468_ace0)
            .wrapping_mul(opaque_u128(0x0fed_cba9_8765_4321_1111_2222_3333_4444))
            == 0x1234_5678_9abc_def0_1357_9bdf_2468_ace0u128
                .wrapping_mul(0x0fed_cba9_8765_4321_1111_2222_3333_4444),
        "u128 full-limb wrapping mul"
    );
    assert!((opaque_u128(1) << opaque_u32(100)) == 1u128 << 100, "u128 shift left");
    assert!((u128_high >> opaque_u32(127)) == 1, "u128 shift right");
    assert!(!opaque_u128(0) == u128::MAX, "u128 not");

    assert!(opaque_i128(i128::MAX).wrapping_add(opaque_i128(1)) == i128::MIN, "i128 wrapping add");
    assert!(opaque_i128(i128::MIN).wrapping_sub(opaque_i128(1)) == i128::MAX, "i128 wrapping sub");
    assert!(opaque_i128(-((1i128 << 100) + 17)) / opaque_i128(3) == -((1i128 << 100) + 17) / 3, "i128 division");
    assert!(opaque_i128(-((1i128 << 100) + 17)) % opaque_i128(3) == -((1i128 << 100) + 17) % 3, "i128 remainder");
    assert!((opaque_i128(-1) >> opaque_u32(100)) == -1, "i128 arithmetic shift right");

    let (wrapped_u8, overflow_u8) = opaque_u8(u8::MAX).overflowing_add(opaque_u8(1));
    assert!(wrapped_u8 == 0 && overflow_u8, "u8 overflowing add");
    let (wrapped_i128, overflow_i128) = opaque_i128(i128::MAX).overflowing_add(opaque_i128(1));
    assert!(wrapped_i128 == i128::MIN && overflow_i128, "i128 overflowing add");
    let (wrapped_u128, overflow_u128) = opaque_u128(u128::MAX).overflowing_mul(opaque_u128(2));
    assert!(wrapped_u128 == u128::MAX - 1 && overflow_u128, "u128 overflowing mul");
    let (wrapped_i128_mul, overflow_i128_mul) =
        opaque_i128(i128::MAX).overflowing_mul(opaque_i128(2));
    assert!(wrapped_i128_mul == -2 && overflow_i128_mul, "i128 overflowing mul");
}

fn runtime_float_ops() {
    let h1 = opaque_f16(10.0f16);
    let h2 = opaque_f16(3.0f16);
    assert!(h1 + h2 == 13.0f16, "f16 add");
    assert!(h1 - h2 == 7.0f16, "f16 sub");
    assert!(h1 * h2 == 30.0f16, "f16 mul");
    assert!(h1 / h2 == 10.0f16 / 3.0f16, "f16 div rounding");
    assert!(h1 % h2 == 1.0f16, "f16 remainder");
    assert!((-opaque_f16(2.0f16)).to_bits() == (-2.0f16).to_bits(), "f16 negation bits");
    assert!(opaque_f16(f16::from_bits(0x7e55)).to_bits() == 0x7e55, "f16 raw bits");

    let f32_nan = opaque_f32(f32::NAN);
    let f64_nan = opaque_f64(f64::NAN);
    assert!(f32_nan != f32_nan, "runtime f32 NaN");
    assert!(f64_nan != f64_nan, "runtime f64 NaN");
    assert!(opaque_f32(f32::INFINITY) + opaque_f32(f32::NEG_INFINITY) != 0.0, "runtime f32 infinity");
}

fn runtime_casts() {
    assert!(opaque_i8(-1) as u8 == u8::MAX, "i8 to u8");
    assert!(opaque_u8(u8::MAX) as i8 == -1, "u8 to i8");
    assert!(opaque_i16(-1) as u64 == u64::MAX, "i16 to u64 sign extension");
    assert!(opaque_u16(u16::MAX) as u64 == 65_535, "u16 to u64 zero extension");
    assert!(opaque_u32(u32::MAX) as u64 == 4_294_967_295, "u32 to u64 zero extension");
    assert!(opaque_u64(u64::MAX) as u32 == u32::MAX, "u64 to u32 truncation");
    assert!(opaque_i64(-1) as u128 == u128::MAX, "i64 to u128 sign extension");
    assert!(opaque_u64(u64::MAX) as u128 == u64::MAX as u128, "u64 to u128 zero extension");
    assert!(opaque_i64(-1) as i128 == -1, "i64 to i128 sign extension");
    assert!(opaque_u64(u64::MAX) as i128 == u64::MAX as i128, "u64 to i128 zero extension");
    assert!(opaque_i128(-1) as u128 == u128::MAX, "i128 to u128 bits");
    assert!(opaque_u128(u128::MAX) as i128 == -1, "u128 to i128 bits");
    assert!(opaque_u128((1u128 << 100) + 7) as u64 == 7, "u128 to u64 truncation");

    assert!(opaque_u32(u32::MAX) as f64 == 4_294_967_295.0, "u32 to f64");
    assert!(opaque_u64(u64::MAX) as f64 == u64::MAX as f64, "u64 to f64");
    assert!(opaque_i128(-(1i128 << 100)) as f64 == -(1i128 << 100) as f64, "i128 to f64");
    assert!(opaque_u128(1u128 << 127) as f64 == (1u128 << 127) as f64, "u128 to f64");

    assert!(opaque_f64(f64::NAN) as i32 == 0, "NaN to i32");
    assert!(opaque_f64(f64::INFINITY) as i16 == i16::MAX, "infinity to i16 saturates");
    assert!(opaque_f64(-1.0) as u64 == 0, "negative float to u64 saturates");
    assert!(opaque_f64(18_446_744_073_709_551_616.0) as u64 == u64::MAX, "large float to u64 saturates");
    assert!(opaque_f64(170_141_183_460_469_231_731_687_303_715_884_105_728.0) as i128 == i128::MAX, "large float to i128 saturates");
    assert!(opaque_f64(-170_141_183_460_469_231_731_687_303_715_884_105_728.0) as i128 == i128::MIN, "small float to i128 saturates");
    assert!(opaque_f64(340_282_366_920_938_463_463_374_607_431_768_211_456.0) as u128 == u128::MAX, "large float to u128 saturates");
    assert!(opaque_f64(-10.0) as u128 == 0, "negative float to u128 saturates");

    assert!(opaque_f32(1.5) as f16 == 1.5f16, "f32 to f16");
    assert!(opaque_f64(1.5) as f16 == 1.5f16, "f64 to f16");
    assert!(opaque_f16(1.5f16) as f32 == 1.5, "f16 to f32");
    assert!(opaque_f16(65_504.0f16) as u32 == 65_504, "f16 to u32");

    assert!(opaque_i128(-(1i128 << 100)) as f128 == -(1i128 << 100) as f128, "i128 to f128");
    assert!(opaque_u128(u128::MAX) as f128 == u128::MAX as f128, "u128 to f128");
    assert!(opaque_f64(1.25) as f128 == 1.25f128, "f64 to f128 exact widening");
    assert!(opaque_f128(123.75f128) as i128 == 123, "f128 to i128 truncation");
    assert!(opaque_f128(-1.0f128) as u128 == 0, "negative f128 to u128 saturates");
    assert!(opaque_f128(1.5f128) as f64 == 1.5, "f128 to f64");
    assert!(opaque_f128(1.5f128) as f16 == 1.5f16, "f128 to f16");
    let half_midpoint_bits = (0x3fffu128 << 112) | (1u128 << 101);
    let half_midpoint = opaque_f128(f128::from_bits(opaque_u128(half_midpoint_bits)));
    let above_half_midpoint =
        opaque_f128(f128::from_bits(opaque_u128(half_midpoint_bits + 1)));
    assert!(half_midpoint as f16 == 1.0f16, "f128 to f16 ties to even");
    assert!(
        (above_half_midpoint as f16).to_bits() == 0x3c01,
        "f128 to f16 rounds above midpoint"
    );
}

fn runtime_pointer_width() {
    assert!(core::mem::size_of::<usize>() == 8, "usize is 64-bit");
    assert!(core::mem::size_of::<isize>() == 8, "isize is 64-bit");
    let value = 123u32;
    let address = (&value as *const u32) as usize;
    assert!(address > u32::MAX as usize, "pointer address preserves high 32 bits");
    assert!((address as u64) > u32::MAX as u64, "pointer to u64 preserves high bits");
}

macro_rules! assert_f128_bits {
    ($value:expr, $expected:expr, $message:literal $(,)?) => {
        assert!($value.to_bits() == $expected, $message);
    };
}

fn main() {
    runtime_integer_ops();
    runtime_float_ops();
    runtime_casts();
    runtime_pointer_width();

    assert!(sparse_switch(-1000) == 10, "sparse switch negative case");
    assert!(sparse_switch(0) == 20, "sparse switch zero case");
    assert!(sparse_switch(1000) == 30, "sparse switch positive case");
    assert!(sparse_switch(7) == 40, "sparse switch default case");

    // u8 comparisons
    test_comparisons!(u8, 5u8, 5u8, 10u8, 2u8, 0u8, 0u8);

    // i8 comparisons
    test_comparisons!(i8, 5i8, 5i8, 10i8, -2i8, 0i8, 0i8);

    // u16 comparisons
    test_comparisons!(u16, 5u16, 5u16, 10u16, 2u16, 0u16, 0u16);

    // i16 comparisons
    test_comparisons!(i16, 5i16, 5i16, 10i16, -2i16, 0i16, 0i16);

    // f16 comparisons
    test_comparisons_float!(f16, 5.0f16, 5.0f16, 10.5f16, -2.1f16, 0.0f16, -0.0f16, f16::NAN);

    // u32 comparisons
    test_comparisons!(u32, 5u32, 5u32, 10u32, 2u32, 0u32, 0u32);

    // i32 comparisons
    test_comparisons!(i32, 5i32, 5i32, 10i32, -2i32, 0i32, 0i32);

    // f32 comparisons
    test_comparisons_float!(f32, 5.0f32, 5.0f32, 10.5f32, -2.1f32, 0.0f32, -0.0f32, f32::NAN);    

    // i64 comparisons
    test_comparisons!(i64, 500i64, 500i64, 1000i64, -200i64, 0i64, 0i64);

    // f64 comparisons
    test_comparisons_float!(f64, 5.0f64, 5.0f64, 10.5f64, -2.1f64, 0.0f64, -0.0f64, f64::NAN);

    // u128 comparisons
    test_comparisons!(u128, 5_000_000_000_000_000_000_000u128, 5_000_000_000_000_000_000_000u128, 10_000_000_000_000_000_000_000u128, 2u128, 0u128, 0u128);

    // i128 comparisons
    test_comparisons!(i128, 5_000_000_000_000_000_000_000i128, 5_000_000_000_000_000_000_000i128, 10_000_000_000_000_000_000_000i128, -2i128, 0i128, 0i128);

    // f128 comparisons
    test_comparisons_float!(
        f128,
        opaque_f128(5.0f128),
        opaque_f128(5.0f128),
        opaque_f128(10.5f128),
        opaque_f128(-2.1f128),
        opaque_f128(0.0f128),
        opaque_f128(-0.0f128),
        opaque_f128(f128::NAN)
    );
    
    // u8 binary operations
    test_binary_ops!(
        u8,
        0b1100_1010u8,
        0b1010_0110u8,
        0u8,
        0b1000_0010u8,
        0b1110_1110u8,
        0b0110_1100u8
    );

    // i8 binary operations
    test_binary_ops!(
        i8,
        -54i8, // 0b11001010
        -90i8, // 0b10100110
        0i8,
        -126i8, // a & b = 0b10000010
        -18i8,  // a | b = 0b11101110
        108i8   // a ^ b = 0b01101100
    );

    // u16 binary operations
    test_binary_ops!(
        u16,
        0xACF0u16, // 1010110011110000
        0x5A0Fu16, // 0101101000001111
        0u16,
        0x0800u16, // a & b
        0xFEFFu16, // a | b
        0xF6FFu16  // a ^ b
    );

    // i16 binary operations
    test_binary_ops!(
        i16,
        -21264i16, // 0xACF0
        23055i16,  // 0x5A0F
        0i16,
        2048i16,   // a & b = 0x0800
        -257i16,  // a | b = 0xFEFF
        -2305i16   // a ^ b = 0xF6FF
    );

    // u32 binary operations
    test_binary_ops!(
        u32,
        0xDEADBEEFu32,
        0xFEEDC0DEu32,
        0u32,
        0xDEAD80CEu32, // a & b
        0xFEEDFEFFu32, // a | b
        0x20407E31u32  // a ^ b
    );

    // i32 binary operations
    test_binary_ops!(
        i32,
        0b1100_1010i32,
        0b1010_0110i32,
        0i32,
        0b1000_0010i32,
        0b1110_1110i32,
        0b0110_1100i32
    );

    // u64 binary operations
    test_binary_ops!(
        u64,
        0x1234_5678_9ABC_DEF0u64,
        0x0FED_CBA9_8765_4321u64,
        0u64,
        0x0224_4228_8224_4220u64,
        0x1FFD_DFF9_9FFD_DFF1u64,
        0x1DD9_9DD1_1DD9_9DD1u64
    );

    // i64 binary operations
    test_binary_ops!(
        i64,
        0x1234_5678_9ABC_DEF0i64,
        0x0FED_CBA9_8765_4321i64,
        0i64,
        0x0224_4228_8224_4220i64,
        0x1FFD_DFF9_9FFD_DFF1i64,
        0x1DD9_9DD1_1DD9_9DD1i64
    );

    // u128 binary operations
    test_binary_ops!(
        u128,
        0x1234_5678_9ABC_DEF0_1234_5678_9ABC_DEF0u128,
        0x0FED_CBA9_8765_4321_0FED_CBA9_8765_4321u128,
        0u128,
        0x0224_4228_8224_4220_0224_4228_8224_4220u128,
        0x1FFD_DFF9_9FFD_DFF1_1FFD_DFF9_9FFD_DFF1u128,
        0x1DD9_9DD1_1DD9_9DD1_1DD9_9DD1_1DD9_9DD1u128
    );

   // i128 binary operations
    test_binary_ops!(
        i128,
        0x1234_5678_9ABC_DEF0_1234_5678_9ABC_DEF0i128,
        0x0FED_CBA9_8765_4321_0FED_CBA9_8765_4321i128,
        0i128,
        0x0224_4228_8224_4220_0224_4228_8224_4220i128,
        0x1FFD_DFF9_9FFD_DFF1_1FFD_DFF9_9FFD_DFF1i128,
        0x1DD9_9DD1_1DD9_9DD1_1DD9_9DD1_1DD9_9DD1i128
    );

    // u8 operations
    test_ops!(u8, 10u8, 5u8, 0u8, 15u8, 5u8, 50u8, 2u8);

    // i8 operations
    test_ops!(i8, 10i8, 5i8, 0i8, 15i8, 5i8, 50i8, 2i8);
    
    // u16 operations
    test_ops!(u16, 10u16, 5u16, 0u16, 15u16, 5u16, 50u16, 2u16);

    // i16 operations
    test_ops!(i16, 10i16, 5i16, 0i16, 15i16, 5i16, 50i16, 2i16);
    
    // f16 operations
    test_ops!(f16, 10.0f16, 2.0f16, 0.0f16, 12.0f16, 8.0f16, 20.0f16, 5.0f16);
    
    // u32 operations
    test_ops!(u32, 10000u32, 5000u32, 0u32, 15000u32, 5000u32, 50000000u32, 2u32);
    
    // i32 operations
    test_ops!(i32, 10000i32, 5000i32, 0i32, 15000i32, 5000i32, 50000000i32, 2i32);
    
    // f32 operations
    test_ops!(f32, 10.5f32, 2.5f32, 0.0f32, 13.0f32, 8.0f32, 26.25f32, 4.2f32);
    
    // u64 operations
    test_ops!(u64, 1000000u64, 200000u64, 0u64, 1200000u64, 800000u64, 200000000000u64, 5u64);
    
    // i64 operations
    test_ops!(i64, 1000000i64, 200000i64, 0i64, 1200000i64, 800000i64, 200000000000i64, 5i64);
    
    // f64 operations
    test_ops!(f64, 10.5f64, 2.5f64, 0.0f64, 13.0f64, 8.0f64, 26.25f64, 4.2f64);
    
    // u128 operations
    test_ops!(
        u128, 
        1000000000000000000u128, 
        200000000000000000u128, 
        0u128, 
        1200000000000000000u128, 
        800000000000000000u128, 
        200000000000000000000000000000000000u128, 
        5u128
    );

    // i128 operations
    test_ops!(
        i128, 
        1000000000000000000i128, 
        200000000000000000i128, 
        0i128, 
        1200000000000000000i128, 
        800000000000000000i128, 
        200000000000000000000000000000000000i128, 
        5i128
    );

    // f128 operations
    test_ops!(
        f128, 
        opaque_f128(10.0f128),
        opaque_f128(2.0f128),
        opaque_f128(0.0f128),
        12.0f128, 
        8.0f128, 
        20.0f128, 
        5.0f128
    );

    const F128_SIGN: u128 = 1u128 << 127;
    const F128_ONE: u128 = 0x3fff_0000_0000_0000_0000_0000_0000_0000;
    const F128_INFINITY: u128 = 0x7fff_0000_0000_0000_0000_0000_0000_0000;
    const F128_MAX: u128 = 0x7ffe_ffff_ffff_ffff_ffff_ffff_ffff_ffff;
    const F128_NAN_PAYLOAD: u128 = 0x7fff_8000_0000_0000_0000_0000_0000_1234;

    // Every category of binary128 value must survive a JVM round trip bit-for-bit.
    assert_f128_bits!(opaque_f128(f128::from_bits(0)), 0, "f128 positive zero bits");
    assert_f128_bits!(
        opaque_f128(f128::from_bits(F128_SIGN)),
        F128_SIGN,
        "f128 negative zero bits",
    );
    assert_f128_bits!(
        opaque_f128(f128::from_bits(1)),
        1,
        "f128 smallest subnormal bits",
    );
    assert_f128_bits!(
        opaque_f128(f128::from_bits(F128_INFINITY)),
        F128_INFINITY,
        "f128 infinity bits",
    );
    assert_f128_bits!(
        opaque_f128(f128::from_bits(F128_NAN_PAYLOAD)),
        F128_NAN_PAYLOAD,
        "f128 NaN payload bits",
    );

    let one = opaque_f128(f128::from_bits(F128_ONE));
    let epsilon = opaque_f128(f128::EPSILON);
    assert_f128_bits!(one + epsilon, F128_ONE + 1, "f128 retains full precision");
    assert_f128_bits!(
        opaque_f128(f128::from_bits(1)) + opaque_f128(f128::from_bits(1)),
        2,
        "f128 subnormal addition",
    );
    assert_f128_bits!(
        opaque_f128(f128::from_bits(F128_MAX)) + opaque_f128(f128::from_bits(F128_MAX)),
        F128_INFINITY,
        "f128 overflow to infinity",
    );
    assert_f128_bits!(
        opaque_f128(1.0f128) / opaque_f128(10.0f128),
        0x3ffb_9999_9999_9999_9999_9999_9999_999a,
        "f128 division rounds to nearest even",
    );
    assert!(
        opaque_f128(6.0f128) / opaque_f128(-2.0f128) == -3.0f128,
        "f128 division sign",
    );
    assert!(5.5f128 % 2.0f128 == 1.5f128, "f128 remainder semantics");

    let positive_nan = opaque_f128(f128::from_bits(F128_NAN_PAYLOAD));
    assert_f128_bits!(-positive_nan, F128_NAN_PAYLOAD ^ F128_SIGN, "f128 negation bits");
    let invalid = opaque_f128(f128::INFINITY) + opaque_f128(f128::NEG_INFINITY);
    assert!(invalid != invalid, "f128 infinity plus negative infinity is NaN");
    let zero_over_zero = opaque_f128(0.0f128) / opaque_f128(0.0f128);
    assert!(zero_over_zero != zero_over_zero, "f128 zero divided by zero is NaN");
}
