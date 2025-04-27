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

macro_rules! test_comparisons_float_no_nan {
    ($type:ty, $a:expr, $b:expr, $c:expr, $d:expr, $zero:expr, $nzero:expr) => {
        assert!($a == $b, concat!(stringify!($type), ": a == b"));
        assert!($a != $c, concat!(stringify!($type), ": a != c"));
        assert!($a < $c, concat!(stringify!($type), ": a < c"));
        assert!($a <= $b, concat!(stringify!($type), ": a <= b"));
        assert!($c > $a, concat!(stringify!($type), ": c > a"));
        assert!($a >= $b, concat!(stringify!($type), ": a >= b"));
        assert!($d < $a, concat!(stringify!($type), ": d < a"));
        assert!($zero == $nzero, concat!(stringify!($type), ": 0.0 == -0.0"));
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

fn main() {
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
    test_comparisons_float_no_nan!(f128, 5.0f128, 5.0f128, 10.5f128, -2.1f128, 0.0f128, -0.0f128);
    
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
        10.0f128, 
        2.0f128, 
        0.0f128, 
        12.0f128, 
        8.0f128, 
        20.0f128, 
        5.0f128
    );
}