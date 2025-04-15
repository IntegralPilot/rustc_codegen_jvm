fn main() {
    {
        // i32 comparisons
        let a = 5i32;
        let b = 5i32;
        let c = 10i32;
        let d = -2i32;
        assert!(a == b, "i32: a == b");
        assert!(a != c, "i32: a != c");
        assert!(a != d, "i32: a != d");
        assert!(a < c, "i32: a < c");
        assert!(a <= c, "i32: a <= c");
        assert!(a <= b, "i32: a <= b");
        assert!(c > a, "i32: c > a");
        assert!(c >= a, "i32: c >= a");
        assert!(a >= b, "i32: a >= b");
        assert!(d < a, "i32: d < a");

        // i64 comparisons
        let a64 = 500i64;
        let b64 = 500i64;
        let c64 = 1000i64;
        assert!(a64 == b64, "i64: a64 == b64");
        assert!(a64 != c64, "i64: a64 != c64");
        assert!(a64 < c64, "i64: a64 < c64");
        assert!(a64 <= b64, "i64: a64 <= b64");
        assert!(c64 > a64, "i64: c64 > a64");
        assert!(a64 >= b64, "i64: a64 >= b64");

        // f32 comparisons
        let fa = 5.0f32;
        let fb = 5.0f32;
        let fc = 10.5f32;
        let fd = -2.1f32;
        let fzero = 0.0f32;
        let fnzero = -0.0f32;
        let fnan = f32::NAN;
        assert!(fa == fb, "f32: fa == fb");
        assert!(fa != fc, "f32: fa != fc");
        assert!(fa < fc, "f32: fa < fc");
        assert!(fa <= fb, "f32: fa <= fb");
        assert!(fc > fa, "f32: fc > fa");
        assert!(fa >= fb, "f32: fa >= fb");
        assert!(fd < fa, "f32: fd < fa");
        assert!(fzero == fnzero, "f32: 0.0 == -0.0"); // Special zero comparison
        assert!(!(fnan == fnan), "f32: !(nan == nan)"); // NaN special comparison
        assert!(fnan != fnan, "f32: nan != nan"); // NaN special comparison

        // f64 comparisons
        let fa64 = 5.0f64;
        let fb64 = 5.0f64;
        let fc64 = 10.5f64;
        let fd64 = -2.1f64;
        let fzero64 = 0.0f64;
        let fnzero64 = -0.0f64;
        let fnan64 = f64::NAN;
        assert!(fa64 == fb64, "f64: fa64 == fb64");
        assert!(fa64 != fc64, "f64: fa64 != fc64");
        assert!(fa64 < fc64, "f64: fa64 < fc64");
        assert!(fa64 <= fb64, "f64: fa64 <= fb64");
        assert!(fc64 > fa64, "f64: fc64 > fa64");
        assert!(fa64 >= fb64, "f64: fa64 >= fb64");
        assert!(fd64 < fa64, "f64: fd64 < fa64");
        assert!(fzero64 == fnzero64, "f64: 0.0 == -0.0"); // Special zero comparison
        assert!(!(fnan64 == fnan64), "f64: !(nan == nan)"); // NaN special comparison
        assert!(fnan64 != fnan64, "f64: nan != nan"); // NaN special comparison
    }

    {
        // i32 binary AND tests
        let a = 0b1100_1010i32;
        let b = 0b1010_0110i32;
        let zero = 0i32;
        assert!((a & b) == 0b1000_0010i32, "i32: a & b");
        assert!((a & zero) == 0, "i32: a & 0");
        assert!((b & zero) == 0, "i32: b & 0");
        assert!((a & a) == a, "i32: a & a");

        // i64 binary AND tests with valid hex literals
        let ua: i64 = 0x1234_5678_9ABC_DEF0;
        let ub: i64 = 0x0FED_CBA9_8765_4321;
        // Precomputed bitwise AND:
        //   0x1234_5678_9ABC_DEF0
        // & 0x0FED_CBA9_8765_4321
        // = 0x0224_4228_8224_4220
        let expected_and: i64 = 0x0224_4228_8224_4220;
        assert!((ua & ub) == expected_and, "i64: ua & ub");
        assert!((ua & 0) == 0, "i64: ua & 0");
        assert!((ua & ua) == ua, "i64: ua & ua");
    }

    {
        // i32 binary OR tests
        let a = 0b1100_1010i32;
        let b = 0b1010_0110i32;
        let zero = 0i32;
        assert!((a | b) == 0b1110_1110i32, "i32: a | b");
        assert!((a | zero) == a, "i32: a | 0");
        assert!((b | zero) == b, "i32: b | 0");
        assert!((a | a) == a, "i32: a | a");

        // i64 binary OR tests with valid hex literals
        let ua: i64 = 0x1234_5678_9ABC_DEF0;
        let ub: i64 = 0x0FED_CBA9_8765_4321;
        // Precomputed bitwise OR:
        //   0x1234_5678_9ABC_DEF0
        // | 0x0FED_CBA9_8765_4321
        // = 0x1FFD_DFF9_9FFD_DFF1
        let expected_or: i64 = 0x1FFD_DFF9_9FFD_DFF1;
        assert!((ua | ub) == expected_or, "i64: ua | ub");
        assert!((ua | 0) == ua, "i64: ua | 0");
        assert!((ua | ua) == ua, "i64: ua | ua");
    }

    // --- Binary XOR (^) Tests ---
    {
        // i32 binary XOR tests
        let a = 0b1100_1010i32;
        let b = 0b1010_0110i32;
        let zero = 0i32;
        assert!((a ^ b) == 0b0110_1100i32, "i32: a ^ b");
        assert!((a ^ zero) == a, "i32: a ^ 0");
        assert!((b ^ zero) == b, "i32: b ^ 0");
        assert!((a ^ a) == 0, "i32: a ^ a");

        // i64 binary XOR tests with valid hex literals
        let ua: i64 = 0x1234_5678_9ABC_DEF0;
        let ub: i64 = 0x0FED_CBA9_8765_4321;
        // Precomputed XOR:
        //   0x1234_5678_9ABC_DEF0
        // ^ 0x0FED_CBA9_8765_4321
        // = 0x1DD9_9DD1_1DD9_9DD1
        let expected_xor: i64 = 0x1DD9_9DD1_1DD9_9DD1;
        assert!((ua ^ ub) == expected_xor, "i64: ua ^ ub");
        assert!((ua ^ 0) == ua, "i64: ua ^ 0");
        assert!((ua ^ ua) == 0, "i64: ua ^ ua");
    }

    // --- Shift Left (<<) Tests ---
    {
        // i32 shift left tests
        let a = 5i32; // 0b101
        let neg = -5i32;
        assert!((a << 0) == 5, "i32: a << 0");
        assert!((a << 1) == 10, "i32: a << 1"); // 0b1010
        assert!((a << 2) == 20, "i32: a << 2"); // 0b10100
        assert!((neg << 1) == -10, "i32: neg << 1");

        // i64 shift left tests (using a small value)
        let ua = 7i64;
        assert!((ua << 0) == 7, "i64: ua << 0");
        assert!((ua << 1) == 14, "i64: ua << 1");
        assert!((ua << 10) == 7168, "i64: ua << 10"); // (7 * 1024)
    }

    // --- Shift Right (>>) Tests ---
    {
        // i32 shift right tests (Arithmetic Shift Right)
        let a = 20i32;  // 0b10100
        let neg = -20i32; // ...11101100
        let min = i32::MIN; // 0x80000000
        assert!((a >> 0) == 20, "i32: a >> 0");
        assert!((a >> 1) == 10, "i32: a >> 1");
        assert!((a >> 2) == 5, "i32: a >> 2");
        assert!((a >> 3) == 2, "i32: a >> 3");
        assert!((a >> 4) == 1, "i32: a >> 4");
        assert!((a >> 5) == 0, "i32: a >> 5");
        assert!((neg >> 0) == -20, "i32: neg >> 0");
        assert!((neg >> 1) == -10, "i32: neg >> 1");
        assert!((neg >> 2) == -5, "i32: neg >> 2");

        // i64 shift right tests using a positive value
        let pos64: i64 = 1024;
        assert!((pos64 >> 0) == 1024, "i64: pos64 >> 0");
        assert!((pos64 >> 1) == 512, "i64: pos64 >> 1");
        assert!((pos64 >> 10) == 1, "i64: pos64 >> 10");
        assert!((pos64 >> 11) == 0, "i64: pos64 >> 11");

        // i64 shift right tests using a negative value (arithmetic shift right)
        let neg64: i64 = -1024;
        assert!((neg64 >> 0) == -1024, "i64: neg64 >> 0");
        assert!((neg64 >> 1) == -512, "i64: neg64 >> 1");
        assert!((neg64 >> 10) == -1, "i64: neg64 >> 10");
    }
}