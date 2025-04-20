use num_bigint::BigUint;
use num_traits::{One, ToPrimitive, Zero};
use std::mem;

/// Convert an `f128` (binary128) to a decimal `String` with up to 34 significant digits,
/// rounding to nearest, ties-to-even.
///
/// Special‑cases:
/// - `NaN` → `"NaN"`
/// - `+∞` → `"inf"`, `-∞` → `"-inf"`
/// - `0.0` → `"0.0"`, `-0.0` → `"-0.0"`
pub fn f128_to_string(x: f128) -> String {
    // --- Special cases ---
    if x.is_nan() {
        return "NaN".to_string();
    }
    if x.is_infinite() {
        return if x.is_sign_negative() {
            "-inf".into()
        } else {
            "inf".into()
        };
    }
    if x == 0.0_f128 {
        // preserves sign of zero
        return if x.is_sign_negative() {
            "-0.0".into()
        } else {
            "0.0".into()
        };
    }

    // --- Unpack IEEE‑754 bits ---
    const PREC: usize = 34; // max significant decimal digits for f128
    const EXP_BIAS: i32 = 16383; // binary128 exponent bias

    // Transmute to raw bits
    let bits: u128 = unsafe { mem::transmute(x) };
    let sign_bit = (bits >> 127) != 0;
    let exp_bits = ((bits >> 112) & 0x7fff) as i32;
    let frac_bits = bits & ((1u128 << 112) - 1);

    // Build the integer mantissa and true exponent
    let (mantissa, exp2) = if exp_bits == 0 {
        // subnormal: exponent = 1-bias, no implicit leading 1
        (BigUint::from(frac_bits), 1 - EXP_BIAS - 112)
    } else {
        // normal: implicit leading 1
        (
            BigUint::from(frac_bits) + (BigUint::one() << 112),
            exp_bits - EXP_BIAS - 112,
        )
    };

    // Scale into an integer numerator / denominator = mantissa * 2^exp2
    let mut num = mantissa.clone();
    let mut den = BigUint::one();
    if exp2 >= 0 {
        num <<= exp2 as usize;
    } else {
        den <<= (-exp2) as usize;
    }

    // --- Integer part + remainder ---
    let int_part = &num / &den;
    let rem = num % &den;

    // Convert the integer part to decimal
    let mut int_str = int_part.to_str_radix(10);

    // --- Fractional digits generation (PREC+1 for rounding) ---
    let mut frac_digits: Vec<u8> = Vec::new();
    let mut rem2 = rem.clone();
    for _ in 0..=PREC {
        if rem2.is_zero() {
            break;
        }
        rem2 *= 10u32;
        let d = (&rem2 / &den).to_u8().unwrap();
        frac_digits.push(d);
        rem2 %= &den;
    }

    // --- Round to nearest, ties-to-even ---
    if frac_digits.len() > PREC {
        let next = frac_digits[PREC];
        // any non‑zero bits beyond PREC+1 make it “> 5”
        let tie_or_above = next > 5 || (next == 5 && rem2 != BigUint::zero());

        let mut round_up = false;
        if tie_or_above {
            if next > 5 {
                round_up = true;
            } else {
                // exactly 5: round to even last digit
                round_up = frac_digits[PREC - 1] % 2 == 1;
            }
        }
        frac_digits.truncate(PREC);

        if round_up {
            // propagate carry in the fractional digits
            let mut i = PREC - 1;
            loop {
                if frac_digits[i] == 9 {
                    frac_digits[i] = 0;
                    if i == 0 {
                        // carry into integer part
                        let mut big_int = BigUint::parse_bytes(int_str.as_bytes(), 10).unwrap();
                        big_int += BigUint::one();
                        int_str = big_int.to_str_radix(10);
                        break;
                    }
                    i -= 1;
                } else {
                    frac_digits[i] += 1;
                    break;
                }
            }
        }
    }

    // Drop any trailing zeros in the fraction
    while frac_digits.last() == Some(&0) {
        frac_digits.pop();
    }

    // --- Assemble final string ---
    let mut out = String::new();
    if sign_bit {
        out.push('-');
    }
    out.push_str(&int_str);

    if !frac_digits.is_empty() {
        out.push('.');
        for &d in &frac_digits {
            out.push((b'0' + d) as char);
        }
    }

    out
}
