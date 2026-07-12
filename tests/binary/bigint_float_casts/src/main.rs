// Regression test: conversions between BigInteger-backed integers (u128/i128)
// and JVM floats.
//
// The translator's BigInteger-to-primitive unboxing only knew intValue and
// longValue; when the declared type was f64 it emitted intValue()I and then
// stored the (1-slot) int into a (2-slot) double, corrupting the operand
// stack (caught by ProGuard's stack-size computation). Every declared
// primitive width must pick the matching *Value() accessor: floatValue /
// doubleValue for floats.

// Generic hops force values through monomorphized call boundaries, where the
// definition-side and call-site-side types must agree on unboxing.
fn pass_through<T>(v: T) -> T {
    v
}

fn as_f64(v: u128) -> f64 {
    v as f64
}

fn as_f32(v: u128) -> f32 {
    v as f32
}

fn ratio(a: u128, b: u128) -> f64 {
    // Mirrors the num_bigint nth_root shape: two big-int derived doubles
    // combined with float arithmetic.
    a as f64 / b as f64
}

fn main() {
    // Widening casts out of BigInteger-backed types.
    let big: u128 = 1_000_000;
    assert!(big as f64 == 1_000_000.0);
    assert!(big as f32 == 1_000_000.0f32);
    assert!(as_f64(big) == 1_000_000.0);
    assert!(as_f32(big) == 1_000_000.0f32);

    let signed: i128 = -250;
    assert!(signed as f64 == -250.0);
    assert!(signed as f32 == -250.0f32);

    // Through a generic call boundary, then converted.
    let hopped = pass_through(big);
    assert!(hopped as f64 == 1_000_000.0);
    let hopped_f = pass_through(big as f64);
    assert!(hopped_f == 1_000_000.0);

    // Float arithmetic on big-int derived values.
    assert!(ratio(84, 2) == 42.0);
    let x: u128 = 7;
    let y = x as f64 * 6.0;
    assert!(y == 42.0);

    // Back into BigInteger-backed types.
    let back = 42.9_f64 as u128;
    assert!(back == 42);
    let back_signed = -42.9_f64 as i128;
    assert!(back_signed == -42);

    // Integer accessor paths must keep working alongside the float ones.
    assert!(big as u64 == 1_000_000);
    assert!(big as u32 == 1_000_000);
    assert!(big as i64 == 1_000_000);
    assert!((signed as i32) == -250);

    // Round trips.
    let round: u128 = (big as f64) as u128;
    assert!(round == 1_000_000);
    let small: u128 = 3;
    assert!(small as f32 as u128 == 3);
}
