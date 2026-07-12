#[inline(never)]
fn opaque_i32(v: i32) -> i32 { v }

#[inline(never)]
fn opaque_f64(v: f64) -> f64 { v }

#[inline(never)]
fn opaque_u32(v: u32) -> u32 { v }

#[inline(never)]
fn opaque_i64(v: i64) -> i64 { v }

fn main() {
    let x: i32 = opaque_i32(42);
    let y: f64 = x as f64;
    assert!(y == 42.0, "i32 -> f64: expected 42.0, got {}", y);

    let x: i32 = opaque_i32(-100);
    let y: f64 = x as f64;
    assert!(y == -100.0, "i32 -> f64 negative: expected -100.0, got {}", y);

    let x: i32 = opaque_i32(i32::MAX);
    let y: f64 = x as f64;
    assert!(y == 2147483647.0, "i32 -> f64 max: expected 2147483647.0, got {}", y);

    let x: i32 = opaque_i32(i32::MIN);
    let y: f64 = x as f64;
    assert!(y == -2147483648.0, "i32 -> f64 min: expected -2147483648.0, got {}", y);

    let x: i32 = opaque_i32(42);
    let y: f32 = x as f32;
    assert!(y == 42.0, "i32 -> f32: expected 42.0, got {}", y);

    let x: u32 = opaque_u32(42);
    let y: f64 = x as f64;
    assert!(y == 42.0, "u32 -> f64: expected 42.0, got {}", y);

    let x: i64 = opaque_i64(42);
    let y: f64 = x as f64;
    assert!(y == 42.0, "i64 -> f64: expected 42.0, got {}", y);

    let x: f64 = opaque_f64(42.0);
    let y: i32 = x as i32;
    assert!(y == 42, "f64 -> i32: expected 42, got {}", y);

    const C: i32 = 42;
    const CF: f64 = C as f64;
    assert!(CF == 42.0, "const i32 -> f64: expected 42.0, got {}", CF);

    const C2: i32 = -100;
    const CF2: f64 = C2 as f64;
    assert!(CF2 == -100.0, "const i32 -> f64 negative: expected -100.0, got {}", CF2);

    const C3: i32 = i32::MAX;
    const CF3: f64 = C3 as f64;
    assert!(CF3 == 2147483647.0, "const i32 -> f64 max: expected 2147483647.0, got {}", CF3);

    const C4: i32 = i32::MIN;
    const CF4: f64 = C4 as f64;
    assert!(CF4 == -2147483648.0, "const i32 -> f64 min: expected -2147483648.0, got {}", CF4);
}
