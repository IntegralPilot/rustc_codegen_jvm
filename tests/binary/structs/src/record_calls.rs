use std::hint::black_box;

#[derive(Clone, Copy)]
struct Values {
    bits: u64,
    x: f64,
    y: f32,
    enabled: bool,
}

#[inline(never)]
fn update(mut value: Values) -> Values {
    value.x += value.y as f64;
    value.enabled = !value.enabled;
    value
}

#[inline(never)]
fn suffix(value: Values, data: &[i64]) -> &[i64] {
    &data[(value.bits & 1) as usize..]
}

#[inline(never)]
fn pair(value: (f64, f64)) -> (f64, f64) {
    value
}

#[inline(never)]
fn narrow(value: (u8, i16)) -> u32 {
    (value.0 as u32) * 65536 + value.1 as u16 as u32
}

pub fn run() {
    let narrow_fn = black_box(narrow as fn((u8, i16)) -> u32);
    assert_eq!(narrow_fn(black_box((255, -128))), 0xffff80);
    let value = black_box(Values {
        bits: u64::MAX,
        x: -0.0,
        y: 3.5,
        enabled: true,
    });
    let direct = update(value);
    let indirect = black_box(update as fn(Values) -> Values)(value);
    for result in [direct, indirect] {
        assert_eq!(result.bits, u64::MAX);
        assert_eq!(result.x, 3.5);
        assert!(!result.enabled);
    }
    assert_eq!(value.x.to_bits(), (-0.0f64).to_bits());
    assert!(value.enabled);
    let data = black_box([11, 23, 47]);
    assert_eq!(suffix(value, &data), &[23, 47]);
    let read = black_box(suffix as fn(Values, &[i64]) -> &[i64]);
    assert_eq!(read(value, &data), &[23, 47]);
    let bits = 0x7ff8_1234_5678_9abcu64;
    let values = black_box((f64::from_bits(bits), -0.0));
    let result = black_box(pair as fn((f64, f64)) -> (f64, f64))(values);
    assert_eq!(result.0.to_bits(), bits);
    assert_eq!(result.1.to_bits(), (-0.0f64).to_bits());
}
