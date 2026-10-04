use std::hint::black_box;

const fn words() -> [u64; 9001] {
    let mut result = [0; 9001];
    let mut i = 0;
    while i < result.len() {
        result[i] = (i as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15);
        i += 1;
    }
    result
}

static WORDS: [u64; 9001] = words();
static OTHER: [u64; 9001] = words();
static SIGNED: [i16; 700] = [-12345; 700];
static CHARS: [u16; 700] = [0xfedc; 700];
static FLOATS: [f32; 300] = [f32::from_bits(0x8000_0000); 300];
static DOUBLES: [f64; 150] = [f64::from_bits(0x7ff8_1234_5678_9abc); 150];
static BOOLS: [bool; 1200] = [true; 1200];

#[derive(Clone, Copy)]
struct Row {
    pair: [u8; 2],
    signed: i16,
    bits: u64,
}
const fn rows() -> [Row; 10001] {
    let mut rows = [Row {
        pair: [0; 2],
        signed: -12345,
        bits: u64::MAX,
    }; 10001];
    let mut i = 0;
    while i < rows.len() {
        rows[i].pair = (i as u16).to_le_bytes();
        i += 1;
    }
    rows
}
static ROWS: [Row; 10001] = rows();
static OTHER_ROWS: [Row; 10001] = rows();

pub fn run() {
    let rows = black_box(&ROWS);
    for (i, row) in rows.iter().enumerate() {
        assert_eq!(row.pair, (i as u16).to_le_bytes());
        assert_eq!(row.signed, -12345);
        assert_eq!(row.bits, u64::MAX);
    }
    assert!(!core::ptr::eq(rows, black_box(&OTHER_ROWS)));
    let mut copied = rows[8192];
    copied.pair[1] = 0;
    assert_eq!(rows[8192].pair[1], 32);
    assert_eq!(copied.pair[1], 0);
    let words = black_box(&WORDS);
    for i in 0..words.len() {
        assert_eq!(words[i], (i as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15));
    }
    assert!(!core::ptr::eq(words, black_box(&OTHER)));
    for &n in black_box(&SIGNED) {
        assert_eq!(n, -12345);
    }
    for &n in black_box(&CHARS) {
        assert_eq!(n, 0xfedc);
    }
    for &n in black_box(&FLOATS) {
        assert_eq!(n.to_bits(), 0x8000_0000);
    }
    for &n in black_box(&DOUBLES) {
        assert_eq!(n.to_bits(), 0x7ff8_1234_5678_9abc);
    }
    for &n in black_box(&BOOLS) {
        assert!(n);
    }
    let mut copy = *words;
    copy[8192] = 9;
    assert_ne!(words[8192], copy[8192]);
}
