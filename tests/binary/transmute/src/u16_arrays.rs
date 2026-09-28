use std::hint::black_box;

#[inline(never)]
fn halves(word: &u32) -> &[u16; 2] {
    unsafe { &*(word as *const u32).cast() }
}

#[inline(never)]
fn halves_mut(word: &mut u32) -> &mut [u16; 2] {
    unsafe { &mut *(word as *mut u32).cast() }
}

pub fn run() {
    // The array view is backed by a scalar allocation, not a native u16 array.
    // Include the high bit to detect accidental sign extension or narrowing.
    let mut word = black_box(u32::from_ne_bytes([0x34, 0x12, 0xff, 0xff]));
    let view = black_box(*halves(black_box(&word)));
    assert_eq!(view[0], u16::from_ne_bytes([0x34, 0x12]));
    assert_eq!(view[1], u16::MAX);

    halves_mut(black_box(&mut word))[0] = u16::from_ne_bytes([0x00, 0x80]);
    assert_eq!(word.to_ne_bytes(), [0x00, 0x80, 0xff, 0xff]);
    assert_eq!(halves(black_box(&word))[0], u16::from_ne_bytes([0x00, 0x80]));
    assert_eq!(view[0], u16::from_ne_bytes([0x34, 0x12]));
}
