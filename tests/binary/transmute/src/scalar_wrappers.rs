use std::{hint::black_box, mem::transmute, num::NonZeroUsize};

#[repr(transparent)]
struct Word(u64);

#[repr(transparent)]
struct Nested(Word);

#[repr(C)]
struct Float(f64);

#[inline(never)]
fn round_trip(value: u64) -> u64 {
    unsafe {
        let word: Nested = transmute(value);
        let float: Float = transmute(word);
        transmute(float)
    }
}

pub fn run() {
    for bits in [0, 1, u64::MAX, 0x8000_0000_0000_0000, 0x7ff8_0000_0000_1234] {
        assert_eq!(round_trip(black_box(bits)), bits);
    }
    for bits in [0, 1, 17, usize::MAX] {
        let optional = black_box(NonZeroUsize::new(black_box(bits)));
        let encoded: usize = unsafe { transmute(optional) };
        assert_eq!(encoded, bits);
        let decoded: Option<NonZeroUsize> = unsafe { transmute(black_box(encoded)) };
        assert_eq!(
            decoded.map(NonZeroUsize::get),
            optional.map(NonZeroUsize::get)
        );
    }
    for bits in [1, 17, usize::MAX] {
        let nonzero: NonZeroUsize = unsafe { transmute(black_box(bits)) };
        assert_eq!(nonzero.get(), bits);
        let result: usize = unsafe { transmute(black_box(nonzero)) };
        assert_eq!(result, bits);
    }
}
