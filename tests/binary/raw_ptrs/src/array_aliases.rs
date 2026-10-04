#[repr(C)]
struct Pair {
    first: u32,
    second: u32,
}

#[inline(never)]
unsafe fn replace(pointer: *mut Pair) {
    unsafe {
        (*pointer).first = 0x4433_2211;
    }
}

#[inline(never)]
unsafe fn read_second(pointer: *const Pair) -> u32 {
    unsafe { (*pointer).second }
}

pub fn run() {
    let mut values = [std::hint::black_box(1u32), 2];
    let pair = values.as_mut_ptr().cast::<Pair>();
    unsafe {
        replace(pair);
    }
    // Direct array indexing must observe writes through a decoded struct view.
    assert_eq!(values[0], 0x4433_2211);
    values[1] = 0x8877_6655;
    assert_eq!(unsafe { read_second(pair) }, 0x8877_6655);

    // Views made before the alias exists must retain the same storage too.
    #[repr(align(4))]
    struct Bytes([u8; 8]);
    let mut bytes = Bytes([std::hint::black_box(0u8); 8]);
    let pointer = bytes.0.as_mut_ptr();
    let view = std::ptr::slice_from_raw_parts(pointer, 8);
    unsafe {
        replace(pointer.cast());
    }
    let view = unsafe { &*view };
    assert_eq!(&view[..4], &[0x11, 0x22, 0x33, 0x44]);
}
