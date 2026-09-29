#![feature(custom_inner_attributes)]
#![feature(register_tool)]
#![register_tool(jvm_codegen)]
#![jvm_codegen::export]
pub fn address_bits(expose: bool) -> usize {
    let value = std::hint::black_box(17_u64);
    let pointer = std::hint::black_box(&value as *const u64);
    if expose {
        pointer as usize
    } else {
        // This is also how the pinned core::ptr::addr implementation reads bits.
        unsafe { std::mem::transmute::<*const u64, usize>(pointer) }
    }
}

pub fn format_many(count: u32) -> usize {
    (0..count).map(|value| format!("{value}").len()).sum()
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Pixel {
    pub bytes: [u8; 4],
}

pub fn pixel_storage() -> *mut Pixel {
    Box::into_raw(Box::new(Pixel { bytes: [1, 2, 3, 4] }))
}

pub unsafe fn free_pixel(pointer: *mut Pixel) {
    unsafe { drop(Box::from_raw(pointer)); }
}

pub fn first_word(words: [u32; 2]) -> u32 {
    words[0]
}
