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
