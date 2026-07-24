unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:fillRandom"]
    fn fill_random(destination: *mut u8, length: usize);
}

pub fn fill_bytes(bytes: &mut [u8]) {
    unsafe { fill_random(bytes.as_mut_ptr(), bytes.len()) }
}
