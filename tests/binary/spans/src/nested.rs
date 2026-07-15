unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:raise:(Lorg/rustlang/runtime/Pointer;J)V"]
    fn raise(message: *const u8, length: usize) -> !;
}

#[inline(never)]
fn leaf() -> ! {
    unsafe { raise(core::ptr::null(), 0) }
}

#[inline(never)]
pub fn outer() {
    leaf();
}
