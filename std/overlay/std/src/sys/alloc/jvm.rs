use crate::alloc::Layout;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/Pointer:allocateBytes"]
    fn allocate_bytes(size: usize, alignment: usize) -> *mut u8;

    #[link_name = "jvm:static:org/rustlang/runtime/Pointer:reallocateBytes"]
    fn reallocate_bytes(
        pointer: *mut u8,
        old_size: usize,
        alignment: usize,
        new_size: usize,
    ) -> *mut u8;

    #[link_name = "jvm:static:org/rustlang/runtime/Pointer:deallocateBytes"]
    fn deallocate_bytes(pointer: *mut u8);
}

pub unsafe fn alloc(layout: Layout) -> *mut u8 {
    unsafe { allocate_bytes(layout.size(), layout.align()) }
}

pub unsafe fn alloc_zeroed(layout: Layout) -> *mut u8 {
    // JVM arrays are zero-initialized.
    unsafe { allocate_bytes(layout.size(), layout.align()) }
}

pub unsafe fn dealloc(pointer: *mut u8, _layout: Layout) {
    unsafe { deallocate_bytes(pointer) }
}

pub unsafe fn realloc(pointer: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    unsafe { reallocate_bytes(pointer, layout.size(), layout.align(), new_size) }
}
