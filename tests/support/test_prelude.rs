extern crate alloc;

struct JvmAllocator;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/Pointer:allocateBytes:(JJ)Lorg/rustlang/runtime/Pointer;"]
    fn allocate_bytes(size: usize, alignment: usize) -> *mut u8;

    #[link_name = "jvm:static:org/rustlang/runtime/Pointer:reallocateBytes:(Lorg/rustlang/runtime/Pointer;JJJ)Lorg/rustlang/runtime/Pointer;"]
    fn reallocate_bytes(
        pointer: *mut u8,
        old_size: usize,
        alignment: usize,
        new_size: usize,
    ) -> *mut u8;
}

unsafe impl core::alloc::GlobalAlloc for JvmAllocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        unsafe { allocate_bytes(layout.size(), layout.align()) }
    }

    unsafe fn dealloc(&self, pointer: *mut u8, _layout: core::alloc::Layout) {
        unsafe { deallocate_bytes(pointer) }
    }

    unsafe fn realloc(
        &self,
        pointer: *mut u8,
        layout: core::alloc::Layout,
        new_size: usize,
    ) -> *mut u8 {
        unsafe { reallocate_bytes(pointer, layout.size(), layout.align(), new_size) }
    }

    unsafe fn alloc_zeroed(&self, layout: core::alloc::Layout) -> *mut u8 {
        // JVM arrays are zero-initialized.
        unsafe { allocate_bytes(layout.size(), layout.align()) }
    }
}

#[global_allocator]
static GLOBAL_ALLOCATOR: JvmAllocator = JvmAllocator;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:raise:(Lorg/rustlang/runtime/Pointer;J)V"]
    fn raise_panic(message: *const u8, length: usize) -> !;
}

#[panic_handler]
fn panic(info: &core::panic::PanicInfo<'_>) -> ! {
    let message = if let Some(location) = info.location() {
        alloc::format!(
            "panicked at {}:{}:{}:\n{}",
            location.file(),
            location.line(),
            location.column(),
            info.message(),
        )
    } else {
        alloc::format!("panicked:\n{}", info.message())
    };

    unsafe { 
        raise_panic(message.as_ptr(), message.len()) 
    }
}

#[lang = "start"]
fn start<T>(main: fn() -> T, _: isize, _: *const *const u8, _: u8) -> isize {
    main();
    0
}

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/Pointer:deallocateBytes:(Lorg/rustlang/runtime/Pointer;)V"]
    fn deallocate_bytes(pointer: *mut u8);
}
