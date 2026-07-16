const PANIC_BUFFER_CAPACITY: usize = 256;
const TRUNCATION_MARKER: &[u8] = b"... <panic message truncated>";

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

    unsafe fn dealloc(&self, _pointer: *mut u8, _layout: core::alloc::Layout) {
        // Dropping the last Pointer carrier releases the JVM byte array to GC.
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

struct PanicBuffer<'a> {
    bytes: &'a mut [u8],
    len: usize,
    truncated: bool,
}

impl<'a> PanicBuffer<'a> {
    fn new(bytes: &'a mut [u8]) -> Self {
        Self {
            bytes,
            len: 0,
            truncated: false,
        }
    }

    fn append_bytes(&mut self, source: &[u8]) {
        let start = self.len;
        let mut index = 0;
        while index < source.len() {
            self.bytes[start + index] = source[index];
            index += 1;
        }
        self.len = start + source.len();
    }

    fn finish(&mut self) -> &str {
        if self.truncated {
            self.append_bytes(TRUNCATION_MARKER);
        }

        // Every write is copied from a valid `str`, and truncated writes stop
        // only at a UTF-8 character boundary.
        unsafe { core::str::from_utf8_unchecked(&self.bytes[..self.len]) }
    }
}

impl core::fmt::Write for PanicBuffer<'_> {
    fn write_str(&mut self, text: &str) -> core::fmt::Result {
        if self.truncated {
            return Err(core::fmt::Error);
        }

        let payload_capacity = PANIC_BUFFER_CAPACITY - TRUNCATION_MARKER.len();
        let available = payload_capacity - self.len;
        if text.len() <= available {
            self.append_bytes(text.as_bytes());
            return Ok(());
        }

        let mut end = available;
        while end > 0 && !text.is_char_boundary(end) {
            end -= 1;
        }
        let bytes = text.as_bytes();
        let mut index = 0;
        while index < end {
            self.bytes[self.len + index] = bytes[index];
            index += 1;
        }
        self.len += end;
        self.truncated = true;
        Err(core::fmt::Error)
    }
}

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:raise:(Lorg/rustlang/runtime/Pointer;J)V"]
    fn raise_panic(message: *const u8, length: usize) -> !;
}

#[panic_handler]
fn panic(info: &core::panic::PanicInfo<'_>) -> ! {
    let mut storage = [0; PANIC_BUFFER_CAPACITY];
    let mut buffer = PanicBuffer::new(&mut storage);
    if let Some(location) = info.location() {
        let _ = core::fmt::write(
            &mut buffer,
            format_args!(
                "panicked at {}:{}:{}:\n{}",
                location.file(),
                location.line(),
                location.column(),
                info.message(),
            ),
        );
    } else {
        let _ = core::fmt::write(
            &mut buffer,
            format_args!("panicked:\n{}", info.message()),
        );
    }

    let message = buffer.finish();
    unsafe { raise_panic(message.as_ptr(), message.len()) }
}

#[lang = "start"]
fn start<T>(main: fn() -> T, _: isize, _: *const *const u8, _: u8) -> isize {
    main();
    0
}
