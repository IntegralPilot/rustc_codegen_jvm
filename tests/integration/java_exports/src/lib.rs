#![feature(register_tool)]
#![register_tool(jvm_codegen)]

// Rust-public implementation details do not acquire a Java method surface.
pub struct Internal {
    pub value: i32,
}

impl Internal {
    #[inline(never)]
    pub fn read(&self) -> i32 {
        self.value
    }
}

pub fn internal_function(value: i32) -> i32 {
    Internal { value }.read()
}

#[jvm_codegen::export]
pub fn answer() -> i32 {
    internal_function(42)
}

#[jvm_codegen::export]
pub struct Counter {
    pub value: i32,
}

impl Counter {
    pub fn add(&mut self, amount: i32) -> i32 {
        self.value += amount;
        self.value
    }
}

#[jvm_codegen::export]
pub mod api {
    pub enum Choice {
        First,
        Second,
    }

    pub mod calls {
        pub fn choose(value: bool) -> super::Choice {
            if value {
                super::Choice::First
            } else {
                super::Choice::Second
            }
        }
    }
}

// Rust calls and Java receiver methods must share the exported upstream schema.
#[jvm_codegen::export]
pub fn upstream(mut value: export_provider::Shared) -> export_provider::Shared {
    value.add(export_provider::internal_value(export_provider::Internal {
        value: 1,
        extra: 1,
    }));
    value
}

static DROP_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

// Rust visibility alone does not require an erased Java destruction callback.
pub struct InternalDrop {
    value: f64,
}

impl Drop for InternalDrop {
    fn drop(&mut self) {
        assert_eq!(self.value, 42.0);
        DROP_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    }
}

#[jvm_codegen::export]
pub fn internal_drop_value() -> *mut InternalDrop {
    Box::into_raw(Box::new(InternalDrop { value: 42.0 }))
}

#[jvm_codegen::export]
pub unsafe fn destroy_internal(value: *mut InternalDrop) {
    unsafe {
        drop(Box::from_raw(value));
    }
    // Trait erasure still needs a concrete destruction adapter.
    let erased: Box<dyn std::any::Any> = Box::new(InternalDrop { value: 42.0 });
    drop(erased);
}

#[jvm_codegen::export]
pub struct ExportedDrop {
    pub value: f64,
}

impl Drop for ExportedDrop {
    fn drop(&mut self) {
        assert_eq!(self.value, 7.0);
        DROP_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    }
}

#[jvm_codegen::export]
pub fn drop_count() -> usize {
    DROP_COUNT.load(std::sync::atomic::Ordering::SeqCst)
}
