use crate::ffi::CStr;
use crate::io;
use crate::num::NonZero;
use crate::thread::ThreadInit;
use crate::time::Duration;

pub const DEFAULT_MIN_STACK_SIZE: usize = 2 * 1024 * 1024;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:spawn"]
    fn jvm_spawn(init: *mut ThreadInit, stack_size: i64) -> u64;

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:join"]
    fn jvm_join(handle: u64);

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:detach"]
    fn jvm_detach(handle: u64);

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:currentThreadId"]
    fn jvm_current_thread_id() -> u64;

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:availableProcessors"]
    fn jvm_available_processors() -> u32;

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:yieldCurrent"]
    fn jvm_yield_now();

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:sleep"]
    fn jvm_sleep(seconds: i64, nanoseconds: u32);

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:setCurrentName"]
    fn jvm_set_name(bytes: *const u8, length: usize);
}

pub struct Thread {
    handle: u64,
}

impl Thread {
    // unsafe: see thread::Builder::spawn_unchecked for safety requirements
    pub unsafe fn new(stack: usize, init: Box<ThreadInit>) -> io::Result<Thread> {
        let Ok(stack_size) = i64::try_from(stack) else {
            return Err(io::const_error!(io::ErrorKind::InvalidInput, "thread stack is too large"));
        };
        let data = Box::into_raw(init);
        let handle = unsafe { jvm_spawn(data, stack_size) };
        if handle == 0 {
            unsafe { drop(Box::from_raw(data)) };
            Err(io::const_error!(io::ErrorKind::Other, "the JVM could not start a thread"))
        } else {
            Ok(Thread { handle })
        }
    }

    pub fn join(self) {
        unsafe { jvm_join(self.handle) };
    }
}

impl Drop for Thread {
    fn drop(&mut self) {
        unsafe { jvm_detach(self.handle) };
    }
}

#[inline(never)]
fn run_thread(init: Box<ThreadInit>) {
    init.init()();
}

#[unsafe(no_mangle)]
pub extern "C" fn __jvm_thread_start(init: *mut u8) {
    let init = unsafe { Box::from_raw(init.cast::<ThreadInit>()) };
    run_thread(init);
    unsafe { crate::sys::thread_local::key::destroy_tls() };
}

pub fn available_parallelism() -> io::Result<NonZero<usize>> {
    NonZero::new(unsafe { jvm_available_processors() } as usize)
        .ok_or(io::Error::UNKNOWN_THREAD_COUNT)
}

pub fn current_os_id() -> Option<u64> {
    NonZero::new(unsafe { jvm_current_thread_id() }).map(NonZero::get)
}

pub fn yield_now() {
    unsafe { jvm_yield_now() };
}

pub fn set_name(name: &CStr) {
    let bytes = name.to_bytes();
    unsafe { jvm_set_name(bytes.as_ptr(), bytes.len()) };
}

pub fn sleep(mut duration: Duration) {
    const MAX_SECONDS: u64 = i64::MAX as u64 / 1_000_000_000;
    while duration.as_secs() > MAX_SECONDS {
        unsafe { jvm_sleep(MAX_SECONDS as i64, 0) };
        duration -= Duration::from_secs(MAX_SECONDS);
    }
    unsafe { jvm_sleep(duration.as_secs() as i64, duration.subsec_nanos()) };
}
