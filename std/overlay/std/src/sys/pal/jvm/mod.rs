#![deny(unsafe_op_in_unsafe_fn)]

use crate::io;

pub mod futex;

// SAFETY: called exactly once by the standard-library runtime.
pub unsafe fn init(_argc: isize, _argv: *const *const u8, _sigpipe: u8) {}

// SAFETY: called exactly once after all Rust-controlled main-thread work.
pub unsafe fn cleanup() {
    unsafe { crate::sys::thread_local::key::destroy_tls() };
}

pub fn unsupported<T>() -> io::Result<T> {
    Err(unsupported_err())
}

pub fn unsupported_err() -> io::Error {
    io::Error::UNSUPPORTED_PLATFORM
}

pub fn abort_internal() -> ! {
    core::intrinsics::abort()
}
