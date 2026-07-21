use crate::sync::atomic::Atomic;
use crate::time::Duration;

/// Atomic storage used by the standard library's futex-based synchronization.
pub type Futex = Atomic<Primitive>;
pub type Primitive = u32;
pub type SmallFutex = Atomic<SmallPrimitive>;
pub type SmallPrimitive = u32;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:futexWait:(Lorg/rustlang/runtime/Pointer;IJ)Z"]
    fn jvm_futex_wait(address: *const u32, expected: u32, timeout_nanos: i64) -> bool;

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:futexWake:(Lorg/rustlang/runtime/Pointer;)Z"]
    fn jvm_futex_wake(address: *const u32) -> bool;

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:futexWakeAll:(Lorg/rustlang/runtime/Pointer;)V"]
    fn jvm_futex_wake_all(address: *const u32);
}

pub fn futex_wait(futex: &Atomic<u32>, expected: u32, timeout: Option<Duration>) -> bool {
    let timeout_nanos = timeout
        .and_then(|duration| i64::try_from(duration.as_nanos()).ok())
        .unwrap_or(-1);
    unsafe { jvm_futex_wait(futex.as_ptr(), expected, timeout_nanos) }
}

#[inline]
pub fn futex_wake(futex: &Atomic<u32>) -> bool {
    unsafe { jvm_futex_wake(futex.as_ptr()) }
}

#[inline]
pub fn futex_wake_all(futex: &Atomic<u32>) {
    unsafe { jvm_futex_wake_all(futex.as_ptr()) }
}
