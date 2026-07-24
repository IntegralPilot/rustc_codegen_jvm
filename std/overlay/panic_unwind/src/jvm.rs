use alloc::boxed::Box;
use alloc::string::String;
use core::any::Any;
use core::slice;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:raisePayload"]
    fn raise_payload(payload: *mut u8);

    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:takePayload"]
    fn take_payload(caught_exception: *mut u8) -> *mut u8;

    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:isRustPanic"]
    fn is_rust_panic(caught_exception: *mut u8) -> bool;

    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:foreignFailureMessage"]
    fn foreign_failure_message(caught_exception: *mut u8) -> *const u8;

    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:foreignFailureMessageLength"]
    fn foreign_failure_message_length(caught_exception: *mut u8) -> usize;
}

pub(crate) unsafe fn cleanup(payload: *mut u8) -> Box<dyn Any + Send> {
    if !unsafe { is_rust_panic(payload) } {
        let message = unsafe { foreign_failure_message(payload) };
        let length = unsafe { foreign_failure_message_length(payload) };
        let bytes = unsafe { slice::from_raw_parts(message, length) };
        return Box::new(String::from_utf8_lossy(bytes).into_owned());
    }
    let payload = unsafe { take_payload(payload) };
    unsafe { *Box::from_raw(payload.cast::<Box<dyn Any + Send>>()) }
}

pub(crate) unsafe fn panic(payload: Box<dyn Any + Send>) -> u32 {
    let payload = Box::into_raw(Box::new(payload)).cast::<u8>();
    unsafe { raise_payload(payload) };
    unreachable!()
}
