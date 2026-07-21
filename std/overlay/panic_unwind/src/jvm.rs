use alloc::boxed::Box;
use core::any::Any;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:raisePayload:(Lorg/rustlang/runtime/Pointer;)V"]
    fn raise_payload(payload: *mut u8);

    #[link_name = "jvm:static:org/rustlang/runtime/PanicSupport:takePayload:(Lorg/rustlang/runtime/Pointer;)Lorg/rustlang/runtime/Pointer;"]
    fn take_payload(caught_exception: *mut u8) -> *mut u8;
}

pub(crate) unsafe fn cleanup(payload: *mut u8) -> Box<dyn Any + Send> {
    let payload = unsafe { take_payload(payload) };
    unsafe { *Box::from_raw(payload.cast::<Box<dyn Any + Send>>()) }
}

pub(crate) unsafe fn panic(payload: Box<dyn Any + Send>) -> u32 {
    let payload = Box::into_raw(Box::new(payload)).cast::<u8>();
    unsafe { raise_payload(payload) };
    unreachable!()
}
