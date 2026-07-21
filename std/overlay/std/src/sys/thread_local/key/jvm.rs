pub type Key = usize;
pub type Dtor = unsafe extern "C" fn(*mut u8);

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:tlsCreate:(Lorg/rustlang/runtime/Pointer;)J"]
    fn jvm_tls_create(dtor: *mut ()) -> usize;

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:tlsGet:(J)Lorg/rustlang/runtime/Pointer;"]
    fn jvm_tls_get(key: usize) -> *mut u8;

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:tlsSet:(JLorg/rustlang/runtime/Pointer;)V"]
    fn jvm_tls_set(key: usize, value: *mut u8);

    #[link_name = "jvm:static:org/rustlang/runtime/ThreadSupport:tlsRunDestructors:()V"]
    fn jvm_tls_run_destructors();
}

#[inline]
pub fn create(dtor: Option<Dtor>) -> Key {
    let dtor = dtor.map_or(ptr::null_mut(), |dtor| dtor as *mut ());
    unsafe { jvm_tls_create(dtor) }
}

#[inline]
pub unsafe fn set(key: Key, value: *mut u8) {
    unsafe { jvm_tls_set(key, value) };
}

#[inline]
pub unsafe fn get(key: Key) -> *mut u8 {
    unsafe { jvm_tls_get(key) }
}

#[inline]
pub unsafe fn destroy(_key: Key) {
    // Keys and their destructor records remain valid for the process lifetime.
}

pub unsafe fn destroy_tls() {
    unsafe { jvm_tls_run_destructors() };
    crate::rt::thread_cleanup();
}
use crate::ptr;
