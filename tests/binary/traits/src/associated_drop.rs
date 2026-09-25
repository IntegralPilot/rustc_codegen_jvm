use std::any::Any;
use std::sync::atomic::{AtomicUsize, Ordering};

static DROPS: AtomicUsize = AtomicUsize::new(0);

trait Marker {
    type Value: Any;
}
struct Owned;
struct Payload(String);
impl Marker for Owned {
    type Value = Payload;
}
impl Drop for Payload {
    fn drop(&mut self) {
        assert_eq!(self.0, "associated pointee");
        DROPS.fetch_add(1, Ordering::SeqCst);
    }
}

#[inline(never)]
fn erase<M: Marker>(pointer: *mut M::Value) -> *mut dyn Any {
    pointer
}

pub fn run() {
    let value = Box::new(Payload("associated pointee".into()));
    let erased = erase::<Owned>(Box::into_raw(value));
    let value = unsafe { Box::from_raw(erased) };
    assert!(value.is::<Payload>());
    drop(value);
    assert_eq!(DROPS.load(Ordering::SeqCst), 1);
}
