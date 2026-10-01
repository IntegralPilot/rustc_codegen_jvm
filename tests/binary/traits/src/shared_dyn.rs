use std::sync::Arc;

#[inline(never)]
fn clone_at_address<T: Clone>(value: &T) -> T {
    // Force a storage codec for a wrapper around a dynamically sized tail.
    let copy = std::mem::ManuallyDrop::new(value.clone());
    unsafe { std::ptr::read(std::hint::black_box(&*copy)) }
}

pub fn run() {
    let bias = 7;
    let callback: Arc<dyn Fn(i32) -> i32 + Send + Sync> = Arc::new(move |x| x + bias);
    let weak = Arc::downgrade(&callback);
    let copy = clone_at_address(&callback);
    let weak_copy = clone_at_address(&weak);
    assert_eq!(copy(35), 42);
    assert_eq!(weak_copy.upgrade().unwrap()(1), 8);
    drop(copy);
    drop(callback);
    assert!(weak_copy.upgrade().is_none());

    let bytes: Arc<[u8]> = Arc::from([3, 5, 7]);
    let bytes_copy = clone_at_address(&bytes);
    assert_eq!(&*bytes_copy, &[3, 5, 7]);
    let text: Arc<str> = Arc::from("tail");
    assert_eq!(&*clone_at_address(&text), "tail");

    let owned = String::from("moved");
    let once: Box<dyn FnOnce() -> String> = Box::new(move || owned);
    assert_eq!(once(), "moved");
    let mut total = 0;
    let mut mutable: Box<dyn FnMut(i32) -> i32> = Box::new(move |n| {
        total += n;
        total
    });
    assert_eq!(mutable(3), 3);
    assert_eq!(mutable(4), 7);

    let text: &'static str = "borrowed result";
    let borrowed: Arc<dyn Fn() -> &'static str + Send + Sync> = Arc::new(move || text);
    assert_eq!(clone_at_address(&borrowed)(), "borrowed result");
}
