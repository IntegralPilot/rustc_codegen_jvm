use std::sync::{
    LazyLock,
    atomic::{AtomicUsize, Ordering},
};

struct State {
    lock: AtomicUsize,
    data: AtomicUsize,
}

static STATE: LazyLock<State> = LazyLock::new(|| State {
    lock: AtomicUsize::new(0),
    data: AtomicUsize::new(0),
});

unsafe extern "C" {
    #[link_name = "jvm:static:java/lang/System:gc"]
    fn java_gc();
}

#[inline(never)]
fn acquire() -> &'static State {
    let state = &*STATE;
    state.lock.store(8, Ordering::SeqCst);
    state
}

pub fn run() {
    // LazyLock borrows its initialized value through a ManuallyDrop union
    // field. A returned borrow must retain that field's backing storage even
    // after the temporary pointers used to obtain it have been collected.
    for _ in 0..3 {
        let state = acquire();
        unsafe { java_gc() };
        state.data.store(17, Ordering::SeqCst);
        state.lock.store(0, Ordering::SeqCst);
        assert_eq!(STATE.lock.load(Ordering::SeqCst), 0);
        assert_eq!(STATE.data.load(Ordering::SeqCst), 17);
    }
}
