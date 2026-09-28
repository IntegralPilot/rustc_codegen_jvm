use std::cell::Cell;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
struct State {
    lock: AtomicUsize,
    data: Cell<usize>,
}
#[repr(C)]
struct View {
    lock: AtomicUsize,
    data: Cell<usize>,
}

#[inline(never)]
fn update(view: &View, lock: &AtomicUsize) {
    view.data.set(1);
    lock.store(0, Ordering::Relaxed);
    view.data.set(2);
    assert_eq!(lock.load(Ordering::Relaxed), 0);
    assert_eq!(view.lock.load(Ordering::Relaxed), 0);
}

pub fn run() {
    let state = State {
        lock: AtomicUsize::new(8),
        data: Cell::new(0),
    };
    let view = unsafe { &*(&state as *const State).cast::<View>() };
    update(view, &state.lock);
    assert_eq!(state.lock.load(Ordering::Relaxed), 0);
    assert_eq!(state.data.get(), 2);
}
