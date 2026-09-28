use std::cell::Cell;
use std::hint::black_box;
use std::mem::MaybeUninit;
use std::ptr;

struct Worker {
    value: usize,
}

#[repr(transparent)]
struct Wrapped(Worker);

thread_local! {
    static CURRENT: Cell<*const Worker> = const { Cell::new(ptr::null()) };
}

#[inline(never)]
fn roundtrip(pointer: *const Worker) -> *const Worker {
    CURRENT.set(black_box(pointer));
    let copied = black_box(CURRENT.get());
    assert_eq!(copied, pointer);
    CURRENT.set(ptr::null());
    copied
}

pub fn run() {
    // Reading a raw pointer must not read the value it points at.
    assert!(CURRENT.get().is_null());
    assert!(roundtrip(ptr::null()).is_null());
    roundtrip(ptr::dangling());
    roundtrip(ptr::without_provenance(0x1234));

    let worker = Worker { value: 42 };
    assert_eq!(unsafe { (*roundtrip(&worker)).value }, 42);

    let workers = [Worker { value: 1 }, Worker { value: 2 }];
    roundtrip(workers.as_ptr().wrapping_add(workers.len()));

    let uninit = MaybeUninit::<Worker>::uninit();
    roundtrip(uninit.as_ptr());

    // Recovering an existing managed wrapper must still preserve its field
    // and address, for both scalar storage and array elements.
    let wrapped = Wrapped(Worker { value: 7 });
    let pointer = (&wrapped as *const Wrapped).cast::<Worker>();
    assert_eq!(unsafe { (*roundtrip(pointer)).value }, 7);
    let wrapped = [Wrapped(Worker { value: 8 }), Wrapped(Worker { value: 9 })];
    let pointer = wrapped.as_ptr().wrapping_add(1).cast::<Worker>();
    assert_eq!(unsafe { (*roundtrip(pointer)).value }, 9);
    assert!(CURRENT.get().is_null());
}
