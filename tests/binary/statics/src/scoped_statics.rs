use std::hint::black_box;

trait Element {
    const VTABLE: &'static u32;
}

struct First;
struct Second;

impl Element for First {
    const VTABLE: &'static u32 = {
        static VTABLE: u32 = 11;
        &VTABLE
    };
}

impl Element for Second {
    const VTABLE: &'static u32 = {
        static VTABLE: u32 = 22;
        &VTABLE
    };
}

fn shared<T>() -> &'static u32 {
    static VALUE: u32 = 33;
    &VALUE
}

pub fn run() {
    // Typst's native rule registry keys elements by these static addresses.
    let first = black_box(First::VTABLE);
    let second = black_box(Second::VTABLE);
    assert!(!core::ptr::eq(first, second));
    assert_eq!(*first, 11);
    assert_eq!(*second, 22);

    // Sibling blocks also have identical named paths but distinct definitions.
    let left = {
        static VALUE: u32 = 44;
        black_box(&VALUE)
    };
    let right = {
        static VALUE: u32 = 44;
        black_box(&VALUE)
    };
    assert!(!core::ptr::eq(left, right));
    assert_eq!(*left, 44);
    assert_eq!(*right, 44);

    // A single definition stays shared across generic instantiations.
    assert!(core::ptr::eq(black_box(shared::<u8>()), shared::<u16>()));
}
