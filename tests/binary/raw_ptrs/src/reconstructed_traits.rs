use std::mem::{align_of_val, size_of_val, transmute, transmute_copy};

trait Value {
    fn get(&self) -> i32;
    fn set(&mut self, value: i32);
}

#[repr(C)]
struct Data {
    value: i32,
    untouched: i32,
}

#[repr(transparent)]
struct Packed(Data);

impl Value for Packed {
    fn get(&self) -> i32 {
        self.0.value
    }

    fn set(&mut self, value: i32) {
        self.0.value = value;
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct FatPointer {
    data: *const (),
    vtable: *const (),
}

// The vtable and data pointer originate independently, as in Typst's
// repr(transparent) content wrappers. No live trait-object carrier exists.
const VTABLE: *const () = unsafe {
    transmute::<*const dyn Value, FatPointer>(std::ptr::null::<Packed>() as *const dyn Value).vtable
};

#[inline(never)]
unsafe fn from_parts<T: ?Sized>(data: *const (), vtable: *const ()) -> *mut T {
    unsafe { transmute_copy(&FatPointer { data, vtable }) }
}

fn callable_round_trip() {
    let first = Box::new(12usize);
    let second = String::from("abc");
    let callback = move || *first + second.len();
    let erased = &callback as &dyn Fn() -> usize;
    let parts: FatPointer = unsafe { transmute(erased) };
    let rebuilt = unsafe { &*from_parts::<dyn Fn() -> usize>(parts.data, parts.vtable) };
    assert_eq!(rebuilt(), 15);
}

pub fn run() {
    callable_round_trip();
    let mut data = Data {
        value: 12,
        untouched: 99,
    };
    let pointer = std::ptr::from_mut(&mut data).cast::<()>();
    unsafe {
        let value = &mut *from_parts::<dyn Value>(pointer, VTABLE);
        assert_eq!(size_of_val(value), size_of::<Packed>());
        assert_eq!(align_of_val(value), align_of::<Packed>());
        assert_eq!(value.get(), 12);
        value.set(42);
    }
    assert_eq!(data.value, 42);
    assert_eq!(data.untouched, 99);
    unsafe {
        let value = &*from_parts::<dyn Value>(pointer, VTABLE);
        assert_eq!(value.get(), 42);
    }
}
