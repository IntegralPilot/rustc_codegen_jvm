// Regression for #61, reduced from SmallVecData's inline_mut/heap_mut methods.
// Union receiver aliases must not use deferred-pointer slots for JVM `this`.
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ptr::NonNull;

trait Array {
    type Item;
}
impl Array for [f64; 4] {
    type Item = f64;
}

union Data<A: Array> {
    inline: ManuallyDrop<MaybeUninit<A>>,
    heap: (NonNull<A::Item>, usize),
}

impl<A: Array> Data<A> {
    unsafe fn heap_mut(&mut self) -> (NonNull<A::Item>, &mut usize) {
        let heap = unsafe { &mut self.heap };
        (heap.0, &mut heap.1)
    }

    unsafe fn inline_mut(&mut self) -> NonNull<A::Item> {
        unsafe { NonNull::new_unchecked(self.inline.as_mut_ptr() as *mut A::Item) }
    }
}

pub fn run() {
    let mut data = Data::<[f64; 4]> {
        inline: ManuallyDrop::new(MaybeUninit::new([1.0, 2.0, 3.0, 4.0])),
    };
    unsafe {
        let pointer = data.inline_mut();
        assert_eq!(*pointer.as_ptr().add(2), 3.0);
    }
    let mut heap = [5.0, 6.0, 7.0, 8.0, 9.0];
    data = Data {
        heap: (NonNull::new(heap.as_mut_ptr()).unwrap(), heap.len()),
    };
    unsafe {
        let (pointer, len) = data.heap_mut();
        assert_eq!(pointer.as_ptr(), heap.as_mut_ptr());
        *len -= 1;
        pointer.as_ptr().add(4).write(99.0);
        assert_eq!(data.heap.1, 4);
        assert_eq!(heap[4], 99.0);
    }
}
