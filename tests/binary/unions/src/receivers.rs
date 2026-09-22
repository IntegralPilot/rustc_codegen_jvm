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
    inline_writes();
    heap_writes();
    replace_nested_union();
}

fn inline_writes() {
    let mut data = Data::<[f64; 4]> {
        inline: ManuallyDrop::new(MaybeUninit::new([1.0, 2.0, 3.0, 4.0])),
    };
    unsafe {
        let pointer = data.inline_mut();
        assert_eq!(*pointer.as_ptr().add(2), 3.0);
        pointer.as_ptr().add(2).write(42.0);
        assert_eq!(data.inline.assume_init_ref()[2], 42.0);
        assert_eq!(*data.inline_mut().as_ptr().add(2), 42.0);
    }
}

fn heap_writes() {
    let mut heap = [5.0, 6.0, 7.0, 8.0, 9.0];
    let mut data = Data::<[f64; 4]> {
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

struct Buffer {
    data: Data<[f64; 4]>,
    capacity: usize,
}

impl Buffer {
    fn heap_mut(&mut self) -> (NonNull<f64>, &mut usize) {
        unsafe { self.data.heap_mut() }
    }

    fn replace(&mut self, pointer: NonNull<f64>, len: usize) {
        self.data = Data {
            heap: (pointer, len),
        };
        self.capacity = len;
    }
}

fn replace_nested_union() {
    let mut first = [1.0, 2.0, 3.0, 4.0];
    let mut second = [5.0, 6.0, 7.0, 8.0, 9.0];
    let first_pointer = NonNull::new(first.as_mut_ptr()).unwrap();
    let second_pointer = NonNull::new(second.as_mut_ptr()).unwrap();
    let mut buffer = Buffer {
        data: Data {
            heap: (first_pointer, first.len()),
        },
        capacity: first.len(),
    };
    let (pointer, len) = buffer.heap_mut();
    assert_eq!(pointer, first_pointer);
    *len = 3;

    // Replacing the union must invalidate cached views rooted in its local cell.
    buffer.replace(second_pointer, second.len());
    assert_eq!(buffer.capacity, second.len());
    let (pointer, len) = buffer.heap_mut();
    assert_eq!(pointer, second_pointer);
    assert_eq!(*len, second.len());
    unsafe { pointer.as_ptr().add(4).write(99.0) };
    assert_eq!(second[4], 99.0);
}
