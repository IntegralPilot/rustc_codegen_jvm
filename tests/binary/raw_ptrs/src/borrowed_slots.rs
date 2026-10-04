use std::hint::black_box;

#[derive(Clone, Copy)]
#[repr(C)]
struct Slots {
    slice: &'static [u8],
    fixed: &'static [u8; 4],
    text: &'static str,
    word: &'static u32,
}

#[repr(C)]
struct Nested {
    prefix: u64,
    slots: Slots,
}

#[derive(Clone, Copy)]
struct Saved {
    slice: *mut &'static [u8],
    fixed: *mut &'static [u8; 4],
    text: *mut &'static str,
    word: *mut &'static u32,
}

#[inline(never)]
fn replace<T>(place: &mut T, value: T) -> T {
    std::mem::replace(place, value)
}

pub fn run() {
    static FIRST: [u8; 4] = [3, 5, 7, 11];
    static SECOND: [u8; 4] = [13, 17, 19, 23];
    static WORD: u32 = 29;
    let first = Slots {
        slice: &FIRST,
        fixed: &FIRST,
        text: "héllo",
        word: &WORD,
    };
    let second = Slots {
        slice: &SECOND[1..],
        fixed: &SECOND,
        text: "goodbye",
        word: &WORD,
    };
    parameter_storage(first, second);
    let mut value = Nested {
        prefix: 31,
        slots: first,
    };
    let root = &raw mut value;
    let saved = unsafe {
        Saved {
            slice: &raw mut (*root).slots.slice,
            fixed: &raw mut (*root).slots.fixed,
            text: &raw mut (*root).slots.text,
            word: &raw mut (*root).slots.word,
        }
    };
    unsafe {
        let update: fn(&mut &'static [u8], &'static [u8]) -> &'static [u8] = replace;
        assert_eq!(black_box(update)(&mut *saved.slice, &SECOND), FIRST);
        assert_eq!((*saved.fixed)[2], 7);
        assert_eq!(*saved.text, "héllo");
        root.write(Nested {
            prefix: 37,
            slots: second,
        });
        assert_eq!(*saved.slice, &SECOND[1..]);
        assert_eq!((*saved.fixed)[2], 19);
        assert_eq!(*saved.text, "goodbye");
        // The addresses survive being stored, copied, captured and returned.
        let saved = black_box(saved);
        let captured = move || {
            replace(&mut *saved.fixed, &FIRST);
            replace(&mut *saved.text, "é");
            **saved.word
        };
        assert_eq!(black_box(captured)(), 29);
        assert_eq!((*root).slots.fixed[1], 5);
        let text = (*root).slots.text;
        assert_eq!(text.len(), 2);

        // A later raw byte alias of the enclosing owner updates the same field.
        let bytes = root.cast::<u8>();
        let slice_offset = std::mem::offset_of!(Nested, slots) + std::mem::offset_of!(Slots, slice);
        bytes.add(slice_offset + 8).cast::<usize>().write(2);
        assert_eq!(*saved.slice, &SECOND[1..3]);
        let addr = saved.slice.expose_provenance();
        let recovered = std::ptr::with_exposed_provenance_mut::<&'static [u8]>(addr);
        replace(&mut *recovered, &FIRST[2..]);
        assert_eq!(*saved.slice, &FIRST[2..]);
        assert_eq!(bytes.add(slice_offset + 8).cast::<usize>().read(), 2);
        // Wrapping offsets must retain the pointee stride.
        let back = saved.fixed.wrapping_add(3).wrapping_sub(3);
        assert_eq!(*back, &FIRST);
        (*root).slots = first;
        assert_eq!(*saved.text, "héllo");
        assert_eq!((*saved.slice)[0], 3);
        assert_eq!((*root).prefix, 37);
    }
}

#[inline(never)]
fn parameter_storage(mut first: Slots, second: Slots) {
    #[inline(never)]
    fn scalar(mut value: &'static u32, replacement: &'static u32) -> u32 {
        let slot = &raw mut value;
        let same = slot;
        unsafe {
            slot.write(replacement);
        }
        assert_eq!(value, replacement);
        unsafe { **same }
    }
    #[inline(never)]
    fn views(mut slice: &'static [u8], mut fixed: &'static [u8; 4], second: Slots) {
        let slice_slot = &raw mut slice;
        let fixed_slot = &raw mut fixed;
        let change = || unsafe {
            slice_slot.write(second.slice);
            fixed_slot.write(second.fixed);
        };
        black_box(change)();
        assert_eq!(slice, second.slice);
        assert_eq!(fixed, second.fixed);
        let hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            replace(&mut slice, &second.slice[1..]);
            panic!("after stored borrow write");
        }));
        std::panic::set_hook(hook);
        assert!(result.is_err());
        assert_eq!(slice, &second.slice[1..]);
    }
    static ANOTHER: u32 = 43;
    assert_eq!(scalar(first.word, &ANOTHER), 43);
    views(first.slice, first.fixed, second);
    replace(&mut first, second);
    assert_eq!(first.slice, second.slice);
}
