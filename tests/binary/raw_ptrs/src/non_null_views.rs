use std::{hint::black_box, mem, ptr::NonNull};

#[inline(never)]
fn slice_pointer<T>(value: &mut [T]) -> NonNull<[T]> {
    NonNull::from(value)
}

#[inline(never)]
unsafe fn slice_from_pointer<'a, T>(mut value: NonNull<[T]>) -> &'a mut [T] {
    unsafe { value.as_mut() }
}

#[inline(never)]
fn option_roundtrip<T: ?Sized>(value: Option<NonNull<T>>) -> Option<NonNull<T>> {
    let mut storage = mem::MaybeUninit::<Option<NonNull<T>>>::uninit();
    unsafe {
        storage.as_mut_ptr().write(black_box(value));
        black_box(storage.as_ptr()).read()
    }
}

#[inline(never)]
unsafe fn transmute_slice(value: *mut [u32]) -> NonNull<[u32]> {
    unsafe { mem::transmute(value) }
}

#[inline(never)]
unsafe fn transmute_raw(value: NonNull<[u32]>) -> *mut [u32] {
    unsafe { mem::transmute(value) }
}

#[inline(never)]
unsafe fn transmute_str(value: NonNull<str>) -> *mut str {
    unsafe { mem::transmute(value) }
}

#[inline(never)]
fn unsize(value: NonNull<[u32; 4]>) -> NonNull<[u32]> {
    value
}

#[inline(never)]
fn initialized_box<T>(first: T, second: T) -> Box<[T]> {
    let mut owned = Box::<[T]>::new_uninit_slice(2);
    owned[0].write(first);
    owned[1].write(second);
    unsafe { owned.assume_init() }
}

#[repr(C)]
struct Views {
    slice: NonNull<[u32]>,
    text: NonNull<str>,
}

union SliceBits {
    pointer: NonNull<[u32]>,
    words: [usize; 2],
}

struct Recursive {
    children: NonNull<[Recursive]>,
}

pub fn check() {
    const DATA: &[u32] = &[17, 29, 41];
    const CONSTANT: NonNull<[u32]> = unsafe { NonNull::new_unchecked(DATA as *const _ as *mut _) };
    assert_eq!(unsafe { black_box(CONSTANT).as_ref() }, DATA);
    let mut values = black_box([11, 22, 33, 44]);
    let raw = &raw mut values;
    let all = unsize(NonNull::new(raw).unwrap());
    assert_eq!(all.len(), 4);
    assert_eq!(all.as_ptr().cast::<u32>(), raw.cast::<u32>());

    let view = slice_pointer(&mut values[1..3]);
    assert_eq!(view.len(), 2);
    assert_eq!(view.as_ptr().cast::<u32>(), unsafe {
        raw.cast::<u32>().add(1)
    });
    let roundtrip = unsafe { transmute_slice(transmute_raw(black_box(view))) };
    assert!(std::ptr::eq(roundtrip.as_ptr(), view.as_ptr()));
    unsafe { slice_from_pointer(roundtrip)[1] = 73 };
    assert_eq!(values, [11, 22, 73, 44]);
    let bits = unsafe { black_box(SliceBits { pointer: view }).words };
    assert_eq!(bits[1], 2);
    let restored = unsafe { black_box(SliceBits { words: bits }).pointer };
    assert!(std::ptr::eq(restored.as_ptr(), view.as_ptr()));

    let some = option_roundtrip(Some(view)).unwrap();
    assert!(std::ptr::eq(some.as_ptr(), view.as_ptr()));
    assert!(option_roundtrip::<[u32]>(None).is_none());
    let empty = NonNull::slice_from_raw_parts(NonNull::<u32>::dangling(), 0);
    assert_eq!(option_roundtrip(Some(empty)).unwrap().len(), 0);
    let zst = NonNull::slice_from_raw_parts(NonNull::<()>::dangling(), 19);
    assert_eq!(option_roundtrip(Some(zst)).unwrap().len(), 19);

    let mut text = String::from("xé水z");
    let text_view = NonNull::from(&mut text[1..6]);
    assert_eq!(unsafe { text_view.as_ref() }, "é水");
    assert_eq!(unsafe { &*transmute_str(text_view) }, "é水");
    assert_eq!(
        unsafe { option_roundtrip(Some(text_view)).unwrap().as_ref() },
        "é水"
    );
    assert!(option_roundtrip::<str>(None).is_none());
    assert_eq!(
        unsafe { option_roundtrip(Some(NonNull::from(""))).unwrap().as_ref() },
        ""
    );
    let mut ascii = String::from("abcd");
    let mut ascii_view = NonNull::from(&mut ascii[1..3]);
    unsafe { ascii_view.as_mut().make_ascii_uppercase() };
    assert_eq!(ascii, "aBCd");
    let owned = initialized_box(19u8, 43);
    assert_eq!(&*owned, &[19, 43]);
    assert_eq!(owned.into_vec(), vec![19, 43]);

    // Raw aggregate copies must preserve slice metadata and mutable aliases.
    let stored = Views {
        slice: view,
        text: text_view,
    };
    let mut copied = mem::MaybeUninit::<Views>::uninit();
    unsafe {
        std::ptr::copy_nonoverlapping(&stored, copied.as_mut_ptr(), 1);
        let mut copied = black_box(copied).assume_init();
        assert_eq!(copied.slice.as_ref(), &[22, 73]);
        copied.slice.as_mut()[0] = 91;
        assert_eq!(copied.text.as_ref(), "é水");
    }
    assert_eq!(values[1], 91);

    let first = 3;
    let second = 7;
    let mut references = [&first, &second];
    let reference_view = slice_pointer(&mut references);
    unsafe { slice_from_pointer(reference_view).swap(0, 1) };
    assert!(std::ptr::eq(references[0], &second));
    assert!(std::ptr::eq(references[1], &first));

    // Recursive wrappers retain a nominal break in their storage type graph.
    let leaf = Recursive {
        children: NonNull::slice_from_raw_parts(NonNull::dangling(), 0),
    };
    let leaves = [leaf];
    let root = Recursive {
        children: NonNull::from(&leaves[..]),
    };
    assert_eq!(unsafe { root.children.as_ref() }[0].children.len(), 0);
}
