#[repr(C)]
#[derive(Clone, Copy)]
struct Bytes {
    first: [u8; 4],
    second: [u8; 4],
}

#[inline(never)]
unsafe fn copy(pointer: *const Bytes) -> Bytes {
    unsafe { *pointer }
}

#[inline(never)]
unsafe fn read(pointer: *const Bytes) -> Bytes {
    unsafe { pointer.read_unaligned() }
}

#[inline(never)]
fn observe(value: &Bytes) -> u8 {
    value.first[0]
}

#[inline(never)]
fn replace(value: &mut Bytes) {
    let previous = observe(&*value);
    *value = Bytes {
        first: [previous + 1; 4],
        second: [previous + 2; 4],
    };
    assert_eq!(observe(&*value), previous + 1);
}

pub fn check() {
    let mut storage = [1_u8, 2, 3, 4, 5, 6, 7, 8];
    let pointer = std::hint::black_box(storage.as_mut_ptr().cast::<Bytes>());
    let mut copied = unsafe { copy(pointer) };
    let read_copy = unsafe { read(pointer) };
    storage[1] = 91;
    copied.first[2] = 92;
    assert_eq!(copied.first, [1, 2, 92, 4]);
    assert_eq!(read_copy.first, [1, 2, 3, 4]);
    assert_eq!(storage, [1, 91, 3, 4, 5, 6, 7, 8]);
    let after = unsafe { copy(pointer) };
    assert_eq!(after.first, [1, 91, 3, 4]);
    assert_eq!(after.second, [5, 6, 7, 8]);

    let offset_storage = [99_u8, 4, 5, 6, 7, 8, 9, 10, 11, 98];
    let offset_copy = unsafe { read(offset_storage.as_ptr().add(1).cast::<Bytes>()) };
    assert_eq!(offset_copy.first, [4, 5, 6, 7]);
    assert_eq!(offset_copy.second, [8, 9, 10, 11]);

    // A copied carrier must also detach when the source is an ordinary JVM
    // object rather than reinterpreted byte storage.
    let mut direct = copied;
    let detached = unsafe { copy(&raw const direct) };
    direct.second[0] = 93;
    assert_eq!(detached.second, [5, 6, 7, 8]);
    assert_eq!(direct.second, [93, 6, 7, 8]);

    // Reborrowing a pointer does not create independent storage that needs a
    // second store after replacement, for either bytes or an object carrier.
    replace(unsafe { &mut *pointer });
    assert_eq!(storage, [2, 2, 2, 2, 3, 3, 3, 3]);
    replace(&mut direct);
    assert_eq!(direct.first, [2; 4]);
    assert_eq!(direct.second, [3; 4]);
}
