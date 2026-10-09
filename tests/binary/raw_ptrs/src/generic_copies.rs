use core::ptr;

#[inline(never)]
unsafe fn copy<T>(source: *const T, destination: *mut T, count: usize) {
    unsafe { ptr::copy_nonoverlapping(source, destination, count) }
}

#[inline(never)]
unsafe fn fill<T>(destination: *mut T, byte: u8, count: usize) {
    unsafe { ptr::write_bytes(destination, byte, count) }
}

trait Element {
    type Value: Copy;
}

impl Element for u32 {
    type Value = [u64; 3];
}

#[inline(never)]
unsafe fn copy_associated<T: Element>(source: *const T::Value, destination: *mut T::Value) {
    unsafe { ptr::copy_nonoverlapping(source, destination, 1) }
}

#[repr(align(64))]
struct AlignedZst;

pub fn check() {
    let source = [10_u32, 20, 30, 40];
    let mut destination = [0_u32; 6];
    unsafe {
        copy(source.as_ptr().add(1), destination.as_mut_ptr().add(2), 2);
        copy(source.as_ptr(), destination.as_mut_ptr(), 0);
    }
    assert_eq!(destination, [0, 0, 20, 30, 0, 0]);

    let source = [[1_u64, 2, 3], [4, 5, 6]];
    let mut destination = [[0_u64; 3]; 2];
    unsafe {
        copy(source.as_ptr(), destination.as_mut_ptr(), 2);
        copy_associated::<u32>(source.as_ptr().add(1), destination.as_mut_ptr());
    }
    assert_eq!(destination, [[4, 5, 6], [4, 5, 6]]);

    let first = 7;
    let second = 11;
    let source = [&first, &second];
    let mut destination = [&first; 2];
    unsafe { copy(source.as_ptr(), destination.as_mut_ptr(), 2) }
    assert!(ptr::eq(destination[0], &first));
    assert!(ptr::eq(destination[1], &second));

    let mut words = [0_u32; 8];
    unsafe { fill(words.as_mut_ptr().add(2), 0x5a, 4) }
    assert_eq!(
        words,
        [
            0,
            0,
            0x5a5a_5a5a,
            0x5a5a_5a5a,
            0x5a5a_5a5a,
            0x5a5a_5a5a,
            0,
            0
        ]
    );
    let mut arrays = [[0_u16; 2]; 3];
    unsafe { fill(arrays.as_mut_ptr().add(1), 0xff, 1) }
    assert_eq!(arrays, [[0, 0], [u16::MAX; 2], [0, 0]]);

    // ZST copies and fills require no storage, even at the maximum element count.
    unsafe {
        copy::<AlignedZst>(ptr::dangling(), ptr::dangling_mut(), usize::MAX);
        copy::<u64>(ptr::dangling(), ptr::dangling_mut(), 0);
        fill::<AlignedZst>(ptr::dangling_mut(), 0xff, usize::MAX);
        fill::<u64>(ptr::dangling_mut(), 0xff, 0);
    }
}
