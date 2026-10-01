use std::hint::black_box;

#[inline(never)]
fn array<const N: usize>(values: &[u8]) -> Option<&[u8; N]> {
    values.try_into().ok()
}

#[inline(never)]
unsafe fn words<'a>(data: *mut [u16; 2]) -> &'a mut [u16; 2] {
    unsafe { &mut *data }
}

pub fn run() {
    use std::io::Read;

    // Temporary arrays must remain independent when pointer cells are removed.
    let mut input = black_box(&[11u8, 29, 47, 83][..]);
    let mut collected = [0u8; 4];
    for output in &mut collected {
        let mut one = [0u8; 1];
        input.read_exact(&mut one).unwrap();
        *output = one[0];
    }
    assert_eq!(collected, [11, 29, 47, 83]);
    let snapshot = black_box(collected);
    collected[0] = 101;
    assert_eq!(snapshot, [11, 29, 47, 83]);
    let mut snapshots = Vec::new();
    for i in 0..3 {
        let first = black_box(snapshot);
        let mut second = first;
        second[0] += i;
        snapshots.push(black_box(second));
        assert_eq!(first, [11, 29, 47, 83]);
    }
    snapshots[0][1] = 107;
    assert_eq!(snapshots[0], [11, 107, 47, 83]);
    assert_eq!(snapshots[1], [12, 29, 47, 83]);
    assert_eq!(snapshots[2], [13, 29, 47, 83]);
    assert!(input.read_exact(&mut [0u8; 1]).is_err());

    let mut bytes = black_box([11u8, 23, 37, 41, 53, 67]);
    let call: fn(&[u8]) -> Option<&[u8; 4]> = black_box(array::<4>);
    let middle = call(&bytes[1..5]).unwrap();
    assert_eq!(*middle, [23, 37, 41, 53]);
    assert_eq!(middle.as_ptr(), unsafe { bytes.as_ptr().add(1) });
    assert!(call(&bytes[..3]).is_none());
    assert_eq!(array::<0>(&bytes[3..3]).unwrap().as_ptr(), unsafe {
        bytes.as_ptr().add(3)
    });
    let pointer = bytes.as_mut_ptr();
    unsafe {
        let middle = &mut *pointer.add(2).cast::<[u8; 3]>();
        middle[1] = 79;
    }
    assert_eq!(bytes, [11, 23, 37, 79, 53, 67]);

    let mut wide = black_box([0x1122u16, 0x3344, 0x5566, 0x7788]);
    unsafe {
        let middle = words(wide.as_mut_ptr().add(1).cast::<[u16; 2]>());
        assert_eq!(*middle, [0x3344, 0x5566]);
        middle[0] = 0x99aa;
        let raw: *mut [u16] = middle as *mut [u16; 2];
        (&mut *raw)[1] = 0xbbcc;
    }
    assert_eq!(wide, [0x1122, 0x99aa, 0xbbcc, 0x7788]);

    // A byte projection of wider backing cannot become a detached byte array.
    unsafe {
        let byte = wide.as_mut_ptr().cast::<u8>().add(1);
        let view = &mut *byte.cast::<[u8; 3]>();
        assert_eq!(*view, [0x11, 0xaa, 0x99]);
        view[2] = 0xdd;
    }
    assert_eq!(wide[1], 0xddaa);

    struct Owner {
        before: u64,
        values: [f64; 2],
        after: u8,
    }
    let mut owner = black_box(Owner {
        before: 101,
        values: [f64::from_bits(0x7ff8000000000123), -0.0],
        after: 103,
    });
    let field = black_box(&mut owner.values);
    assert_eq!(field[0].to_bits(), 0x7ff8000000000123);
    assert_eq!(field[1].to_bits(), (-0.0f64).to_bits());
    field[1] = 107.0;
    assert_eq!(owner.values[1], 107.0);
    assert_eq!((owner.before, owner.after), (101, 103));

    // A raw slice can have null data and arbitrary metadata without being dereferenced.
    let null_array = black_box(std::ptr::null::<[u8; 7]>());
    let null_slice: *const [u8] = null_array;
    assert!(null_slice.is_null());
    assert_eq!(null_slice.len(), 7);
    let zst = [(); 9];
    let raw: *const [()] = black_box(&zst as *const [(); 9]);
    assert_eq!(unsafe { (&*raw).len() }, 9);
}
