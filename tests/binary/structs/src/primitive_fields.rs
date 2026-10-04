use std::{hint::black_box, ptr};

#[repr(C)]
#[derive(Clone, Copy)]
struct Scalars {
    signed: (i8, i16, i32, i64),
    unsigned: (u8, u16, u32, u64),
    single: f32,
    double: f64,
    flag: bool,
    character: char,
}

#[inline(never)]
fn check(value: &Scalars) {
    assert_eq!(black_box(value.signed.0) as i64, -113);
    assert_eq!(black_box(value.signed.1) as i64, -30001);
    assert_eq!(black_box(value.signed.2) as i64, -2_000_000_001);
    assert_eq!(black_box(value.signed.3), i64::MIN + 7);
    assert_eq!(black_box(value.unsigned.0) as u64, 241);
    assert_eq!(black_box(value.unsigned.1) as u64, 60001);
    assert_eq!(black_box(value.unsigned.2) as u64, 4_000_000_001);
    assert_eq!(black_box(value.unsigned.3), u64::MAX - 7);
    assert_eq!(value.single.to_bits(), 0x7fc01234);
    assert_eq!(value.double.to_bits(), 0xfff8000000005678);
    assert!(value.flag);
    assert_eq!(value.character, '🦀');
}

#[inline(never)]
fn write(value: &mut Scalars) {
    value.signed.0 = -113;
    value.signed.1 = -30001;
    value.signed.2 = -2_000_000_001;
    value.signed.3 = i64::MIN + 7;
    value.unsigned.0 = 241;
    value.unsigned.1 = 60001;
    value.unsigned.2 = 4_000_000_001;
    value.unsigned.3 = u64::MAX - 7;
    value.single = f32::from_bits(0x7fc01234);
    value.double = f64::from_bits(0xfff8000000005678);
    value.flag = true;
    value.character = '🦀';
}

#[repr(C, packed)]
struct Unaligned {
    prefix: u8,
    value: f64,
    suffix: u8,
}

#[inline(never)]
unsafe fn unaligned(value: *mut Unaligned) {
    unsafe {
        ptr::addr_of_mut!((*value).value).write_unaligned(-0.0);
        assert_eq!(
            ptr::addr_of!((*value).value).read_unaligned().to_bits(),
            1 << 63
        );
    }
}

pub fn run() {
    let initial = Scalars {
        signed: (0, 0, 0, 0),
        unsigned: (0, 0, 0, 0),
        single: 0.0,
        double: 0.0,
        flag: false,
        character: 'a',
    };
    // Check local and heap layouts, including displaced elements and nested tuples.
    let mut local = black_box(initial);
    write(black_box(&mut local));
    check(black_box(&local));
    let mut heap = black_box(vec![initial; 3]);
    write(black_box(&mut heap[1]));
    check(black_box(&heap[1]));
    assert_eq!(heap[0].signed.0, 0);
    assert_eq!(heap[2].unsigned.3, 0);
    // Raw field writes and shared reads must continue to see one location.
    let raw = black_box(heap.as_mut_ptr());
    unsafe {
        ptr::addr_of_mut!((*raw.add(1)).signed.0).write(-7);
        assert_eq!((*raw.add(1)).signed.0, -7);
    }
    write(&mut heap[1]);
    check(&heap[1]);
    let mut packed = Box::new(Unaligned {
        prefix: 19,
        value: 1.0,
        suffix: 23,
    });
    unsafe {
        unaligned(black_box(&mut *packed));
    }
    assert_eq!(packed.prefix, 19);
    assert_eq!(packed.suffix, 23);
}
