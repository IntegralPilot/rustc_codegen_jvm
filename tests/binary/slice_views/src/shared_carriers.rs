use std::{any::TypeId, hint::black_box};

struct First<'a>(&'a mut u32);
struct Second<'a>(&'a mut u32);
impl Drop for First<'_> {
    fn drop(&mut self) {
        *self.0 += 3;
    }
}
impl Drop for Second<'_> {
    fn drop(&mut self) {
        *self.0 += 7;
    }
}
impl PartialEq for First<'_> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
impl PartialEq for Second<'_> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

#[inline(never)]
fn consume(first: First<'_>, second: Second<'_>) {
    assert!(first != first);
    assert!(second == second);
}

pub fn run() {
    layout_identity();
    owned_fields();
    assert_ne!(
        TypeId::of::<First<'static>>(),
        TypeId::of::<Second<'static>>()
    );
    assert_ne!(
        std::any::type_name::<First<'static>>(),
        std::any::type_name::<Second<'static>>()
    );
    let (mut a, mut b) = (11, 13);
    consume(black_box(First(&mut a)), black_box(Second(&mut b)));
    assert_eq!((a, b), (14, 20));
    let x = black_box([1u32, 2, 3]);
    let y = black_box([5u64, 7, 11, 13]);
    // Equal view carrier shapes do not erase element size or borrow bounds.
    struct View<'a, T>(&'a [T]);
    let x = black_box(View(&x[1..]));
    let y = black_box(View(&y[1..3]));
    assert_eq!(x.0, &[2, 3]);
    assert_eq!(y.0, &[7, 11]);
}

#[repr(C)]
struct Aligned {
    first: u8,
    second: u32,
}
#[repr(C, packed)]
struct Packed {
    first: u8,
    second: u32,
}
#[repr(align(4))]
struct Bytes([u8; 8]);
#[repr(C)]
struct Narrow {
    pointer: *const u8,
}
#[repr(C)]
struct Wide {
    pointer: *const u64,
}

#[repr(C)]
struct NestedAligned {
    prefix: u32,
    value: Aligned,
    suffix: u16,
}
#[repr(C)]
struct NestedPacked {
    prefix: u32,
    value: Packed,
    suffix: u16,
}

fn layout_identity() {
    #[repr(align(8))]
    struct NestedBytes([u8; 16]);
    let mut nested = black_box(NestedBytes([
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
    ]));
    unsafe {
        let address = nested.0.as_mut_ptr();
        let a = address.cast::<NestedAligned>().read();
        let b = address.cast::<NestedPacked>().read();
        let packed_value = b.value.second;
        assert_eq!(a.value.second, u32::from_ne_bytes([9, 10, 11, 12]));
        assert_eq!(packed_value, u32::from_ne_bytes([6, 7, 8, 9]));
        assert_eq!(a.suffix, u16::from_ne_bytes([13, 14]));
        assert_eq!(b.suffix, u16::from_ne_bytes([11, 12]));
        (&raw mut (*address.cast::<NestedPacked>()).value.second).write_unaligned(0);
        assert_eq!(
            nested.0,
            [1, 2, 3, 4, 5, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15, 16]
        );
        // Earlier reads remain independent Rust value snapshots.
        assert_eq!(a.value.second, u32::from_ne_bytes([9, 10, 11, 12]));
        assert_eq!(packed_value, u32::from_ne_bytes([6, 7, 8, 9]));
    }
    let mut bytes = black_box(Bytes([1, 2, 3, 4, 5, 6, 7, 8]));
    let address = bytes.0.as_mut_ptr();
    unsafe {
        let aligned = std::ptr::read(address.cast::<Aligned>());
        let packed = std::ptr::read_unaligned(address.cast::<Packed>());
        assert_eq!(aligned.second, u32::from_ne_bytes([5, 6, 7, 8]));
        let packed_second = packed.second;
        assert_eq!(packed_second, u32::from_ne_bytes([2, 3, 4, 5]));
        std::ptr::write_unaligned(
            &raw mut (*address.cast::<Packed>()).second,
            u32::from_ne_bytes([11, 13, 17, 19]),
        );
        assert_eq!(bytes.0, [1, 11, 13, 17, 19, 6, 7, 8]);
    }
    let words = black_box([0x1234_5678_90ab_cdefu64, 0x1122_3344_5566_7788]);
    let narrow = black_box(Narrow {
        pointer: words.as_ptr().cast(),
    });
    unsafe {
        let wide = std::ptr::read((&narrow as *const Narrow).cast::<Wide>());
        assert_eq!(*wide.pointer.add(1), words[1]);
    }
}

#[derive(Clone)]
struct OwnedFirst {
    numbers: Vec<u32>,
    bytes: [u8; 4],
}
#[derive(Clone)]
struct OwnedSecond {
    numbers: Vec<u32>,
    bytes: [u8; 4],
}

fn owned_fields() {
    assert_ne!(TypeId::of::<OwnedFirst>(), TypeId::of::<OwnedSecond>());
    let first = black_box(OwnedFirst {
        numbers: vec![3, 5],
        bytes: [7, 11, 13, 17],
    });
    let second = black_box(OwnedSecond {
        numbers: vec![19, 23],
        bytes: [29, 31, 37, 41],
    });
    let mut a = black_box(&first).clone();
    let mut b = black_box(&second).clone();
    a.numbers[0] = 43;
    a.bytes[0] = 47;
    b.numbers.push(53);
    b.bytes[3] = 59;
    assert_eq!(first.numbers, [3, 5]);
    assert_eq!(first.bytes, [7, 11, 13, 17]);
    assert_eq!(second.numbers, [19, 23]);
    assert_eq!(second.bytes, [29, 31, 37, 41]);
    assert_eq!(a.numbers, [43, 5]);
    assert_eq!(a.bytes, [47, 11, 13, 17]);
    assert_eq!(b.numbers, [19, 23, 53]);
    assert_eq!(b.bytes, [29, 31, 37, 59]);
}
