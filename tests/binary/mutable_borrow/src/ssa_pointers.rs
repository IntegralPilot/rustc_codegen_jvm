//! Field promotion must preserve aliases and cell promotion must preserve joins.
use std::hint::black_box;

#[repr(C)]
struct Record {
    count: u64,
    signed: i32,
    fraction: f64,
}

#[inline(never)]
unsafe fn update(record: *mut Record, alias: *mut u64) {
    unsafe {
        (*record).count += 7;
        assert_eq!(*alias, 18);
        *alias = 29;
        assert_eq!((*record).count, 29);
        (*record).signed -= 5;
        (*record).fraction *= 1.5;
    }
}

#[inline(never)]
unsafe fn choose_and_advance(left: *mut u64, right: *mut u64, count: usize) -> u64 {
    let mut selected = left;
    for step in 0..count {
        selected = if black_box(step & 1) == 0 {
            left
        } else {
            right
        };
        unsafe {
            *selected += step as u64;
        }
    }
    unsafe { *selected }
}

#[inline(never)]
fn escaped_cell(left: &mut u64, right: &mut u64) {
    let mut selected = left as *mut u64;
    let alias = black_box(&mut selected as *mut *mut u64);
    unsafe {
        *alias = right;
        *selected += 10;
    }
}

#[repr(C)]
struct Nested {
    record: Record,
}

#[repr(C)]
struct RecordView {
    count: u64,
    signed: i32,
    fraction: f64,
}

#[inline(never)]
fn replaced_nested_view() {
    let mut nested = Nested {
        record: Record {
            count: 11,
            signed: -9,
            fraction: 2.0,
        },
    };
    let root = black_box(&mut nested as *mut Nested);
    let field = unsafe { core::ptr::addr_of_mut!((*root).record) };
    let view = black_box(field.cast::<RecordView>());
    unsafe {
        assert_eq!((*view).count, 11);
        *root = Nested {
            record: Record {
                count: 37,
                signed: -4,
                fraction: 6.0,
            },
        };
        assert_eq!((*view).count, 37);
        assert_eq!((*view).signed, -4);
        (*view).count = 51;
        assert_eq!((*root).record.count, 51);
        (*root).record.fraction = 7.5;
        assert_eq!((*view).fraction, 7.5);
    }
}

#[repr(u8)]
enum Tag {
    Seven = 7,
    Nine = 9,
}

#[repr(C)]
struct PartiallyInitialized {
    tag: Tag,
    value: u64,
}

#[repr(C)]
struct BooleanNeighbor {
    flag: bool,
    value: u64,
}

#[inline(never)]
unsafe fn initialize_number(pointer: *mut PartiallyInitialized) {
    unsafe {
        (*pointer).value = 42;
    }
}

fn partially_initialized_fields() {
    let mut storage = core::mem::MaybeUninit::<PartiallyInitialized>::zeroed();
    let pointer = black_box(storage.as_mut_ptr());
    // Tag's zero discriminant is invalid, but accessing the initialized number
    // is valid and must not decode the adjacent uninitialized enum.
    unsafe {
        initialize_number(pointer);
        assert_eq!((*pointer).value, 42);
    }
    let _ = black_box((Tag::Seven, Tag::Nine));

    #[repr(align(8))]
    struct Bytes([u8; 16]);
    let mut bytes = Bytes([0xff; 16]);
    let pointer = black_box(bytes.0.as_mut_ptr().cast::<BooleanNeighbor>());
    unsafe {
        (*pointer).value = 42;
        assert_eq!((*pointer).value, 42);
    }
    // A field store must preserve both padding and the raw non-Boolean bits
    // in its neighbor. The complete BooleanNeighbor is never read as a value.
    assert_eq!(&bytes.0[..8], &[0xff; 8]);
}

pub fn run() {
    partially_initialized_fields();
    replaced_nested_view();
    // A generated object and a decoded aggregate in Vec storage exercise both
    // direct and byte-backed owners of the same typed field operations.
    let mut local = Record {
        count: 11,
        signed: -9,
        fraction: 2.0,
    };
    let mut allocated = vec![Record {
        count: 11,
        signed: -9,
        fraction: 2.0,
    }];
    for record in [&mut local, &mut allocated[0]] {
        let base = black_box(record as *mut Record);
        let field = unsafe { core::ptr::addr_of_mut!((*base).count) };
        unsafe {
            update(base, field);
        }
        assert_eq!(record.count, 29);
        assert_eq!(record.signed, -14);
        assert_eq!(record.fraction, 3.0);
        unsafe {
            field.cast::<u8>().write(0x5a);
            assert_eq!((*base).count, 0x5a);
            (*base).count = 0x1234;
            assert_eq!(field.cast::<u8>().read(), 0x34);
        }
    }
    let mut left = 1;
    let mut right = 2;
    assert_eq!(unsafe { choose_and_advance(&mut left, &mut right, 6) }, 11);
    assert_eq!((left, right), (7, 11));
    escaped_cell(&mut left, &mut right);
    assert_eq!((left, right), (7, 21));
    let mut values = [3u64, 5, 7, 11];
    for value in values.iter_mut() {
        *value = value.wrapping_mul(3);
    }
    assert_eq!(values, [9, 15, 21, 33]);
}
