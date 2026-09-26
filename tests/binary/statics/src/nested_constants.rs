//! Nested ICU-style tables must not duplicate their contents at each wrapper.
use std::hint::black_box;

#[derive(Clone, Copy)]
struct Wrap<T>(T);
type Four<T> = Wrap<Wrap<Wrap<Wrap<T>>>>;
type Table = Four<Four<Four<Four<[u32; 256]>>>>;

const fn four<T>(value: T) -> Four<T> {
    Wrap(Wrap(Wrap(Wrap(value))))
}

const TABLE: Table = four(four(four(four([0x1234_5678; 256]))));

#[inline(never)]
fn table() -> Table {
    TABLE
}

fn unwrap_four<T>(value: Four<T>) -> T {
    value.0.0.0.0
}

trait Marker {
    type Data: Sync;
}
struct Numbers;
impl Marker for Numbers {
    type Data = Wrap<[u32; 4]>;
}

// CTFE retains the associated-type projection in the static's declared type.
static PROJECTED: <Numbers as Marker>::Data = Wrap([3, 5, 7, 11]);
static PROJECTED_REFS: [&<Numbers as Marker>::Data; 2] = [&PROJECTED, &PROJECTED];

// The backing arrays are rustc-generated nested statics without declared types.
static BRANDS: &[[u8; 4]] = &[*b"mif1", *b"msf1"];
static NESTED_REFS: &[&[u32]] = &[&[3, 5], &[7, 11, 13]];

#[inline(never)]
fn brands() -> &'static [[u8; 4]] {
    BRANDS
}

pub fn run() {
    let unpack = |value| unwrap_four(unwrap_four(unwrap_four(unwrap_four(value))));
    let mut first = unpack(black_box(table()));
    let second = unpack(black_box(table()));
    assert_eq!(first, [0x1234_5678; 256]);
    first[0] = 42;
    assert_eq!(second[0], 0x1234_5678);
    assert_eq!(first[0], 42);
    assert_eq!(PROJECTED.0, [3, 5, 7, 11]);
    assert_eq!(PROJECTED_REFS[1].0, PROJECTED.0);
    assert!(core::ptr::eq(PROJECTED_REFS[0], PROJECTED_REFS[1]));
    assert!(brands().contains(b"msf1"));
    assert!(!brands().contains(b"nope"));
    assert!(core::ptr::eq(black_box(brands()).as_ptr(), BRANDS.as_ptr()));
    assert_eq!(NESTED_REFS[0], [3, 5]);
    assert_eq!(NESTED_REFS[1], [7, 11, 13]);
}
