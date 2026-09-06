#[derive(Clone, Copy)]
struct Pair { byte: u8, wide: i64 }
#[derive(Clone, Copy)]
struct Nested { empty: (), pair: Pair, tail: (u16, f64) }
#[derive(Clone, Copy)]
struct Empty;
#[derive(Clone, Copy)]
struct Generic<T> { tag: u8, value: T }

impl Pair {
    #[inline(never)]
    fn new(byte: u8, wide: i64) -> Self { Self { byte, wide } }
    #[inline(never)]
    fn sum(self) -> i64 { self.wide.wrapping_add(self.byte as i64) }
    #[inline(never)]
    fn increment(mut self, amount: i64) -> Self {
        self.wide = self.wide.wrapping_add(amount);
        self.byte = self.byte.wrapping_add(1);
        self
    }
}

#[inline(never)]
fn make_nested(byte: u8, wide: i64, float: f64) -> Nested {
    Nested { empty: (), pair: Pair::new(byte, wide), tail: (65530, float) }
}
#[inline(never)]
fn unpack(n: Nested) -> (u8, i64, u16, f64) { (n.pair.byte, n.pair.wide, n.tail.0, n.tail.1) }
#[inline(never)]
fn identity<T>(value: T) -> T { value }
#[inline(never)]
fn generic_pair(tag: u8, value: Pair) -> Generic<Pair> { identity(Generic { tag, value }) }
#[inline(never)]
fn nested_copy(mut value: Nested) -> Nested {
    let old = value;
    value.pair = value.pair.increment(9);
    value.tail.0 = old.pair.byte as u16;
    value.tail.1 = -old.tail.1;
    value
}
#[inline(never)]
fn rotate(mut a: Nested, mut b: Nested, mut count: u32) -> (Nested, Nested) {
    while count != 0 {
        let old = a;
        a = b;
        b = old;
        count -= 1;
    }
    (a, b)
}
#[inline(never)]
fn tuple_projection_swap(mut pair: ((i64, u8), (i64, u8))) -> ((i64, u8), (i64, u8)) {
    pair = (pair.1, pair.0);
    pair
}
#[inline(never)]
fn local_fields(byte: u8, wide: i64) -> i64 {
    let mut pair = Pair { byte, wide };
    let old = pair;
    pair.byte = old.byte.wrapping_add(7);
    pair.wide = old.wide.wrapping_add(pair.byte as i64);
    pair.wide
}
#[inline(never)]
fn empty_return(_: Empty, value: i64) -> (Empty, (), i64) { (Empty, (), value) }
#[inline(never)]
fn scalar_reference_in_struct(value: &u64, tag: u8) -> Generic<&u64> { Generic { tag, value } }
#[inline(never)]
fn aggregate_panic(value: Pair, divisor: i64) -> Pair {
    Pair { byte: value.byte, wide: value.wide / divisor }
}
#[inline(never)]
fn borrowed_copy(value: &Nested) -> Nested { *value }
#[inline(never)]
fn borrow_field(value: &mut Nested) -> &mut i64 { &mut value.pair.wide }
#[inline(never)]
fn store_field(value: &mut i64, next: i64) { *value = next; }
#[inline(never)]
fn mutate_borrowed(value: &mut Nested, byte: u8, wide: i64) -> i64 {
    value.pair.byte = byte;
    store_field(borrow_field(value), wide);
    value.tail.0 = byte as u16;
    value.pair.wide.wrapping_add(value.pair.byte as i64)
}
#[inline(never)]
fn replace_borrowed(value: &mut Nested, next: Nested) -> Nested {
    let old = *value;
    *value = next;
    old
}
#[inline(never)]
fn construct_borrowed(value: &mut Pair, byte: u8, wide: i64) { *value = Pair { byte, wide }; }
#[inline(never)]
unsafe fn borrowed_aliases(write: *mut Nested, read: *const Nested, next: u8) -> u8 {
    unsafe { (*write).pair.byte = next; (*read).pair.byte }
}
#[inline(never)]
fn borrowed_reference<'a>(value: &mut Generic<&'a u64>, next: &'a u64) -> &'a u64 {
    let old = value.value;
    value.value = next;
    old
}
#[inline(never)]
fn opaque_nested(value: Nested) -> Nested { std::hint::black_box(value) }

#[inline(never)]
fn read_reference_pair(pair: (&u8, &u8)) -> u8 { (*pair.0).wrapping_add(*pair.1) }
#[inline(never)]
fn swap_reference_pair<'a>(pair: (&'a u8, &'a u8)) -> (&'a u8, &'a u8) { (pair.1, pair.0) }
// These small functions exercise addressable locals directly; keeping them
// separate from the assertion driver also keeps them within SSA admission.
#[inline(never)]
fn local_pointer_cell(first: &i64, second: &i64) -> i64 {
    let mut pointer = first;
    let cell = &mut pointer;
    *cell = second;
    **cell + *pointer
}
#[inline(never)]
fn local_aggregate_cell(byte: u8, wide: i64) -> (Nested, Nested, i64) {
    let mut value = make_nested(byte, wide, 3.5);
    let whole = &raw mut value;
    let leaf = unsafe { &raw mut (*whole).pair.wide };
    let copy = value;
    unsafe { (*whole).pair = Pair::new(201, 700); }
    let observed = unsafe { *leaf };
    unsafe { *whole = make_nested(231, 900, -7.5); *leaf += 1; }
    (copy, value, observed)
}
#[inline(never)]
fn fieldwise_cell(byte: u8, wide: i64) -> Nested {
    let mut value = make_nested(0, 0, 0.0);
    value.empty = ();
    value.pair.byte = byte;
    value.pair.wide = wide;
    value.tail.0 = 43210;
    value.tail.1 = 4.5;
    mutate_borrowed(&mut value, byte.wrapping_add(1), wide.wrapping_add(1));
    value
}
#[inline(never)]
fn borrowed_copy_independence(value: &mut Nested) -> Nested {
    let copy = borrowed_copy(value);
    value.pair.byte = 99;
    value.tail.0 = 123;
    copy
}
#[inline(never)]
fn empty_cell() -> Empty { let mut value = Empty; let cell = &mut value; *cell = Empty; *cell }
#[inline(never)]
fn advance_non_null(pointer: std::ptr::NonNull<u8>, amount: usize) -> std::ptr::NonNull<u8> {
    unsafe { std::ptr::NonNull::new_unchecked(pointer.as_ptr().add(amount)) }
}
#[inline(never)]
fn non_null_cell(first: std::ptr::NonNull<u64>, next: std::ptr::NonNull<u64>) -> u64 {
    let mut value = first;
    let cell = &mut value;
    let old = *cell;
    *cell = next;
    unsafe { *old.as_ptr() + *value.as_ptr() }
}
#[inline(never)]
fn borrowed_non_null_cell(value: &mut std::ptr::NonNull<u64>, next: std::ptr::NonNull<u64>) -> std::ptr::NonNull<u64> {
    let old = *value;
    *value = next;
    old
}
#[inline(never)]
fn non_null_aggregate(value: Generic<std::ptr::NonNull<u8>>) -> Generic<std::ptr::NonNull<u8>> {
    Generic { tag: value.tag, value: advance_non_null(value.value, 1) }
}
#[derive(Clone, Copy)]
struct Link { value: i64, next: *mut Link }
struct OpaqueTarget { bytes: [u8; 17] }
#[inline(never)]
fn raw_identity<T>(pointer: *mut T) -> *mut T { pointer }
#[inline(never)]
fn reborrow_opaque<T>(value: &mut T) -> &mut T { &mut *value }
#[inline(never)]
fn non_null_identity<T>(pointer: std::ptr::NonNull<T>) -> std::ptr::NonNull<T> { pointer }
#[inline(never)]
fn follow_link(value: &Link) -> i64 { unsafe { (*value.next).value } }
#[inline(never)]
fn replace_link(mut local: Link, next: Link) -> (i64, Link) {
    let base = &raw mut local;
    let field = unsafe { &raw mut (*base).value };
    local = next;
    unsafe { *field += 3; }
    (unsafe { *field }, local)
}
#[inline(never)]
fn reborrow_link(value: &mut Link, next: i64) { reborrow_opaque(value).value = next; }
fn main() {
    let mut opaque = OpaqueTarget { bytes: [19; 17] };
    let pointer = raw_identity(&raw mut opaque);
    let pointer = non_null_identity(std::ptr::NonNull::new(pointer).unwrap());
    unsafe { pointer.as_ptr().as_mut().unwrap().bytes[3] = 73; }
    assert_eq!(reborrow_opaque(&mut opaque).bytes[3], 73);
    let mut array = [[31_u8; 17]; 3];
    let array_pointer = raw_identity(&raw mut array);
    assert_eq!(unsafe { (*array_pointer)[2][16] }, 31);
    let mut tail = Link { value: 413, next: std::ptr::null_mut() };
    let mut head = Link { value: 97, next: &raw mut tail };
    assert_eq!(follow_link(&head), 413);
    reborrow_link(&mut head, 127);
    assert_eq!(head.value, 127);
    let (observed, replaced) = replace_link(head, tail);
    assert_eq!((observed, replaced.value), (416, 416));
    assert_eq!(tail.value, 413);
    let mut cells = [11_u8, 73, 201, 249];
    let start = std::ptr::NonNull::new(cells.as_mut_ptr()).unwrap();
    assert_eq!(unsafe { *advance_non_null(start, 2).as_ptr() }, 201);
    let wrapped = non_null_aggregate(Generic { tag: 197, value: advance_non_null(start, 1) });
    assert_eq!(wrapped.tag, 197);
    assert_eq!(unsafe { *wrapped.value.as_ptr() }, 201);
    let (mut first, mut second) = (13_u64, 97_u64);
    let a = std::ptr::NonNull::from(&mut first);
    let b = std::ptr::NonNull::from(&mut second);
    assert_eq!(non_null_cell(a, b), 110);
    let mut cell = a;
    let old = borrowed_non_null_cell(&mut cell, b);
    assert_eq!(unsafe { *old.as_ptr() }, 13);
    assert_eq!(unsafe { *cell.as_ptr() }, 97);
    assert_eq!(local_pointer_cell(&13, &97), 194);
    let (copy, value, observed) = local_aggregate_cell(17, 23);
    assert_eq!(unpack(copy), (17, 23, 65530, 3.5));
    assert_eq!(unpack(value), (231, 901, 65530, -7.5));
    assert_eq!(observed, 700);
    assert_eq!(unpack(fieldwise_cell(251, 9876)), (252, 9877, 252, 4.5));
    let mut source = make_nested(219, 45, -1.5);
    let independent = borrowed_copy_independence(&mut source);
    assert_eq!(unpack(independent), (219, 45, 65530, -1.5));
    assert_eq!(source.pair.byte, 99);
    let _ = empty_cell();
    let nested_empty = std::mem::ManuallyDrop::new((Empty, std::mem::ManuallyDrop::new(())));
    let _ = std::mem::ManuallyDrop::into_inner(nested_empty);
    for byte in [0, 1, 127, 128, 254, 255] {
        for wide in [0, -1, i64::MIN, i64::MAX, 1 << 40] {
            let original = make_nested(byte, wide, -3.5);
            assert_eq!(unpack(original), (byte, wide, 65530, -3.5));
            assert_eq!(original.pair.sum(), wide.wrapping_add(byte as i64));
            let changed = nested_copy(original);
            assert_eq!(unpack(changed), (byte.wrapping_add(1), wide.wrapping_add(9), byte as u16, 3.5));
            assert_eq!(unpack(original), (byte, wide, 65530, -3.5));
            let wrapped = generic_pair(byte, original.pair);
            assert_eq!(wrapped.tag, byte);
            assert_eq!(wrapped.value.wide, wide);
            assert_eq!(wrapped.value.byte, byte);
            for count in 0..7 {
                let (a, b) = rotate(original, changed, count);
                assert_eq!(unpack(a), unpack(if count % 2 == 0 { original } else { changed }));
                assert_eq!(unpack(b), unpack(if count % 2 == 0 { changed } else { original }));
            }
            assert_eq!(tuple_projection_swap(((wide, byte), (7, 201))), ((7, 201), (wide, byte)));
            assert_eq!(local_fields(byte, wide), wide.wrapping_add(byte.wrapping_add(7) as i64));
        }
    }
    assert_eq!(empty_return(Empty, -42).2, -42);
    let value = u64::MAX;
    let referenced = scalar_reference_in_struct(&value, 255);
    assert_eq!(*referenced.value, value);
    assert_eq!(referenced.tag, 255);
    let mut nested = make_nested(128, -5, 3.5);
    assert_eq!(unpack(borrowed_copy(&nested)), (128, -5, 65530, 3.5));
    assert_eq!(mutate_borrowed(&mut nested, 255, i64::MAX), i64::MAX.wrapping_add(255));
    assert_eq!(unpack(nested), (255, i64::MAX, 255, 3.5));
    let old = replace_borrowed(&mut nested, make_nested(201, 1 << 40, -9.5));
    assert_eq!(unpack(old), (255, i64::MAX, 255, 3.5));
    assert_eq!(unpack(nested), (201, 1 << 40, 65530, -9.5));
    let write = &raw mut nested;
    assert_eq!(unsafe { borrowed_aliases(write, write, 253) }, 253);
    assert_eq!(nested.pair.byte, 253);
    construct_borrowed(&mut nested.pair, 129, i64::MIN);
    assert_eq!(nested.pair.byte, 129);
    assert_eq!(nested.pair.wide, i64::MIN);
    assert_eq!(unpack(opaque_nested(nested)), unpack(nested));
    // A field pointer denotes the same Rust storage after either the field's
    // containing value or the entire outer aggregate is replaced.
    let whole = &raw mut nested;
    let leaf = unsafe { &raw mut (*whole).pair.byte };
    unsafe { construct_borrowed(&mut (*whole).pair, 131, 7); }
    assert_eq!(unsafe { *leaf }, 131);
    unsafe { replace_borrowed(&mut *whole, make_nested(239, -19, 5.5)); }
    assert_eq!(unsafe { *leaf }, 239);
    unsafe { *leaf = 199; }
    assert_eq!(nested.pair.byte, 199);
    // The incoming tuple may hold shared pointer bases with different deferred
    // offsets. Both components must be materialized before scalarized use.
    let bytes = [11, 97, 201];
    assert_eq!(read_reference_pair((&bytes[0], &bytes[2])), 212);
    let swapped = swap_reference_pair((&bytes[0], &bytes[2]));
    assert_eq!((*swapped.0, *swapped.1), (201, 11));
    let other = 17;
    let mut referenced = referenced;
    assert_eq!(*borrowed_reference(&mut referenced, &other), u64::MAX);
    assert_eq!(*referenced.value, 17);
    std::panic::set_hook(Box::new(|_| {}));
    assert!(std::panic::catch_unwind(|| aggregate_panic(Pair::new(255, 7), 0)).is_err());
    println!("SSA aggregate integration passed");
}
