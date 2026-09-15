#[derive(Clone, Copy)]
struct Pair {
    left: i64,
    right: i64,
}
#[derive(Clone, Copy)]
struct View<'a> {
    tag: u8,
    values: &'a [i64],
}

#[inline(never)]
fn length<T>(value: &[T]) -> usize {
    value.len()
}
#[inline(never)]
fn identity<T>(value: &[T]) -> &[T] {
    value
}
#[inline(never)]
fn sum(value: &[i64]) -> i64 {
    let mut result: i64 = 0;
    let mut index = 0;
    while index < value.len() {
        result = result.wrapping_add(value[index]);
        index += 1;
    }
    result
}
#[inline(never)]
fn update(value: &mut [i64], index: usize, next: i64) -> i64 {
    let old = value[index];
    value[index] = next;
    old
}
#[inline(never)]
fn indexed_pair(value: &[Pair], index: usize) -> i64 {
    value[index].left.wrapping_add(value[index].right)
}
#[inline(never)]
fn data<T>(value: &[T]) -> *const T {
    value as *const [T] as *const T
}
#[inline(never)]
fn raw_parts<T>(value: *const T, length: usize) -> *const [T] {
    std::ptr::slice_from_raw_parts(value, length)
}
#[inline(never)]
fn raw_length<T>(value: *const [T]) -> usize {
    value.len()
}
#[inline(never)]
fn string_length(value: &str) -> usize {
    value.len()
}
#[inline(never)]
fn string_identity(value: &str) -> &str {
    value
}
#[inline(never)]
fn replace<'a>(value: &mut &'a [i64], next: &'a [i64]) -> usize {
    let old = *value;
    *value = next;
    old.len()
}
#[inline(never)]
fn wrapper(value: View<'_>) -> i64 {
    value.tag as i64 + sum(value.values)
}
#[inline(never)]
fn first_and_last(value: &[i64]) -> i64 {
    let [first, .., last] = value else {
        return -1;
    };
    first.wrapping_add(*last)
}
#[inline(never)]
fn literal() -> &'static str {
    "A\0é🦀"
}

fn main() {
    let mut array = [11, 22, 33, 44, 55, 66];
    assert_eq!(sum(identity(&array[1..5])), 154);
    assert_eq!(length(&array[2..2]), 0);
    assert_eq!(update(&mut array[1..5], 2, 101), 44);
    assert_eq!(array[3], 101);
    assert_eq!(first_and_last(&array[1..5]), 77);
    assert_eq!(first_and_last(&[]), -1);
    let slice = &array[2..5];
    let pointer = data(slice);
    assert_eq!(unsafe { *pointer }, 33);
    assert_eq!(unsafe { *pointer.add(1) }, 101);
    let raw = raw_parts(pointer, 3);
    assert_eq!(raw_length(raw), 3);
    assert_eq!(sum(unsafe { &*raw }), 189);
    let mut current = &array[..2];
    assert_eq!(replace(&mut current, slice), 2);
    assert_eq!(sum(current), 189);
    assert_eq!(
        wrapper(View {
            tag: 7,
            values: current
        }),
        196
    );
    let pairs = [
        Pair {
            left: -8,
            right: 19,
        },
        Pair {
            left: 42,
            right: 100,
        },
    ];
    assert_eq!(indexed_pair(&pairs[1..], 0), 142);
    // A pointer-only slice can retain an opaque element shape.
    let nested = [[1u8; 17]; 3];
    assert_eq!(length(identity(&nested[..])), 3);
    let huge = (1usize << 40) + 23;
    let dangling = std::ptr::NonNull::<()>::dangling().as_ptr();
    let raw_zst = raw_parts(dangling, huge);
    assert_eq!(raw_length(raw_zst), huge);
    assert_eq!(length(unsafe { &*raw_zst }), huge);
    assert_eq!(string_length(string_identity(literal())), 8);
    assert_eq!(
        string_identity(literal()).as_bytes(),
        &[65, 0, 195, 169, 240, 159, 166, 128]
    );
    println!("SSA views passed");
}
