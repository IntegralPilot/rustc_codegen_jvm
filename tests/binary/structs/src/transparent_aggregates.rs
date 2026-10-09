use std::marker::PhantomData;

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(C)]
struct Coord {
    x: f64,
    y: f64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(transparent)]
struct Point(Coord);

#[derive(Clone, Copy, Debug, PartialEq)]
struct Rows([Point; 3], PhantomData<u8>);

#[derive(Clone, Copy, Debug, PartialEq)]
struct Pair((Point, Point));

#[inline(never)]
fn update(point: &mut Point, x: f64) -> f64 {
    let old = point.0.x;
    point.0.x = x;
    point.0.y += 1.0;
    old
}

#[inline(never)]
fn row_slice(rows: &mut Rows) -> &mut [Point] {
    &mut rows.0
}

#[inline(never)]
fn read(point: &Point) -> f64 {
    point.0.x + point.0.y
}

// Keep a named wrapper to break recursive JVM descriptors.
struct Recursive([*const Recursive; 1]);

struct TupleNode((std::ptr::NonNull<TupleNode>,));
struct NestedNode((u32, (u64, [*const NestedNode; 1])));
struct MutualA((std::ptr::NonNull<MutualB>,));
struct MutualB([*const MutualA; 1]);
struct NullableNode((Option<std::ptr::NonNull<NullableNode>>,));
enum OnePointer {
    Value(*const EnumNode),
}
struct EnumNode((OnePointer,));
struct CallableNode((fn(CallableNode) -> CallableNode,));
enum DirectEnum {
    Value(std::ptr::NonNull<DirectEnum>),
}
enum MutualEnumA {
    Value(std::ptr::NonNull<MutualEnumB>),
}
enum MutualEnumB {
    Value((u8, *const MutualEnumA)),
}

fn callable_identity(node: CallableNode) -> CallableNode {
    node
}

fn recursive_carriers() {
    let node = TupleNode((std::ptr::NonNull::dangling(),));
    assert_eq!(node.0.0, std::ptr::NonNull::dangling());
    let nested = NestedNode((7, (19, [std::ptr::null()])));
    assert_eq!(nested.0.0, 7);
    assert_eq!(nested.0.1.0, 19);
    assert!(nested.0.1.1[0].is_null());
    let a = MutualA((std::ptr::NonNull::dangling(),));
    let b = MutualB([std::ptr::null()]);
    assert_eq!(a.0.0, std::ptr::NonNull::dangling());
    assert!(b.0[0].is_null());
    let none = NullableNode((None,));
    let some = NullableNode((Some(std::ptr::NonNull::dangling()),));
    assert!(none.0.0.is_none());
    assert_eq!(some.0.0.unwrap(), std::ptr::NonNull::dangling());
    let node = EnumNode((OnePointer::Value(std::ptr::null()),));
    let OnePointer::Value(pointer) = node.0.0;
    assert!(pointer.is_null());
    let node = CallableNode((callable_identity,));
    let callback = node.0.0;
    let node = callback(node);
    assert!(std::ptr::fn_addr_eq(node.0.0, callback));
    let DirectEnum::Value(pointer) = DirectEnum::Value(std::ptr::NonNull::dangling());
    assert_eq!(pointer, std::ptr::NonNull::dangling());
    let MutualEnumA::Value(pointer) = MutualEnumA::Value(std::ptr::NonNull::dangling());
    assert_eq!(pointer, std::ptr::NonNull::dangling());
    let MutualEnumB::Value((number, pointer)) = MutualEnumB::Value((23, std::ptr::null()));
    assert_eq!(number, 23);
    assert!(pointer.is_null());
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Pixel([u8; 4]);

#[inline(never)]
fn fill_pixels(pixels: &mut [Pixel], value: Pixel) {
    let mut cursor = pixels.as_mut_ptr();
    let end = unsafe { cursor.add(pixels.len()) };
    while cursor != end {
        assert!(cursor < end);
        unsafe {
            cursor.write(value);
            cursor = cursor.add(1);
        }
    }
    assert!(cursor <= end && cursor >= end);
}

pub fn run() {
    recursive_carriers();
    let point = Point(Coord { x: 2.5, y: 4.0 });
    let mut copy = point;
    assert_eq!(update(&mut copy, 8.0), 2.5);
    assert_eq!(read(&point), 6.5);
    assert_eq!(read(&copy), 13.0);
    assert_eq!(format!("{point:?}"), "Point(Coord { x: 2.5, y: 4.0 })");

    let mut rows = Rows([point, copy, point], PhantomData);
    let saved = rows;
    row_slice(&mut rows)[0].0.y = 16.0;
    assert_eq!(rows.0[0].0.y, 16.0);
    assert_eq!(saved.0[0].0.y, 4.0);
    assert_eq!(rows.0[1], copy);

    let mut pair = Pair((point, copy));
    let original = pair;
    update(&mut pair.0.1, 11.0);
    assert_eq!(read(&original.0.1), 13.0);
    assert_eq!(read(&pair.0.1), 17.0);

    unsafe {
        let coord = &mut copy as *mut Point as *mut Coord;
        (*coord).x = -3.0;
        assert_eq!(read(&copy), 2.0);
        let mut bytes = [0u8; std::mem::size_of::<Point>()];
        std::ptr::write_unaligned(bytes.as_mut_ptr().cast::<Point>(), copy);
        assert_eq!(
            std::ptr::read_unaligned(bytes.as_ptr().cast::<Point>()),
            copy
        );
    }

    let node = Recursive([std::ptr::null()]);
    assert!(node.0[0].is_null());

    let mut pixels = [Pixel([0; 4]); 8];
    fill_pixels(&mut pixels[1..7], Pixel([3, 5, 7, 11]));
    assert_eq!(pixels[0], Pixel([0; 4]));
    assert_eq!(pixels[7], Pixel([0; 4]));
    assert!(
        pixels[1..7]
            .iter()
            .all(|pixel| *pixel == Pixel([3, 5, 7, 11]))
    );
    fill_pixels(&mut pixels[3..3], Pixel([255; 4]));
    assert_eq!(pixels[3], Pixel([3, 5, 7, 11]));
}
