use std::{hint::black_box, marker::PhantomData};

// Shared storage must preserve projections and codecs for borrowed fields, arrays, and skipped ZST fields.
#[repr(C)]
#[derive(Clone, Copy)]
struct Position {
    x: f64,
    y: f64,
}
#[repr(C)]
#[derive(Clone, Copy)]
struct Extent {
    width: f64,
    height: f64,
}
#[derive(Clone, Copy)]
struct Borrowed<'a> {
    marker: PhantomData<u8>,
    name: &'a str,
    positions: &'a [Position],
    scratch: [u8; 3],
}

#[inline(never)]
fn move_x(position: &mut Position, delta: f64) {
    position.x += delta;
}
#[inline(never)]
fn grow(extent: &mut Extent, delta: f64) {
    extent.height += delta;
}

#[inline(never)]
fn translate(mut position: Position) -> Position {
    position.x += 17.0;
    position
}

#[inline(never)]
fn scale(mut extent: Extent) -> Extent {
    extent.height *= 3.0;
    extent
}

pub fn run() {
    let mut position = black_box(Position { x: 3.5, y: 7.25 });
    let mut extent = black_box(Extent {
        width: 11.0,
        height: 2.5,
    });
    move_x(&mut position, 1.5);
    grow(&mut extent, 4.5);
    assert_eq!((position.x, position.y), (5.0, 7.25));
    assert_eq!((extent.width, extent.height), (11.0, 7.0));
    // Shared JVM call interfaces must preserve distinct Rust function identities.
    let translate = black_box(translate as fn(Position) -> Position);
    let scale = black_box(scale as fn(Extent) -> Extent);
    let shifted = translate(position);
    let enlarged = scale(extent);
    assert_eq!((shifted.x, shifted.y), (22.0, 7.25));
    assert_eq!((enlarged.width, enlarged.height), (11.0, 21.0));
    assert_ne!(black_box(translate as usize), black_box(scale as usize));
    let reinterpreted: Extent = unsafe { std::mem::transmute(black_box(position)) };
    assert_eq!((reinterpreted.width, reinterpreted.height), (5.0, 7.25));
    let rows = [position, Position { x: 12.0, y: 13.0 }];
    let borrowed = black_box(Borrowed {
        marker: PhantomData,
        name: "positional",
        positions: &rows,
        scratch: [4, 5, 6],
    });
    let mut copy = black_box(borrowed);
    copy.scratch[1] = 19;
    assert_eq!(borrowed.scratch, [4, 5, 6]);
    assert_eq!(copy.name, "positional");
    assert_eq!(copy.positions[1].y, 13.0);
    assert_eq!(copy.scratch, [4, 19, 6]);
}
