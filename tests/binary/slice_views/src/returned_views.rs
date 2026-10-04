use std::hint::black_box;

#[inline(never)]
fn pick<T>(values: &[T], index: usize) -> &T {
    &values[index]
}
#[inline(never)]
fn range<T>(values: &[T], start: usize, end: usize) -> &[T] {
    &values[start..end]
}
#[inline(never)]
fn recurse<T>(values: &[T], index: usize) -> &T {
    if index == 0 {
        pick(values, 0)
    } else {
        recurse(&values[1..], index - 1)
    }
}
#[inline(never)]
fn text(value: &str) -> &str {
    &value[2..]
}
#[inline(never)]
fn maybe_panic<T>(values: &[T], fail: bool) -> &[T] {
    if fail {
        panic!("borrowed return unwind")
    }
    &values[1..]
}

pub fn run() {
    returned_aggregates();
    returned_vecs();
    nested_vecs();
    let words = black_box([3u32, 5, 8, 13, 21]);
    let first = pick(&words, 1);
    let second = recurse(&words, 4);
    // Both locations remain live across later calls using the same scratch.
    assert_eq!((*first, *second), (5, 21));
    let left = range(&words, 0, 2);
    let right = range(&words, 3, 5);
    assert_eq!(left, &[3, 5]);
    assert_eq!(right, &[13, 21]);
    let indirect: fn(&[u32], usize) -> &u32 = black_box(pick);
    assert_eq!(*indirect(&words, 2), 8);
    let slice: fn(&[u32], usize, usize) -> &[u32] = black_box(range);
    assert_eq!(slice(&words, 2, 4), &[8, 13]);
    let closure: fn(&[u32]) -> &[u32] = black_box(|values| &values[1..3]);
    assert_eq!(closure(&words), &[5, 8]);
    let borrow_text: fn(&str) -> &str = black_box(text);
    assert_eq!(borrow_text(black_box("étoile")), "toile");
    let keep = maybe_panic(&words, false);
    let failure = std::panic::catch_unwind(|| maybe_panic(&words, black_box(true)));
    assert!(failure.is_err());
    assert_eq!(keep, &[5, 8, 13, 21]);
    assert_eq!(maybe_panic(&words, false), keep);
    let mut mutable = black_box([1, 2, 3, 4]);
    let mut iter = mutable.iter_mut();
    let a = iter.next().unwrap();
    let b = iter.next().unwrap();
    *a = 17;
    *b = 19;
    assert_eq!(mutable, [17, 19, 3, 4]);
    let items = black_box([String::from("alpha"), String::from("beta")]);
    assert_eq!(pick(&items, 0), "alpha");
    assert_eq!(recurse(&items, 1), "beta");
    let huge = unsafe {
        std::slice::from_raw_parts(
            std::ptr::NonNull::<()>::dangling().as_ptr(),
            (1usize << 40) + 23,
        )
    };
    assert_eq!(range(huge, 11, huge.len()).len(), (1usize << 40) + 12);
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(C)]
struct Position {
    advance: i32,
    x: i32,
    y: i32,
}
#[inline(never)]
fn at_mut<T>(values: &mut [T], index: usize) -> &mut T {
    &mut values[index]
}
#[inline(never)]
fn adjust(value: &mut Position) {
    value.advance += 3;
    value.x -= 5;
}

fn returned_aggregates() {
    let mut positions = black_box(vec![
        Position {
            advance: 11,
            x: 13,
            y: 17
        };
        5
    ]);
    let adjuster: fn(&mut [Position], usize) -> &mut Position = black_box(at_mut);
    adjust(adjuster(&mut positions, 2));
    assert_eq!(
        positions[2],
        Position {
            advance: 14,
            x: 8,
            y: 17
        }
    );
    assert_eq!(
        positions[1],
        Position {
            advance: 11,
            x: 13,
            y: 17
        }
    );
    let mut iter = positions.iter_mut();
    let left = iter.next().unwrap();
    let right = iter.next().unwrap();
    left.advance = 41;
    right.advance = 43;
    left.y += 19;
    assert_eq!(
        positions[0],
        Position {
            advance: 41,
            x: 13,
            y: 36
        }
    );
    assert_eq!(
        positions[1],
        Position {
            advance: 43,
            x: 13,
            y: 17
        }
    );
}

fn returned_vecs() {
    let mut vectors: [Vec<i32>; 2] = [Vec::new(), Vec::new()];
    for index in 0..2 {
        for element in 0..5 {
            at_mut(&mut vectors, black_box(index)).push(10 * index as i32 + element);
        }
    }
    assert_eq!(vectors[0], [0, 1, 2, 3, 4]);
    assert_eq!(vectors[1], [10, 11, 12, 13, 14]);
}

struct Plan {
    counts: [usize; 2],
    stages: [Vec<(usize, Option<fn()>)>; 2],
}
impl Plan {
    #[inline(never)]
    fn add(&mut self, index: usize, callback: Option<fn()>) {
        at_mut(&mut self.stages, index).push((self.counts[index], callback));
        self.counts[index] += 1;
    }
}
fn nested_vecs() {
    let mut plan = black_box(Plan {
        counts: [0; 2],
        stages: [Vec::new(), Vec::new()],
    });
    plan.add(0, Some(|| {}));
    plan.add(0, None);
    plan.add(1, None);
    assert_eq!(plan.counts, [2, 1]);
    assert_eq!(plan.stages[0].len(), 2);
    assert_eq!(plan.stages[1].len(), 1);
    assert_eq!(plan.stages[1][0].0, 0);
}
