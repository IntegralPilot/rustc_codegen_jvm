use std::hint::black_box;

trait Property {
    const FOLD: Option<fn(i32, i32) -> i32>;
}

struct Sum;
struct Plain;

impl Property for Sum {
    const FOLD: Option<fn(i32, i32) -> i32> = Some(add);
}

impl Property for Plain {
    const FOLD: Option<fn(i32, i32) -> i32> = None;
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn fold<P: Property>(a: i32, b: i32) -> i32 {
    match black_box(P::FOLD) {
        Some(f) => f(a, b),
        None => a,
    }
}

pub fn run() {
    assert_eq!(fold::<Sum>(black_box(19), 23), 42);
    assert_eq!(fold::<Plain>(black_box(19), 23), 19);

    const CLOSURE: Option<fn(i32) -> i32> = Some(|x| x + 1);
    assert_eq!(black_box(CLOSURE).unwrap()(41), 42);

    // Scalar ABI aggregates retain their nominal type, even when their only
    // non-zero-sized field is represented by a pointer during const evaluation.
    const TUPLE: (Option<fn(i32, i32) -> i32>, ()) = (Some(add), ());
    assert_eq!(black_box(TUPLE).0.unwrap()(19, 23), 42);

    struct Wrapped(Option<fn(i32, i32) -> i32>);
    const WRAPPED: Wrapped = Wrapped(Some(add));
    assert_eq!(black_box(WRAPPED).0.unwrap()(19, 23), 42);

    const REFERENCE: Option<&i32> = Some(&42);
    assert_eq!(*black_box(REFERENCE).unwrap(), 42);
}
