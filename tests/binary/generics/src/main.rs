// Generic identity function
fn identity<T>(x: T) -> T {
    x
}

// Generic function with two parameters
fn swap<T, U>(a: T, b: U) -> (U, T) {
    (b, a)
}

// Generic struct
#[derive(Clone, Copy)]
struct Pair<T, U> {
    first: T,
    second: U,
}

impl<T, U> Pair<T, U> {
    fn new(first: T, second: U) -> Self {
        Pair { first, second }
    }

    fn flip(self) -> Pair<U, T> {
        Pair {
            first: self.second,
            second: self.first,
        }
    }
}

fn main() {
    let a = identity(42);
    let b = identity("hello");

    assert!(a == 42);
    assert!(b.len() == 5);

    let swapped = swap(1, "two");
    assert!(swapped.0 == "two");
    assert!(swapped.1 == 1);

    let pair = Pair::new(10, 20);
    let flipped = pair.flip();
    assert!(flipped.first == 20);
    assert!(flipped.second == 10);

    let nested_pair = Pair::new(pair, swapped);
    assert!(nested_pair.first.first == 10);
    assert!(nested_pair.second.0 == "two");
}
