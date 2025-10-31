fn identity<T>(x: T) -> T {
    x
}

fn main() {
    let a = identity(42);
    let b = identity("hello");

    assert!(a == 42);
    assert!(b.len() == 5);
}