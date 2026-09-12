struct Number {
    value: i32,
}

#[inline(never)]
fn adder<T: Copy + std::ops::Add<Output = T>>(increment: T) -> impl Fn(T) -> T {
    move |value| value + increment
}

fn main() {
    // The opaque return type and its defining MIR must name the same carrier.
    let add_int = adder(7i32);
    let add_long = adder(10_000_000_000i64);
    assert_eq!(add_int(35), 42);
    assert_eq!(add_long(42), 10_000_000_042);
    let erased: Box<dyn Fn(i32) -> i32> = Box::new(add_int);
    assert_eq!(erased(1), 8);

    let mut calls = 0;
    let mut closure = move |left: i32, right: i16| {
        calls += 1;
        left + right as i32 + calls
    };

    assert!(closure(39, 2) == 42);
    assert!(closure(37, 3) == 42);

    let mut object_calls = 0;
    let mut object_closure = move |left: Number, right: Number| {
        object_calls += 1;
        left.value + right.value + object_calls
    };
    assert!(object_closure(Number { value: 19 }, Number { value: 22 }) == 42);
}
