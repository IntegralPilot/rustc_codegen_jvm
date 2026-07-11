fn consume_unit(_: (), value: i32) -> i32 {
    value
}

fn produce_unit() {}

fn round_trip_unit(value: ()) -> () {
    value
}

fn tuple_with_unit(value: (i32, (), i32)) -> i32 {
    value.0 + value.2
}

fn main() {
    let local = ();
    let moved = local;
    assert!(consume_unit(moved, 42) == 42);

    produce_unit();
    assert!(round_trip_unit(()) == ());

    let tuple = (19, (), 23);
    assert!(tuple_with_unit(tuple) == 42);

    let captured = ();
    let closure = move |_: ()| {
        let _used = captured;
        42
    };
    assert!(closure(()) == 42);
}
