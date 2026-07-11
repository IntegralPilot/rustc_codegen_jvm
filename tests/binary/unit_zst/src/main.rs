use core::convert::Infallible;

#[allow(dead_code)]
enum OnlyLive {
    Live,
    Impossible(Infallible),
}

#[allow(dead_code)]
enum ValueOrNever {
    Value(i32),
    Impossible(Infallible),
}

struct ResidualHolder {
    value: Option<Infallible>,
}

struct Marker;

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

#[allow(dead_code)]
fn propagate_unit(value: Option<()>) -> Option<i32> {
    value?;
    Some(42)
}

fn only_live() -> OnlyLive {
    OnlyLive::Live
}

fn payload_or_never(value: i32) -> ValueOrNever {
    ValueOrNever::Value(value)
}

fn residual_holder() -> ResidualHolder {
    ResidualHolder { value: None }
}

fn residual_tuple() -> (Option<Infallible>, Option<Infallible>) {
    (None, None)
}

fn residual_array() -> [Option<Infallible>; 2] {
    [None, None]
}

fn marker() -> Marker {
    Marker
}

fn consume_marker(_: Marker) -> i32 {
    42
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

    let residual: Option<Infallible> = None;
    assert!(residual.is_none());
    assert!(residual_holder().value.is_none());
    let tuple = residual_tuple();
    assert!(tuple.0.is_none());
    assert!(tuple.1.is_none());
    let array = residual_array();
    assert!(array[0].is_none());
    assert!(array[1].is_none());
    assert!(consume_marker(marker()) == 42);
    let captured_marker = marker();
    let marker_closure = move || consume_marker(captured_marker);
    assert!(marker_closure() == 42);

    assert!(matches!(only_live(), OnlyLive::Live));
    match payload_or_never(42) {
        ValueOrNever::Value(value) => assert!(value == 42),
        ValueOrNever::Impossible(never) => match never {},
    }
}
