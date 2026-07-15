#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

struct Number {
    value: i32,
}

fn main() {
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
