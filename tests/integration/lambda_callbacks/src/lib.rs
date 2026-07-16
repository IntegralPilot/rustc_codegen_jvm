#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

pub fn apply_i32(callback: &dyn Fn(i32) -> i32, value: i32) -> i32 {
    callback(value)
}

pub fn combine_i32(
    callback: &dyn Fn(i32, i32) -> i32,
    left: i32,
    right: i32,
) -> i32 {
    callback(left, right)
}

pub fn apply_twice(callback: &dyn Fn(i32) -> i32, value: i32) -> i32 {
    callback(callback(value))
}

pub fn apply_mut_i32(callback: &mut dyn FnMut(i32) -> i32, value: i32) -> i32 {
    callback(value)
}

pub fn choose_i32(
    predicate: &dyn Fn(i32) -> bool,
    value: i32,
    when_true: i32,
    when_false: i32,
) -> i32 {
    if predicate(value) {
        when_true
    } else {
        when_false
    }
}

pub fn supply_i32(callback: &dyn Fn() -> i32) -> i32 {
    callback()
}

pub fn apply_f64(callback: &dyn Fn(f64) -> f64, value: f64) -> f64 {
    callback(value)
}

pub fn call_unit(callback: &dyn Fn(()) -> ()) {
    callback(())
}

pub fn rust_closure_test(value: i32) -> i32 {
    let offset = 2;
    apply_i32(&|input| input * 4 + offset, value)
}

pub fn rust_fn_mut_test(value: i32) -> i32 {
    let mut running_total = 10;
    let mut callback = |input| {
        running_total += input;
        running_total
    };
    let first = apply_mut_i32(&mut callback, value);
    let second = apply_mut_i32(&mut callback, value);
    first + second
}

fn add_three(value: i32) -> i32 {
    value + 3
}

pub fn rust_fn_pointer_dyn_test(value: i32) -> i32 {
    let callback: fn(i32) -> i32 = add_three;
    apply_i32(&callback, value)
}

pub fn rust_function_pointer() -> fn(i32) -> i32 {
    add_three
}

pub fn rust_non_capturing_closure_pointer() -> fn(i32) -> i32 {
    |value| value * 2
}
