#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

fn main() {
    let value = 42;
    panic!("This is a formatted panic message: {}", value);
}
