#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

fn main() {
    let mut sum = 0;
    for x in [1, 2, 3] {
        sum += x;
    }
    assert!(sum == 6);
}
