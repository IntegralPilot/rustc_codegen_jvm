#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

mod nested;

fn main() {
    nested::outer();
}
