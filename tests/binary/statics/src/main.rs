#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

struct Settings {
    base: i32,
    enabled: bool,
}

fn add_one(value: i32) -> i32 {
    value + 1
}

static ANSWER: i32 = 42;
static ANSWER_REF: &i32 = &ANSWER;
static SETTINGS: Settings = Settings {
    base: 40,
    enabled: true,
};
static VALUES: [i32; 4] = [3, 5, 8, 13];
static MESSAGE: &str = "static value";
static OPERATION: fn(i32) -> i32 = add_one;

mod nested {
    pub static OFFSET: i32 = 2;
}

fn main() {
    assert!(ANSWER == 42);
    assert!(*ANSWER_REF == ANSWER);
    assert!(SETTINGS.enabled);
    assert!(SETTINGS.base + nested::OFFSET == ANSWER);
    assert!(VALUES[0] + VALUES[1] + VALUES[2] + VALUES[3] == 29);
    assert!(MESSAGE == "static value");
    assert!(OPERATION(41) == ANSWER);
}
