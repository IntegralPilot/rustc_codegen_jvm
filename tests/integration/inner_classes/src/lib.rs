#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

pub enum TrafficLight {
    Red,
    Yellow,
    Green,
}

pub enum MaybeNumber {
    None,
    Some(i32),
    Pair(i32, bool),
}

pub struct NumberBox {
    pub value: i32,
}

pub enum NestedEquality {
    Maybe(MaybeNumber),
    Boxed(NumberBox),
}

pub fn get_light_action(light: TrafficLight) -> &'static str {
    match light {
        TrafficLight::Red => "Stop",
        TrafficLight::Yellow => "Caution",
        TrafficLight::Green => "Go",
    }
}

pub fn make_none() -> MaybeNumber {
    MaybeNumber::None
}

pub fn make_some(value: i32) -> MaybeNumber {
    MaybeNumber::Some(value)
}

pub fn make_pair(value: i32, flag: bool) -> MaybeNumber {
    MaybeNumber::Pair(value, flag)
}

pub fn make_number_box(value: i32) -> NumberBox {
    NumberBox { value }
}

pub fn wrap_maybe(value: MaybeNumber) -> NestedEquality {
    NestedEquality::Maybe(value)
}

pub fn wrap_boxed(value: NumberBox) -> NestedEquality {
    NestedEquality::Boxed(value)
}
