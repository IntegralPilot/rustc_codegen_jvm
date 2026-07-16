#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

/// A Java class can implement this generated interface and Rust will dispatch
/// through it exactly as it would through any other `dyn Accumulator`.
pub trait Accumulator {
    fn add(&mut self, value: i32) -> i32;
    fn current(&self) -> i32;
}

pub fn exercise_accumulator(accumulator: &mut dyn Accumulator) -> i32 {
    let first = accumulator.add(4);
    let second = accumulator.add(-1);
    first * 100 + second * 10 + accumulator.current()
}

pub fn combine_accumulators(
    left: &mut dyn Accumulator,
    right: &mut dyn Accumulator,
) -> i32 {
    left.add(3) + right.add(5) + left.current() + right.current()
}

/// Associated-type bindings receive their own readable JVM interface so that
/// Java implementors retain the precise Rust ABI instead of using Object.
pub trait Projector {
    type Output;

    fn project(&mut self, value: i32) -> Self::Output;
}

pub fn exercise_i64_projector(projector: &mut dyn Projector<Output = i64>) -> i64 {
    projector.project(7) + projector.project(-2)
}

struct RustProjector {
    bias: i64,
}

impl Projector for RustProjector {
    type Output = i64;

    fn project(&mut self, value: i32) -> i64 {
        self.bias += 1;
        (value as i64) * 10 + self.bias
    }
}

pub fn rust_projector_smoke() -> i64 {
    let mut projector = RustProjector { bias: 2 };
    exercise_i64_projector(&mut projector)
}
