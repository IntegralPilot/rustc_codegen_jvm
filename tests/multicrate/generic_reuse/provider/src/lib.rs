#![no_std]

// Provider crate for the generic_reuse regression test. The downstream binary
// instantiates these generics with types the provider never used, and re-uses
// instantiations the provider only partially exercised.

use core::ops::{Add, Mul};

pub trait DynValue {
    type Item;

    fn next(&mut self) -> Self::Item;
}

pub fn pull_i32(value: &mut dyn DynValue<Item = i32>) -> i32 {
    value.next()
}

pub fn pull_i64(value: &mut dyn DynValue<Item = i64>) -> i64 {
    value.next()
}

pub struct Holder<T> {
    value: T,
}

impl<T: Copy> Holder<T> {
    pub fn new(value: T) -> Holder<T> {
        Holder { value }
    }

    pub fn get(&self) -> T {
        self.value
    }

    pub fn replace(&mut self, value: T) -> T {
        let old = self.value;
        self.value = value;
        old
    }
}

// The provider instantiates Holder<i32> but only ever calls `new` and `get`.
// The binary also calls `replace` on Holder<i32>, so both crates emit a
// Holder_i32 class with complementary method sets — the java-linker must
// merge them instead of dropping one copy.
pub fn provider_score() -> i32 {
    let holder = Holder::new(41);
    holder.get() + 1
}

// A generic function containing a capturing closure. The closure's JVM class
// is named from the monomorphized instance, so whichever crate instantiates
// scaled_sum must define the closure class itself; previously the class was
// only defined for local DefIds, producing NoClassDefFoundError at runtime.
pub fn scaled_sum<T: Copy + Add<Output = T> + Mul<Output = T>>(a: T, b: T, k: T) -> T {
    let scale = move |v: T| v * k;
    scale(a) + scale(b)
}

// The provider also instantiates scaled_sum::<i32>, so the same closure
// instance exists in both crates and the linker must deduplicate it.
pub fn provider_scaled() -> i32 {
    scaled_sum(2, 3, 4)
}

pub struct ProviderCounter {
    next: i32,
    end: i32,
}

impl ProviderCounter {
    pub fn new(next: i32, end: i32) -> Self {
        Self { next, end }
    }
}

impl Iterator for ProviderCounter {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next == self.end {
            None
        } else {
            let value = self.next;
            self.next += 1;
            Some(value)
        }
    }
}

pub struct GenericMethodOwner;

impl GenericMethodOwner {
    pub fn identity<T: Copy>(&self, value: T) -> T {
        value
    }
}
