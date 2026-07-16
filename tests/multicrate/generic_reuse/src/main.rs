#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

use provider::{DynValue, Holder, provider_scaled, provider_score, pull_i32, pull_i64, scaled_sum};

struct LocalI32(i32);

impl DynValue for LocalI32 {
    type Item = i32;

    fn next(&mut self) -> Self::Item {
        let value = self.0;
        self.0 += 1;
        value
    }
}

struct LocalI64(i64);

impl DynValue for LocalI64 {
    type Item = i64;

    fn next(&mut self) -> Self::Item {
        let value = self.0;
        self.0 += 2;
        value
    }
}

fn main() {
    // Instantiation the provider exercised itself.
    assert!(provider_score() == 42);

    // Same instantiation (Holder<i32>), but calling a method the provider
    // never instantiated — forces the complementary-method-set merge.
    let mut shared = Holder::new(1);
    assert!(shared.replace(5) == 1);
    assert!(shared.get() == 5);

    // Fresh instantiations the provider never saw.
    let mut long_holder = Holder::new(10_i64);
    assert!(long_holder.get() == 10);
    assert!(long_holder.replace(-3) == 10);
    assert!(long_holder.get() == -3);

    let float_holder = Holder::new(2.5_f64);
    assert!(float_holder.get() == 2.5);

    // Generic-with-closure instantiated upstream (dedup against provider's
    // copy) and downstream with a brand-new type (local closure class).
    assert!(provider_scaled() == 20);
    assert!(scaled_sum(2, 3, 4) == 20);
    assert!(scaled_sum(1.5_f64, 2.5_f64, 2.0_f64) == 8.0);
    assert!(scaled_sum(100_i64, 200_i64, 3_i64) == 900);

    // The provider owns these trait-object function ABIs while the concrete
    // implementors and adapters live downstream. Both crates must derive the
    // same specialized JVM interface name for each associated-type binding.
    let mut local_i32 = LocalI32(7);
    assert!(pull_i32(&mut local_i32) == 7);
    assert!(pull_i32(&mut local_i32) == 8);
    let mut local_i64 = LocalI64(100);
    assert!(pull_i64(&mut local_i64) == 100);
    assert!(pull_i64(&mut local_i64) == 102);
}
