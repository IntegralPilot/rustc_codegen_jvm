#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

use provider::{Holder, provider_scaled, provider_score, scaled_sum};

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
}
