use std::vec;
use std::vec::Vec;
use provider::{
    DynValue, GenericMethodOwner, Holder, ProviderConstructed, ProviderCounter,
    invoke_callback_through_a_deliberately_long_generic_wrapper_that_exercises_hashed_closure_names_and_forces_the_fallback_path,
    invoke_result_closure, metadata, provider_result_identity, provider_scaled, provider_score,
    pull_i32, pull_i64, scaled_sum, use_private_token,
};

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

    // Exercise core::hint::black_box through both an upstream instantiation
    // and a downstream-only closure instantiation of the generic wrapper.
    assert!(provider_result_identity().is_ok());
    assert!(invoke_result_closure(|| Ok(())).is_ok());
    assert!(use_private_token("downstream") == 42);

    // The provider's inner closure is instantiated with two different
    // downstream closure types whose readable class names exceed the limit.
    let mut first = 0;
    invoke_callback_through_a_deliberately_long_generic_wrapper_that_exercises_hashed_closure_names_and_forces_the_fallback_path(
        || first = 11,
    );
    assert!(first == 11);
    let mut second = 0;
    invoke_callback_through_a_deliberately_long_generic_wrapper_that_exercises_hashed_closure_names_and_forces_the_fallback_path(
        || second = 22,
    );
    assert!(second == 22);

    // The provider owns these trait-object function ABIs while the concrete
    // implementors and adapters live downstream. Both crates must derive the
    // same specialized JVM interface name for each associated-type binding.
    let mut local_i32 = LocalI32(7);
    assert!(pull_i32(&mut local_i32) == 7);
    assert!(pull_i32(&mut local_i32) == 8);
    let mut local_i64 = LocalI64(100);
    assert!(pull_i64(&mut local_i64) == 100);
    assert!(pull_i64(&mut local_i64) == 102);

    // The blanket `IntoIterator for I where I: Iterator` method is not an
    // instance method on an upstream concrete iterator class. A downstream
    // `collect` must call the resolved monomorphized impl statically.
    let collected = ProviderCounter::new(4, 8).collect::<Vec<_>>();
    assert!(collected == vec![4, 5, 6, 7]);

    // A generic method on a non-generic upstream type is instantiated only
    // by the downstream crate. Its body must still be attached to that type.
    let owner = GenericMethodOwner;
    assert!(owner.identity(123_i64) == 123);

    // The provider never uses this tuple constructor as a value. Its
    // zero-sized function-item carrier must therefore be emitted downstream.
    let constructed = Some(42_u32).map(ProviderConstructed).unwrap();
    assert!(constructed.0 == 42);

    // An external function named `metadata` is still an ordinary Rust call.
    assert!(matches!(metadata(&42), Err(42)));
}
