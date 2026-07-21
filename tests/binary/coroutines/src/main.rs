#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use core::ops::{Coroutine, CoroutineState};
use core::pin::Pin;

fn resume<G, R>(coroutine: Pin<&mut G>, argument: R) -> CoroutineState<G::Yield, G::Return>
where
    G: Coroutine<R>,
{
    coroutine.resume(argument)
}

fn main() {
    // The simplest state machine: two suspension points followed by a return.
    let mut basic = #[coroutine]
    || {
        yield 10_i32;
        yield 20_i32;
        30_i32
    };
    let mut basic = Pin::new(&mut basic);
    assert!(matches!(
        resume(basic.as_mut(), ()),
        CoroutineState::Yielded(10)
    ));
    assert!(matches!(
        resume(basic.as_mut(), ()),
        CoroutineState::Yielded(20)
    ));
    assert!(matches!(
        resume(basic.as_mut(), ()),
        CoroutineState::Complete(30)
    ));

    // Captures, resume arguments, mutable locals, and aggregates which remain
    // live across suspension points all become fields in the coroutine object.
    let captured = 7_i32;
    let mut stateful = #[coroutine]
    move |mut input: i32| {
        let mut total = captured + input;
        input = yield total;
        total += input;
        let saved = [total, total + 1, total + 2];
        input = yield saved[1];
        saved[2] + input
    };
    let mut stateful = Pin::new(&mut stateful);
    assert!(matches!(
        resume(stateful.as_mut(), 5),
        CoroutineState::Yielded(12)
    ));
    assert!(matches!(
        resume(stateful.as_mut(), 8),
        CoroutineState::Yielded(21)
    ));
    assert!(matches!(
        resume(stateful.as_mut(), 4),
        CoroutineState::Complete(26)
    ));

    // Different suspension variants may keep different locals alive. Test
    // both paths so discriminants and variant-specific field projections are
    // checked independently.
    let mut positive = #[coroutine]
    |choose_positive: bool| {
        if choose_positive {
            let kept = 41_i32;
            yield kept;
            kept + 1
        } else {
            let kept = -10_i32;
            yield kept;
            kept - 1
        }
    };
    let mut positive = Pin::new(&mut positive);
    assert!(matches!(
        resume(positive.as_mut(), true),
        CoroutineState::Yielded(41)
    ));
    assert!(matches!(
        resume(positive.as_mut(), false),
        CoroutineState::Complete(42)
    ));

    let mut negative = #[coroutine]
    |choose_positive: bool| {
        if choose_positive {
            let kept = 41_i32;
            yield kept;
            kept + 1
        } else {
            let kept = -10_i32;
            yield kept;
            kept - 1
        }
    };
    let mut negative = Pin::new(&mut negative);
    assert!(matches!(
        resume(negative.as_mut(), false),
        CoroutineState::Yielded(-10)
    ));
    assert!(matches!(
        resume(negative.as_mut(), true),
        CoroutineState::Complete(-11)
    ));

    // Non-move capture checks the reference-shaped capture path.
    let base = 100_i32;
    let mut borrowed = #[coroutine]
    || {
        yield base + 1;
        base + 2
    };
    let mut borrowed = Pin::new(&mut borrowed);
    assert!(matches!(
        resume(borrowed.as_mut(), ()),
        CoroutineState::Yielded(101)
    ));
    assert!(matches!(
        resume(borrowed.as_mut(), ()),
        CoroutineState::Complete(102)
    ));

    // Unit values and zero-sized captures have no JVM payload, but must still
    // participate correctly in the Rust coroutine state machine.
    struct Marker;
    let marker = Marker;
    let mut zero_sized = #[coroutine]
    move || {
        let _ = &marker;
        yield ();
    };
    let mut zero_sized = Pin::new(&mut zero_sized);
    assert!(matches!(
        resume(zero_sized.as_mut(), ()),
        CoroutineState::Yielded(())
    ));
    assert!(matches!(
        resume(zero_sized.as_mut(), ()),
        CoroutineState::Complete(())
    ));
}
