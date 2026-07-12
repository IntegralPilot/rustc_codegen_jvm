// Regression test: trait methods dispatched on zero-sized receivers.
//
// A unit receiver has no runtime representation, so there is no object to
// invokevirtual on. ZST receivers must route through compiled static dispatch instead.

trait Score {
    fn score(&self, base: i32) -> i32;
    fn tag(&self) -> i32;
}

impl Score for () {
    fn score(&self, base: i32) -> i32 {
        base + 1
    }

    fn tag(&self) -> i32 {
        7
    }
}

// Zero-sized struct receiver: same class of bug as `()`.
#[derive(Clone, Copy)]
struct Marker;

impl Score for Marker {
    fn score(&self, base: i32) -> i32 {
        base * 2
    }

    fn tag(&self) -> i32 {
        11
    }
}

// Forwarding impl mirroring core's `impl Debug for &T`, which is where the
// original failure surfaced: `<&() as Debug>::fmt` re-dispatching to
// `<() as Debug>::fmt` on the pointee.
impl<T: Score> Score for &T {
    fn score(&self, base: i32) -> i32 {
        (**self).score(base)
    }

    fn tag(&self) -> i32 {
        (**self).tag()
    }
}

// Generic dispatch so the receiver type is only known at monomorphization.
fn total<T: Score>(value: T, base: i32) -> i32 {
    value.score(base) + value.tag()
}

fn by_ref<T: Score>(value: &T, base: i32) -> i32 {
    value.score(base)
}

fn main() {
    // Direct calls on the unit value.
    let unit = ();
    assert!(unit.score(10) == 11);
    assert!(unit.tag() == 7);

    // Through generics (T = () and T = Marker).
    assert!(total((), 100) == 108);
    assert!(total(Marker, 100) == 211);

    // Through the &T forwarding impl (T = () — the original failure shape).
    assert!(by_ref(&(), 5) == 6);
    assert!(total(&(), 20) == 28);
    assert!(by_ref(&Marker, 5) == 10);

    // Double indirection for good measure.
    let unit_ref = &();
    assert!(by_ref(&unit_ref, 3) == 4);
}
