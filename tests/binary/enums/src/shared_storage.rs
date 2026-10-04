use std::{any::TypeId, hint::black_box};

#[derive(Clone, PartialEq, Debug)]
struct First(i32);
#[derive(Clone, PartialEq, Debug)]
struct Second(i32);

#[derive(Clone, PartialEq, Debug)]
enum Choice<T> {
    Empty,
    Value(T),
    Pair(T, T),
}

#[derive(Clone, PartialEq, Debug)]
enum RenamedChoice<T> {
    Absent,
    One(T),
    Two(T, T),
}

#[derive(Clone, PartialEq, Debug)]
enum NamedChoice<T> {
    Missing,
    Single { payload: T },
    Both { left: T, right: T },
}

#[inline(never)]
fn copy<T: Clone>(value: &Choice<T>) -> Choice<T> {
    black_box(value).clone()
}

#[repr(i64)]
enum DifferentTags<T> {
    Empty = -19,
    Value(T) = 101,
    Pair(T, T) = 9001,
}

pub fn run() {
    assert_ne!(
        TypeId::of::<Choice<First>>(),
        TypeId::of::<RenamedChoice<First>>()
    );
    let renamed = black_box(RenamedChoice::Two(First(79), First(83)));
    let mut copied = black_box(renamed.clone());
    if let RenamedChoice::Two(ref mut first, _) = copied {
        first.0 = 89;
    }
    assert_eq!(renamed, RenamedChoice::Two(First(79), First(83)));
    assert_ne!(copied, renamed);
    assert_eq!(format!("{renamed:?}"), "Two(First(79), First(83))");
    assert_eq!(
        black_box(RenamedChoice::One(First(97))),
        RenamedChoice::One(First(97))
    );
    assert_eq!(
        black_box(RenamedChoice::<First>::Absent),
        RenamedChoice::Absent
    );
    let constructor = black_box(RenamedChoice::One as fn(First) -> RenamedChoice<First>);
    assert_eq!(constructor(First(101)), RenamedChoice::One(First(101)));
    let named = black_box(NamedChoice::Both {
        left: First(103),
        right: First(107),
    });
    let mut copied = black_box(named.clone());
    if let NamedChoice::Both { ref mut right, .. } = copied {
        right.0 = 109;
    }
    assert_ne!(copied, named);
    assert_eq!(
        named,
        NamedChoice::Both {
            left: First(103),
            right: First(107)
        }
    );
    assert_eq!(
        format!("{named:?}"),
        "Both { left: First(103), right: First(107) }"
    );
    assert_eq!(
        black_box(NamedChoice::Single {
            payload: First(113)
        }),
        NamedChoice::Single {
            payload: First(113)
        }
    );
    assert_eq!(
        black_box(NamedChoice::<First>::Missing),
        NamedChoice::Missing
    );
    assert_ne!(
        TypeId::of::<Choice<First>>(),
        TypeId::of::<Choice<Second>>()
    );
    let a = black_box(Choice::Pair(First(17), First(29)));
    let b = black_box(Choice::Pair(Second(31), Second(43)));
    assert_eq!(copy(&a), a);
    assert_eq!(copy(&b), b);
    let mut snapshot = copy(&a);
    if let Choice::Pair(ref mut first, _) = snapshot {
        first.0 = 59;
    }
    assert_eq!(a, Choice::Pair(First(17), First(29)));
    assert_ne!(snapshot, a);
    assert_eq!(
        copy(&black_box(Choice::Value(First(61)))),
        Choice::Value(First(61))
    );
    assert_eq!(copy(&black_box(Choice::<Second>::Empty)), Choice::Empty);
    for value in [
        DifferentTags::Empty,
        DifferentTags::Value(67),
        DifferentTags::Pair(71, 73),
    ] {
        let value = black_box(value);
        let tag = unsafe { (&raw const value).cast::<i64>().read() };
        match value {
            DifferentTags::Empty => assert_eq!(tag, -19),
            DifferentTags::Value(v) => {
                assert_eq!(tag, 101);
                assert_eq!(v, 67);
            }
            DifferentTags::Pair(a, b) => {
                assert_eq!(tag, 9001);
                assert_eq!((a, b), (71, 73));
            }
        }
    }
    // Equal JVM primitive payloads retain their separate Rust byte layouts.
    for value in [Ok(0x1234u16), Err(-123i16)] {
        let value = black_box(value);
        let copy = unsafe { (&raw const value).read() };
        assert_eq!(value, copy);
    }
    for value in [Ok(0x1234_5678u32), Err(-123456i32)] {
        let value = black_box(value);
        let copy = unsafe { (&raw const value).read() };
        assert_eq!(value, copy);
    }
}
