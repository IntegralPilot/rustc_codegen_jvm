use std::hint::black_box;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Row {
    value: u64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Rows<const N: usize> {
    values: [Row; N],
    tag: Option<u64>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Choice<const N: usize> {
    Empty,
    Rows(Rows<N>),
}

pub fn run() {
    let a = black_box(Rows {
        values: [Row { value: 17 }; 2],
        tag: Some(u64::MAX),
    });
    let b = black_box(Rows {
        values: [Row { value: 31 }; 3],
        tag: None,
    });
    let mut copied = black_box(a);
    copied.values[0].value = 43;
    copied.tag = None;
    assert_eq!(a.values[0].value, 17);
    assert_eq!(a.tag, Some(u64::MAX));
    assert_ne!(copied, a);
    assert_eq!(b.values.len(), 3);
    assert_eq!(b.values[2].value, 31);
    let a = black_box(Choice::Rows(a));
    let b = black_box(Choice::Rows(b));
    assert_ne!(a, Choice::Empty);
    assert_ne!(b, Choice::Empty);
    assert_eq!(a, black_box(a));
    assert_eq!(b, black_box(b));
    if let Choice::Rows(rows) = b {
        assert_eq!(rows.tag, None);
    } else {
        panic!();
    }
}
