struct Builder {
    points: Vec<i32>,
    pending: bool,
}

struct Storage {
    inner: Builder,
    outer: Builder,
}

struct References<'a> {
    inner: &'a mut Builder,
    outer: &'a mut Builder,
}

#[inline(never)]
fn swap_and_append(mut refs: References<'_>) {
    std::mem::swap(&mut refs.inner, &mut refs.outer);
    append(refs.inner);
    append(refs.outer);
}

#[inline(never)]
fn append(builder: &mut Builder) {
    if builder.pending {
        builder.points.push(42);
        builder.pending = false;
    }
}

pub fn run() {
    let mut storage = Storage {
        inner: Builder {
            points: vec![1],
            pending: false,
        },
        outer: Builder {
            points: vec![2],
            pending: true,
        },
    };
    swap_and_append(References {
        inner: &mut storage.inner,
        outer: &mut storage.outer,
    });
    assert_eq!(storage.inner.points, [1]);
    assert_eq!(storage.outer.points, [2, 42]);
    assert!(!storage.outer.pending);
}
