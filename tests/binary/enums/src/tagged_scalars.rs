use std::hint::black_box;

#[derive(Clone, Copy, Debug, PartialEq)]
struct State {
    before: u8,
    optional: Option<usize>,
    after: u16,
}

#[inline(never)]
fn choose(value: usize, present: bool) -> Option<usize> {
    if present { Some(value) } else { None }
}
#[inline(never)]
fn through_callback(call: fn(usize, bool) -> Option<usize>, value: usize) -> Option<usize> {
    call(value, true)
}
#[inline(never)]
fn fields(value: &mut State) -> Option<usize> {
    value.optional.take()
}

pub fn check() {
    static VALUES: [Option<usize>; 4] = [None, Some(0), Some(usize::MAX), Some(42)];
    for (index, expected) in VALUES.iter().copied().enumerate() {
        let mut state = State {
            before: 7,
            optional: expected,
            after: 900,
        };
        assert_eq!(
            black_box(state),
            State {
                before: 7,
                optional: expected,
                after: 900
            }
        );
        assert_eq!(fields(black_box(&mut state)), expected);
        assert_eq!(state.optional, None);
        assert_eq!((state.before, state.after), (7, 900));
        state.optional = through_callback(black_box(choose), index);
        let closure = move || state.optional;
        assert_eq!(black_box(closure)(), Some(index));
    }
    for value in [0, 1, usize::MAX, usize::MAX - 1] {
        assert_eq!(choose(black_box(value), true), Some(value));
        assert_eq!(choose(value, black_box(false)), None);
    }
    let constructor: fn(u64) -> Option<u64> = Some;
    assert_eq!(black_box(constructor)(u64::MAX), Some(u64::MAX));
    let mut value = Some(8u64);
    let raw = black_box(&raw mut value);
    unsafe {
        if let Some(payload) = &mut *raw {
            *black_box(&raw mut *payload) = u64::MAX;
        }
        assert_eq!(raw.read(), Some(u64::MAX));
        raw.write(None);
        assert_eq!(raw.read(), None);
        raw.write(Some(0));
        assert_eq!(raw.read(), Some(0));
    }
    let mut value = Some(-1i64);
    if let Some(payload) = black_box(&mut value) {
        *payload = i64::MIN;
    }
    assert_eq!(value, Some(i64::MIN));
    assert_eq!(
        black_box(Some(Some(usize::MAX))).flatten(),
        Some(usize::MAX)
    );
    assert_eq!(black_box(Some(None::<usize>)).flatten(), None);
    let mut total = None;
    for i in 0..1000 {
        total = Some(black_box(total).unwrap_or(0usize).wrapping_add(i));
    }
    assert_eq!(total, Some(499500));
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let mut retained = Some(7usize);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        retained = choose(usize::MAX, true);
        panic!("after tagged result");
    }));
    std::panic::set_hook(hook);
    assert!(result.is_err());
    assert_eq!(retained, Some(usize::MAX));
}
