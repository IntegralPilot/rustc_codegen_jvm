#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
#[repr(u8)]
enum Mode {
    Len,
    Len_,
    Len__,
}

#[inline(never)]
fn advance(mode: Mode) -> Mode {
    match mode {
        Mode::Len__ => Mode::Len_,
        Mode::Len_ => Mode::Len,
        Mode::Len => Mode::Len__,
    }
}

struct Marker(u32);
#[allow(non_camel_case_types)]
struct Marker_(u64);
struct Wrapper<T>(T);

pub fn run() {
    let first = std::hint::black_box(Wrapper(Marker(12)));
    let second = std::hint::black_box(Wrapper(Marker_(42)));
    assert_eq!(first.0.0, 12);
    assert_eq!(second.0.0, 42);
    let mut mode = std::hint::black_box(Mode::Len__);
    for expected in [1, 0, 2, 1, 0, 2] {
        mode = advance(mode);
        assert_eq!(mode as u8, expected);
    }
}
