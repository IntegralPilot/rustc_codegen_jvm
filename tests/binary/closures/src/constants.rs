// Reconstruct closure captures from their scalar CTFE values.
struct Distance(f64);

impl Distance {
    #[inline(never)]
    fn advance(&mut self, amount: f64) -> f64 {
        self.0 += amount;
        self.0
    }
}

const fn accumulator() -> impl FnMut(f64) -> f64 {
    let mut offset = Distance(0.0);
    move |amount| offset.advance(amount)
}

enum Position {
    Start,
    End,
}

struct Wrapped(Position);

impl Wrapped {
    #[inline(never)]
    fn value(&self) -> u8 {
        match self.0 {
            Position::Start => 3,
            Position::End => 7,
        }
    }
}

const fn position(end: bool) -> impl Fn() -> u8 {
    let capture = Wrapped(if end { Position::End } else { Position::Start });
    move || capture.value()
}

pub fn check() {
    let mut add = const { accumulator() };
    assert_eq!(add(2.5), 2.5);
    assert_eq!(add(4.0), 6.5);
    assert_eq!(const { position(false) }(), 3);
    assert_eq!(const { position(true) }(), 7);
}
