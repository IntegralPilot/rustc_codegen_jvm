enum Void {}
struct Hidden(Void);

enum Number {
    Zero,
    One,
    Hidden(Hidden),
}

struct Mapping(Hidden);

impl Hidden {
    #[inline(never)]
    fn make_mut(&mut self) -> std::io::Result<()> {
        match self.0 {}
    }
}

impl Mapping {
    #[inline(never)]
    fn make_mut(mut self) -> std::io::Result<Hidden> {
        self.0.make_mut()?;
        Ok(self.0)
    }
}

#[inline(never)]
fn tag(number: Number) -> u8 {
    match number {
        Number::Zero => 0,
        Number::One => 1,
        Number::Hidden(hidden) => match hidden.0 {},
    }
}

pub fn run() {
    // The unreachable payload does not prevent materializing the other,
    // fieldless variants from their scalar MIR constants.
    assert_eq!(tag(Number::Zero), 0);
    assert_eq!(tag(Number::One), 1);
    // Force codegen of the impossible success path without creating an invalid
    // value. This is the shape used by memmap2 on unsupported platforms.
    let make_mut = std::hint::black_box(Mapping::make_mut);
    let absent = std::hint::black_box(None::<Mapping>);
    assert!(absent.map(make_mut).is_none());
}
