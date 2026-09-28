struct Section<'a>(&'a [u8]);

enum Payload<'a> {
    Section(Section<'a>),
}

// The callback's result borrows from the enclosing function's lifetime.
// Lowering its enum fields must not pass escaping regions to `needs_drop`.
#[inline(never)]
fn parse_section<'a>(bytes: &'a [u8], variant: fn(Section<'a>) -> Payload<'a>) -> Payload<'a> {
    variant(Section(bytes))
}

pub fn run() {
    let bytes = [42];
    let Payload::Section(section) = parse_section(&bytes, |section| Payload::Section(section));
    assert_eq!(section.0[0], 42);
}
