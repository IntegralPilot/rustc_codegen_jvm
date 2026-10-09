#![feature(iter_advance_by)]

fn main() {
    let text = std::hint::black_box(String::from("abé🦀z"));
    let mut chars = text.chars();
    assert_eq!(chars.advance_by(std::hint::black_box(3)), Ok(()));
    assert_eq!(chars.next(), Some('🦀'));
    assert_eq!(chars.next(), Some('z'));
    assert_eq!(chars.next(), None);

    let mut words = std::hint::black_box(vec![String::from("one"), String::from("two")]);
    assert_eq!(
        words
            .iter()
            .nth(std::hint::black_box(1))
            .map(String::as_str),
        Some("two")
    );
    words
        .iter_mut()
        .nth(std::hint::black_box(1))
        .unwrap()
        .push('!');
    assert_eq!(words[1], "two!");
}
