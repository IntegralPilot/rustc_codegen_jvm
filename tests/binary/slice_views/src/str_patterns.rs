use std::hint::black_box;

pub fn run() {
    let punctuation = black_box(['x', '「', '🦀', 'y']);
    let pattern = &punctuation[1..3];
    let text = black_box("x「hello🦀");
    let text = &text[1..];

    // Character slices match any listed character, not the whole sequence.
    assert!(text.starts_with(pattern));
    assert!(text.ends_with(pattern));
    assert!(black_box("🦀rust").starts_with(pattern));
    assert!(!black_box("xhello").starts_with(pattern));
    assert!(!black_box("").starts_with(pattern));
    assert!(!text.starts_with(black_box(&[] as &[char])));
    assert!(text.starts_with(black_box(&['「', '🦀'])));
    assert!(text.starts_with(black_box(['「', '🦀'])));

    let prefix = black_box("「he");
    assert!(text.starts_with(black_box(&prefix)));
    assert!(!text.starts_with(black_box(&"hello")));

    let mut calls = 0;
    assert!(text.starts_with(|c| {
        calls += 1;
        c == black_box('「')
    }));
    assert_eq!(calls, 1);
    assert!(!black_box("").starts_with(|_| {
        calls += 1;
        true
    }));
    assert_eq!(calls, 1);
    let predicate = black_box(char::is_alphabetic as fn(char) -> bool);
    assert!(black_box("éclair").starts_with(predicate));
    assert!(!black_box("123").starts_with(predicate));

    // The existing string and scalar-character fast paths must still work.
    assert!(text.starts_with(prefix));
    assert!(text.starts_with(black_box("")));
    assert!(!text.starts_with(black_box("hello")));
    assert!(black_box("🦀rust").starts_with(black_box('🦀')));
    assert!(!black_box("").starts_with(black_box('🦀')));
}
