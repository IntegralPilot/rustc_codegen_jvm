fn main() {
    // Neither the dependency body nor std::env is defined in this crate.
    assert_eq!(lto_provider::answer(), 42);
    assert!(!std::env::args().next().unwrap().is_empty());
}
