struct Holder<T>(T);

fn main() {
    // Cargo aliases must preserve the defining crate's identity in fields,
    // functions, statics, and shared upstream/downstream monomorphizations.
    assert_eq!(core::hint::black_box(old::Node(1)).0, 1);
    assert_eq!(core::hint::black_box(new::Node(2)).0, 2);
    assert_eq!(old::make().0, 11);
    assert_eq!(new::make().0, 22);
    assert_eq!(old::version(), 11);
    assert_eq!(new::version(), 22);
    assert!(!core::ptr::eq(&old::VALUE, &new::VALUE));
    let old = old::boxed();
    let new = new::boxed();
    assert_eq!(core::hint::black_box(Holder(&*old)).0.value(), 11);
    assert_eq!(core::hint::black_box(Holder(&*new)).0.value(), 22);
    assert_eq!(old::clone_node(&old::nodes()[0]).0, 11);
    assert_eq!(new::clone_node(&new::nodes()[0]).0, 22);
}
