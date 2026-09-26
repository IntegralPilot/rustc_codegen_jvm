#[derive(Default)]
struct Named(u32);

#[inline(never)]
fn concrete_default() -> Named
where
    Named: Default,
{
    Named::default()
}

pub fn run() {
    let a = {
        struct Helper(bool);
        std::hint::black_box(Helper(true)).0
    };
    let b = {
        struct Helper(Option<bool>);
        std::hint::black_box(Helper(Some(false))).0
    };
    assert!(a);
    assert_eq!(b, Some(false));
    assert_eq!(concrete_default().0, 0);
}
