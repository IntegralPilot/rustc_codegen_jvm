fn collatz(n: u32) -> u32 {
    if n == 1 {
        1
    } else if n % 2 == 0 {
        collatz(n / 2)
    } else {
        collatz(3 * n + 1)
    }
}

fn check_up_to(current: u32, limit: u32) {
    if current > limit {
        return;
    } else {
        let result = collatz(current);
        assert!(result == 1);
        check_up_to(current + 1, limit);
    }
}

const U32_TYPE_ID: core::any::TypeId = core::any::TypeId::of::<u32>();
const U64_TYPE_ID: core::any::TypeId = core::any::TypeId::of::<u64>();

// Check some core stuff to check it's compiling right
fn check_core_constants() {
    assert!(U32_TYPE_ID == U32_TYPE_ID);
    assert!(!(U32_TYPE_ID == U64_TYPE_ID));

    let increment = &|value: u32| value + 1;
    assert!((*increment)(41) == 42);

    let value = Some(7_u8);
    assert!(value == Some(7));
}

fn main() {
    check_core_constants();
    // test check_up_to a few times
    check_up_to(1, 10);
    check_up_to(70, 100);
    check_up_to(1000, 1010);
    check_up_to(10000, 10010);
}
