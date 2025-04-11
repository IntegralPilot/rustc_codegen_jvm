fn is_even_plus_one(n: i32) -> i32 {
    if n % 2 == 0 {
        n + 1
    } else {
        n - 1
    }
}

fn main() {
    let result = is_even_plus_one(10);
    // (not assert_eq! because that does dereferencing which we don't support yet)
    assert!(result == 11, "Expected 11, got {}", result);
    let another_result = is_even_plus_one(7);
    assert!(another_result == 6, "Expected 6, got {}", another_result);
}