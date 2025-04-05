fn is_even_plus_one(n: i32) -> i32 {
    if n % 2 == 0 {
        n + 1
    } else {
        n - 1
    }
}

fn main() {
    let result = is_even_plus_one(10);
    let another_result = is_even_plus_one(7);
}