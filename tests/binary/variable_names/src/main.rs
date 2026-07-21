#[inline(never)]
fn exercise_variable_names(first: i32, wide: i64) -> i32 {
    let doubled = first * 2;
    let mut total = 0;
    let mut index = 0;
    while index < doubled {
        total += 1;
        index += 1;
    }
    let first = total + wide as i32;
    if first > doubled { first } else { doubled }
}

fn main() {
    let result = exercise_variable_names(2, 1);
    assert!(result == 5);
}
