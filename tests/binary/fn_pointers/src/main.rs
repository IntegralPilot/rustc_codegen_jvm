fn derivative(f: fn(f64) -> f64, x: f64, h: f64) -> f64 {
    (f(x + h) - f(x)) / h
}

fn apply_twice(f: fn(f64) -> f64, x: f64) -> f64 {
    f(f(x))
}

fn choose_curve(which: i32) -> fn(f64) -> f64 {
    if which == 0 {
        constant
    } else if which == 1 {
        linear
    } else {
        square
    }
}

fn linear(x: f64) -> f64 {
    2.0 * x + 3.0
}

fn square(x: f64) -> f64 {
    x * x
}

fn constant(_x: f64) -> f64 {
    42.0
}

fn combine(f: fn(i32, i32) -> i32, a: i32, b: i32) -> i32 {
    f(a, b) + f(b, a)
}

fn choose_int_op(addition: bool) -> fn(i32, i32) -> i32 {
    if addition {
        add
    } else {
        skew
    }
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn skew(a: i32, b: i32) -> i32 {
    a * 3 - b
}

fn main() {
    let res_const = derivative(constant, 10.0, 0.125);
    assert!(res_const == 0.0);

    let res_linear = derivative(linear, 5.0, 0.125);
    assert!(res_linear == 2.0);

    let res_square = derivative(square, 3.0, 0.5);
    assert!(res_square == 6.5);

    let chosen_const = choose_curve(0);
    assert!(chosen_const(99.0) == 42.0);

    let chosen_linear = choose_curve(1);
    assert!(derivative(chosen_linear, 7.0, 0.25) == 2.0);

    let chosen_square = choose_curve(2);
    assert!(derivative(chosen_square, 4.0, 0.25) == 8.25);

    let mut local_fn: fn(f64) -> f64 = linear;
    assert!(local_fn(2.0) == 7.0);
    local_fn = square;
    assert!(local_fn(4.0) == 16.0);

    assert!(apply_twice(linear, 4.0) == 25.0);

    let add_ptr = choose_int_op(true);
    assert!(add_ptr(9, 4) == 13);
    assert!(combine(add, 2, 5) == 14);

    let skew_ptr = choose_int_op(false);
    assert!(skew_ptr(9, 4) == 23);
    assert!(combine(skew, 2, 5) == 14);
}
