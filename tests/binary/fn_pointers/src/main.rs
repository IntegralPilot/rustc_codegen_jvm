#[derive(Copy, Clone)]
enum ComputeStep<T> {
    Unary(fn(T) -> T),
    Binary(fn(T, T) -> T, T),
    Constant(T),
}

#[derive(Copy, Clone)]
struct Processor<T> {
    step: ComputeStep<T>,
    fallback: fn(T) -> T,
}

#[derive(Copy, Clone)]
struct Pair<A, B> {
    first: A,
    second: B,
}

fn square_f64(x: f64) -> f64 {
    x * x
}

fn halving_f64(x: f64) -> f64 {
    x / 2.0
}

fn scale_f64(x: f64, factor: f64) -> f64 {
    x * factor
}

fn negate_i32(x: i32) -> i32 {
    -x
}

fn add_i32(x: i32, y: i32) -> i32 {
    x + y
}

fn add_five_i32(x: i32) -> i32 {
    x + 5
}

fn identity<T>(x: T) -> T {
    x
}

fn run_processor<T: Copy>(proc: Processor<T>, input: T, use_fallback: bool) -> T {
    if use_fallback {
        (proc.fallback)(input)
    } else {
        match proc.step {
            ComputeStep::Unary(f) => f(input),
            ComputeStep::Binary(f, arg) => f(input, arg),
            ComputeStep::Constant(val) => val,
        }
    }
}

fn apply_pair_pipeline<T>(pair: Pair<fn(T) -> T, fn(T) -> T>, val: T) -> T {
    let intermediate = (pair.first)(val);
    (pair.second)(intermediate)
}

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

    let f64_proc: Processor<f64> = Processor {
        step: ComputeStep::Binary(scale_f64 as fn(f64, f64) -> f64, 3.0),
        fallback: halving_f64 as fn(f64) -> f64,
    };
    assert!(run_processor(f64_proc, 4.0, false) == 12.0);
    assert!(run_processor(f64_proc, 4.0, true) == 2.0);

    let i32_proc: Processor<i32> = Processor {
        step: ComputeStep::Unary(negate_i32 as fn(i32) -> i32),
        fallback: identity::<i32> as fn(i32) -> i32,
    };
    assert!(run_processor(i32_proc, 10, false) == -10);
    assert!(run_processor(i32_proc, 10, true) == 10);

    let const_proc: Processor<i32> = Processor {
        step: ComputeStep::Constant(42),
        fallback: identity::<i32> as fn(i32) -> i32,
    };
    assert!(run_processor(const_proc, 10, false) == 42);

    let pipeline: Pair<fn(i32) -> i32, fn(i32) -> i32> = Pair {
        first: negate_i32 as fn(i32) -> i32,
        second: add_five_i32 as fn(i32) -> i32,
    };
    assert!(apply_pair_pipeline(pipeline, 10) == -5);

    let complex_step: ComputeStep<Pair<fn(i32) -> i32, fn(i32) -> i32>> = ComputeStep::Constant(Pair {
        first: negate_i32 as fn(i32) -> i32,
        second: add_five_i32 as fn(i32) -> i32,
    });

    match complex_step {
        ComputeStep::Constant(pair) => {
            assert!(apply_pair_pipeline(pair, 20) == -15);
        }
        _ => {
            assert!(false);
        }
    }
}