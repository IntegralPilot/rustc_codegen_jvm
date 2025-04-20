fn fibonacci(n: usize) -> usize {
    if n == 0 { return 0; }
    if n == 1 { return 1; }

    let mut a: usize = 0;
    let mut b: usize = 1;
    let mut i: usize = 2;

    while i <= n {
        let temp = a + b;
        a = b;
        b = temp;
        i += 1;
    }

    b
}

fn fib_recursive(n: usize) -> usize {
    match n {
        0 => 0,
        1 => 1,
        _ => fib_recursive(n - 1) + fib_recursive(n - 2),
    }
}

fn main() {
    assert!(fibonacci(0) == 0);
    assert!(fibonacci(1) == 1);
    assert!(fibonacci(5) == 5);
    assert!(fibonacci(10) == 55);
    assert!(fibonacci(15) == 610);
    assert!(fibonacci(20) == 6765);
    assert!(fibonacci(25) == 75025);
    assert!(fibonacci(30) == 832040);

    assert!(fib_recursive(0) == 0);
    assert!(fib_recursive(1) == 1);
    assert!(fib_recursive(5) == 5);
    assert!(fib_recursive(10) == 55);
    assert!(fib_recursive(15) == 610);
    assert!(fib_recursive(20) == 6765);
    assert!(fib_recursive(25) == 75025);
    assert!(fib_recursive(30) == 832040);
}