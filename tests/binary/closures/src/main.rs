fn main() {
    // A simple closure that adds two numbers
    let add = |a: i32, b: i32| a + b;
    assert!(add(3, 4) == 7);
    assert!(add(-1, 1) == 0);
    assert!(add(0, 0) == 0);
    assert!(add(5, 5) == 10);
    assert!(add(10, 20) == 30);
    assert!(add(-5, -5) == -10);
    assert!(add(-3, 2) == -1);
    assert!(add(-2, 3) == 1);
}
