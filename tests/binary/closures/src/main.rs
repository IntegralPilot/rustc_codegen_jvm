mod constants;

#[inline(never)]
fn duplicate<F: Fn(i32) -> i32 + Clone>(f: F, value: i32) -> i32 {
    let copy = f.clone();
    copy(value) + f(value)
}

fn erased_environments() {
    let increment = |x: i32| x + 1;
    assert_eq!(std::mem::size_of_val(&increment), 0);
    assert_eq!(duplicate(increment, 20), 42);
    let callable: fn(i32) -> i32 = increment;
    assert_eq!(callable(6), 7);
    let dynamic: &dyn Fn(i32) -> i32 = &increment;
    assert_eq!(dynamic(8), 9);
    let owned: Box<dyn Fn(i32) -> i32> = Box::new(increment);
    assert_eq!(owned(10), 11);
    let once: Box<dyn FnOnce(i32) -> i32> = Box::new(|x| x * 2);
    assert_eq!(once(6), 12);
    let instances = [increment; 4];
    assert_eq!(instances.iter().map(|f| f(3)).sum::<i32>(), 16);
    let selected = Some(increment).unwrap();
    assert_eq!(selected(4), 5);
}

fn main() {
    constants::check();
    erased_environments();
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

    // A capturing closure
    let offset = 11;
    let add_offset = |value: i32| value + offset;
    assert!(add_offset(0) == 11);
    assert!(add_offset(31) == 42);
}
