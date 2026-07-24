use std::mem::{align_of, align_of_val, replace, size_of_val, swap};
use std::panic::AssertUnwindSafe;
use std::sync::atomic::{AtomicUsize, Ordering};

static PANIC_HOOK_CALLS: AtomicUsize = AtomicUsize::new(0);

trait Shape {
    fn area(&self) -> f64;
}

struct Circle(f64);
struct Square(f64);
struct Wrapped<T: ?Sized> {
    tag: u8,
    value: T,
}

impl Shape for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * self.0 * self.0
    }
}

impl Shape for Square {
    fn area(&self) -> f64 {
        self.0 * self.0
    }
}

fn fat_pointer_games() {
    let values = vec![1, 2, 3, 4, 5];
    let slice: &[i32] = &values[1..4];
    println!("slice: {slice:?}, len={}", slice.len());
    assert!(slice == [2, 3, 4]);
    assert!(slice.len() == 3);

    let circle: Box<dyn Shape> = Box::new(Circle(2.0));
    println!(
        "area={:.4}, size_of_val={}",
        circle.area(),
        size_of_val(&*circle)
    );
    assert!((circle.area() - 4.0 * std::f64::consts::PI).abs() < 1e-10);
    assert!(size_of_val(&*circle) == size_of::<Circle>());
    assert!(align_of_val(&*circle) == align_of::<Circle>());

    let square: Box<dyn Shape> = Box::new(Square(3.0));
    println!(
        "area={:.4}, size_of_val={}",
        square.area(),
        size_of_val(&*square)
    );
    assert!(square.area() == 9.0);
    assert!(size_of_val(&*square) == size_of::<Square>());
    assert!(align_of_val(&*square) == align_of::<Square>());

    let wrapped: Box<Wrapped<dyn Shape>> = Box::new(Wrapped {
        tag: 7,
        value: Circle(2.0),
    });
    assert!(wrapped.tag == 7);
    assert!((wrapped.value.area() - 4.0 * std::f64::consts::PI).abs() < 1e-10);
    assert!(size_of_val(&*wrapped) == size_of::<Wrapped<Circle>>());
    assert!(align_of_val(&*wrapped) == align_of::<Wrapped<Circle>>());

    let shapes: [Box<dyn Shape>; 2] = [Box::new(Circle(1.0)), Box::new(Square(2.0))];
    let total: f64 = shapes.iter().map(|shape| shape.area()).sum();
    println!("total area={total:.4}");
    assert!((total - (std::f64::consts::PI + 4.0)).abs() < 1e-10);
}

fn swap_replace_games() {
    let mut first = String::from("hello");
    let mut second = String::from("world");
    swap(&mut first, &mut second);
    println!("after swap: a={first} b={second}");
    assert!(first == "world");
    assert!(second == "hello");

    let mut values = vec![1, 2, 3];
    let old = replace(&mut values, vec![9, 9]);
    println!("old={old:?} new={values:?}");
    assert!(old == [1, 2, 3]);
    assert!(values == [9, 9]);

    let mut growth = Vec::with_capacity(1);
    growth.push(1);
    let pointer_before = growth.as_ptr();
    for value in 2..100 {
        growth.push(value);
    }
    let pointer_after = growth.as_ptr();
    println!("realloc happened: {}", pointer_before != pointer_after);
    let sum = growth.iter().sum::<i32>();
    println!("sum: {sum}");
    assert!(sum == 4950);
}

fn overflow_and_panics() {
    let left: u8 = std::hint::black_box(250);
    let right: u8 = std::hint::black_box(10);

    let wrapped = left.wrapping_add(right);
    println!("wrapping_add: {wrapped}");
    assert!(wrapped == 4);
    let checked = left.checked_add(right);
    println!("checked_add: {checked:?}");
    assert!(checked.is_none());
    let saturated = left.saturating_add(right);
    println!("saturating_add: {saturated}");
    assert!(saturated == 255);

    let hook_calls = PANIC_HOOK_CALLS.load(Ordering::Relaxed);
    let overflow = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let value = std::hint::black_box(left + right);
        println!("should only print in release: {value}");
    }));
    println!("overflow panicked: {}", overflow.is_err());
    assert!(overflow.is_err() == cfg!(debug_assertions));
    assert!(
        PANIC_HOOK_CALLS.load(Ordering::Relaxed)
            == hook_calls + usize::from(cfg!(debug_assertions))
    );

    let text = String::from("héllo");
    println!(
        "byte len={}, char count={}",
        text.len(),
        text.chars().count()
    );
    assert!(text.len() == 6);
    assert!(text.chars().count() == 5);
    let hook_calls = PANIC_HOOK_CALLS.load(Ordering::Relaxed);
    let invalid_boundary = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let invalid = std::hint::black_box(&text[0..2]);
        println!("should not print: {invalid}");
    }));
    println!("utf8 slice panicked: {}", invalid_boundary.is_err());
    assert!(invalid_boundary.is_err());
    assert!(PANIC_HOOK_CALLS.load(Ordering::Relaxed) == hook_calls + 1);

    let dividend = std::hint::black_box(10);
    let divisor = std::hint::black_box(0);
    let hook_calls = PANIC_HOOK_CALLS.load(Ordering::Relaxed);
    let divide_by_zero = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let quotient = std::hint::black_box(dividend / divisor);
        println!("should not print: {quotient}");
    }));
    println!("div by zero panicked: {}", divide_by_zero.is_err());
    assert!(divide_by_zero.is_err());
    assert!(PANIC_HOOK_CALLS.load(Ordering::Relaxed) == hook_calls + 1);
}

fn thread_panic_games() {
    let failed = std::thread::spawn(|| {
        let values = vec![1, 2, 3];
        println!("in thread, accessing values[10]");
        values[10]
    });
    match failed.join() {
        Ok(value) => panic!("out-of-bounds thread unexpectedly returned {value}"),
        Err(_) => println!("thread panicked as expected"),
    }

    let successful = std::thread::spawn(|| 42);
    let result = successful.join();
    println!("thread2 result: {result:?}");
    assert!(result.unwrap() == 42);
}

fn main() {
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        PANIC_HOOK_CALLS.fetch_add(1, Ordering::Relaxed);
        default_hook(info);
    }));

    fat_pointer_games();
    swap_replace_games();
    overflow_and_panics();
    thread_panic_games();
    println!("DONE");
}
