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
        if result != 1 {
            panic!("The collatz conjecture broke? This shouldn't happen.");
        }
        check_up_to(current + 1, limit);
    }
}

fn main() {
    // test check_up_to a few times
    check_up_to(1, 10);
    check_up_to(70, 100);
    check_up_to(1000, 1010);
    check_up_to(10000, 10010);
}
