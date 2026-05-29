// Tests panicking. Formatting is technically `alloc` stuff, we're only trying to get `core` working for now
// Tester.py compares the actual panic output (message + backtrace) with the expected output

fn main() {
    let value = 42;
    panic!("This is a formatted panic message: {}", value);
}
