// Tests panicking. Formatting is technically `alloc` stuff, we're only trying to get `core` working for now
// Tester.py compares the actual panic output (message + backtrace) with the expected output

fn main() {
    panic!("This is a panic message!");
}
