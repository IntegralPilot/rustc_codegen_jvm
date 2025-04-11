// Tests both formatting and panicking - in one! :)
// Tester.py compares the actual panic output (message + backtrace) with the expected output

fn main() {
    let number = 42;
    let other_string = "Hello, world!";
    let bool = true;
    let float = 3.14;
    panic!("This is a panic message with a number: {}, a string: {}, a boolean: {}, and a float: {}", number, other_string, bool, float);
}
