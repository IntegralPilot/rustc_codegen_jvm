use std::panic;

struct PanicsOnDrop;

impl Drop for PanicsOnDrop {
    fn drop(&mut self) {
        panic!("second panic during unwind");
    }
}

fn trigger_double_panic() {
    let _guard = PanicsOnDrop;
    panic!("first panic");
}

fn main() {
    let result = panic::catch_unwind(trigger_double_panic);
    assert!(result.is_err());
    panic!("a panic during unwinding must abort before catch_unwind returns");
}
