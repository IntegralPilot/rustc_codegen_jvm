pub use foreign_interface_api::*;

pub struct Counter {
    pub value: i32,
}

impl Runnable for Counter {
    fn run(&mut self) {
        self.value += 1;
    }
}

impl Operator for Counter {
    fn apply_as_int(&self, value: i32) -> i32 {
        self.value + value
    }
}

impl Serializable for Counter {}

impl Base for Counter {
    fn bias(&self) -> i32 {
        self.value
    }
}

impl Wide for Counter {
    fn apply(&self, value: i64, scale: f64) -> i64 {
        (value as f64 * scale) as i64 + self.value as i64
    }
}

pub fn run_rust(counter: &mut dyn Runnable) {
    counter.run();
}

pub fn apply_rust(operator: &dyn Operator, value: i32) -> i32 {
    operator.apply_as_int(value)
}

pub fn wide_rust(wide: &dyn Wide) -> i64 {
    wide.apply(5, 2.0) + wide.offset() as i64
}

pub fn rust_dispatch() -> i32 {
    let mut counter = Counter { value: 1 };
    run_rust(&mut counter);
    apply_rust(&counter, 10)
}

impl ObjectOperator for Counter {
    fn apply(&self, value: *mut JavaObject) -> *mut JavaObject {
        value
    }
}

pub fn make_generic() -> Generic<i64> {
    Generic {
        value: 7,
        extra: 123,
    }
}

pub fn make_noncopy() -> Generic<String> {
    Generic {
        value: 9,
        extra: String::new(),
    }
}

pub fn object_rust(op: &dyn ObjectOperator, value: *mut JavaObject) -> *mut JavaObject {
    op.apply(value)
}
