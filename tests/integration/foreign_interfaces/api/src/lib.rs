#![feature(extern_types, register_tool)]
#![register_tool(jvm_codegen)]

#[jvm::interface("java.lang.Runnable")]
pub trait Runnable {
    fn run(&mut self);
}

#[jvm::interface("java.util.function.IntUnaryOperator", rename_all = "camelCase")]
pub trait Operator {
    fn apply_as_int(&self, value: i32) -> i32;
}

#[jvm::interface("java.io.Serializable")]
pub trait Serializable {}

#[jvm::interface("Main.Base")]
pub trait Base {
    fn bias(&self) -> i32;
}

#[jvm::interface("Main.Wide")]
pub trait Wide: Base {
    #[jvm::method("applyLong")]
    fn apply(&self, value: i64, scale: f64) -> i64;

    fn offset(&self) -> i32 {
        self.bias() + 2
    }
}

#[jvm::class("java.lang.Object")]
pub struct JavaObject;

#[jvm::interface("java.util.function.UnaryOperator")]
pub trait ObjectOperator {
    fn apply(&self, value: *mut JavaObject) -> *mut JavaObject;
}

pub struct Generic<T> {
    pub value: i32,
    pub extra: T,
}

impl<T: Copy> Operator for Generic<T> {
    fn apply_as_int(&self, value: i32) -> i32 {
        self.value * value
    }
}

impl<T: Copy> Serializable for Generic<T> {}
