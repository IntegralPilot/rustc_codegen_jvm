#![feature(register_tool)]
#![register_tool(jvm_codegen)]

#[jvm_codegen::export]
pub struct Shared {
    pub value: i32,
}

impl Shared {
    #[inline(never)]
    pub fn add(&mut self, amount: i32) -> i32 {
        self.value += amount;
        self.value
    }
}

pub struct Internal {
    pub value: i32,
}

#[inline(never)]
pub fn internal_value(value: Internal) -> i32 {
    value.value
}
