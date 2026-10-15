#![feature(register_tool)]
#![register_tool(jvm_codegen)]

#[jvm::annotation("Main.Tag", value = "upstream")]
pub struct Generic<T> {
    pub value: T,
}

impl<T> Generic<T> {
    pub fn answer(&self) -> i32 {
        42
    }
}

pub fn make() -> Generic<i32> {
    Generic { value: 7 }
}
