#![feature(custom_inner_attributes)]
#![feature(register_tool)]
#![register_tool(jvm_codegen)]
#![jvm_codegen::export]
pub struct Ciallo {
    pub count: i32,
    pub desc: &'static str,
}

pub fn ciallo(a: Ciallo) -> &'static str {
    if a.count == 233 && a.desc == "wooooooooo" {
        "Hello from Rust!"
    } else {
        "An error occured"
    }
}
