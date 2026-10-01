#![feature(custom_inner_attributes)]
#![feature(register_tool)]
#![register_tool(jvm_codegen)]
#![jvm_codegen::export]
pub fn multiply(left: i32, right: i32) -> i32 {
    left * right
}

#[unsafe(no_mangle)]
pub extern "C" fn exported_add(left: i32, right: i32) -> i32 {
    left + right
}
