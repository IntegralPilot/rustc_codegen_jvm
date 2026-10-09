#![feature(register_tool)]
#![register_tool(jvm_codegen)]

#[jvm_codegen::export]
pub fn triple(value: u32) -> u32 {
    value * 3
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn library_tests_run_through_cargo_jvm() {
        assert_eq!(triple(14), 42);
    }
}
