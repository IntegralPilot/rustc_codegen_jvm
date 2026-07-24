pub use super::common::Args;
use crate::ffi::OsString;
use crate::string::String;
use crate::vec::Vec;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:argumentCount"]
    fn argument_count() -> usize;

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:argumentLength"]
    fn argument_length(index: usize) -> usize;

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:copyArgument"]
    fn copy_argument(index: usize, destination: *mut u8);
}

pub fn args() -> Args {
    let count = unsafe { argument_count() };
    let mut arguments = Vec::with_capacity(count);
    for index in 0..count {
        let length = unsafe { argument_length(index) };
        let mut bytes = Vec::with_capacity(length);
        bytes.resize(length, 0);
        unsafe { copy_argument(index, bytes.as_mut_ptr()) };
        let argument = unsafe { String::from_utf8_unchecked(bytes) };
        arguments.push(OsString::from(argument));
    }
    Args::new(arguments)
}
