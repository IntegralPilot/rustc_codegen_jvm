use crate::ffi::OsString;
use crate::io;
use crate::string::String;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:hostnameLength"]
    fn jvm_hostname_length() -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:copyHostname"]
    fn jvm_copy_hostname(destination: *mut u8);
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lastErrorKind"]
    fn jvm_last_error_kind() -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lastErrorMessageLength"]
    fn jvm_last_error_message_length() -> usize;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:copyLastErrorMessage"]
    fn jvm_copy_last_error_message(destination: *mut u8);
}

pub fn hostname() -> io::Result<OsString> {
    let length = unsafe { jvm_hostname_length() };
    if length < 0 {
        return Err(last_error());
    }
    let mut bytes = vec![0; length as usize];
    unsafe { jvm_copy_hostname(bytes.as_mut_ptr()) };
    Ok(OsString::from(unsafe {
        String::from_utf8_unchecked(bytes)
    }))
}

fn last_error() -> io::Error {
    let length = unsafe { jvm_last_error_message_length() };
    let mut bytes = vec![0; length];
    unsafe { jvm_copy_last_error_message(bytes.as_mut_ptr()) };
    let message = unsafe { String::from_utf8_unchecked(bytes) };
    let kind = match unsafe { jvm_last_error_kind() } {
        2 => io::ErrorKind::PermissionDenied,
        4 => io::ErrorKind::InvalidInput,
        13 => io::ErrorKind::Unsupported,
        _ => io::ErrorKind::Other,
    };
    io::Error::new(kind, message)
}
