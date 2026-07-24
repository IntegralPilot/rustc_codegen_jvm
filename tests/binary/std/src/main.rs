use std::env;
use std::fs;
use std::io::{self, Read};
use std::net::Ipv6Addr;

fn main() {
    test_arguments();
    test_environment();
    test_stdin();
    test_pointer_formatting();
    test_ipv6_formatting();
    test_unsupported_filesystem_error();
    println!("std support ok");
}

fn test_arguments() {
    let arguments = env::args().collect::<Vec<_>>();
    assert_eq!(arguments, ["rust-jvm", "alpha", "two words", "café"]);

    let os_arguments = env::args_os()
        .map(|argument| argument.into_string().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(os_arguments, arguments);
}

fn test_environment() {
    const KEY: &str = "RCJ_STDLIB_ENV_TEST";
    unsafe { env::set_var(KEY, "value=with=equals") };
    assert_eq!(env::var(KEY).unwrap(), "value=with=equals");

    let entries = env::vars().collect::<Vec<_>>();
    assert!(entries.iter().any(|(key, value)| {
        key == KEY && value == "value=with=equals"
    }));
    assert!(env::vars_os().any(|(key, value)| {
        key == KEY && value == "value=with=equals"
    }));

    unsafe { env::remove_var(KEY) };
    assert!(env::var_os(KEY).is_none());
    assert!(!env::vars().any(|(key, _)| key == KEY));
}

fn test_stdin() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    assert_eq!(input, "first line\nsecond café line\n");
}

fn test_pointer_formatting() {
    let bytes: &[u8] = b"";
    let text: &str = "";
    assert_eq!(format!("{text:p}"), format!("{:p}", text as *const _));
    assert_eq!(format!("{bytes:p}"), format!("{:p}", bytes as *const _));
}

fn test_ipv6_formatting() {
    let address = Ipv6Addr::new(0xae, 0, 0, 0, 0, 0xffff, 0x0102, 0x0304);
    assert_eq!(
        address.segments(),
        [0xae, 0, 0, 0, 0, 0xffff, 0x0102, 0x0304]
    );
    assert_eq!(
        address.to_string(),
        "ae::ffff:102:304"
    );
}

fn test_unsupported_filesystem_error() {
    let error = fs::write("jvm-unsupported-file", b"contents").unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::Unsupported);
    assert_eq!(
        error.to_string(),
        "operation not supported on this platform"
    );
}
