use std::env;
use std::io::{self, Read};

fn main() {
    test_arguments();
    test_environment();
    test_stdin();
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
