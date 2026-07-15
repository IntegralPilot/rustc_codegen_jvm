#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

#[panic_handler]
fn panic(_: &core::panic::PanicInfo<'_>) -> ! {
    unsafe { core::hint::unreachable_unchecked() }
}

#[lang = "start"]
fn start<T>(main: fn() -> T, _: isize, _: *const *const u8, _: u8) -> isize {
    main();
    0
}


fn main() {
    let value = 42;
    panic!("This is a formatted panic message: {}", value);
}
