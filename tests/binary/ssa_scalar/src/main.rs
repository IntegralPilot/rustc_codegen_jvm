#![feature(core_intrinsics)]
#![allow(internal_features)]
// Run with RCGJ_SSA=scalar to exercise direct MIR → SSA. Assertions and
// unsupported reference/call bodies also check interoperability with legacy IR.
#[inline(never)]
fn loop_carried(mut a: u64, mut b: u64, mut n: u32) -> u64 {
    while n != 0 {
        let old = a;
        a = b;
        b = old;
        n -= 1;
    }
    a * 100 + b
}
#[inline(never)]
fn choose(x: i32) -> i32 {
    match x { -1000 => 17, 0 => 23, 1000 => 41, _ => 99 }
}
#[inline(never)]
fn shift_byte(x: u8, y: u32) -> u8 { x << (y & 7) }
#[inline(never)]
fn shift_short(x: i16, y: u64) -> i16 { x >> (y & 15) }
#[inline(never)]
fn widen(x: u8) -> u64 { x as u64 }
#[inline(never)]
fn floating(x: f64, y: f64) -> bool { x < y }
#[inline(never)]
fn float_u8(x: f64) -> u8 { x as u8 }
#[inline(never)]
fn float_u64(x: f64) -> u64 { x as u64 }
#[inline(never)]
fn unsigned_float(x: u64) -> f64 { x as f64 }
#[inline(never)]
fn unicode(x: char) -> u32 { x as u32 }
#[inline(never)]
fn unit(_: (), x: i64) -> i64 { x }
#[inline(never)]
fn no_arguments() -> u32 { 1234 }
#[inline(never)]
fn put(value: &mut u64, next: u64) { *value = next; }
#[inline(never)]
fn get(value: &u64) -> u64 { *value }
#[inline(never)]
fn reborrow(value: &mut u64) -> &mut u64 { value }
#[inline(never)]
fn local_cell(mut value: u64) -> u64 {
    put(reborrow(&mut value), 0xfedc_ba98_7654_3210);
    get(&value)
}
#[inline(never)]
unsafe fn aliases(write: *mut i32, read: *const i32, value: i32) -> i32 {
    unsafe { *write = value; *read }
}
#[inline(never)]
fn cast_cell(mut value: u32) -> u8 {
    let pointer = (&raw mut value) as *mut u8;
    unsafe { *pointer = 0xf1; *pointer }
}
#[inline(never)]
fn forward_panic() -> u64 { panic_callee() }
#[inline(never)]
fn panic_callee() -> u64 { panic!("SSA call unwind") }
#[inline(never)]
fn call_narrow(value: u8) -> u64 { widen(value) }
#[inline(never)]
fn call_unit(value: i64) -> i64 { unit((), value) }
#[inline(never)]
fn checked_sum(a: i8, b: i8) -> i8 { a + b }
#[inline(never)]
fn checked_product(a: i64, b: i64) -> i64 { a * b }
#[inline(never)]
fn division(a: i64, b: i64) -> i64 { a / b }
#[inline(never)]
fn checked_negation(a: i64) -> i64 { -a }
#[inline(never)]
fn overflow_flag(a: u64, b: u64) -> bool { core::intrinsics::mul_with_overflow(a, b).1 }
#[inline(never)]
fn tuple_copies(a: i64, b: i64) -> i64 {
    let mut pair = (a, b);
    let old = pair;
    pair.0 = old.1;
    pair.1 = old.0;
    pair.0.wrapping_sub(pair.1)
}
#[inline(never)]
fn bits8(value: i8) -> u32 { value.count_ones() | (value.leading_zeros() << 8) | (value.trailing_zeros() << 16) }
#[inline(never)]
fn reverse8(value: i8) -> i8 { value.reverse_bits() }
#[inline(never)]
fn swap16(value: i16) -> i16 { value.swap_bytes() }
#[inline(never)]
fn opaque_sum(a: i32, b: i32) -> i32 { std::hint::black_box(a).wrapping_add(b) }
#[inline(never)]
unsafe fn offset_read(pointer: *const u64, offset: isize) -> u64 { unsafe { *pointer.offset(offset) } }
#[inline(never)]
unsafe fn wrapping_read(pointer: *const u64, offset: isize) -> u64 { unsafe { *pointer.wrapping_offset(offset) } }
#[track_caller]
#[inline(never)]
fn tracked_division(a: i64, b: i64) -> i64 { a / b }
#[track_caller]
#[inline(never)]
fn tracked_forward(a: i64, b: i64) -> i64 { tracked_division(a, b) }
fn main() {
    for n in 0..100 {
        assert_eq!(loop_carried(20_000_000_000, 70_000_000_000, n),
            if n % 2 == 0 { 2_070_000_000_000 } else { 7_020_000_000_000 });
    }
    for x in 0..=255u8 {
        assert_eq!(widen(x), x as u64);
        for y in 0..80 {
            assert_eq!(shift_byte(x, y), ((x as u32) << (y & 7)) as u8);
            assert_eq!(shift_short(-30000, y as u64), (-30000i32 >> (y & 15)) as i16);
        }
    }
    for (x, expected) in [(-1000,17), (0,23), (1000,41), (-1,99), (i32::MIN,99)] {
        assert_eq!(choose(x), expected);
    }
    assert!(!floating(f64::NAN, 1.0));
    assert!(!floating(1.0, f64::NAN));
    assert!(!floating(-0.0, 0.0));
    for (x, expected) in [(f64::NAN,0), (-1.0,0), (127.9,127), (255.0,255), (1e30,255)] {
        assert_eq!(float_u8(x), expected);
    }
    assert_eq!(float_u64(f64::INFINITY), u64::MAX);
    assert_eq!(float_u64(-1.0), 0);
    assert_eq!(float_u64(f64::NAN), 0);
    assert_eq!(unsigned_float(u64::MAX), 18446744073709551616.0);
    assert_eq!(unicode('🦀'), 0x1f980);
    assert_eq!(unit((), -123), -123);
    assert_eq!(no_arguments(), 1234);
    assert_eq!(local_cell(7), 0xfedc_ba98_7654_3210);
    let mut value = 3;
    let pointer = &raw mut value;
    assert_eq!(unsafe { aliases(pointer, pointer, -42) }, -42);
    assert_eq!(value, -42);
    assert_eq!(cast_cell(0x1234_5678), 0xf1);
    for n in 0..=255 { assert_eq!(call_narrow(n), n as u64); }
    assert_eq!(call_unit(-567), -567);
    std::panic::set_hook(Box::new(|_| {}));
    let panic = std::panic::catch_unwind(forward_panic).unwrap_err();
    assert_eq!(panic.downcast_ref::<&str>(), Some(&"SSA call unwind"));
    assert_eq!(checked_sum(12, 13), 25);
    assert_eq!(checked_product(-7, 6), -42);
    assert_eq!(division(-17, 5), -3);
    assert_eq!(checked_negation(-42), 42);
    assert!(std::panic::catch_unwind(|| division(7, 0)).is_err());
    assert!(std::panic::catch_unwind(|| division(i64::MIN, -1)).is_err());
    #[cfg(debug_assertions)] {
        let panic = std::panic::catch_unwind(|| checked_sum(127, 1)).unwrap_err();
        assert_eq!(panic.downcast_ref::<&str>(), Some(&"attempt to add with overflow"));
        assert!(std::panic::catch_unwind(|| checked_product(i64::MIN, -1)).is_err());
        assert!(std::panic::catch_unwind(|| checked_negation(i64::MIN)).is_err());
    }
    #[cfg(not(debug_assertions))] {
        assert_eq!(checked_sum(127, 1), -128);
        assert_eq!(checked_product(i64::MIN, -1), i64::MIN);
        assert_eq!(checked_negation(i64::MIN), i64::MIN);
    }
    for a in [0, 1, 2, u64::MAX, 1 << 63, u32::MAX as u64] {
        for b in [0, 1, 2, u64::MAX, 1 << 63, u32::MAX as u64] {
            assert_eq!(overflow_flag(a, b), a.overflowing_mul(b).1);
        }
    }
    assert_eq!(tuple_copies(123, 456), 333);
    for a in i8::MIN..=i8::MAX {
        let bits = (a as u8) as u32;
        assert_eq!(bits8(a), bits.count_ones() | ((bits.leading_zeros() - 24) << 8) | (bits.trailing_zeros().min(8) << 16));
        assert_eq!(reverse8(a), (bits.reverse_bits() >> 24) as i8);
    }
    for a in [i16::MIN, -1, 0, 0x1234, i16::MAX] { assert_eq!(swap16(a), ((a as u16) << 8 | (a as u16) >> 8) as i16); }
    assert_eq!(opaque_sum(i32::MAX, 1), i32::MIN);
    let values = [7, 13, u64::MAX];
    let pointer = values.as_ptr();
    assert_eq!(unsafe { offset_read(pointer, 2) }, u64::MAX);
    assert_eq!(unsafe { wrapping_read(pointer.wrapping_offset(2), -2) }, 7);
    use std::sync::atomic::{AtomicUsize, Ordering};
    static PANIC_LINE: AtomicUsize = AtomicUsize::new(0);
    std::panic::set_hook(Box::new(|info| { PANIC_LINE.store(info.location().unwrap().line() as usize, Ordering::SeqCst); }));
    let expected_line = line!() as usize + 1;
    assert!(std::panic::catch_unwind(|| tracked_forward(7, 0)).is_err());
    assert_eq!(PANIC_LINE.load(Ordering::SeqCst), expected_line);
    println!("SSA scalar integration passed");
}
