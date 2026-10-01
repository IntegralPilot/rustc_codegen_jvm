use std::{cmp::Ordering, hint::black_box};

#[inline(never)]
fn mask<T>(left: *const T, right: *const T) -> u8 {
    u8::from(left < right)
        | (u8::from(left <= right) << 1)
        | (u8::from(left > right) << 2)
        | (u8::from(left >= right) << 3)
}

#[inline(never)]
fn stored_address<T>(pointer: *const T) -> usize {
    let mut bytes = [0_u8; size_of::<usize>()];
    unsafe {
        bytes.as_mut_ptr().cast::<*const T>().write_unaligned(pointer);
    }
    usize::from_ne_bytes(bytes)
}

fn check_pair<T>(left: *const T, right: *const T) {
    // Compare against numeric addresses to check wrapping offsets and projected fields.
    let expected = match left.addr().cmp(&right.addr()) {
        Ordering::Less => 3,
        Ordering::Equal => 10,
        Ordering::Greater => 12,
    };
    assert_eq!(mask(black_box(left), black_box(right)), expected);
    assert_eq!(stored_address(black_box(left)), left.addr());
    assert_eq!(stored_address(black_box(right)), right.addr());
}

#[repr(C)]
struct Pair {
    first: i32,
    second: i32,
}

pub fn check() {
    let values = black_box([17_i32, 19, 23, 29]);
    let other = black_box([31_i32, 37, 41, 43]);
    let base = values.as_ptr();
    for left in [isize::MIN, -64, -4, 0, 4, 64, isize::MAX] {
        for right in [isize::MIN, -4, 0, 4, isize::MAX] {
            check_pair(
                base.wrapping_byte_offset(left),
                base.wrapping_byte_offset(right),
            );
        }
    }
    check_pair(base, other.as_ptr());
    for address in [
        0,
        1,
        isize::MAX as usize,
        (isize::MAX as usize) + 1,
        usize::MAX,
    ] {
        let pointer = std::ptr::without_provenance::<i32>(black_box(address));
        check_pair(pointer, base);
        check_pair(pointer, pointer);
        check_pair(pointer, std::ptr::null());
    }
    let pair = black_box(Pair {
        first: 47,
        second: 53,
    });
    let first = std::ptr::addr_of!(pair.first);
    let second = std::ptr::addr_of!(pair.second);
    for displacement in [isize::MIN, isize::MAX] {
        let wrapped = second.wrapping_byte_offset(displacement);
        assert_eq!(wrapped.addr(), second.addr().wrapping_add_signed(displacement));
        check_pair(wrapped, first);
    }
    check_pair(first, second);
    check_pair(second.wrapping_byte_offset(-4), first);
    check_pair(first.wrapping_add(1), second);
    let aggregate = &pair as *const Pair;
    check_pair(aggregate, aggregate.wrapping_add(1));
    check_pair(aggregate.wrapping_offset(-1), aggregate);
}
