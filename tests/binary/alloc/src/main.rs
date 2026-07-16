#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

extern crate alloc;

include!("../../../support/test_prelude.rs");

use alloc::alloc::{alloc_zeroed, dealloc};
use alloc::vec::Vec;
use core::alloc::Layout;

fn main() {
    let mut values = Vec::with_capacity(1);
    for value in 0_u64..64 {
        values.push(value * value + 3);
    }

    assert!(values.len() == 64);
    assert!(values.capacity() >= values.len());
    for (index, value) in values.iter().enumerate() {
        let index = index as u64;
        assert!(*value == index * index + 3);
    }

    assert!(values.pop() == Some(63 * 63 + 3));
    values.insert(4, 999);
    assert!(values[4] == 999);
    assert!(values.remove(4) == 999);
    assert!(values[4] == 4 * 4 + 3);

    let layout = Layout::from_size_align(32, 16).unwrap();
    unsafe {
        let bytes = alloc_zeroed(layout);
        assert!(!bytes.is_null());
        assert!((bytes as usize) & (layout.align() - 1) == 0);
        for index in 0..layout.size() {
            assert!(*bytes.add(index) == 0);
            *bytes.add(index) = index as u8;
        }
        for index in 0..layout.size() {
            assert!(*bytes.add(index) == index as u8);
        }
        dealloc(bytes, layout);
    }
}
