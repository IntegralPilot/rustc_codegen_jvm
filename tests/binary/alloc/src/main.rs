#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

extern crate alloc;

include!("../../../support/test_prelude.rs");

use alloc::alloc::{alloc_zeroed, dealloc, realloc};
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::alloc::Layout;

static mut DROP_COUNTER: usize = 0;

struct DropTracker {
    _val: u32,
}

impl Drop for DropTracker {
    fn drop(&mut self) {
        unsafe {
            DROP_COUNTER += 1;
        }
    }
}

trait Action {
    fn perform(&self) -> u32;
}

struct ConcreteAction {
    factor: u32,
}

impl Action for ConcreteAction {
    fn perform(&self) -> u32 {
        self.factor * 2
    }
}

fn main() {
    test_vec_basic();
    test_zst_allocations();
    test_drop_semantics();
    test_dynamic_dispatch();
    test_various_alignments();
    test_manual_realloc();
}

fn test_vec_basic() {
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
}

fn test_zst_allocations() {
    let mut zsts: Vec<()> = Vec::new();
    for _ in 0..100 {
        zsts.push(());
    }
    assert!(zsts.len() == 100);
    assert!(zsts.capacity() > 100);
    assert!(zsts.pop() == Some(()));

    let boxed_zst: Box<()> = Box::new(());
    let raw_ptr = Box::into_raw(boxed_zst);
    assert!(!raw_ptr.is_null());
    unsafe {
        Box::from_raw(raw_ptr);
    }
}

fn test_drop_semantics() {
    unsafe {
        DROP_COUNTER = 0;
    }

    {
        let mut trackers = Vec::new();
        for i in 0..10 {
            trackers.push(DropTracker { _val: i });
        }
        trackers.pop();
        assert!(unsafe { DROP_COUNTER } == 1);
    }

    assert!(unsafe { DROP_COUNTER } == 10);
}

fn test_dynamic_dispatch() {
    let action: Box<dyn Action> = Box::new(ConcreteAction { factor: 21 });
    assert!(action.perform() == 42);
}

fn test_various_alignments() {
    let alignments = [1, 2, 4, 8, 16, 64];
    for &align in &alignments {
        let layout = Layout::from_size_align(32, align).unwrap();
        unsafe {
            let bytes = alloc_zeroed(layout);
            assert!(!bytes.is_null());
            assert!((bytes as usize) & (layout.align() - 1) == 0);

            for index in 0..layout.size() {
                assert!(*bytes.add(index) == 0);
                *bytes.add(index) = index as u8;
            }

            dealloc(bytes, layout);
        }
    }
}

fn test_manual_realloc() {
    let original_layout = Layout::from_size_align(16, 8).unwrap();
    let new_layout = Layout::from_size_align(32, 8).unwrap();

    unsafe {
        let ptr = alloc_zeroed(original_layout);
        assert!(!ptr.is_null());

        for i in 0..16 {
            *ptr.add(i) = i as u8;
        }

        let new_ptr = realloc(ptr, original_layout, new_layout.size());
        assert!(!new_ptr.is_null());
        assert!((new_ptr as usize) & (new_layout.align() - 1) == 0);

        for i in 0..16 {
            assert!(*new_ptr.add(i) == i as u8);
        }

        for i in 16..32 {
            *new_ptr.add(i) = i as u8;
        }

        dealloc(new_ptr, new_layout);
    }
}
