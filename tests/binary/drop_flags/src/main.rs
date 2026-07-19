#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

static mut DROP_TOTAL: i32 = 0;

struct DropToken(i32);

impl Drop for DropToken {
    fn drop(&mut self) {
        unsafe {
            DROP_TOTAL += self.0;
        }
    }
}

#[inline(never)]
fn conditionally_reassign(initialize_first: bool) {
    let mut token: DropToken;
    if initialize_first {
        token = DropToken(1);
    }
    token = DropToken(2);
    drop(token);
}

fn main() {
    conditionally_reassign(false);
    assert!(unsafe { DROP_TOTAL } == 2);

    unsafe {
        DROP_TOTAL = 0;
    }
    conditionally_reassign(true);
    assert!(unsafe { DROP_TOTAL } == 3);
}
