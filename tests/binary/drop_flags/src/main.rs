#![feature(register_tool)]
#![register_tool(jvm)]

static mut DROP_TOTAL: i32 = 0;

struct DropToken(i32);

impl Drop for DropToken {
    fn drop(&mut self) {
        unsafe {
            DROP_TOTAL += self.0;
        }
    }
}

enum DropLeaf {
    Token(DropToken),
    Empty,
}

impl Drop for DropLeaf {
    fn drop(&mut self) {
        unsafe {
            DROP_TOTAL += 100;
        }
    }
}

enum DropOuter {
    #[jvm::subtype]
    Leaf(DropLeaf),
    Other(DropToken),
}

impl Drop for DropOuter {
    fn drop(&mut self) {
        unsafe {
            DROP_TOTAL += 1000;
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

    unsafe {
        DROP_TOTAL = 0;
    }
    drop(DropOuter::Leaf(DropLeaf::Token(DropToken(5))));
    assert!(unsafe { DROP_TOTAL } == 1105);
}
