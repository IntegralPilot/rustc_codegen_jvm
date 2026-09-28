use std::hint::black_box;

#[repr(C)]
struct Node {
    data: *const u8,
    next: *const Node,
    value: u32,
}

unsafe impl Sync for Node {}

static SENTINEL: Node = Node {
    data: (&raw const SENTINEL).cast(),
    next: &raw const SENTINEL,
    value: 42,
};

mod left {
    use super::Node;
    pub(super) static NODE: Node = Node {
        data: (&raw const super::right::NODE).cast(),
        next: &raw const super::right::NODE,
        value: 11,
    };
}

mod right {
    use super::Node;
    pub(super) static NODE: Node = Node {
        data: (&raw const super::left::NODE).cast(),
        next: &raw const super::left::NODE,
        value: 22,
    };
}

struct Ring {
    next: &'static [Ring],
    value: u32,
}

static RING: [Ring; 2] = [
    Ring {
        next: &RING,
        value: 33,
    },
    Ring {
        next: &RING,
        value: 44,
    },
];

pub fn run() {
    let sentinel = black_box(&SENTINEL);
    assert!(!sentinel.data.is_null());
    assert!(std::ptr::eq(
        sentinel.data,
        (sentinel as *const Node).cast()
    ));
    assert!(std::ptr::eq(sentinel.next, sentinel));
    assert_eq!(unsafe { (*sentinel.next).value }, 42);

    let left = black_box(&left::NODE);
    let right = black_box(&right::NODE);
    assert!(std::ptr::eq(left.next, right));
    assert!(std::ptr::eq(right.next, left));
    assert_eq!(unsafe { (*(*left.next).next).value }, 11);

    let ring = black_box(&RING);
    assert!(std::ptr::eq(ring[0].next.as_ptr(), ring.as_ptr()));
    assert_eq!(ring[0].next.len(), 2);
    assert_eq!(ring[0].next[1].value, 44);
    assert_eq!(ring[1].next[0].value, 33);
}
