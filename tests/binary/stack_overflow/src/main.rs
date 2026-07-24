#![allow(unconditional_recursion)]

#[inline(never)]
fn overflow(depth: usize) -> usize {
    std::hint::black_box(depth);
    overflow(depth.wrapping_add(1)).wrapping_add(1)
}

fn main() {
    let result = std::panic::catch_unwind(|| overflow(0));
    panic!("stack overflow was recoverable: {result:?}");
}
