#![allow(unconditional_recursion)]

#[inline(never)]
fn overflow(depth: usize) -> usize {
    std::hint::black_box(depth);
    overflow(depth.wrapping_add(1)).wrapping_add(1)
}

fn main() {
    println!("value = {}", overflow(0));
}
