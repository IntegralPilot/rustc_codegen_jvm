#[inline(never)]
pub fn answer() -> u32 {
    std::hint::black_box(42)
}
