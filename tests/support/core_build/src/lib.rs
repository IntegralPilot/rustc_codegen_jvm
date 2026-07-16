#![no_std]

// The test harness builds this crate once before starting its worker pool.
// With the shared target directory, that warms Cargo's real `core`, `alloc`
// and `compiler_builtins` artifacts for every test in the selected profile.
pub fn core_is_ready() -> bool {
    true
}
