// The test harness builds this crate once before starting its worker pool.
// With the shared target directory, that warms Cargo's JVM `std` artifacts
// for every test in the selected profile.
pub fn std_is_ready() -> bool {
    true
}
