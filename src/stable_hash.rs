//! Small deterministic hashes for generated JVM identifiers.

pub(crate) fn short_hash(input: &str, length: usize) -> String {
    assert!(length <= 16, "FNV-1a identifier hashes are at most 16 hex digits");

    let mut hash = 0xcbf29ce484222325u64;
    for byte in input.bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }

    format!("{hash:016x}")[..length].to_string()
}