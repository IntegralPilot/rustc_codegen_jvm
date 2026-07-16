//! Small deterministic hashes for generated JVM identifiers.

pub(crate) fn short_hash(input: &str, length: usize) -> String {
    assert!(
        length <= 16,
        "FNV-1a identifier hashes are at most 16 hex digits"
    );

    let mut hash = 0xcbf29ce484222325u64;
    for byte in input.bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }

    format!("{hash:016x}")[..length].to_string()
}

/// Prefer a readable generated identifier and retain a hash only as the
/// bounded fallback for identities that would exceed the JVM-friendly limit.
pub(crate) fn readable_or_hashed_name(
    prefix: &str,
    readable_suffix: &str,
    identity: &str,
    max_len: usize,
) -> String {
    let readable = if readable_suffix.is_empty() {
        prefix.to_string()
    } else {
        format!("{prefix}_{readable_suffix}")
    };
    if readable.len() <= max_len {
        readable
    } else {
        format!("{prefix}_{}", short_hash(identity, 10))
    }
}
