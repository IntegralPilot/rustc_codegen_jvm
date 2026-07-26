//! Small deterministic hashes for generated JVM identifiers.

use std::hash::{Hash, Hasher};

struct Fnv1aHasher(u64);

impl Default for Fnv1aHasher {
    fn default() -> Self {
        Self(0xcbf29ce484222325)
    }
}

impl Hasher for Fnv1aHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.0 ^= u64::from(*byte);
            self.0 = self.0.wrapping_mul(0x100000001b3);
        }
    }
}

pub(crate) fn short_hash_bytes(input: &[u8], length: usize) -> String {
    assert!(
        length <= 16,
        "FNV-1a identifier hashes are at most 16 hex digits"
    );

    let mut hash = 0xcbf29ce484222325u64;
    for byte in input {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }

    format!("{hash:016x}")[..length].to_string()
}

pub(crate) fn short_hash(input: &str, length: usize) -> String {
    short_hash_bytes(input.as_bytes(), length)
}

pub(crate) fn short_hash_value(value: &impl Hash, length: usize) -> String {
    assert!(
        length <= 16,
        "FNV-1a identifier hashes are at most 16 hex digits"
    );

    let mut hasher = Fnv1aHasher::default();
    value.hash(&mut hasher);
    format!("{:016x}", hasher.finish())[..length].to_string()
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
        let complete_identity = format!("{identity}\0{readable_suffix}");
        format!("{prefix}_{}", short_hash(&complete_identity, 10))
    }
}

#[cfg(test)]
mod tests {
    use super::readable_or_hashed_name;

    #[test]
    fn hashed_fallback_preserves_readable_specialization_identity() {
        let first = readable_or_hashed_name("Closure", &"First".repeat(40), "shared", 160);
        let second = readable_or_hashed_name("Closure", &"Second".repeat(40), "shared", 160);

        assert_ne!(first, second);
    }
}
