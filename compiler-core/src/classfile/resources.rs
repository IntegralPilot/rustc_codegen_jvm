//! Binary constant payloads, independent of Rust allocation identity.
pub const BUNDLE_PREFIX: &str = "@resource:";
pub const DIRECTORY: &str = "META-INF/rust-data/";
pub const BLOCK_BYTES: usize = 64 * 1024;

#[derive(Clone, Debug)]
pub struct Resource {
    pub name: String,
    pub bytes: Vec<u8>,
}

impl Resource {
    pub fn new(bytes: Vec<u8>) -> Self {
        // Two byte hashes make names deterministic across hosts.
        // The linker compares complete payloads and rejects hash collisions.
        let mut a = 0xcbf29ce484222325u64;
        let mut b = 0x84222325cbf29ce4u64;
        for &byte in &bytes {
            a = (a ^ u64::from(byte)).wrapping_mul(0x100000001b3);
            b = (b ^ u64::from(byte)).wrapping_mul(0x9e3779b185ebca87);
        }
        Self {
            name: format!("{DIRECTORY}{a:016x}{b:016x}"),
            bytes,
        }
    }
}

pub fn valid_name(name: &str) -> bool {
    name.strip_prefix(DIRECTORY)
        .is_some_and(|hash| hash.len() == 32 && hash.bytes().all(|b| b.is_ascii_hexdigit()))
}
