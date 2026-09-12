use super::bundle::{Record, Writer};
use rustc_hash::{FxHashMap, FxHasher};
use std::{
    hash::Hasher,
    io,
    sync::{Arc, Mutex},
};

type Variants = Mutex<Vec<Variant>>;

/// Class bytes live only in their shard bundle. Hashes filter comparisons; an
/// exact byte comparison still decides whether two contributions are identical.
#[derive(Default)]
pub struct ClassRegistry {
    classes: Mutex<FxHashMap<String, Arc<Variants>>>,
}

struct Variant {
    hash: u64,
    len: usize,
    record: Record,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Emission {
    Duplicate,
    Written { name_collision: bool },
}

impl ClassRegistry {
    pub fn emit(&self, output: &mut Writer, name: &str, bytes: &[u8]) -> io::Result<Emission> {
        let mut hasher = FxHasher::default();
        hasher.write(bytes);
        self.emit_hashed(output, name, bytes, hasher.finish())
    }

    fn emit_hashed(
        &self,
        output: &mut Writer,
        name: &str,
        bytes: &[u8],
        hash: u64,
    ) -> io::Result<Emission> {
        let variants = {
            let mut classes = self
                .classes
                .lock()
                .map_err(|_| io::Error::other("class registry lock poisoned"))?;
            if let Some(variants) = classes.get(name) {
                Arc::clone(variants)
            } else {
                let variants = Arc::default();
                classes.insert(name.to_owned(), Arc::clone(&variants));
                variants
            }
        };
        // Only workers emitting this same class wait during a comparison/write.
        // Publishing follows the successful write, so I/O failure cannot leave a
        // reservation that another worker waits for forever.
        let mut variants = variants
            .lock()
            .map_err(|_| io::Error::other("class variants lock poisoned"))?;
        for variant in variants
            .iter()
            .filter(|v| v.hash == hash && v.len == bytes.len())
        {
            if variant.record.equals(bytes)? {
                return Ok(Emission::Duplicate);
            }
        }
        let name_collision = !variants.is_empty();
        let record = output.append(name, bytes)?;
        variants.push(Variant {
            hash,
            len: bytes.len(),
            record,
        });
        Ok(Emission::Written { name_collision })
    }
}

#[cfg(test)]
mod tests;
