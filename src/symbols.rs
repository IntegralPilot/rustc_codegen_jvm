//! Small rlib indexes allow reuse before MIR or generated helpers are built.
use rustc_data_structures::stable_hash::{StableHash, StableHasher};
use rustc_hash::FxHashSet;
use rustc_hashes::Hash64;
use rustc_middle::ty::{Instance, TyCtxt};
use std::{
    io::{self, BufReader, Read, Seek, SeekFrom, Write},
    path::Path,
};
const MAGIC: &[u8; 8] = b"RCJVMS3\0";

pub(crate) fn key<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> u64 {
    let instance = tcx.erase_and_anonymize_regions(instance);
    tcx.with_stable_hashing_context(|mut hcx| {
        let mut hash = StableHasher::new();
        instance.stable_hash(&mut hcx, &mut hash);
        hash.finish::<Hash64>().as_u64()
    })
}

/// Availability records describe complete definitions. One codec holder can contain recipes from
/// several crates.
#[derive(Default)]
pub(crate) struct Upstream {
    pub bodies: FxHashSet<u64>,
    pub types: FxHashSet<String>,
    pub codecs: FxHashSet<String>,
}

#[derive(Default)]
pub(crate) struct Provided {
    pub bodies: Vec<u64>,
    pub types: Vec<String>,
    pub codecs: Vec<String>,
}

impl Provided {
    pub fn is_empty(&self) -> bool {
        self.bodies.is_empty() && self.types.is_empty() && self.codecs.is_empty()
    }
}

pub(crate) fn upstream(tcx: TyCtxt<'_>) -> io::Result<Upstream> {
    let mut symbols = Upstream::default();
    for &krate in tcx.crates(()) {
        let Some(path) = &tcx.used_crate_source(krate).rlib else {
            continue;
        };
        let mut reader = BufReader::new(std::fs::File::open(path)?);
        let len = reader.get_ref().metadata()?.len();
        jvm_compiler_core::classfile::archive::members(
            &mut reader,
            len,
            |reader, name, start, len| {
                if !name.ends_with(".jvmsymbols") {
                    return Ok(());
                }
                reader.seek(SeekFrom::Start(start))?;
                read(&mut reader.take(len), &mut symbols)
            },
        )?;
    }
    Ok(symbols)
}

fn read(reader: &mut impl Read, symbols: &mut Upstream) -> io::Result<()> {
    let invalid = || {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "invalid JVM symbol index; rebuild dependencies",
        )
    };
    let mut magic = [0; 8];
    reader.read_exact(&mut magic)?;
    if &magic != MAGIC {
        return Err(invalid());
    }
    loop {
        let mut kind = [0];
        if reader.read(&mut kind)? == 0 {
            return Ok(());
        }
        if kind[0] == 0 {
            let mut bytes = [0; 8];
            reader.read_exact(&mut bytes)?;
            symbols.bodies.insert(u64::from_le_bytes(bytes));
        } else if kind[0] == 1 || kind[0] == 2 {
            let mut length = [0; 4];
            reader.read_exact(&mut length)?;
            let length = u32::from_le_bytes(length) as usize;
            if length > 65535 {
                return Err(invalid());
            }
            let mut bytes = vec![0; length];
            reader.read_exact(&mut bytes)?;
            let name = String::from_utf8(bytes).map_err(|_| invalid())?;
            if kind[0] == 1 {
                symbols.types.insert(name);
            } else {
                symbols.codecs.insert(name);
            }
        } else {
            return Err(invalid());
        }
    }
}

pub(crate) fn write(path: &Path, symbols: &Provided) -> io::Result<()> {
    let mut out = std::io::BufWriter::new(std::fs::File::create(path)?);
    out.write_all(MAGIC)?;
    for symbol in &symbols.bodies {
        out.write_all(&[0])?;
        out.write_all(&symbol.to_le_bytes())?;
    }
    for (kind, names) in [(1, &symbols.types), (2, &symbols.codecs)] {
        for name in names {
            out.write_all(&[kind])?;
            out.write_all(&(u32::try_from(name.len()).map_err(io::Error::other)?).to_le_bytes())?;
            out.write_all(name.as_bytes())?;
        }
    }
    out.flush()
}
