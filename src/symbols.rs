//! Small rlib indexes allow reuse before MIR or generated helpers are built.
use rustc_data_structures::stable_hash::{StableHash, StableHasher};
use rustc_hash::FxHashSet;
use rustc_hashes::Hash64;
use rustc_middle::ty::{Instance, TyCtxt};
use std::{
    io::{self, BufReader, Read, Seek, SeekFrom, Write},
    path::Path,
};
const MAGIC: &[u8; 8] = b"RCJVMS1\0";

pub(crate) fn key<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> u64 {
    let instance = tcx.erase_and_anonymize_regions(instance);
    tcx.with_stable_hashing_context(|mut hcx| {
        let mut hash = StableHasher::new();
        instance.stable_hash(&mut hcx, &mut hash);
        hash.finish::<Hash64>().as_u64()
    })
}

pub(crate) fn upstream(tcx: TyCtxt<'_>) -> io::Result<FxHashSet<u64>> {
    let mut symbols = FxHashSet::default();
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
                let mut magic = [0; 8];
                reader.read_exact(&mut magic)?;
                if &magic != MAGIC || len < 8 || len % 8 != 0 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "invalid JVM symbol index",
                    ));
                }
                for _ in 0..(len - 8) / 8 {
                    let mut bytes = [0; 8];
                    reader.read_exact(&mut bytes)?;
                    symbols.insert(u64::from_le_bytes(bytes));
                }
                Ok(())
            },
        )?;
    }
    Ok(symbols)
}

pub(crate) fn write(path: &Path, symbols: &[u64]) -> io::Result<()> {
    let mut out = std::io::BufWriter::new(std::fs::File::create(path)?);
    out.write_all(MAGIC)?;
    for symbol in symbols {
        out.write_all(&symbol.to_le_bytes())?;
    }
    out.flush()
}
