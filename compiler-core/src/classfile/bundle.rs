//! Streaming shard bundles. Records remain readable for exact duplicate checks
//! without retaining the class bytes in the compiler heap.
use std::{
    fs::File,
    io::{self, BufWriter, Read, Seek, SeekFrom, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

pub const MAGIC: &[u8; 8] = b"RCJVMB1\0";

pub struct Writer {
    output: BufWriter<File>,
    path: Arc<PathBuf>,
    position: u64,
    records: usize,
}

#[derive(Clone)]
pub(super) struct Record {
    path: Arc<PathBuf>,
    offset: u64,
}

impl Writer {
    pub fn create(path: &Path) -> io::Result<Self> {
        let mut output = BufWriter::new(File::create(path)?);
        output.write_all(MAGIC)?;
        Ok(Self {
            output,
            path: Arc::new(path.to_owned()),
            position: MAGIC.len() as u64,
            records: 0,
        })
    }

    pub fn is_empty(&self) -> bool {
        self.records == 0
    }

    pub(super) fn append(&mut self, name: &str, bytes: &[u8]) -> io::Result<Record> {
        let name_len = u32::try_from(name.len())
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
        let offset = self.position + 12 + u64::from(name_len);
        self.output.write_all(&name_len.to_le_bytes())?;
        self.output.write_all(&(bytes.len() as u64).to_le_bytes())?;
        self.output.write_all(name.as_bytes())?;
        self.output.write_all(bytes)?;
        // A published record must be visible to another worker immediately.
        self.output.flush()?;
        self.position = offset + bytes.len() as u64;
        self.records += 1;
        Ok(Record {
            path: Arc::clone(&self.path),
            offset,
        })
    }
}

impl Record {
    pub(super) fn equals(&self, bytes: &[u8]) -> io::Result<bool> {
        // Open only for a candidate comparison: retaining a file descriptor for
        // every owner shard would exhaust the host limit on large crates.
        let mut reader = File::open(&*self.path)?;
        reader.seek(SeekFrom::Start(self.offset))?;
        let mut buffer = [0u8; 8192];
        for expected in bytes.chunks(buffer.len()) {
            let actual = &mut buffer[..expected.len()];
            reader.read_exact(actual)?;
            if actual != expected {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

pub fn read_magic(reader: &mut impl Read) -> io::Result<()> {
    let mut magic = [0; MAGIC.len()];
    reader.read_exact(&mut magic)?;
    if &magic != MAGIC {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "invalid JVM class bundle magic",
        ));
    }
    Ok(())
}

/// Read one record, distinguishing a clean end of the bundle from truncation.
pub fn read_record(reader: &mut impl Read) -> io::Result<Option<(String, Vec<u8>)>> {
    let mut header = [0u8; 12];
    loop {
        match reader.read(&mut header[..1]) {
            Ok(0) => return Ok(None),
            Ok(_) => break,
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        }
    }
    reader.read_exact(&mut header[1..])?;
    let name_len = u32::from_le_bytes(header[..4].try_into().unwrap()) as usize;
    let byte_len = usize::try_from(u64::from_le_bytes(header[4..].try_into().unwrap()))
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
    let mut name = vec![0; name_len];
    reader.read_exact(&mut name)?;
    let name =
        String::from_utf8(name).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
    let mut bytes = vec![0; byte_len];
    reader.read_exact(&mut bytes)?;
    Ok(Some((name, bytes)))
}
