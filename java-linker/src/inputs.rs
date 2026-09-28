use crate::*;

use jvm_compiler_core::classfile::summary;
use std::io::SeekFrom;

#[derive(Clone, Copy, Debug)]
pub(crate) struct Fragment {
    pub(crate) file: usize,
    pub(crate) offset: u64,
    pub(crate) len: usize,
}

#[derive(Debug)]
pub(crate) struct Group {
    pub(crate) name: String,
    pub(crate) fragments: Vec<Fragment>,
    pub(crate) bytes: usize,
}

/// The linker retains byte ranges, not all input archives and class bodies.
#[derive(Default)]
pub(crate) struct Index {
    pub(crate) paths: Vec<PathBuf>,
    pub(crate) groups: Vec<Group>,
    pub(crate) mains: HashSet<String>,
    positions: HashMap<String, usize>,
    scratch: Vec<u8>,
}

fn invalid(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.into())
}

fn range_end(start: u64, len: u64, limit: u64) -> io::Result<u64> {
    start
        .checked_add(len)
        .filter(|&end| end <= limit)
        .ok_or_else(|| invalid("input record extends past its containing file/member"))
}

impl Index {
    pub(crate) fn collect(
        classes: &[String],
        bundles: &[String],
        archives: &[String],
    ) -> io::Result<Self> {
        let mut index = Self::default();
        for (paths, kind) in [(classes, 0), (bundles, 1), (archives, 2)] {
            for path in paths {
                let path = PathBuf::from(path);
                if !path.is_file() {
                    eprintln!(
                        "Warning: Input is not a file: {}. Skipping.",
                        path.display()
                    );
                    continue;
                }
                let mut reader = BufReader::new(fs::File::open(&path)?);
                let len = reader.get_ref().metadata()?.len();
                let file = index.paths.len();
                index.paths.push(path.clone());
                let result = match kind {
                    0 => index.record(&mut reader, file, 0, len, None),
                    1 => index.bundle(&mut reader, file, 0, len),
                    _ => index.archive(&mut reader, file, len),
                };
                result.map_err(|error| {
                    io::Error::new(error.kind(), format!("{}: {error}", path.display()))
                })?;
            }
        }
        // Construction-only lookup/scratch does not overlap class merging.
        index.positions = HashMap::default();
        index.scratch = Vec::new();
        Ok(index)
    }

    fn record(
        &mut self,
        reader: &mut (impl Read + Seek),
        file: usize,
        offset: u64,
        len: u64,
        name: Option<String>,
    ) -> io::Result<()> {
        let len = usize::try_from(len).map_err(|_| invalid("class exceeds host address space"))?;
        reader.seek(SeekFrom::Start(offset))?;
        self.scratch.resize(len, 0);
        reader.read_exact(&mut self.scratch)?;
        let summary = summary::read(&self.scratch)?;
        if summary.has_main {
            self.mains.insert(summary.name.clone());
        }
        let name = name.unwrap_or(summary.name) + ".class";
        let next = self.groups.len();
        let &mut position = self.positions.entry(name.clone()).or_insert(next);
        if position == next {
            self.groups.push(Group {
                name,
                fragments: Vec::new(),
                bytes: 0,
            });
        }
        let group = &mut self.groups[position];
        group.bytes = group
            .bytes
            .checked_add(len)
            .ok_or_else(|| invalid("class group size overflow"))?;
        group.fragments.push(Fragment { file, offset, len });
        Ok(())
    }

    fn bundle(
        &mut self,
        reader: &mut (impl Read + Seek),
        file: usize,
        start: u64,
        end: u64,
    ) -> io::Result<()> {
        let mut offset = range_end(start, CLASS_BUNDLE_MAGIC.len() as u64, end)?;
        reader.seek(SeekFrom::Start(start))?;
        bundle::read_magic(reader)?;
        while offset < end {
            let name_start = range_end(offset, 12, end)?;
            let mut header = [0; 12];
            reader.read_exact(&mut header)?;
            let name_len = u32::from_le_bytes(header[..4].try_into().unwrap());
            let byte_len = u64::from_le_bytes(header[4..].try_into().unwrap());
            let data_start = range_end(name_start, u64::from(name_len), end)?;
            let data_end = range_end(data_start, byte_len, end)?;
            let mut name = vec![0; name_len as usize];
            reader.read_exact(&mut name)?;
            let name = String::from_utf8(name).map_err(|e| invalid(e.to_string()))?;
            self.record(reader, file, data_start, byte_len, Some(name))?;
            offset = data_end;
        }
        Ok(())
    }

    fn archive(
        &mut self,
        reader: &mut (impl Read + Seek),
        file: usize,
        end: u64,
    ) -> io::Result<()> {
        jvm_compiler_core::classfile::archive::members(reader, end, |reader, name, start, len| {
            if len >= 4 {
                reader.seek(SeekFrom::Start(start))?;
                let mut prefix = [0; 8];
                let count = len.min(8) as usize;
                reader.read_exact(&mut prefix[..count])?;
                if count == 8 && &prefix == CLASS_BUNDLE_MAGIC {
                    self.bundle(reader, file, start, start + len)?;
                } else if name.ends_with(".class") || prefix[..4] == *b"\xca\xfe\xba\xbe" {
                    self.record(reader, file, start, len, None)?;
                }
            }
            Ok(())
        })
    }
}

/// Keep frequently reused archives open across all merge batches. Positional
/// reads let workers share them without reopening or racing on a seek cursor.
#[derive(Default)]
pub(crate) struct Readers {
    files: HashMap<usize, InputFile>,
}

impl Readers {
    pub(crate) fn open(index: &Index) -> io::Result<Self> {
        let mut uses = vec![0usize; index.paths.len()];
        for group in &index.groups {
            for fragment in &group.fragments {
                uses[fragment.file] += 1;
            }
        }
        let mut frequent: Vec<_> = uses
            .into_iter()
            .enumerate()
            .filter(|(_, n)| *n > 1)
            .collect();
        frequent.sort_unstable_by_key(|&(file, n)| (std::cmp::Reverse(n), file));
        let files = frequent
            .into_iter()
            .take(64)
            .map(|(file, _)| Ok((file, InputFile::open(&index.paths[file])?)))
            .collect::<io::Result<_>>()?;
        Ok(Self { files })
    }

    pub(crate) fn load(&self, paths: &[PathBuf], group: &Group) -> io::Result<Vec<ClassInfo>> {
        group
            .fragments
            .iter()
            .map(|fragment| {
                let mut data = vec![0; fragment.len];
                if let Some(file) = self.files.get(&fragment.file) {
                    file.read(&mut data, fragment.offset)?;
                } else {
                    InputFile::open(&paths[fragment.file])?.read(&mut data, fragment.offset)?;
                }
                Ok(ClassInfo {
                    jar_entry_name: group.name.clone(),
                    data,
                })
            })
            .collect()
    }
}

struct InputFile {
    file: fs::File,
    #[cfg(not(unix))]
    cursor: std::sync::Mutex<()>,
}

impl InputFile {
    fn open(path: &Path) -> io::Result<Self> {
        Ok(Self {
            file: fs::File::open(path)?,
            #[cfg(not(unix))]
            cursor: std::sync::Mutex::new(()),
        })
    }

    fn read(&self, data: &mut [u8], offset: u64) -> io::Result<()> {
        #[cfg(unix)]
        {
            use std::os::unix::fs::FileExt;
            self.file.read_exact_at(data, offset)
        }
        #[cfg(not(unix))]
        {
            let _cursor = self
                .cursor
                .lock()
                .map_err(|_| io::Error::other("input file lock poisoned"))?;
            let mut file = &self.file;
            file.seek(SeekFrom::Start(offset))?;
            file.read_exact(data)
        }
    }
}
