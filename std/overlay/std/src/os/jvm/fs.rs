//! JVM-specific extensions to [`std::fs`](crate::fs).

#![stable(feature = "jvm_os_ext", since = "1.99.0")]

use crate::fmt;
use crate::fs;
use crate::hash::{Hash, Hasher};
use crate::io;
use crate::sys::{AsInner, AsInnerMut};

/// An opaque identifier supplied by the host filesystem.
///
/// File keys have no portable numeric representation. They can be compared or
/// hashed while the filesystem remains unchanged.
#[stable(feature = "jvm_os_ext", since = "1.99.0")]
#[derive(Clone)]
pub struct FileKey(crate::sys::fs::FileKey);

#[stable(feature = "jvm_os_ext", since = "1.99.0")]
impl PartialEq for FileKey {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[stable(feature = "jvm_os_ext", since = "1.99.0")]
impl Eq for FileKey {}

#[stable(feature = "jvm_os_ext", since = "1.99.0")]
impl Hash for FileKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[stable(feature = "jvm_os_ext", since = "1.99.0")]
impl fmt::Debug for FileKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// JVM-specific extensions to [`fs::Metadata`].
#[stable(feature = "jvm_os_ext", since = "1.99.0")]
pub trait MetadataExt {
    /// Returns an opaque identity for the file when the host provider supplies one.
    #[stable(feature = "jvm_os_ext", since = "1.99.0")]
    fn file_key(&self) -> Option<FileKey>;
}

#[stable(feature = "jvm_os_ext", since = "1.99.0")]
impl MetadataExt for fs::Metadata {
    fn file_key(&self) -> Option<FileKey> {
        self.as_inner().file_key().map(FileKey)
    }
}

/// JVM-specific extensions to [`fs::OpenOptions`].
#[stable(feature = "jvm_os_ext", since = "1.99.0")]
pub trait OpenOptionsExt {
    /// Sets POSIX permission bits to apply atomically when creating a file.
    ///
    /// Only the `0o777` read, write, and execute bits are supported. Opening
    /// returns [`io::ErrorKind::Unsupported`] if a new file must be created on
    /// a host filesystem without POSIX permissions. The host may remove
    /// permissions through mechanisms such as a process umask.
    #[stable(feature = "jvm_os_ext", since = "1.99.0")]
    fn mode(&mut self, mode: u32) -> &mut Self;
}

#[stable(feature = "jvm_os_ext", since = "1.99.0")]
impl OpenOptionsExt for fs::OpenOptions {
    fn mode(&mut self, mode: u32) -> &mut Self {
        self.as_inner_mut().mode(mode);
        self
    }
}

/// Positional file I/O that does not change the file's current cursor.
#[stable(feature = "jvm_os_ext", since = "1.99.0")]
pub trait FileExt {
    /// Reads bytes starting at `offset` without changing the current cursor.
    #[stable(feature = "jvm_os_ext", since = "1.99.0")]
    fn read_at(&self, buffer: &mut [u8], offset: u64) -> io::Result<usize>;

    /// Reads exactly enough bytes to fill `buffer` without changing the cursor.
    #[stable(feature = "jvm_os_ext", since = "1.99.0")]
    fn read_exact_at(&self, mut buffer: &mut [u8], mut offset: u64) -> io::Result<()> {
        while !buffer.is_empty() {
            match self.read_at(buffer, offset) {
                Ok(0) => break,
                Ok(read) => {
                    buffer = &mut buffer[read..];
                    offset += read as u64;
                }
                Err(error) if error.is_interrupted() => {}
                Err(error) => return Err(error),
            }
        }
        if buffer.is_empty() {
            Ok(())
        } else {
            Err(io::Error::READ_EXACT_EOF)
        }
    }

    /// Writes bytes starting at `offset` without changing the current cursor.
    #[stable(feature = "jvm_os_ext", since = "1.99.0")]
    fn write_at(&self, buffer: &[u8], offset: u64) -> io::Result<usize>;

    /// Writes the entire buffer at `offset` without changing the cursor.
    #[stable(feature = "jvm_os_ext", since = "1.99.0")]
    fn write_all_at(&self, mut buffer: &[u8], mut offset: u64) -> io::Result<()> {
        while !buffer.is_empty() {
            match self.write_at(buffer, offset) {
                Ok(0) => return Err(io::Error::WRITE_ALL_EOF),
                Ok(written) => {
                    buffer = &buffer[written..];
                    offset += written as u64;
                }
                Err(error) if error.is_interrupted() => {}
                Err(error) => return Err(error),
            }
        }
        Ok(())
    }
}

#[stable(feature = "jvm_os_ext", since = "1.99.0")]
impl FileExt for fs::File {
    fn read_at(&self, buffer: &mut [u8], offset: u64) -> io::Result<usize> {
        self.as_inner().read_at(buffer, offset)
    }

    fn write_at(&self, buffer: &[u8], offset: u64) -> io::Result<usize> {
        self.as_inner().write_at(buffer, offset)
    }
}
