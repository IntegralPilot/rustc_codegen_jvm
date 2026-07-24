use crate::ffi::OsString;
use crate::fmt;
use crate::fs::TryLockError;
use crate::hash::{Hash, Hasher};
use crate::io::{self, BorrowedCursor, IoSlice, IoSliceMut, SeekFrom};
use crate::path::{Path, PathBuf};
use crate::string::String;
use crate::sys::time::{SystemTime, UNIX_EPOCH};
use crate::time::Duration;
use crate::vec::Vec;

pub use crate::sys::fs::common::{remove_dir_all, Dir};

const OPTION_READ: i32 = 1;
const OPTION_WRITE: i32 = 1 << 1;
const OPTION_APPEND: i32 = 1 << 2;
const OPTION_TRUNCATE: i32 = 1 << 3;
const OPTION_CREATE: i32 = 1 << 4;
const OPTION_CREATE_NEW: i32 = 1 << 5;

const TYPE_FILE: u8 = 1;
const TYPE_DIRECTORY: u8 = 2;
const TYPE_SYMLINK: u8 = 3;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:lastErrorKind"]
    fn jvm_last_error_kind() -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:lastErrorMessageLength"]
    fn jvm_last_error_message_length() -> usize;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:copyLastErrorMessage"]
    fn jvm_copy_last_error_message(destination: *mut u8);

    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:open"]
    fn jvm_open(path: *const u8, path_length: usize, options: i32, creation_mode: i32) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:duplicate"]
    fn jvm_duplicate(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:close"]
    fn jvm_close(handle: u64);
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:read"]
    fn jvm_read(handle: u64, destination: *mut u8, length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:readAt"]
    fn jvm_read_at(handle: u64, destination: *mut u8, length: usize, offset: i64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:write"]
    fn jvm_write(handle: u64, source: *const u8, length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:writeAt"]
    fn jvm_write_at(handle: u64, source: *const u8, length: usize, offset: i64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:createIoVector"]
    fn jvm_create_io_vector(count: usize, read: bool) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:addIoVectorBuffer"]
    fn jvm_add_io_vector_buffer(handle: u64, buffer: *mut u8, length: usize) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:readVectored"]
    fn jvm_read_vectored(file_handle: u64, vector_handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:writeVectored"]
    fn jvm_write_vectored(file_handle: u64, vector_handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:closeIoVector"]
    fn jvm_close_io_vector(handle: u64);
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:seek"]
    fn jvm_seek(handle: u64, origin: i32, offset: i64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:tell"]
    fn jvm_tell(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:size"]
    fn jvm_size(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:truncate"]
    fn jvm_truncate(handle: u64, size: u64) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:sync"]
    fn jvm_sync(handle: u64, metadata: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:lock"]
    fn jvm_lock(handle: u64, shared: bool, blocking: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:unlock"]
    fn jvm_unlock(handle: u64) -> i32;

    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadata"]
    fn jvm_metadata(path: *const u8, path_length: usize, follow_links: bool) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:fileMetadata"]
    fn jvm_file_metadata(file_handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataSize"]
    fn jvm_metadata_size(handle: u64) -> u64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataType"]
    fn jvm_metadata_type(handle: u64) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataReadonly"]
    fn jvm_metadata_readonly(handle: u64) -> bool;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataHasPosixPermissions"]
    fn jvm_metadata_has_posix_permissions(handle: u64) -> bool;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataPermissionMode"]
    fn jvm_metadata_permission_mode(handle: u64) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataFileKey"]
    fn jvm_metadata_file_key(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:duplicateFileKey"]
    fn jvm_duplicate_file_key(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:fileKeyEquals"]
    fn jvm_file_key_equals(first: u64, second: u64) -> bool;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:fileKeyHash"]
    fn jvm_file_key_hash(handle: u64) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:closeFileKey"]
    fn jvm_close_file_key(handle: u64);
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataModifiedSeconds"]
    fn jvm_metadata_modified_seconds(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataModifiedNanos"]
    fn jvm_metadata_modified_nanos(handle: u64) -> u32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataAccessedSeconds"]
    fn jvm_metadata_accessed_seconds(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataAccessedNanos"]
    fn jvm_metadata_accessed_nanos(handle: u64) -> u32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataCreatedSeconds"]
    fn jvm_metadata_created_seconds(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:metadataCreatedNanos"]
    fn jvm_metadata_created_nanos(handle: u64) -> u32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:closeMetadata"]
    fn jvm_close_metadata(handle: u64);

    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:setPermissions"]
    fn jvm_set_permissions(
        path: *const u8,
        path_length: usize,
        readonly: bool,
        has_posix_mode: bool,
        permission_mode: i32,
    ) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:setFilePermissions"]
    fn jvm_set_file_permissions(
        handle: u64,
        readonly: bool,
        has_posix_mode: bool,
        permission_mode: i32,
    ) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:setTimes"]
    fn jvm_set_times(
        path: *const u8,
        path_length: usize,
        has_accessed: bool,
        accessed_seconds: i64,
        accessed_nanos: u32,
        has_modified: bool,
        modified_seconds: i64,
        modified_nanos: u32,
        follow_links: bool,
    ) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:setFileTimes"]
    fn jvm_set_file_times(
        handle: u64,
        has_accessed: bool,
        accessed_seconds: i64,
        accessed_nanos: u32,
        has_modified: bool,
        modified_seconds: i64,
        modified_nanos: u32,
    ) -> i32;

    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:openDirectory"]
    fn jvm_open_directory(path: *const u8, path_length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:directoryCount"]
    fn jvm_directory_count(handle: u64) -> usize;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:directoryNameLength"]
    fn jvm_directory_name_length(handle: u64, index: usize) -> usize;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:copyDirectoryName"]
    fn jvm_copy_directory_name(handle: u64, index: usize, destination: *mut u8);
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:closeDirectory"]
    fn jvm_close_directory(handle: u64);

    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:createDirectory"]
    fn jvm_create_directory(path: *const u8, path_length: usize) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:removeFile"]
    fn jvm_remove_file(path: *const u8, path_length: usize) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:removeDirectory"]
    fn jvm_remove_directory(path: *const u8, path_length: usize) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:rename"]
    fn jvm_rename(old: *const u8, old_length: usize, new: *const u8, new_length: usize) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:copy"]
    fn jvm_copy(from: *const u8, from_length: usize, to: *const u8, to_length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:hardLink"]
    fn jvm_hard_link(
        source: *const u8,
        source_length: usize,
        link: *const u8,
        link_length: usize,
    ) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:symbolicLink"]
    fn jvm_symbolic_link(
        source: *const u8,
        source_length: usize,
        link: *const u8,
        link_length: usize,
    ) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:readLink"]
    fn jvm_read_link(path: *const u8, path_length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:canonicalize"]
    fn jvm_canonicalize(path: *const u8, path_length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:copyPathResult"]
    fn jvm_copy_path_result(destination: *mut u8);
    #[link_name = "jvm:static:org/rustlang/runtime/FileSystemSupport:exists"]
    fn jvm_exists(path: *const u8, path_length: usize) -> i32;
}

pub struct File(u64);

#[derive(Clone)]
pub struct FileAttr {
    size: u64,
    permissions: FilePermissions,
    file_type: FileType,
    file_key: Option<FileKey>,
    modified: Timestamp,
    accessed: Timestamp,
    created: Timestamp,
}

pub struct ReadDir {
    handle: u64,
    index: usize,
    count: usize,
    root: PathBuf,
}

pub struct DirEntry {
    path: PathBuf,
    file_name: OsString,
}

#[derive(Clone, Debug)]
pub struct OpenOptions {
    read: bool,
    write: bool,
    append: bool,
    truncate: bool,
    create: bool,
    create_new: bool,
    creation_mode: Option<u32>,
}

#[derive(Copy, Clone, Debug, Default)]
pub struct FileTimes {
    accessed: Option<SystemTime>,
    modified: Option<SystemTime>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct FilePermissions {
    readonly: bool,
    has_posix_mode: bool,
    mode: u16,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct FileType(u8);

pub struct FileKey(u64);

#[derive(Debug)]
pub struct DirBuilder;

struct MetadataHandle(u64);

#[derive(Copy, Clone)]
struct Timestamp {
    seconds: i64,
    nanos: u32,
}

struct IoVectorHandle(u64);

impl Drop for MetadataHandle {
    fn drop(&mut self) {
        unsafe { jvm_close_metadata(self.0) };
    }
}

impl Drop for IoVectorHandle {
    fn drop(&mut self) {
        unsafe { jvm_close_io_vector(self.0) };
    }
}

impl Clone for FileKey {
    fn clone(&self) -> Self {
        let handle = unsafe { jvm_duplicate_file_key(self.0) };
        assert!(handle > 0, "the JVM returned an invalid file-key handle");
        FileKey(handle as u64)
    }
}

impl Drop for FileKey {
    fn drop(&mut self) {
        unsafe { jvm_close_file_key(self.0) };
    }
}

impl PartialEq for FileKey {
    fn eq(&self, other: &Self) -> bool {
        unsafe { jvm_file_key_equals(self.0, other.0) }
    }
}

impl Eq for FileKey {}

impl Hash for FileKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe { jvm_file_key_hash(self.0) }.hash(state);
    }
}

impl fmt::Debug for FileKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("FileKey(..)")
    }
}

impl FileAttr {
    pub fn size(&self) -> u64 {
        self.size
    }

    pub fn perm(&self) -> FilePermissions {
        self.permissions.clone()
    }

    pub fn file_type(&self) -> FileType {
        self.file_type
    }

    pub fn file_key(&self) -> Option<FileKey> {
        self.file_key.clone()
    }

    pub fn modified(&self) -> io::Result<SystemTime> {
        system_time_from_timestamp(self.modified)
    }

    pub fn accessed(&self) -> io::Result<SystemTime> {
        system_time_from_timestamp(self.accessed)
    }

    pub fn created(&self) -> io::Result<SystemTime> {
        system_time_from_timestamp(self.created)
    }
}

impl FilePermissions {
    pub fn readonly(&self) -> bool {
        self.readonly
    }

    pub fn set_readonly(&mut self, readonly: bool) {
        self.readonly = readonly;
        if self.has_posix_mode {
            if readonly {
                self.mode &= !0o222;
            } else {
                self.mode |= 0o222;
            }
        }
    }
}

impl fmt::Debug for FilePermissions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FilePermissions")
            .field("readonly", &self.readonly)
            .field("mode", &self.has_posix_mode.then_some(self.mode))
            .finish()
    }
}

impl FileTimes {
    pub fn set_accessed(&mut self, time: SystemTime) {
        self.accessed = Some(time);
    }

    pub fn set_modified(&mut self, time: SystemTime) {
        self.modified = Some(time);
    }
}

impl FileType {
    pub fn is_dir(&self) -> bool {
        self.0 == TYPE_DIRECTORY
    }

    pub fn is_file(&self) -> bool {
        self.0 == TYPE_FILE
    }

    pub fn is_symlink(&self) -> bool {
        self.0 == TYPE_SYMLINK
    }
}

impl Hash for FileType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl fmt::Debug for FileType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FileType")
            .field("is_file", &self.is_file())
            .field("is_dir", &self.is_dir())
            .field("is_symlink", &self.is_symlink())
            .finish()
    }
}

impl fmt::Debug for ReadDir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ReadDir").field(&self.root).finish()
    }
}

impl Iterator for ReadDir {
    type Item = io::Result<DirEntry>;

    fn next(&mut self) -> Option<io::Result<DirEntry>> {
        if self.index >= self.count {
            return None;
        }
        let index = self.index;
        self.index += 1;
        let length = unsafe { jvm_directory_name_length(self.handle, index) };
        let mut bytes = vec![0; length];
        unsafe { jvm_copy_directory_name(self.handle, index, bytes.as_mut_ptr()) };
        let file_name = os_string(bytes);
        Some(Ok(DirEntry {
            path: self.root.join(&file_name),
            file_name,
        }))
    }
}

impl Drop for ReadDir {
    fn drop(&mut self) {
        unsafe { jvm_close_directory(self.handle) };
    }
}

impl DirEntry {
    pub fn path(&self) -> PathBuf {
        self.path.clone()
    }

    pub fn file_name(&self) -> OsString {
        self.file_name.clone()
    }

    pub fn metadata(&self) -> io::Result<FileAttr> {
        stat(&self.path)
    }

    pub fn file_type(&self) -> io::Result<FileType> {
        lstat(&self.path).map(|attributes| attributes.file_type())
    }
}

impl OpenOptions {
    pub fn new() -> OpenOptions {
        OpenOptions {
            read: false,
            write: false,
            append: false,
            truncate: false,
            create: false,
            create_new: false,
            creation_mode: None,
        }
    }

    pub fn read(&mut self, read: bool) {
        self.read = read;
    }

    pub fn write(&mut self, write: bool) {
        self.write = write;
    }

    pub fn append(&mut self, append: bool) {
        self.append = append;
    }

    pub fn truncate(&mut self, truncate: bool) {
        self.truncate = truncate;
    }

    pub fn create(&mut self, create: bool) {
        self.create = create;
    }

    pub fn create_new(&mut self, create_new: bool) {
        self.create_new = create_new;
    }

    pub fn mode(&mut self, mode: u32) {
        self.creation_mode = Some(mode);
    }

    fn creation_mode(&self) -> io::Result<i32> {
        if !self.create && !self.create_new {
            return Ok(-1);
        }
        match self.creation_mode {
            Some(mode) if mode & !0o777 != 0 => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "JVM POSIX creation mode contains unsupported bits",
            )),
            Some(mode) => Ok(mode as i32),
            None => Ok(-1),
        }
    }

    fn bits(&self) -> io::Result<i32> {
        if !self.read && !self.write && !self.append {
            let message = if self.create || self.create_new || self.truncate {
                "creating or truncating a file requires write or append access"
            } else {
                "must specify at least one of read, write, or append access"
            };
            return Err(io::Error::new(io::ErrorKind::InvalidInput, message));
        }
        if !self.write && !self.append && (self.create || self.create_new || self.truncate) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "creating or truncating a file requires write or append access",
            ));
        }
        if self.append && self.truncate && !self.create_new {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "creating or truncating a file requires write or append access",
            ));
        }

        Ok((if self.read { OPTION_READ } else { 0 })
            | (if self.write { OPTION_WRITE } else { 0 })
            | (if self.append { OPTION_APPEND } else { 0 })
            | (if self.truncate { OPTION_TRUNCATE } else { 0 })
            | (if self.create { OPTION_CREATE } else { 0 })
            | (if self.create_new {
                OPTION_CREATE_NEW
            } else {
                0
            }))
    }
}

impl File {
    pub fn open(path: &Path, options: &OpenOptions) -> io::Result<File> {
        let bytes = path.as_os_str().as_encoded_bytes();
        let handle = unsafe {
            jvm_open(
                bytes.as_ptr(),
                bytes.len(),
                options.bits()?,
                options.creation_mode()?,
            )
        };
        result_handle(handle).map(File)
    }

    pub fn file_attr(&self) -> io::Result<FileAttr> {
        let handle = unsafe { jvm_file_metadata(self.0) };
        file_attr_from_handle(handle)
    }

    pub fn fsync(&self) -> io::Result<()> {
        result_code(unsafe { jvm_sync(self.0, true) })
    }

    pub fn datasync(&self) -> io::Result<()> {
        result_code(unsafe { jvm_sync(self.0, false) })
    }

    pub fn lock(&self) -> io::Result<()> {
        result_code(unsafe { jvm_lock(self.0, false, true) })
    }

    pub fn lock_shared(&self) -> io::Result<()> {
        result_code(unsafe { jvm_lock(self.0, true, true) })
    }

    pub fn try_lock(&self) -> Result<(), TryLockError> {
        try_lock_result(unsafe { jvm_lock(self.0, false, false) })
    }

    pub fn try_lock_shared(&self) -> Result<(), TryLockError> {
        try_lock_result(unsafe { jvm_lock(self.0, true, false) })
    }

    pub fn unlock(&self) -> io::Result<()> {
        result_code(unsafe { jvm_unlock(self.0) })
    }

    pub fn truncate(&self, size: u64) -> io::Result<()> {
        if size > i64::MAX as u64 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "file size exceeds the JVM signed range",
            ));
        }
        result_code(unsafe { jvm_truncate(self.0, size) })
    }

    pub fn read(&self, buf: &mut [u8]) -> io::Result<usize> {
        result_count(unsafe { jvm_read(self.0, buf.as_mut_ptr(), buf.len()) })
    }

    pub fn read_vectored(&self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        if bufs.is_empty() {
            return Ok(0);
        }
        let vector = create_io_vector(bufs.len(), true)?;
        for buffer in bufs {
            result_code(unsafe {
                jvm_add_io_vector_buffer(vector.0, buffer.as_mut_ptr(), buffer.len())
            })?;
        }
        result_count(unsafe { jvm_read_vectored(self.0, vector.0) })
    }

    pub fn is_read_vectored(&self) -> bool {
        true
    }

    pub fn read_at(&self, buf: &mut [u8], offset: u64) -> io::Result<usize> {
        let offset = checked_offset(offset)?;
        result_count(unsafe { jvm_read_at(self.0, buf.as_mut_ptr(), buf.len(), offset) })
    }

    pub fn read_buf(&self, mut cursor: BorrowedCursor<'_, u8>) -> io::Result<()> {
        let read = result_count(unsafe {
            jvm_read(
                self.0,
                cursor.as_mut().as_mut_ptr().cast(),
                cursor.capacity(),
            )
        })?;
        unsafe { cursor.advance(read) };
        Ok(())
    }

    pub fn write(&self, buf: &[u8]) -> io::Result<usize> {
        result_count(unsafe { jvm_write(self.0, buf.as_ptr(), buf.len()) })
    }

    pub fn write_vectored(&self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        if bufs.is_empty() {
            return Ok(0);
        }
        let vector = create_io_vector(bufs.len(), false)?;
        for buffer in bufs {
            result_code(unsafe {
                jvm_add_io_vector_buffer(vector.0, buffer.as_ptr().cast_mut(), buffer.len())
            })?;
        }
        result_count(unsafe { jvm_write_vectored(self.0, vector.0) })
    }

    pub fn is_write_vectored(&self) -> bool {
        true
    }

    pub fn write_at(&self, buf: &[u8], offset: u64) -> io::Result<usize> {
        let offset = checked_offset(offset)?;
        result_count(unsafe { jvm_write_at(self.0, buf.as_ptr(), buf.len(), offset) })
    }

    pub fn flush(&self) -> io::Result<()> {
        Ok(())
    }

    pub fn seek(&self, position: SeekFrom) -> io::Result<u64> {
        let (origin, offset) = match position {
            SeekFrom::Start(offset) => {
                let offset = i64::try_from(offset).map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "seek position exceeds the JVM signed range",
                    )
                })?;
                (0, offset)
            }
            SeekFrom::Current(offset) => (1, offset),
            SeekFrom::End(offset) => (2, offset),
        };
        result_position(unsafe { jvm_seek(self.0, origin, offset) })
    }

    pub fn size(&self) -> Option<io::Result<u64>> {
        Some(result_position(unsafe { jvm_size(self.0) }))
    }

    pub fn tell(&self) -> io::Result<u64> {
        result_position(unsafe { jvm_tell(self.0) })
    }

    pub fn duplicate(&self) -> io::Result<File> {
        result_handle(unsafe { jvm_duplicate(self.0) }).map(File)
    }

    pub fn set_permissions(&self, permissions: FilePermissions) -> io::Result<()> {
        result_code(unsafe {
            jvm_set_file_permissions(
                self.0,
                permissions.readonly,
                permissions.has_posix_mode,
                permissions.mode.into(),
            )
        })
    }

    pub fn set_times(&self, times: FileTimes) -> io::Result<()> {
        let (has_accessed, accessed) = optional_timestamp(times.accessed)?;
        let (has_modified, modified) = optional_timestamp(times.modified)?;
        result_code(unsafe {
            jvm_set_file_times(
                self.0,
                has_accessed,
                accessed.seconds,
                accessed.nanos,
                has_modified,
                modified.seconds,
                modified.nanos,
            )
        })
    }
}

impl Drop for File {
    fn drop(&mut self) {
        unsafe { jvm_close(self.0) };
    }
}

impl fmt::Debug for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("File").field("handle", &self.0).finish()
    }
}

impl DirBuilder {
    pub fn new() -> DirBuilder {
        DirBuilder
    }

    pub fn mkdir(&self, path: &Path) -> io::Result<()> {
        result_code(with_path(path, |pointer, length| unsafe {
            jvm_create_directory(pointer, length)
        }))
    }
}

pub fn readdir(path: &Path) -> io::Result<ReadDir> {
    let bytes = path.as_os_str().as_encoded_bytes();
    let raw_handle = unsafe { jvm_open_directory(bytes.as_ptr(), bytes.len()) };
    let handle = result_handle(raw_handle)?;
    Ok(ReadDir {
        handle,
        index: 0,
        count: unsafe { jvm_directory_count(handle) },
        root: path.to_path_buf(),
    })
}

pub fn unlink(path: &Path) -> io::Result<()> {
    path_code(path, jvm_remove_file)
}

pub fn rename(old: &Path, new: &Path) -> io::Result<()> {
    two_path_code(old, new, jvm_rename)
}

pub fn set_perm(path: &Path, permissions: FilePermissions) -> io::Result<()> {
    let bytes = path.as_os_str().as_encoded_bytes();
    result_code(unsafe {
        jvm_set_permissions(
            bytes.as_ptr(),
            bytes.len(),
            permissions.readonly,
            permissions.has_posix_mode,
            permissions.mode.into(),
        )
    })
}

pub fn set_times(path: &Path, times: FileTimes) -> io::Result<()> {
    set_path_times(path, times, true)
}

pub fn set_times_nofollow(path: &Path, times: FileTimes) -> io::Result<()> {
    set_path_times(path, times, false)
}

pub fn rmdir(path: &Path) -> io::Result<()> {
    path_code(path, jvm_remove_directory)
}

pub fn exists(path: &Path) -> io::Result<bool> {
    let bytes = path.as_os_str().as_encoded_bytes();
    match unsafe { jvm_exists(bytes.as_ptr(), bytes.len()) } {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(last_error()),
    }
}

pub fn readlink(path: &Path) -> io::Result<PathBuf> {
    path_result(path, jvm_read_link)
}

pub fn symlink(original: &Path, link: &Path) -> io::Result<()> {
    two_path_code(original, link, jvm_symbolic_link)
}

pub fn link(source: &Path, destination: &Path) -> io::Result<()> {
    two_path_code(source, destination, jvm_hard_link)
}

pub fn stat(path: &Path) -> io::Result<FileAttr> {
    metadata_for_path(path, true)
}

pub fn lstat(path: &Path) -> io::Result<FileAttr> {
    metadata_for_path(path, false)
}

pub fn canonicalize(path: &Path) -> io::Result<PathBuf> {
    path_result(path, jvm_canonicalize)
}

pub fn copy(from: &Path, to: &Path) -> io::Result<u64> {
    let from = from.as_os_str().as_encoded_bytes();
    let to = to.as_os_str().as_encoded_bytes();
    result_position(unsafe { jvm_copy(from.as_ptr(), from.len(), to.as_ptr(), to.len()) })
}

fn metadata_for_path(path: &Path, follow_links: bool) -> io::Result<FileAttr> {
    let bytes = path.as_os_str().as_encoded_bytes();
    file_attr_from_handle(unsafe { jvm_metadata(bytes.as_ptr(), bytes.len(), follow_links) })
}

fn file_attr_from_handle(raw_handle: i64) -> io::Result<FileAttr> {
    let handle = MetadataHandle(result_handle(raw_handle)?);
    let permission_mode = unsafe { jvm_metadata_permission_mode(handle.0) };
    Ok(FileAttr {
        size: unsafe { jvm_metadata_size(handle.0) },
        permissions: FilePermissions {
            readonly: unsafe { jvm_metadata_readonly(handle.0) },
            has_posix_mode: unsafe { jvm_metadata_has_posix_permissions(handle.0) },
            mode: permission_mode as u16,
        },
        file_type: FileType(unsafe { jvm_metadata_type(handle.0) } as u8),
        file_key: match unsafe { jvm_metadata_file_key(handle.0) } {
            0 => None,
            file_key_handle => {
                assert!(
                    file_key_handle > 0,
                    "the JVM returned an invalid file-key handle"
                );
                Some(FileKey(file_key_handle as u64))
            }
        },
        modified: Timestamp {
            seconds: unsafe { jvm_metadata_modified_seconds(handle.0) },
            nanos: unsafe { jvm_metadata_modified_nanos(handle.0) },
        },
        accessed: Timestamp {
            seconds: unsafe { jvm_metadata_accessed_seconds(handle.0) },
            nanos: unsafe { jvm_metadata_accessed_nanos(handle.0) },
        },
        created: Timestamp {
            seconds: unsafe { jvm_metadata_created_seconds(handle.0) },
            nanos: unsafe { jvm_metadata_created_nanos(handle.0) },
        },
    })
}

fn set_path_times(path: &Path, times: FileTimes, follow_links: bool) -> io::Result<()> {
    let bytes = path.as_os_str().as_encoded_bytes();
    let (has_accessed, accessed) = optional_timestamp(times.accessed)?;
    let (has_modified, modified) = optional_timestamp(times.modified)?;
    result_code(unsafe {
        jvm_set_times(
            bytes.as_ptr(),
            bytes.len(),
            has_accessed,
            accessed.seconds,
            accessed.nanos,
            has_modified,
            modified.seconds,
            modified.nanos,
            follow_links,
        )
    })
}

fn optional_timestamp(time: Option<SystemTime>) -> io::Result<(bool, Timestamp)> {
    match time {
        Some(time) => Ok((true, timestamp_from_system_time(time)?)),
        None => Ok((
            false,
            Timestamp {
                seconds: 0,
                nanos: 0,
            },
        )),
    }
}

fn timestamp_from_system_time(time: SystemTime) -> io::Result<Timestamp> {
    match time.sub_time(&UNIX_EPOCH) {
        Ok(duration) => Ok(Timestamp {
            seconds: i64::try_from(duration.as_secs()).map_err(|_| timestamp_overflow())?,
            nanos: duration.subsec_nanos(),
        }),
        Err(duration) => {
            let seconds = i64::try_from(duration.as_secs()).map_err(|_| timestamp_overflow())?;
            if duration.subsec_nanos() == 0 {
                Ok(Timestamp {
                    seconds: seconds.checked_neg().ok_or_else(timestamp_overflow)?,
                    nanos: 0,
                })
            } else {
                Ok(Timestamp {
                    seconds: seconds
                        .checked_add(1)
                        .and_then(i64::checked_neg)
                        .ok_or_else(timestamp_overflow)?,
                    nanos: 1_000_000_000 - duration.subsec_nanos(),
                })
            }
        }
    }
}

fn system_time_from_timestamp(timestamp: Timestamp) -> io::Result<SystemTime> {
    if timestamp.nanos >= 1_000_000_000 {
        return Err(timestamp_data_error());
    }
    let value = if timestamp.seconds >= 0 {
        UNIX_EPOCH.checked_add_duration(&Duration::new(timestamp.seconds as u64, timestamp.nanos))
    } else if timestamp.nanos == 0 {
        UNIX_EPOCH.checked_sub_duration(&Duration::from_secs(timestamp.seconds.unsigned_abs()))
    } else {
        UNIX_EPOCH.checked_sub_duration(&Duration::new(
            timestamp.seconds.unsigned_abs() - 1,
            1_000_000_000 - timestamp.nanos,
        ))
    };
    value.ok_or_else(timestamp_data_error)
}

fn timestamp_overflow() -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidInput,
        "filesystem timestamp exceeds the JVM signed range",
    )
}

fn timestamp_data_error() -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        "filesystem timestamp returned by the JVM is out of range",
    )
}

fn result_code(code: i32) -> io::Result<()> {
    if code < 0 {
        Err(last_error())
    } else {
        Ok(())
    }
}

fn result_count(value: i64) -> io::Result<usize> {
    if value < 0 {
        Err(last_error())
    } else {
        usize::try_from(value)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "JVM byte count overflow"))
    }
}

fn result_position(value: i64) -> io::Result<u64> {
    if value < 0 {
        Err(last_error())
    } else {
        Ok(value as u64)
    }
}

fn result_handle(value: i64) -> io::Result<u64> {
    if value <= 0 {
        Err(last_error())
    } else {
        Ok(value as u64)
    }
}

fn create_io_vector(count: usize, read: bool) -> io::Result<IoVectorHandle> {
    result_handle(unsafe { jvm_create_io_vector(count, read) }).map(IoVectorHandle)
}

fn checked_offset(offset: u64) -> io::Result<i64> {
    i64::try_from(offset).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "file offset exceeds the JVM signed range",
        )
    })
}

fn try_lock_result(code: i32) -> Result<(), TryLockError> {
    match code {
        0 => Ok(()),
        1 => Err(TryLockError::WouldBlock),
        _ => Err(TryLockError::Error(last_error())),
    }
}

fn last_error() -> io::Error {
    let length = unsafe { jvm_last_error_message_length() };
    let mut bytes = vec![0; length];
    unsafe { jvm_copy_last_error_message(bytes.as_mut_ptr()) };
    let message = unsafe { String::from_utf8_unchecked(bytes) };
    io::Error::new(error_kind(unsafe { jvm_last_error_kind() }), message)
}

fn error_kind(kind: i32) -> io::ErrorKind {
    match kind {
        1 => io::ErrorKind::NotFound,
        2 => io::ErrorKind::PermissionDenied,
        3 => io::ErrorKind::AlreadyExists,
        4 => io::ErrorKind::InvalidInput,
        5 => io::ErrorKind::InvalidData,
        6 => io::ErrorKind::WouldBlock,
        7 => io::ErrorKind::NotADirectory,
        8 => io::ErrorKind::IsADirectory,
        9 => io::ErrorKind::DirectoryNotEmpty,
        10 => io::ErrorKind::ReadOnlyFilesystem,
        11 => io::ErrorKind::FilesystemLoop,
        12 => io::ErrorKind::Interrupted,
        13 => io::ErrorKind::Unsupported,
        14 => io::ErrorKind::CrossesDevices,
        _ => io::ErrorKind::Other,
    }
}

fn os_string(bytes: Vec<u8>) -> OsString {
    OsString::from(unsafe { String::from_utf8_unchecked(bytes) })
}

fn path_result(
    path: &Path,
    operation: unsafe extern "C" fn(*const u8, usize) -> i64,
) -> io::Result<PathBuf> {
    let bytes = path.as_os_str().as_encoded_bytes();
    let length = operation_result(unsafe { operation(bytes.as_ptr(), bytes.len()) })?;
    let mut result = vec![0; length];
    unsafe { jvm_copy_path_result(result.as_mut_ptr()) };
    Ok(PathBuf::from(os_string(result)))
}

fn operation_result(value: i64) -> io::Result<usize> {
    result_count(value)
}

fn path_code(
    path: &Path,
    operation: unsafe extern "C" fn(*const u8, usize) -> i32,
) -> io::Result<()> {
    let bytes = path.as_os_str().as_encoded_bytes();
    result_code(unsafe { operation(bytes.as_ptr(), bytes.len()) })
}

fn two_path_code(
    first: &Path,
    second: &Path,
    operation: unsafe extern "C" fn(*const u8, usize, *const u8, usize) -> i32,
) -> io::Result<()> {
    let first = first.as_os_str().as_encoded_bytes();
    let second = second.as_os_str().as_encoded_bytes();
    result_code(unsafe { operation(first.as_ptr(), first.len(), second.as_ptr(), second.len()) })
}

fn with_path<T>(path: &Path, operation: impl FnOnce(*const u8, usize) -> T) -> T {
    let bytes = path.as_os_str().as_encoded_bytes();
    operation(bytes.as_ptr(), bytes.len())
}
