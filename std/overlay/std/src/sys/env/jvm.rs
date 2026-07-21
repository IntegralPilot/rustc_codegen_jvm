pub use super::common::Env;
use crate::ffi::{OsStr, OsString};
use crate::string::String;
use crate::vec::Vec;
use crate::io;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:environmentValueLength:(Lorg/rustlang/runtime/Pointer;J)J"]
    fn environment_value_length(key: *const u8, key_length: usize) -> isize;

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:copyEnvironmentValue:(Lorg/rustlang/runtime/Pointer;JLorg/rustlang/runtime/Pointer;)V"]
    fn copy_environment_value(key: *const u8, key_length: usize, destination: *mut u8);

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:setEnvironment:(Lorg/rustlang/runtime/Pointer;JLorg/rustlang/runtime/Pointer;J)V"]
    fn set_environment(key: *const u8, key_length: usize, value: *const u8, value_length: usize);

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:removeEnvironment:(Lorg/rustlang/runtime/Pointer;J)V"]
    fn remove_environment(key: *const u8, key_length: usize);

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:beginEnvironmentSnapshot:()J"]
    fn begin_environment_snapshot() -> u64;

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:environmentSnapshotCount:(J)J"]
    fn environment_snapshot_count(snapshot: u64) -> usize;

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:environmentSnapshotKeyLength:(JJ)J"]
    fn environment_snapshot_key_length(snapshot: u64, index: usize) -> usize;

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:environmentSnapshotValueLength:(JJ)J"]
    fn environment_snapshot_value_length(snapshot: u64, index: usize) -> usize;

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:copyEnvironmentSnapshotKey:(JJLorg/rustlang/runtime/Pointer;)V"]
    fn copy_environment_snapshot_key(snapshot: u64, index: usize, destination: *mut u8);

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:copyEnvironmentSnapshotValue:(JJLorg/rustlang/runtime/Pointer;)V"]
    fn copy_environment_snapshot_value(snapshot: u64, index: usize, destination: *mut u8);

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:endEnvironmentSnapshot:(J)V"]
    fn end_environment_snapshot(snapshot: u64);
}

pub fn env() -> Env {
    struct Snapshot(u64);

    impl Drop for Snapshot {
        fn drop(&mut self) {
            unsafe { end_environment_snapshot(self.0) };
        }
    }

    let snapshot = Snapshot(unsafe { begin_environment_snapshot() });
    let count = unsafe { environment_snapshot_count(snapshot.0) };
    let mut entries = Vec::with_capacity(count);
    for index in 0..count {
        let key_length = unsafe { environment_snapshot_key_length(snapshot.0, index) };
        let value_length = unsafe { environment_snapshot_value_length(snapshot.0, index) };
        let mut key = vec![0; key_length];
        let mut value = vec![0; value_length];
        unsafe {
            copy_environment_snapshot_key(snapshot.0, index, key.as_mut_ptr());
            copy_environment_snapshot_value(snapshot.0, index, value.as_mut_ptr());
        }
        entries.push((os_string(key), os_string(value)));
    }
    Env::new(entries)
}

pub fn getenv(key: &OsStr) -> Option<OsString> {
    let key = key.as_encoded_bytes();
    let length = unsafe { environment_value_length(key.as_ptr(), key.len()) };
    if length < 0 {
        return None;
    }

    let mut bytes = vec![0; length as usize];
    unsafe { copy_environment_value(key.as_ptr(), key.len(), bytes.as_mut_ptr()) };
    Some(os_string(bytes))
}

fn os_string(bytes: Vec<u8>) -> OsString {
    OsString::from(unsafe { String::from_utf8_unchecked(bytes) })
}

pub unsafe fn setenv(key: &OsStr, value: &OsStr) -> io::Result<()> {
    let key = key.as_encoded_bytes();
    let value = value.as_encoded_bytes();
    validate_key(key)?;
    if value.contains(&0) {
        return Err(io::const_error!(
            io::ErrorKind::InvalidInput,
            "environment value contains NUL"
        ));
    }
    unsafe { set_environment(key.as_ptr(), key.len(), value.as_ptr(), value.len()) };
    Ok(())
}

pub unsafe fn unsetenv(key: &OsStr) -> io::Result<()> {
    let key = key.as_encoded_bytes();
    validate_key(key)?;
    unsafe { remove_environment(key.as_ptr(), key.len()) };
    Ok(())
}

fn validate_key(key: &[u8]) -> io::Result<()> {
    if key.is_empty() || key.contains(&0) || key.contains(&b'=') {
        Err(io::const_error!(
            io::ErrorKind::InvalidInput,
            "invalid environment variable name"
        ))
    } else {
        Ok(())
    }
}
