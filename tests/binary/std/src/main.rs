use std::collections::{hash_map::DefaultHasher, HashSet};
use std::env;
use std::fs::{self, File, FileTimes, OpenOptions};
use std::hash::{Hash, Hasher};
use std::io::{
    self, BufRead, BufReader, BufWriter, IoSlice, IoSliceMut, Read, Seek, SeekFrom, Write,
};
use std::net::Ipv6Addr;
use std::os::jvm::fs::{FileExt, MetadataExt, OpenOptionsExt};
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

fn main() {
    test_arguments();
    test_environment();
    test_stdin();
    test_pointer_formatting();
    test_ipv6_formatting();
    test_filesystem();
    println!("std support ok");
}

fn test_arguments() {
    let arguments = env::args().collect::<Vec<_>>();
    assert_eq!(arguments, ["rust-jvm", "alpha", "two words", "café"]);

    let os_arguments = env::args_os()
        .map(|argument| argument.into_string().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(os_arguments, arguments);
}

fn test_environment() {
    const KEY: &str = "RCJ_STDLIB_ENV_TEST";
    unsafe { env::set_var(KEY, "value=with=equals") };
    assert_eq!(env::var(KEY).unwrap(), "value=with=equals");

    let entries = env::vars().collect::<Vec<_>>();
    assert!(entries
        .iter()
        .any(|(key, value)| { key == KEY && value == "value=with=equals" }));
    assert!(env::vars_os().any(|(key, value)| { key == KEY && value == "value=with=equals" }));

    unsafe { env::remove_var(KEY) };
    assert!(env::var_os(KEY).is_none());
    assert!(!env::vars().any(|(key, _)| key == KEY));
}

fn test_stdin() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    assert_eq!(input, "first line\nsecond café line\n");
}

fn test_pointer_formatting() {
    let bytes: &[u8] = b"";
    let text: &str = "";
    assert_eq!(format!("{text:p}"), format!("{:p}", text as *const _));
    assert_eq!(format!("{bytes:p}"), format!("{:p}", bytes as *const _));
}

fn test_ipv6_formatting() {
    let address = Ipv6Addr::new(0xae, 0, 0, 0, 0, 0xffff, 0x0102, 0x0304);
    assert_eq!(
        address.segments(),
        [0xae, 0, 0, 0, 0, 0xffff, 0x0102, 0x0304]
    );
    assert_eq!(address.to_string(), "ae::ffff:102:304");
}

fn test_filesystem() {
    let root = PathBuf::from("target/std-fs-self-test");
    if root.exists() {
        fs::remove_dir_all(&root).unwrap();
    }
    fs::create_dir_all(root.join("nested/deeper")).unwrap();

    test_file_io(&root);
    test_open_options_and_clone(&root);
    test_creation_modes(&root);
    test_metadata_permissions_and_times(&root);
    test_directories_and_paths(&root);
    test_links(&root);
    test_filesystem_errors(&root);

    fs::remove_dir_all(&root).unwrap();
    assert!(!root.exists());
}

fn test_file_io(root: &Path) {
    let path = root.join("héllo.txt");
    fs::write(&path, b"hello from rust on jvm").unwrap();
    assert_eq!(fs::read(&path).unwrap(), b"hello from rust on jvm");
    assert_eq!(fs::read_to_string(&path).unwrap(), "hello from rust on jvm");

    let buffered = root.join("buffered.txt");
    {
        let mut writer = BufWriter::new(File::create(&buffered).unwrap());
        writer.write_all(b"first\nsecond\n").unwrap();
        writer.flush().unwrap();
    }
    let lines = BufReader::new(File::open(&buffered).unwrap())
        .lines()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    assert_eq!(lines, ["first", "second"]);

    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(&path)
        .unwrap();
    file.seek(SeekFrom::Start(6)).unwrap();
    file.write_all(b"FROM").unwrap();
    file.seek(SeekFrom::Current(-4)).unwrap();
    let mut replacement = [0; 4];
    file.read_exact(&mut replacement).unwrap();
    assert_eq!(&replacement, b"FROM");
    file.seek(SeekFrom::End(-3)).unwrap();
    let mut suffix = String::new();
    file.read_to_string(&mut suffix).unwrap();
    assert_eq!(suffix, "jvm");

    file.set_len(32).unwrap();
    assert_eq!(file.metadata().unwrap().len(), 32);
    file.seek(SeekFrom::Start(31)).unwrap();
    let mut last = [1];
    file.read_exact(&mut last).unwrap();
    assert_eq!(last, [0]);
    file.set_len(5).unwrap();
    assert_eq!(file.metadata().unwrap().len(), 5);
    file.sync_data().unwrap();
    file.sync_all().unwrap();

    let vectored = root.join("vectored.bin");
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(&vectored)
        .unwrap();
    let written = file
        .write_vectored(&[
            IoSlice::new(b"vector"),
            IoSlice::new(b""),
            IoSlice::new(b"-write"),
        ])
        .unwrap();
    assert_eq!(written, 12);
    assert_eq!(file.stream_position().unwrap(), 12);

    file.seek(SeekFrom::Start(0)).unwrap();
    let mut first = [0; 3];
    let mut empty = [];
    let mut second = [0; 5];
    let mut buffers = [
        IoSliceMut::new(&mut first),
        IoSliceMut::new(&mut empty),
        IoSliceMut::new(&mut second),
    ];
    assert_eq!(file.read_vectored(&mut buffers).unwrap(), 8);
    assert_eq!(&first, b"vec");
    assert_eq!(&second, b"tor-w");

    file.seek(SeekFrom::Start(7)).unwrap();
    file.write_all_at(b"XY", 1).unwrap();
    assert_eq!(file.stream_position().unwrap(), 7);
    let mut positional = [0; 6];
    file.read_exact_at(&mut positional, 0).unwrap();
    assert_eq!(&positional, b"vXYtor");
    assert_eq!(file.stream_position().unwrap(), 7);
    assert_eq!(
        file.read_at(&mut positional, u64::MAX).unwrap_err().kind(),
        io::ErrorKind::InvalidInput
    );
}

fn test_open_options_and_clone(root: &Path) {
    let path = root.join("options.txt");
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create_new(true)
        .open(&path)
        .unwrap();
    file.write_all(b"abc").unwrap();
    let mut cloned = file.try_clone().unwrap();
    cloned.write_all(b"def").unwrap();
    assert_eq!(file.stream_position().unwrap(), 6);

    OpenOptions::new()
        .append(true)
        .open(&path)
        .unwrap()
        .write_all(b"ghi")
        .unwrap();
    assert_eq!(fs::read(&path).unwrap(), b"abcdefghi");

    OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(&path)
        .unwrap()
        .write_all(b"x")
        .unwrap();
    assert_eq!(fs::read(&path).unwrap(), b"x");

    let invalid = OpenOptions::new().open(&path).unwrap_err();
    assert_eq!(invalid.kind(), io::ErrorKind::InvalidInput);
    let existing = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .unwrap_err();
    assert_eq!(existing.kind(), io::ErrorKind::AlreadyExists);

    let locking = OpenOptions::new()
        .read(true)
        .write(true)
        .open(&path)
        .unwrap();
    locking.lock().unwrap();
    assert!(matches!(
        locking.try_clone().unwrap().try_lock(),
        Err(fs::TryLockError::WouldBlock)
    ));
    locking.unlock().unwrap();
    locking.try_lock_shared().unwrap();
    locking.unlock().unwrap();
}

fn test_creation_modes(root: &Path) {
    let existing = root.join("mode-existing.txt");
    fs::write(&existing, b"existing").unwrap();
    let was_readonly = fs::metadata(&existing).unwrap().permissions().readonly();
    OpenOptions::new()
        .write(true)
        .create(true)
        .mode(0o000)
        .open(&existing)
        .unwrap();
    assert_eq!(
        fs::metadata(&existing).unwrap().permissions().readonly(),
        was_readonly,
        "creation mode must not modify an existing file"
    );

    OpenOptions::new()
        .read(true)
        .mode(u32::MAX)
        .open(&existing)
        .unwrap();

    let invalid = root.join("invalid-mode.txt");
    let error = OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o1000)
        .open(&invalid)
        .unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
    assert!(!invalid.exists());

    let readonly = root.join("readonly-at-creation.txt");
    match OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o444)
        .open(&readonly)
    {
        Ok(file) => {
            drop(file);
            assert!(fs::metadata(&readonly).unwrap().permissions().readonly());
        }
        Err(error) if error.kind() == io::ErrorKind::Unsupported => {
            assert!(
                !readonly.exists(),
                "unsupported atomic permissions must not leave a file behind"
            );
        }
        Err(error) => panic!("unexpected creation-mode error: {error}"),
    }
}

fn test_metadata_permissions_and_times(root: &Path) {
    let path = root.join("metadata.bin");
    fs::write(&path, [1, 2, 3, 4]).unwrap();
    let metadata = fs::metadata(&path).unwrap();
    assert!(metadata.is_file());
    assert!(!metadata.is_dir());
    assert!(!metadata.file_type().is_symlink());
    assert_eq!(metadata.len(), 4);
    metadata.modified().unwrap();
    metadata.accessed().unwrap();
    metadata.created().unwrap();

    match (metadata.file_key(), fs::metadata(&path).unwrap().file_key()) {
        (Some(first), Some(second)) => {
            assert_eq!(first, second);
            assert_eq!(file_key_hash(&first), file_key_hash(&second));
            let mut keys = HashSet::new();
            assert!(keys.insert(first.clone()));
            assert!(!keys.insert(second));
            assert_eq!(format!("{first:?}"), "FileKey(..)");
        }
        (None, None) => {}
        _ => panic!("the filesystem returned an inconsistent file key"),
    }
    if let (Some(file), Some(directory)) =
        (metadata.file_key(), fs::metadata(root).unwrap().file_key())
    {
        assert_ne!(file, directory);
    }

    let mut permissions = metadata.permissions();
    permissions.set_readonly(true);
    fs::set_permissions(&path, permissions.clone()).unwrap();
    assert!(fs::metadata(&path).unwrap().permissions().readonly());
    permissions.set_readonly(false);
    File::open(&path)
        .unwrap()
        .set_permissions(permissions)
        .unwrap();
    assert!(!fs::metadata(&path).unwrap().permissions().readonly());

    let timestamp = SystemTime::UNIX_EPOCH + Duration::new(1_700_000_000, 123_456_700);
    File::open(&path)
        .unwrap()
        .set_times(
            FileTimes::new()
                .set_accessed(timestamp)
                .set_modified(timestamp),
        )
        .unwrap();
    let updated = fs::metadata(&path).unwrap();
    assert_eq!(
        updated
            .modified()
            .unwrap()
            .duration_since(timestamp)
            .unwrap(),
        Duration::ZERO
    );
    assert_eq!(
        updated
            .accessed()
            .unwrap()
            .duration_since(timestamp)
            .unwrap(),
        Duration::ZERO
    );
}

fn test_directories_and_paths(root: &Path) {
    let source = root.join("source.txt");
    let copied = root.join("copied.txt");
    let renamed = root.join("renamed.txt");
    fs::write(&source, b"copy me").unwrap();
    assert_eq!(fs::copy(&source, &copied).unwrap(), 7);
    assert_eq!(fs::read(&copied).unwrap(), b"copy me");
    fs::rename(&copied, &renamed).unwrap();
    assert!(!copied.exists());
    assert!(renamed.exists());

    let canonical = fs::canonicalize(&renamed).unwrap();
    assert!(canonical.is_absolute(), "canonical path: {canonical:?}");
    assert!(canonical.ends_with("renamed.txt"));

    let entries = fs::read_dir(root)
        .unwrap()
        .map(|entry| {
            let entry = entry.unwrap();
            entry.file_type().unwrap();
            entry.metadata().unwrap();
            entry.file_name().into_string().unwrap()
        })
        .collect::<Vec<_>>();
    assert!(entries.contains(&"source.txt".to_string()));
    assert!(entries.contains(&"renamed.txt".to_string()));
    assert!(entries.contains(&"nested".to_string()));

    fs::remove_file(&renamed).unwrap();
    fs::remove_file(&source).unwrap();
    fs::remove_dir_all(root.join("nested")).unwrap();
}

fn test_links(root: &Path) {
    let source = root.join("link-source.txt");
    let hard = root.join("hard-link.txt");
    fs::write(&source, b"linked").unwrap();
    fs::hard_link(&source, &hard).unwrap();
    assert_eq!(fs::read(&hard).unwrap(), b"linked");
    assert_eq!(
        fs::metadata(&source).unwrap().file_key(),
        fs::metadata(&hard).unwrap().file_key()
    );
    fs::remove_file(&hard).unwrap();

    let symbolic = root.join("symbolic-link.txt");
    match fs::soft_link(Path::new("link-source.txt"), &symbolic) {
        Ok(()) => {
            assert!(fs::symlink_metadata(&symbolic)
                .unwrap()
                .file_type()
                .is_symlink());
            assert_eq!(
                fs::read_link(&symbolic).unwrap(),
                Path::new("link-source.txt")
            );
            assert_eq!(fs::read(&symbolic).unwrap(), b"linked");
            assert_eq!(
                fs::metadata(&symbolic).unwrap().file_key(),
                fs::metadata(&source).unwrap().file_key()
            );
            if let (Some(link), Some(target)) = (
                fs::symlink_metadata(&symbolic).unwrap().file_key(),
                fs::metadata(&source).unwrap().file_key(),
            ) {
                assert_ne!(link, target);
            }
            fs::remove_file(&symbolic).unwrap();
        }
        Err(error)
            if matches!(
                error.kind(),
                io::ErrorKind::PermissionDenied | io::ErrorKind::Unsupported
            ) => {}
        Err(error) => panic!("unexpected symbolic-link error: {error}"),
    }
    fs::remove_file(source).unwrap();
}

fn file_key_hash(key: &impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

fn test_filesystem_errors(root: &Path) {
    assert_eq!(
        fs::read_dir(Path::new("")).unwrap_err().kind(),
        io::ErrorKind::NotFound
    );

    let missing = root.join("missing");
    assert_eq!(
        fs::read(&missing).unwrap_err().kind(),
        io::ErrorKind::NotFound
    );
    assert!(!missing.exists());
    assert_eq!(missing.try_exists().unwrap(), false);

    let nonempty = root.join("nonempty");
    fs::create_dir(&nonempty).unwrap();
    fs::write(nonempty.join("child"), b"x").unwrap();
    assert_eq!(
        fs::remove_dir(&nonempty).unwrap_err().kind(),
        io::ErrorKind::DirectoryNotEmpty
    );
    assert_eq!(
        fs::remove_file(&nonempty).unwrap_err().kind(),
        io::ErrorKind::IsADirectory
    );
    assert_eq!(
        fs::read_dir(nonempty.join("child")).unwrap_err().kind(),
        io::ErrorKind::NotADirectory
    );
    fs::remove_dir_all(nonempty).unwrap();
}
