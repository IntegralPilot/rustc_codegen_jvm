use super::*;
use crate::classfile::bundle::{read_magic, read_record};
use std::{
    fs,
    io::Cursor,
    sync::atomic::{AtomicUsize, Ordering},
};

static NEXT: AtomicUsize = AtomicUsize::new(0);

fn directory() -> std::path::PathBuf {
    let path = std::env::temp_dir().join(format!(
        "jvm-bundle-test-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    fs::create_dir(&path).unwrap();
    path
}

#[test]
fn collisions_compare_exact_bytes_after_writer_is_dropped() {
    let directory = directory();
    let registry = ClassRegistry::default();
    let mut first = Writer::create(&directory.join("first")).unwrap();
    let bytes = vec![42; 20_000];
    assert_eq!(
        registry.emit_hashed(&mut first, "A", &bytes, 0).unwrap(),
        Emission::Written {
            name_collision: false
        }
    );
    drop(first);
    let mut second = Writer::create(&directory.join("second")).unwrap();
    assert_eq!(
        registry.emit_hashed(&mut second, "A", &bytes, 0).unwrap(),
        Emission::Duplicate
    );
    assert!(second.is_empty());
    let mut different = bytes;
    different[19_999] = 43;
    assert_eq!(
        registry
            .emit_hashed(&mut second, "A", &different, 0)
            .unwrap(),
        Emission::Written {
            name_collision: true
        }
    );
    assert_eq!(
        registry
            .emit_hashed(&mut second, "B", &different, 0)
            .unwrap(),
        Emission::Written {
            name_collision: false
        }
    );
    drop(second);
    drop(registry);
    fs::remove_dir_all(directory).unwrap();
}

#[test]
fn concurrent_writers_publish_each_exact_contribution_once() {
    let directory = directory();
    let registry = ClassRegistry::default();
    let barrier = std::sync::Barrier::new(4);
    std::thread::scope(|scope| {
        for worker in 0..4 {
            let registry = &registry;
            let barrier = &barrier;
            let directory = &directory;
            scope.spawn(move || {
                let mut writer = Writer::create(&directory.join(worker.to_string())).unwrap();
                barrier.wait();
                for class in 0..64 {
                    registry
                        .emit(&mut writer, &format!("C{class}"), &[class; 32])
                        .unwrap();
                }
            });
        }
    });
    let mut records = std::collections::BTreeMap::new();
    for worker in 0..4 {
        let mut reader = fs::File::open(directory.join(worker.to_string())).unwrap();
        read_magic(&mut reader).unwrap();
        while let Some((name, bytes)) = read_record(&mut reader).unwrap() {
            assert_eq!(bytes, vec![name[1..].parse::<u8>().unwrap(); 32]);
            assert!(records.insert(name, bytes).is_none());
        }
    }
    assert_eq!(records.len(), 64);
    drop(registry);
    fs::remove_dir_all(directory).unwrap();
}

#[test]
fn exact_comparisons_flush_live_buffers_and_preserve_later_records() {
    let directory = directory();
    let registry = ClassRegistry::default();
    let path = directory.join("first");
    let mut first = Writer::create(&path).unwrap();
    let mut second = Writer::create(&directory.join("second")).unwrap();
    registry.emit_hashed(&mut first, "A", b"abc", 0).unwrap();
    assert_eq!(fs::metadata(&path).unwrap().len(), 0);
    assert_eq!(
        registry.emit_hashed(&mut second, "A", b"abc", 0).unwrap(),
        Emission::Duplicate
    );
    registry.emit_hashed(&mut first, "B", b"later", 0).unwrap();
    assert_eq!(
        registry.emit_hashed(&mut second, "B", b"other", 0).unwrap(),
        Emission::Written {
            name_collision: true
        }
    );
    first.finish().unwrap();
    let mut input = fs::File::open(&path).unwrap();
    read_magic(&mut input).unwrap();
    assert_eq!(
        read_record(&mut input).unwrap(),
        Some(("A".into(), b"abc".to_vec()))
    );
    assert_eq!(
        read_record(&mut input).unwrap(),
        Some(("B".into(), b"later".to_vec()))
    );
    assert!(read_record(&mut input).unwrap().is_none());
    drop(second);
    drop(input);
    fs::remove_dir_all(directory).unwrap();
}

#[test]
fn truncated_records_are_errors_and_empty_tail_is_valid() {
    assert!(read_record(&mut Cursor::new([])).unwrap().is_none());
    for length in 1..12 {
        assert_eq!(
            read_record(&mut Cursor::new(vec![0; length]))
                .unwrap_err()
                .kind(),
            io::ErrorKind::UnexpectedEof
        );
    }
    let mut bytes = Vec::new();
    bytes.extend_from_slice(&1u32.to_le_bytes());
    bytes.extend_from_slice(&3u64.to_le_bytes());
    bytes.extend_from_slice(b"A12");
    assert_eq!(
        read_record(&mut Cursor::new(bytes)).unwrap_err().kind(),
        io::ErrorKind::UnexpectedEof
    );
}
