use std::hint::black_box;

// Shared JVM storage must preserve distinct Rust byte layouts.
#[repr(C, u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
enum Narrow {
    Empty,
    Value(u32),
}

#[repr(C, u64)]
#[derive(Clone, Copy, Debug, PartialEq)]
enum Wide {
    Empty,
    Value(u32),
}

pub fn run() {
    assert_eq!(std::mem::size_of::<Narrow>(), 8);
    assert_eq!(std::mem::size_of::<Wide>(), 16);
    let mut narrow = black_box(Narrow::Value(0x12345678));
    let mut wide = black_box(Wide::Value(0xabcdef01));
    unsafe {
        let p = black_box(&raw mut narrow).cast::<u8>();
        let q = black_box(&raw mut wide).cast::<u8>();
        assert_eq!(p.read(), 1);
        assert_eq!(q.cast::<u64>().read_unaligned(), 1);
        assert_eq!(p.add(4).cast::<u32>().read_unaligned(), 0x12345678);
        assert_eq!(q.add(8).cast::<u32>().read_unaligned(), 0xabcdef01);
        p.add(4).cast::<u32>().write_unaligned(17);
        q.add(8).cast::<u32>().write_unaligned(31);
    }
    assert_eq!(narrow, Narrow::Value(17));
    assert_eq!(wide, Wide::Value(31));
    narrow = black_box(Narrow::Empty);
    wide = black_box(Wide::Empty);
    unsafe {
        assert_eq!(black_box(&raw const narrow).cast::<u8>().read(), 0);
        assert_eq!(black_box(&raw const wide).cast::<u64>().read_unaligned(), 0);
    }
}
