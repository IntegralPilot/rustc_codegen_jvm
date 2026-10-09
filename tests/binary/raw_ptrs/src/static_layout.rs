//! Exact array lengths survive erased JVM array classes and ABI boundaries.
#[inline(never)]
fn advance<const N: usize>(p: *mut [u8; N]) -> *mut [u8; N] {
    unsafe { p.add(1) }
}
#[inline(never)]
fn consume<const N: usize>(p: *const [u8; N]) -> [u8; N] {
    unsafe { p.read() }
}
#[repr(C)]
struct Stored { four: *mut [u8; 4], eight: *mut [u8; 8] }

pub fn run() {
    let mut data = core::array::from_fn::<_, 32, _>(|i| i as u8);
    let root = data.as_mut_ptr();
    let nonnull = core::ptr::NonNull::new(root.cast::<[u8; 8]>()).unwrap();
    assert_eq!(consume(unsafe { nonnull.add(1) }.as_ptr()), [8, 9, 10, 11, 12, 13, 14, 15]);
    let mut boxed = Box::new([3u8; 8]);
    boxed[7] = 91;
    assert_eq!(consume(&*boxed), [3, 3, 3, 3, 3, 3, 3, 91]);
    let fourth = advance(root.cast::<[u8; 4]>());
    let eighth = advance(root.cast::<[u8; 8]>());
    assert_eq!(consume(fourth), [4, 5, 6, 7]);
    assert_eq!(consume(eighth), [8, 9, 10, 11, 12, 13, 14, 15]);
    let mut stored = Stored { four: fourth, eight: eighth };
    let callback: fn(*mut [u8; 8]) -> *mut [u8; 8] = core::hint::black_box(advance::<8>);
    stored.eight = callback(stored.eight);
    unsafe { stored.four.write([40, 41, 42, 43]); }
    assert_eq!(&data[4..8], &[40, 41, 42, 43]);
    assert_eq!(consume(stored.eight), [16, 17, 18, 19, 20, 21, 22, 23]);
    let saved = unsafe { core::ptr::read(&stored) };
    stored.four = advance(saved.four);
    assert_eq!(consume(saved.four), [40, 41, 42, 43]);
    assert_eq!(consume(stored.four), [8, 9, 10, 11]);
    let bytes = unsafe { core::slice::from_raw_parts((&stored as *const Stored).cast::<u8>(), core::mem::size_of::<Stored>()) };
    let mut image = [0u8; core::mem::size_of::<Stored>()];
    image.copy_from_slice(bytes);
    let restored = unsafe { core::ptr::read_unaligned(image.as_ptr().cast::<Stored>()) };
    assert_eq!(consume(restored.eight), consume(stored.eight));
    assert!(core::ptr::null::<[u8; 8]>().is_null());
}
