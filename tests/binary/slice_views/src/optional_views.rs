use std::hint::black_box;

static TEXT: [Option<&str>; 3] = [None, Some(""), Some("aé🦀")];
static BYTES: [Option<&[u8]>; 3] = [None, Some(&[]), Some(&[2, 5, 9])];
static ARRAY: [Option<&[u16; 2]>; 2] = [None, Some(&[31, 47])];

#[inline(never)]
fn identity<T>(value: T) -> T {
    black_box(value)
}

#[derive(Clone)]
struct Stored<'a> {
    text: Option<&'a str>,
    bytes: Option<&'a [u8]>,
    array: Option<&'a [u16; 2]>,
}

pub fn run() {
    for value in black_box(TEXT) {
        let call: fn(Option<&'static str>) -> Option<&'static str> = black_box(identity);
        let result = call(value);
        assert_eq!(result, value);
    }
    for value in black_box(BYTES) {
        let result = identity(value);
        assert_eq!(result, value);
    }
    for value in black_box(ARRAY) {
        let result = identity(value);
        assert_eq!(result, value);
    }
    let mut stored = Vec::new();
    for index in 0..3 {
        stored.push(Stored {
            text: TEXT[index],
            bytes: BYTES[index],
            array: ARRAY[index % 2],
        });
    }
    let copy = identity(stored.clone());
    for (index, value) in copy.iter().enumerate() {
        assert_eq!(value.text, TEXT[index]);
        assert_eq!(value.bytes, BYTES[index]);
        assert_eq!(value.array, ARRAY[index % 2]);
    }
    let mut data = [3u8, 7, 11, 13];
    let mut optional = Some(&mut data[1..3]);
    identity(optional.take()).unwrap()[0] = 23;
    assert!(identity(optional).is_none());
    assert_eq!(data, [3, 23, 11, 13]);
    let none = None::<&[u32; 0]>;
    let empty = Some(&[] as &[u32; 0]);
    assert!(identity(none).is_none());
    assert_eq!(identity(empty).unwrap().len(), 0);
    let zst = [(); 17];
    assert_eq!(identity(Some(&zst[..])).unwrap().len(), 17);
    unsafe {
        // None permits unspecified metadata. Check both set bits and uninitialized metadata without reading either.
        let junk: Option<&[u8]> = std::mem::transmute([0usize, usize::MAX]);
        assert!(identity(junk).is_none());
        let mut uninit = std::mem::MaybeUninit::<Option<&str>>::uninit();
        uninit.as_mut_ptr().cast::<usize>().write(0);
        assert!(identity(uninit.assume_init()).is_none());
    }
}
