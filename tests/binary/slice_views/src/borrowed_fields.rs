use std::hint::black_box;

#[derive(Clone, Copy)]
#[repr(C)]
struct BorrowedFields<'a> {
    bytes: Option<&'a [u8]>,
    text: Option<&'a str>,
    number: Option<&'a i64>,
}

#[inline(never)]
unsafe fn replace<T>(field: *mut T, value: T) {
    unsafe {
        field.write(black_box(value));
    }
}

pub fn run() {
    let bytes = black_box([3u8, 5, 7, 11]);
    let number = black_box(-127i64);
    let mut owner = BorrowedFields {
        bytes: None,
        text: None,
        number: None,
    };
    let root = &raw mut owner;
    unsafe {
        let data = &raw mut (*root).bytes;
        let text = &raw mut (*root).text;
        let scalar = &raw mut (*root).number;
        replace(data, Some(&bytes[1..]));
        replace(text, Some("éclair"));
        replace(scalar, Some(&number));
        assert_eq!(owner.bytes, Some(&bytes[1..]));
        assert_eq!(owner.text, Some("éclair"));
        assert_eq!(owner.number, Some(&number));
        let copy = black_box(owner);
        replace(
            root,
            BorrowedFields {
                bytes: Some(&bytes[..1]),
                text: Some("first"),
                number: None,
            },
        );
        // Existing field locations follow replacement of their whole owner.
        replace(data, Some(&bytes[2..]));
        replace(text, None);
        replace(scalar, Some(&number));
        assert_eq!(owner.bytes, Some(&bytes[2..]));
        assert_eq!(owner.text, None);
        assert_eq!(owner.number, Some(&number));
        assert_eq!(copy.bytes, Some(&bytes[1..]));
        assert_eq!(copy.text, Some("éclair"));
    }
}
