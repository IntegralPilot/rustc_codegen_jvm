use std::{cell::Cell, hint::black_box, mem::MaybeUninit};

#[repr(C)]
#[derive(Clone, Copy)]
struct Inner {
    number: u32,
    flag: bool,
}

#[repr(C)]
struct Stored<'a> {
    prefix: u64,
    inner: Inner,
    text: &'a str,
    values: &'a [u32],
    shared: Cell<u64>,
    suffix: u64,
}

#[inline(never)]
fn read(value: &Stored<'_>) -> u32 {
    value.inner.number
}

#[inline(never)]
fn write(value: &mut Stored<'_>, number: u32) {
    value.inner.number = number;
    value.inner.flag = !value.inner.flag;
}

#[inline(never)]
fn replace<'a>(value: &mut Stored<'a>, text: &'a str, values: &'a [u32]) -> Inner {
    let previous = value.inner;
    value.inner = Inner {
        number: 17,
        flag: true,
    };
    value.text = text;
    value.values = values;
    previous
}

#[inline(never)]
fn shared(value: &Stored<'_>) -> u64 {
    value.shared.set(value.shared.get() + 1);
    value.shared.get()
}

pub fn run() {
    owned_fields();
    let values = [11, 13, 19, 23];
    let mut storage = Box::new(Stored {
        prefix: 0x1234_5678_9abc_def0,
        inner: Inner {
            number: 3,
            flag: false,
        },
        text: "before",
        values: &values[..2],
        shared: Cell::new(5),
        suffix: 0xfedc_ba98_7654_3210,
    });
    assert_eq!(read(black_box(&storage)), 3);
    write(black_box(&mut storage), 0xfedc_ba98);
    assert_eq!(read(&storage), 0xfedc_ba98);
    assert!(storage.inner.flag);
    let previous = replace(&mut storage, "after λ", &values[2..]);
    assert_eq!(previous.number, 0xfedc_ba98);
    assert!(previous.flag);
    assert_eq!(read(&storage), 17);
    assert_eq!(storage.text, "after λ");
    assert_eq!(storage.values, [19, 23]);
    assert_eq!(shared(&storage), 6);
    assert_eq!(shared(&storage), 7);
    assert_eq!(storage.prefix, 0x1234_5678_9abc_def0);
    assert_eq!(storage.suffix, 0xfedc_ba98_7654_3210);

    // Field projection must not read the uninitialized enclosing value.
    let mut uninit = MaybeUninit::<Stored<'_>>::uninit();
    let root = black_box(uninit.as_mut_ptr());
    unsafe {
        (&raw mut (*root).inner.number).write(31);
        (&raw mut (*root).inner.flag).write(true);
        assert_eq!((&raw const (*root).inner.number).read(), 31);
        assert!((&raw const (*root).inner.flag).read());
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct Pixel {
    marker: u32,
    channels: [u8; 4],
}

#[inline(never)]
fn channels(pixel: &Pixel) -> [u8; 4] {
    pixel.channels
}

fn owned_fields() {
    let mut pixels = vec![
        Pixel {
            marker: 101,
            channels: [1, 2, 3, 4]
        };
        3
    ];
    pixels[1].channels = [11, 13, 17, 19];
    let before = channels(black_box(&pixels[1]));
    pixels[1].channels = [23, 29, 31, 37];
    assert_eq!(before, [11, 13, 17, 19]);
    let mut snapshot = channels(black_box(&pixels[1]));
    snapshot[0] = 43;
    assert_eq!(channels(&pixels[1]), [23, 29, 31, 37]);
    assert_eq!(snapshot, [43, 29, 31, 37]);
    assert_eq!(pixels[1].marker, 101);
    let mut uninit = MaybeUninit::<Pixel>::uninit();
    unsafe {
        let root = black_box(uninit.as_mut_ptr());
        (&raw mut (*root).channels).write([47, 53, 59, 61]);
        let copy = (&raw const (*root).channels).read();
        (&raw mut (*root).channels).write([0; 4]);
        assert_eq!(copy, [47, 53, 59, 61]);
    }
}
