use std::{hint::black_box, mem::MaybeUninit};

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(C)]
struct Pixel {
    r: u8,
    g: i8,
    b: u8,
    a: u8,
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(C)]
struct Pair {
    first: i32,
    second: u32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Reordered {
    a: u8,
    b: u16,
    c: i8,
}

#[inline(never)]
fn update(mut pixel: Pixel, r: u8) -> Pixel {
    pixel.r = r;
    pixel.g = pixel.g.wrapping_sub(1);
    pixel
}

#[inline(never)]
fn replace(pixel: &mut Pixel, value: Pixel) -> Pixel {
    std::mem::replace(pixel, value)
}

#[inline(never)]
fn fields(pixel: &mut Pixel) -> (&mut u8, &mut i8) {
    (&mut pixel.r, &mut pixel.g)
}

trait Read {
    fn read(&self) -> i32;
}
impl Read for Pair {
    fn read(&self) -> i32 {
        self.first
    }
}

#[derive(Clone, Copy)]
struct Wrapper(u32);
impl Wrapper {
    #[inline(never)]
    fn wrapping_add(self, other: Self) -> Self {
        Self(self.0 ^ other.0)
    }
}
impl PartialEq for Wrapper {
    fn eq(&self, other: &Self) -> bool {
        self.0 % 3 == other.0 % 3
    }
}

#[inline(never)]
fn tuple(mut value: (u32, i32)) -> (u32, i32) {
    value.0 = value.0.wrapping_add(1);
    value.1 = value.1.wrapping_sub(1);
    value
}

pub fn run() {
    let pair = black_box((u32::MAX, i32::MIN));
    assert_eq!(black_box(tuple)(pair), (0, i32::MAX));
    assert_eq!(pair, (u32::MAX, i32::MIN));
    let mut pairs = black_box([(1_u16, -2_i16); 3]);
    *black_box(&mut pairs[1].1) = -300;
    assert_eq!(pairs, [(1, -2), (1, -300), (1, -2)]);
    let closure = black_box(|a: u32, b: i32| tuple((a, b)));
    let callable: &dyn Fn(u32, i32) -> (u32, i32) = &closure;
    assert_eq!(black_box(callable)(15, -1), (16, -2));
    let fp: fn(u32, i32) -> (u32, i32) = closure;
    assert_eq!(black_box(fp)(20, -7), (21, -8));
    assert_eq!(
        black_box([3_i32, -5, 8])
            .into_iter()
            .map(|a| (a, a + 1))
            .collect::<Vec<_>>(),
        vec![(3, 4), (-5, -4), (8, 9)]
    );
    assert!(black_box(Wrapper(1)) == black_box(Wrapper(4)));
    assert!(black_box(Wrapper(1)) != black_box(Wrapper(2)));
    assert!(black_box([Wrapper(1), Wrapper(2)]).starts_with(&[Wrapper(4)]));
    assert!(!black_box([Wrapper(1), Wrapper(2)]).starts_with(&[Wrapper(2)]));
    assert_eq!(black_box(Wrapper(3)).wrapping_add(Wrapper(5)).0, 6);
    let constructor = black_box(Wrapper as fn(u32) -> Wrapper);
    assert_eq!(constructor(0xfedcba98).0, 0xfedcba98);
    const P: Pixel = Pixel {
        r: 200,
        g: -127,
        b: 255,
        a: 128,
    };
    let mut value = black_box(P);
    let copied = value;
    value = black_box(update)(value, 3);
    assert_eq!(
        value,
        Pixel {
            r: 3,
            g: -128,
            b: 255,
            a: 128
        }
    );
    assert_eq!(copied, P);
    assert_eq!(replace(&mut value, P).r, 3);
    let (r, g) = fields(&mut value);
    *r = 1;
    *g = -2;
    assert_eq!(
        value,
        Pixel {
            r: 1,
            g: -2,
            b: 255,
            a: 128
        }
    );
    let bytes: [u8; 4] = unsafe { std::mem::transmute(value) };
    assert_eq!(bytes, [1, 254, 255, 128]);
    let reconstructed: Pixel = unsafe { std::mem::transmute(black_box(bytes)) };
    assert_eq!(reconstructed, value);

    let mut array = black_box([P; 3]);
    array[1].g = -1;
    assert_eq!(array[0], P);
    assert_eq!(array[1].g, -1);
    let mut boxed = Box::new(P);
    boxed.b = 42;
    assert_eq!(boxed.b, 42);
    assert_eq!(boxed.g, -127);
    let raw = &raw mut *boxed;
    unsafe {
        (&raw mut (*raw).a).write(0);
        assert_eq!((&raw const (*raw).g).read(), -127);
    }
    assert_eq!(boxed.a, 0);

    let mut uninit = MaybeUninit::<Pair>::uninit();
    let pointer = black_box(uninit.as_mut_ptr());
    unsafe {
        (&raw mut (*pointer).second).write(0xfedcba98);
        (&raw mut (*pointer).first).write(-1234567);
        let mut pair = uninit.assume_init();
        assert_eq!(
            pair,
            Pair {
                first: -1234567,
                second: 0xfedcba98
            }
        );
        pair.second = 0x87654321;
        let reader: &dyn Read = &pair;
        assert_eq!(black_box(reader).read(), -1234567);
        assert_eq!(pair.second, 0x87654321);
        let image: [u8; 8] = std::mem::transmute(pair);
        assert_eq!(&image[..4], &(-1234567_i32).to_ne_bytes());
        assert_eq!(&image[4..], &0x87654321_u32.to_ne_bytes());
    }

    let mut reordered = black_box(Reordered {
        a: 199,
        b: 60001,
        c: -99,
    });
    reordered.b = 65535;
    *black_box(&mut reordered.c) = -1;
    assert_eq!(
        reordered,
        Reordered {
            a: 199,
            b: 65535,
            c: -1
        }
    );

    let mut values = vec![
        Pair {
            first: -1,
            second: u32::MAX
        };
        8
    ];
    for (i, pair) in values.iter_mut().enumerate() {
        pair.first = i as i32 - 4;
    }
    assert_eq!(values.iter().map(|pair| pair.first).sum::<i32>(), -4);
    assert!(values.iter().all(|pair| pair.second == u32::MAX));
}
