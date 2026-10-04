#![feature(custom_inner_attributes)]
#![feature(register_tool)]
#![register_tool(jvm_codegen)]
#![jvm_codegen::export]
pub fn address_bits(expose: bool) -> usize {
    let value = std::hint::black_box(17_u64);
    let pointer = std::hint::black_box(&value as *const u64);
    if expose {
        pointer as usize
    } else {
        // This is also how the pinned core::ptr::addr implementation reads bits.
        unsafe { std::mem::transmute::<*const u64, usize>(pointer) }
    }
}

pub fn format_many(count: u32) -> usize {
    (0..count).map(|value| format!("{value}").len()).sum()
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Pixel {
    pub bytes: [u8; 4],
}

pub fn pixel_storage() -> *mut Pixel {
    Box::into_raw(Box::new(Pixel {
        bytes: [1, 2, 3, 4],
    }))
}

pub unsafe fn free_pixel(pointer: *mut Pixel) {
    unsafe {
        drop(Box::from_raw(pointer));
    }
}

pub fn first_word(words: [u32; 2]) -> u32 {
    words[0]
}

pub fn owned_slice_elements() -> i32 {
    let mut pixels = std::hint::black_box(vec![
        Pixel {
            bytes: [1, 2, 3, 4]
        };
        3
    ]);
    let sum: i32 = pixels
        .windows(2)
        .map(|window| {
            let mut left = window[0];
            let right = window[1];
            left.bytes[0] += 10;
            i32::from(left.bytes[0]) + i32::from(right.bytes[0])
        })
        .sum();
    let snapshot = std::hint::black_box(pixels[1]);
    pixels[1].bytes[0] = 7;
    sum + i32::from(snapshot.bytes[0]) + i32::from(pixels[0].bytes[0])
}

#[repr(C)]
pub struct NestedPair {
    pub first: u64,
    pub second: u64,
}

#[repr(C)]
pub struct NestedWords {
    pub prefix: [u64; 2],
    pub pair: NestedPair,
}

pub fn nested_words() -> NestedWords {
    NestedWords {
        prefix: [13, 17],
        pair: NestedPair {
            first: 19,
            second: (23 << 32) | 29,
        },
    }
}

pub unsafe fn replace_nested_word(value: *mut NestedWords, replacement: u32) -> u32 {
    unsafe {
        let word = (&raw mut (*value).pair.second).cast::<u32>().add(1);
        let previous = *word;
        *word = replacement;
        previous
    }
}

pub unsafe fn read_nested_word(value: *const NestedWords) -> u64 {
    unsafe { (*value).pair.second }
}
