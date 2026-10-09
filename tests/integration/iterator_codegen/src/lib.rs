#![feature(register_tool)]
#![register_tool(jvm_codegen)]

#[jvm_codegen::export]
pub fn range_sum(n: i32) -> i32 {
    (0..n).sum()
}

#[jvm_codegen::export]
pub fn filter_map(n: i32) -> i32 {
    (0..n).filter(|x| x % 3 == 0).map(|x| x * 2).sum()
}

#[jvm_codegen::export]
pub fn zip_enumerate(n: i32) -> i32 {
    (0..n)
        .zip(10..)
        .enumerate()
        .map(|(i, (x, y))| i as i32 + x + y)
        .sum()
}

#[jvm_codegen::export]
pub fn reverse(n: i32) -> i32 {
    (0..n).rev().skip(2).take(4).fold(0, |a, x| a * 10 + x)
}

#[jvm_codegen::export]
pub fn windows(n: i32) -> i32 {
    let values = [n, n + 1, n + 2, n + 3];
    values.windows(2).map(|w| w[0] * w[1]).sum()
}

#[jvm_codegen::export]
pub fn record_windows(n: i32) -> i32 {
    #[derive(Clone, Copy)]
    struct Pair {
        x: i32,
        y: i32,
    }
    let values: Vec<_> = (0..n + 3).map(|x| Pair { x, y: x * 3 }).collect();
    values[1..].windows(2).map(|w| {
        let mut first = w[0];
        first.x += w[1].y;
        first.x + w[0].y
    }).sum()
}

#[jvm_codegen::export]
pub fn nested(n: i32) -> i32 {
    (0..n).flat_map(|x| 0..x).filter(|x| x % 2 == 0).sum()
}

#[jvm_codegen::export]
pub fn mutable(n: i32) -> i32 {
    let mut values = [n, n + 1, n + 2, n + 3];
    values
        .iter_mut()
        .enumerate()
        .for_each(|(i, x)| *x += i as i32);
    values.iter().copied().sum()
}

struct Counter {
    next: i32,
    end: i32,
}
impl Iterator for Counter {
    type Item = i32;
    fn next(&mut self) -> Option<i32> {
        if self.next == self.end {
            None
        } else {
            let value = self.next;
            self.next += 1;
            Some(value)
        }
    }
}

#[jvm_codegen::export]
pub fn custom(n: i32) -> i32 {
    Counter { next: 0, end: n }.map(|x| x * 3).sum()
}

#[jvm_codegen::export]
pub fn chain(n: i32) -> i32 {
    (0..n).chain(n..n + 3).step_by(2).sum()
}

#[jvm_codegen::export]
pub fn short_circuit(n: i32) -> i32 {
    let found = (0..n).find(|&x| x > 4).unwrap_or(-1);
    let sum = (0..n).try_fold(0, |sum, x| if x == 5 { Err(sum) } else { Ok(sum + x) });
    found + sum.unwrap_or_else(|sum| sum)
}

#[jvm_codegen::export]
pub fn chunks(n: i32) -> i32 {
    let values = [n, n + 1, n + 2, n + 3, n + 4, n + 5];
    values.chunks_exact(2).map(|pair| pair[0] + pair[1]).sum()
}

#[jvm_codegen::export]
pub fn captured(n: i32) -> i32 {
    let mut sum = 0;
    (0..n).for_each(|x| sum += x);
    sum
}

#[jvm_codegen::export]
pub fn drops(n: i32) -> i32 {
    struct Item<'a>(&'a std::cell::Cell<i32>);
    impl Drop for Item<'_> {
        fn drop(&mut self) {
            self.0.set(self.0.get() + 1);
        }
    }
    let count = std::cell::Cell::new(0);
    (0..n).map(|_| Item(&count)).take(2).for_each(drop);
    count.get()
}

#[inline]
#[track_caller]
fn tracked() -> u32 {
    std::panic::Location::caller().line()
}

#[jvm_codegen::export]
pub fn caller_location() -> bool {
    let expected = line!() + 1;
    tracked() == expected
}

#[jvm_codegen::export]
pub fn erased_arrays() -> bool {
    let two = [1024, 7];
    let three = [1024, 7, 8];
    format!("{two:03?} {three:03?}") == "[1024, 007] [1024, 007, 008]"
        && format!("{two:#03?}") == "[\n    1024,\n    007,\n]"
}

#[jvm_codegen::export]
pub fn discarded_provenance() -> i32 {
    let value = std::hint::black_box(43);
    let pointer = &value as *const i32;
    let address = pointer.addr();
    let _ = pointer.expose_provenance();
    unsafe { std::ptr::with_exposed_provenance::<i32>(address).read() }
}

#[jvm_codegen::export]
pub fn peekable(n: i32) -> i32 {
    let mut values = (0..n).peekable();
    let mut sum = 0;
    while let Some(&next) = values.peek() {
        sum += next;
        values.next();
    }
    sum
}

#[jvm_codegen::export]
pub fn scan(n: i32) -> i32 {
    (0..n)
        .scan(0, |sum, x| {
            *sum += x;
            Some(*sum)
        })
        .sum()
}

#[jvm_codegen::export]
pub fn flatten(n: i32) -> i32 {
    (0..n)
        .map(|x| if x % 2 == 0 { Some(x) } else { None })
        .flatten()
        .sum()
}

#[jvm_codegen::export]
pub fn borrowed_choice(select: bool) -> i32 {
    let (mut left, mut right) = (3, 5);
    let reference = if select { &mut left } else { &mut right };
    *reference += 7;
    left * 10 + right
}

fn bounds_pair(x: f64, (min, max): (f64, f64)) -> (f64, f64) {
    if x > max {
        (min, x)
    } else if x < min {
        (x, max)
    } else {
        (min, max)
    }
}

#[jvm_codegen::export]
pub fn bounds(n: i32) -> f64 {
    let (min, max) = (0..n)
        .map(|x| (x * 7 % 11) as f64)
        .fold((0.0, 0.0), |range, x| bounds_pair(x, range));
    max - min
}

#[jvm_codegen::export]
pub fn string_pattern() -> bool {
    fn trim<P: FnMut(char) -> bool>(pattern: P) -> bool {
        let function: fn(&str, P) -> &str = str::trim_start_matches::<P>;
        std::hint::black_box(function)("  text  ", pattern) == "text  "
    }
    trim(char::is_whitespace)
}

#[derive(Copy, Clone)]
#[repr(C)]
struct InlineBytes {
    bytes: [u8; 15],
    len: u8,
}
union ByteStorage {
    inline: InlineBytes,
    word: u128,
}
struct SmallBuffer(ByteStorage);
impl SmallBuffer {
    const fn new() -> Self {
        Self(ByteStorage {
            inline: InlineBytes {
                bytes: [0; 15],
                len: 0,
            },
        })
    }
    #[inline]
    fn push(&mut self, byte: u8) {
        let inline = unsafe { &mut self.0.inline };
        let len = inline.len as usize;
        if let Some(slot) = inline.bytes.get_mut(len) {
            *slot = byte;
            inline.len = (len + 1) as u8;
        }
    }
}

#[jvm_codegen::export]
pub fn union_buffer(byte: u32) -> u32 {
    let mut buffer = SmallBuffer::new();
    unsafe {
        buffer
            .0
            .inline
            .bytes
            .as_mut_ptr()
            .add(1)
            .cast::<u32>()
            .write_unaligned(0x7856_3412);
    }
    buffer.push(byte as u8);
    unsafe {
        assert_eq!(
            buffer
                .0
                .inline
                .bytes
                .as_ptr()
                .add(1)
                .cast::<u32>()
                .read_unaligned(),
            0x7856_3412
        );
        buffer.0.inline.bytes[0] as u32
    }
}
