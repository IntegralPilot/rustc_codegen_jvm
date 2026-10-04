use std::hint::black_box;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(i8)]
enum Small {
    Negative = -101,
    Zero = 0,
    Positive = 123,
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u64)]
enum Wide {
    Low = 7,
    High = u64::MAX - 1,
}

#[derive(Clone, Copy)]
#[repr(i16)]
enum Signed {
    Low = -32_000,
    High = 31_000,
}

#[derive(Clone, Copy)]
#[repr(u32)]
enum Unsigned {
    Low = 13,
    High = u32::MAX,
}

static TAGS: [Small; 3] = [Small::Negative, Small::Zero, Small::Positive];

#[inline(never)]
fn rotate(value: &mut Small) -> Small {
    let previous = *value;
    *value = match previous {
        Small::Negative => Small::Zero,
        Small::Zero => Small::Positive,
        Small::Positive => Small::Negative,
    };
    previous
}

trait Tag {
    fn tag(&self) -> i64;
}

impl Tag for Small {
    fn tag(&self) -> i64 {
        *self as i64
    }
}

#[inline(never)]
fn dynamic_tag(value: &dyn Tag) -> i64 {
    value.tag()
}

static DROPS: AtomicUsize = AtomicUsize::new(0);
enum Dropped {
    Once,
    Twice,
}
impl Drop for Dropped {
    fn drop(&mut self) {
        DROPS.fetch_add(
            match self {
                Self::Once => 1,
                Self::Twice => 2,
            },
            Ordering::Relaxed,
        );
    }
}

pub fn run() {
    let mut values = black_box(TAGS);
    let callback: fn(&mut Small) -> Small = black_box(rotate);
    assert_eq!(callback(&mut values[0]), Small::Negative);
    assert_eq!(values[0], Small::Zero);
    assert_eq!(rotate(&mut values[2]), Small::Positive);
    assert_eq!(dynamic_tag(black_box(&values[2])), -101);
    assert_eq!(format!("{:?}", black_box(Small::Negative)), "Negative");
    let mut cell = std::cell::Cell::new(Small::Negative);
    *cell.get_mut() = Small::Positive;
    assert_eq!(cell.get(), Small::Positive);
    let pair = black_box((Wide::Low, Wide::High));
    assert_eq!(pair.0 as u64, 7);
    assert_eq!(pair.1 as u64, u64::MAX - 1);
    assert_eq!(black_box(Signed::Low) as i64, -32_000);
    assert_eq!(black_box(Signed::High) as i64, 31_000);
    assert_eq!(black_box(Unsigned::Low) as u64, 13);
    assert_eq!(black_box(Unsigned::High) as u64, u32::MAX as u64);
    unsafe {
        // Layout-observing aliases must see and update the same storage.
        let raw = std::ptr::addr_of_mut!(values[1]).cast::<i8>();
        raw.write(-101);
        assert_eq!(values[1], Small::Negative);
        assert_eq!(std::mem::transmute::<Small, i8>(black_box(values[1])), -101);
        assert_eq!(
            std::mem::transmute::<u64, Wide>(black_box(u64::MAX - 1)),
            Wide::High
        );
        let mut bytes = [0u8; 9];
        bytes
            .as_mut_ptr()
            .add(1)
            .cast::<Wide>()
            .write_unaligned(Wide::High);
        assert_eq!(
            bytes.as_ptr().add(1).cast::<Wide>().read_unaligned(),
            Wide::High
        );
    }
    drop(black_box(Dropped::Once));
    drop(black_box(Dropped::Twice));
    assert_eq!(DROPS.load(Ordering::Relaxed), 3);
}
