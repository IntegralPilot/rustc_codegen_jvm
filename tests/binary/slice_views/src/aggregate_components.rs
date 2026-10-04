use std::hint::black_box;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
struct Pair {
    x: i32,
    y: i32,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct Record {
    prefix: u64,
    pair: Pair,
    suffix: u64,
}

#[inline(never)]
fn element(values: &[Record], index: usize) -> &Record {
    unsafe { values.get_unchecked(index) }
}

#[inline(never)]
fn field(value: &Record) -> &i32 {
    &value.pair.y
}

#[inline(never)]
fn mutate(values: &mut [Record]) {
    for (index, value) in values.iter_mut().enumerate() {
        let y = black_box(&mut value.pair.y);
        *y += index as i32 + 10;
    }
}

struct Cursor<'a> {
    values: &'a [Record],
    next: Option<&'a Record>,
    index: usize,
}

impl<'a> Cursor<'a> {
    #[inline(never)]
    fn advance(&mut self) -> Option<&'a Record> {
        self.next = self.values.get(self.index);
        self.index += 1;
        self.next
    }
}

pub fn check() {
    let mut records = std::array::from_fn::<_, 6, _>(|i| Record {
        prefix: 0x123456789abcdef0,
        pair: Pair {
            x: i as i32,
            y: 100 + i as i32,
        },
        suffix: 0xfedcba9876543210,
    });
    let middle = black_box(&records[1..5]);
    let mut cursor = Cursor {
        values: middle,
        next: None,
        index: 0,
    };
    let mut sum = 0;
    while let Some(value) = cursor.advance() {
        sum += *field(value);
    }
    assert_eq!(sum, 410);
    assert_eq!(*field(element(middle, 2)), 103);
    assert_eq!(
        middle
            .windows(2)
            .zip(middle.iter())
            .map(|(w, v)| w[1].pair.x - v.pair.x)
            .sum::<i32>(),
        3
    );
    mutate(black_box(&mut records[1..5]));
    for (i, value) in records.iter().enumerate() {
        assert_eq!(
            value.pair.y,
            100 + i as i32 + if (1..5).contains(&i) { i as i32 + 9 } else { 0 }
        );
        assert_eq!(value.prefix, 0x123456789abcdef0);
        assert_eq!(value.suffix, 0xfedcba9876543210);
    }
    // The same projection must retain a byte-backed allocation's provenance.
    let mut storage = [0_u64; 6];
    let raw = storage.as_mut_ptr().cast::<Record>();
    unsafe {
        raw.write(records[2]);
        raw.add(1).write(records[3]);
        let slice = std::slice::from_raw_parts_mut(raw, 2);
        mutate(slice);
        let data = slice.as_mut_ptr();
        let y = std::ptr::addr_of_mut!((*data.add(1)).pair.y);
        assert_eq!(*field(element(slice, 1)), 126);
        assert_eq!((y as usize) - (data as usize), 36);
        let roundtrip = std::ptr::with_exposed_provenance_mut::<i32>(y as usize);
        *roundtrip = 151;
        assert_eq!(*field(element(slice, 1)), 151);
    }
}
