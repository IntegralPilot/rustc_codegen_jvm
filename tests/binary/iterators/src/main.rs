#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

#[derive(Clone, Copy)]
struct Marker;

struct Token(i32);

#[derive(Clone)]
struct CloneOnly(i32);

static mut DROP_TOTAL: i32 = 0;
static DROP_THREE: i32 = 3;
static DROP_FIVE: i32 = 5;
static DROP_SEVEN: i32 = 7;

struct DropToken(i32);
struct RefDrop(&'static i32);
struct ZstDrop;

impl Drop for DropToken {
    fn drop(&mut self) {
        unsafe {
            DROP_TOTAL += self.0;
        }
    }
}

impl Drop for RefDrop {
    fn drop(&mut self) {
        unsafe {
            DROP_TOTAL += *self.0;
        }
    }
}

impl Drop for ZstDrop {
    fn drop(&mut self) {
        unsafe {
            DROP_TOTAL += 1;
        }
    }
}

struct Counter {
    next: i32,
    end: i32,
}

impl Iterator for Counter {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next == self.end {
            None
        } else {
            let value = self.next;
            self.next += 1;
            Some(value)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (self.end - self.next) as usize;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for Counter {}

struct Intermittent {
    state: u8,
}

impl Iterator for Intermittent {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        self.state += 1;
        match self.state {
            1 => Some(10),
            2 => None,
            3 => Some(30),
            _ => None,
        }
    }
}

fn array_iteration() {
    let mut sum = 0;
    for x in [1, 2, 3] {
        sum += x;
    }
    assert!(sum == 6);

    let mut empty_count = 0;
    for _ in [0i32; 0] {
        empty_count += 1;
    }
    assert!(empty_count == 0);

    let mut moved_sum = 0;
    for Token(value) in [Token(4), Token(5), Token(6)] {
        moved_sum += value;
    }
    assert!(moved_sum == 15);

    let mut marker_count = 0;
    for Marker in [Marker; 5] {
        marker_count += 1;
    }
    assert!(marker_count == 5);

    let mut count = 0;
    for _ in [(); 1000] {
        count += 1;
    }
    assert!(count == 1000);
}

fn borrowed_iteration() {
    let values = [2, 4, 6, 8];
    let mut sum = 0;
    for value in values.iter() {
        sum += *value;
    }
    assert!(sum == 20);
    assert!(values[0] == 2 && values[3] == 8);

    let mut mutable = [1, 2, 3, 4];
    for (index, value) in mutable.iter_mut().enumerate() {
        *value += index as i32;
    }
    assert!(mutable == [1, 3, 5, 7]);
}

fn loop_control() {
    let mut sum = 0;
    for value in 0..20 {
        if value % 2 == 0 {
            continue;
        }
        if value > 9 {
            break;
        }
        sum += value;
    }
    assert!(sum == 25);

    let mut pairs = 0;
    'outer: for left in 0..4 {
        for right in 0..4 {
            if left == 2 && right == 1 {
                break 'outer;
            }
            pairs += 1;
        }
    }
    assert!(pairs == 9);
}

fn iterator_state() {
    let mut iter = [10, 20, 30, 40].into_iter();
    assert!(iter.len() == 4);
    assert!(iter.size_hint() == (4, Some(4)));
    assert!(iter.next() == Some(10));
    assert!(iter.next_back() == Some(40));
    assert!(iter.len() == 2);
    assert!(iter.nth(1) == Some(30));
    assert!(iter.next().is_none());
    assert!(iter.next_back().is_none());

    let mut counter = Counter { next: 3, end: 7 };
    assert!(counter.len() == 4);
    assert!(counter.next() == Some(3));
    assert!(counter.size_hint() == (3, Some(3)));
    assert!(counter.fold(0, |total, value| total + value) == 15);
}

fn adapter_chains() {
    let transformed = (0..12)
        .map(|value| value * 2)
        .filter(|value| *value % 3 == 0)
        .skip(1)
        .take(3)
        .enumerate()
        .fold(0, |total, (index, value)| total + value + index as i32);
    assert!(transformed == 39);

    let zipped = [1, 2, 3]
        .into_iter()
        .zip([10, 20, 30])
        .map(|(left, right)| left * right)
        .sum::<i32>();
    assert!(zipped == 140);

    let chained = [1, 2]
        .into_iter()
        .chain([3, 4])
        .rev()
        .fold(0, |digits, value| digits * 10 + value);
    assert!(chained == 4321);

    let flattened = [[1, 2], [3, 4], [5, 6]]
        .into_iter()
        .flatten()
        .filter_map(|value| (value % 2 == 0).then_some(value * 10))
        .sum::<i32>();
    assert!(flattened == 120);
}

fn searching_and_consumers() {
    assert!((1..8).all(|value| value > 0));
    assert!((1..8).any(|value| value == 5));
    assert!((10..20).find(|value| *value % 7 == 0) == Some(14));
    assert!((10..20).position(|value| value == 13) == Some(3));
    assert!((1..=5).product::<i32>() == 120);
    assert!((0..10).step_by(3).sum::<i32>() == 18);
    assert!((0..10).rev().take(4).sum::<i32>() == 30);

    assert!((3..9).count() == 6);
    assert!((3..9).last() == Some(8));
    assert!((3..9).min() == Some(3));
    assert!((3..9).max() == Some(8));
    assert!([3i32, -8, 5].into_iter().max_by_key(|value| value.abs()) == Some(-8));
    assert!((1..8).find_map(|value| (value % 4 == 0).then_some(value * 10)) == Some(40));
    assert!([1, 2, 2, 5].into_iter().is_sorted());
    assert!(![1, 3, 2, 5].into_iter().is_sorted());
    assert!((1..5).eq([1, 2, 3, 4]));
    assert!((1..5).cmp([1, 2, 3, 5]) == core::cmp::Ordering::Less);
    assert!((1..5).reduce(|left, right| left * 10 + right) == Some(1234));
}

fn stateful_adapters() {
    let mut peekable = [1, 2, 3, 4].into_iter().peekable();
    assert!(peekable.peek().copied() == Some(1));
    assert!(peekable.peek().copied() == Some(1));
    assert!(peekable.next_if(|value| *value == 1) == Some(1));
    assert!(peekable.next_if_eq(&9).is_none());
    assert!(peekable.next() == Some(2));
    assert!(peekable.next_back() == Some(4));
    assert!(peekable.next() == Some(3));
    assert!(peekable.next().is_none());

    let mut values = 0..8;
    let prefix = values.by_ref().take(3).sum::<i32>();
    assert!(prefix == 3);
    assert!(values.next() == Some(3));
    assert!(values.sum::<i32>() == 22);

    let mut fused = Intermittent { state: 0 }.fuse();
    assert!(fused.next() == Some(10));
    assert!(fused.next().is_none());
    assert!(fused.next().is_none());

    let cycled = [1, 2, 3]
        .into_iter()
        .cycle()
        .take(8)
        .fold(0, |digits, value| digits * 10 + value);
    assert!(cycled == 12_312_312);
}

fn slice_iterators() {
    let values = [1, 2, 3, 4, 5, 6, 7];
    assert!(values.windows(3).map(|window| window[0] * window[2]).sum::<i32>() == 85);

    let mut chunks = values.chunks_exact(3);
    assert!(chunks.next() == Some(&[1, 2, 3][..]));
    assert!(chunks.next_back() == Some(&[4, 5, 6][..]));
    assert!(chunks.next().is_none());
    let remainder = chunks.remainder();
    assert!(remainder.len() == 1 && remainder[0] == 7);

    let chunk_digits = values
        .rchunks(3)
        .map(|chunk| chunk[0] * 10 + chunk[chunk.len() - 1])
        .sum::<i32>();
    assert!(chunk_digits == 92);

    let mut mutable = [1, 2, 3, 4, 5, 6];
    for chunk in mutable.chunks_mut(2) {
        chunk[0] *= 10;
        chunk[1] *= 100;
    }
    assert!(mutable == [10, 200, 30, 400, 50, 600]);

    let split_total = [1, 0, 2, 3, 0, 4]
        .split(|value| *value == 0)
        .map(|part| part.iter().copied().sum::<i32>())
        .sum::<i32>();
    assert!(split_total == 10);

    assert!(values.iter().copied().sum::<i32>() == 28);
    let clone_only = [CloneOnly(2), CloneOnly(4), CloneOnly(8)];
    assert!(clone_only.iter().cloned().map(|value| value.0).sum::<i32>() == 14);
}

fn generator_and_transform_adapters() {
    assert!(core::iter::empty::<i32>().next().is_none());
    assert!(core::iter::once(7).chain(core::iter::once_with(|| 9)).sum::<i32>() == 16);
    assert!(core::iter::repeat(3).take(5).product::<i32>() == 243);

    let powers = core::iter::successors(Some(1i32), |value| {
        (*value < 16).then_some(*value * 2)
    })
    .sum::<i32>();
    assert!(powers == 31);

    let mut next = 0;
    let generated = core::iter::from_fn(move || {
        next += 1;
        (next <= 4).then_some(next * next)
    })
    .sum::<i32>();
    assert!(generated == 30);

    let flat = (1..5)
        .flat_map(|end| 0..end)
        .fold(0, |total, value| total + value);
    assert!(flat == 10);

    let scanned = (1..6)
        .scan(0, |state, value| {
            *state += value;
            (*state <= 6).then_some(*state)
        })
        .sum::<i32>();
    assert!(scanned == 10);

    let mapped_while = (0..10)
        .map_while(|value| (value < 4).then_some(value * value))
        .sum::<i32>();
    assert!(mapped_while == 14);

    let mut inspected = 0;
    let result = (1..5)
        .inspect(|value| inspected += *value)
        .map(|value| value * 2)
        .sum::<i32>();
    assert!(result == 20 && inspected == 10);
}

fn double_ended_and_short_circuiting() {
    let mut range = 10..=20;
    assert!(range.next() == Some(10));
    assert!(range.next_back() == Some(20));
    assert!(range.nth(2) == Some(13));
    assert!(range.nth_back(1) == Some(18));
    assert!(range.size_hint() == (4, Some(4)));
    assert!(range.sum::<i32>() == 62);

    let mut zipped = [1, 2, 3, 4].into_iter().zip([10, 20, 30]);
    assert!(zipped.next_back() == Some((3, 30)));
    assert!(zipped.next() == Some((1, 10)));
    assert!(zipped.next_back() == Some((2, 20)));
    assert!(zipped.next().is_none());

    let mut enumerated = [10, 20, 30].into_iter().enumerate();
    assert!(enumerated.next_back() == Some((2, 30)));
    assert!(enumerated.next() == Some((0, 10)));
    assert!(enumerated.next_back() == Some((1, 20)));

    assert!((0..10).rfind(|value| *value % 4 == 0) == Some(8));
    assert!([2, 4, 7, 8].into_iter().rposition(|value| value % 2 != 0) == Some(2));

    let mut forward = 1..10;
    let stopped = forward.try_fold(0, |total, value| {
        if value == 5 {
            core::ops::ControlFlow::Break(total)
        } else {
            core::ops::ControlFlow::Continue(total + value)
        }
    });
    assert!(stopped == core::ops::ControlFlow::Break(10));
    assert!(forward.next() == Some(6));

    let mut backward = 1..=7;
    let stopped = backward.try_rfold(0, |total, value| {
        if value == 4 {
            core::ops::ControlFlow::Break(total)
        } else {
            core::ops::ControlFlow::Continue(total + value)
        }
    });
    assert!(stopped == core::ops::ControlFlow::Break(18));
    assert!(backward.next_back() == Some(3));
}

fn drop_behavior() {
    unsafe {
        DROP_TOTAL = 0;
    }
    {
        let mut values = [DropToken(1), DropToken(2), DropToken(4)].into_iter();
        drop(values.next());
        assert!(unsafe { DROP_TOTAL } == 1);
        drop(values);
    }
    assert!(unsafe { DROP_TOTAL } == 7);

    unsafe {
        DROP_TOTAL = 0;
    }
    for value in [DropToken(8), DropToken(16), DropToken(32)] {
        if value.0 == 16 {
            break;
        }
    }
    assert!(unsafe { DROP_TOTAL } == 56);

    unsafe {
        DROP_TOTAL = 0;
    }
    {
        let mut values = [
            RefDrop(&DROP_THREE),
            RefDrop(&DROP_FIVE),
            RefDrop(&DROP_SEVEN),
        ]
        .into_iter();
        drop(values.next());
        assert!(unsafe { DROP_TOTAL } == 3);
        drop(values);
    }
    assert!(unsafe { DROP_TOTAL } == 15);

    unsafe {
        DROP_TOTAL = 0;
    }
    {
        let mut values = [ZstDrop, ZstDrop, ZstDrop].into_iter();
        drop(values.next());
        drop(values);
    }
    assert!(unsafe { DROP_TOTAL } == 3);
}

fn main() {
    array_iteration();
    borrowed_iteration();
    loop_control();
    iterator_state();
    adapter_chains();
    searching_and_consumers();
    stateful_adapters();
    slice_iterators();
    generator_and_transform_adapters();
    double_ended_and_short_circuiting();
    drop_behavior();
}
