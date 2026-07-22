#![feature(iter_map_windows)]
#![feature(iter_advance_by)]
#![feature(ascii_char)]
#![feature(ascii_char_variants)]

use std::string::String;
use std::vec;
use std::vec::Vec;

#[derive(Clone, Copy)]
struct Marker;

struct Token(i32);

#[derive(Clone)]
struct CloneOnly(i32);

struct Mod3(i32);

impl PartialEq for Mod3 {
    fn eq(&self, other: &Self) -> bool {
        self.0 % 3 == other.0 % 3
    }
}

impl Eq for Mod3 {}

impl PartialOrd for Mod3 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Mod3 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.0 % 3).cmp(&(other.0 % 3))
    }
}

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

struct MutableSizeHint((usize, Option<usize>));

impl Iterator for MutableSizeHint {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        let (ref mut lower, ref mut upper) = self.0;
        let next = (*upper != Some(0)).then_some(0);
        *lower = lower.saturating_sub(1);
        if let Some(upper) = upper {
            *upper = upper.saturating_sub(1);
        }
        next
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0
    }
}

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

struct BidirectionalRange {
    start: i32,
    end: i32,
}

impl Iterator for BidirectionalRange {
    type Item = i32;
    fn next(&mut self) -> Option<Self::Item> {
        if self.start < self.end {
            let val = self.start;
            self.start += 1;
            Some(val)
        } else {
            None
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = if self.start < self.end { (self.end - self.start) as usize } else { 0 };
        (remaining, Some(remaining))
    }
}

impl DoubleEndedIterator for BidirectionalRange {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.start < self.end {
            self.end -= 1;
            Some(self.end)
        } else {
            None
        }
    }
}

impl ExactSizeIterator for BidirectionalRange {}

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

fn ascii_escape_iterator() {
    let mut escaped = b"\0fastpath\xffremainder\xff".escape_ascii();
    assert!(escaped.advance_by(4).is_ok());
    assert!(escaped.advance_back_by(4).is_ok());
    assert!(escaped.to_string() == r#"fastpath\xffremainder"#);
}

fn ascii_char_formatting() {
    use std::ascii::Char;

    assert!(std::format!("{:?}", Char::Null) == r#"'\0'"#);
    assert!(Char::CapitalA.to_string() == "A");

    for byte in 0..128_u8 {
        let mut expected = std::format!("{:?}", byte as char);
        if let Some(rest) = expected.strip_prefix("'\\u{") {
            expected = std::format!("'\\x{:0>2}'", rest.strip_suffix("}'").unwrap());
        }
        let actual = std::format!("{:?}", Char::from_u8(byte).unwrap());
        if actual != expected {
            std::println!("ASCII_CHAR_MISMATCH byte={byte} expected={expected} actual={actual}");
            break;
        }
    }

    let mut extended = String::from("abc");
    extended.extend(Char::CapitalA..=Char::CapitalC);
    assert!(extended == "abcABC");
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

fn fixed_u32_slice_iteration() {
    let mut words = [0_u32; 40];
    for (index, word) in words.iter_mut().enumerate() {
        *word = (index as u32 + 1) * 3;
    }

    assert!(words.iter().copied().sum::<u32>() == 2_460);
    assert!(words.iter().rev().copied().take(4).collect::<Vec<_>>() == [120, 117, 114, 111]);

    let mut ends = words.iter();
    assert!(ends.next() == Some(&3));
    assert!(ends.next_back() == Some(&120));
    assert!(ends.len() == 38);

    let mut greater = [0_u32; 40];
    greater.copy_from_slice(&words);
    greater[39] += 1;
    assert!(words.iter().cmp(greater.iter()).is_lt());
    assert!(words.iter().rev().cmp(greater.iter().rev()).is_lt());
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

    let repeated_strings = core::iter::repeat(String::from("owned"))
        .take(3)
        .collect::<Vec<_>>();
    assert!(repeated_strings.len() == 3);
    assert!(repeated_strings.iter().all(|value| value == "owned"));

    let window_count = core::iter::repeat(String::from("window"))
        .map_windows(|window: &[_; 3]| window.len())
        .take(4)
        .sum::<usize>();
    assert!(window_count == 12);

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

fn cloned_generator_with_adapter_capture() {
    let nibbles = b"4869";
    let mut bytes = nibbles
        .chunks_exact(2)
        .map(|pair| match pair {
            [high, low] => [high, low],
            _ => unreachable!(),
        })
        .map(|[&high, &low]| {
            let half = |nibble: u8| (nibble as char).to_digit(16).unwrap() as u8;
            (half(high) << 4) | half(low)
        });
    let generated = core::iter::from_fn(move || bytes.next());
    let mut cloned = generated.clone();

    assert!(cloned.next() == Some(b'H'));
    assert!(cloned.next() == Some(b'i'));
    assert!(cloned.next().is_none());

    let mut original = generated;
    assert!(original.next() == Some(b'H'));
    assert!(original.next() == Some(b'i'));
    assert!(original.next().is_none());
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

fn test_zip_edge_cases() {
    let mut a = [1, 2, 3].into_iter();
    let mut b = [10, 20, 30, 40].into_iter();
    let mut zipped = a.by_ref().zip(b.by_ref());

    assert!(zipped.size_hint() == (3, Some(3)));
    assert!(zipped.next() == Some((1, 10)));
    assert!(zipped.size_hint() == (2, Some(2)));

    assert!(zipped.next_back() == Some((3, 30)));
    assert!(zipped.next() == Some((2, 20)));
    assert!(zipped.next().is_none());
}

fn test_chain_edge_cases() {
    let mut chained = [1, 2].into_iter().chain([3, 4, 5].into_iter());
    assert!(chained.size_hint() == (5, Some(5)));

    assert!(chained.next() == Some(1));
    assert!(chained.next_back() == Some(5));
    assert!(chained.next_back() == Some(4));
    assert!(chained.next() == Some(2));
    assert!(chained.next_back() == Some(3));
    assert!(chained.next().is_none());
    assert!(chained.next_back().is_none());
}

fn test_peek_mut() {
    let mut peekable = [10, 20, 30].into_iter().peekable();
    if let Some(val) = peekable.peek_mut() {
        assert!(*val == 10);
        *val += 5;
    }
    assert!(peekable.next() == Some(15));
    assert!(peekable.next() == Some(20));
}

fn test_unzip_and_partition() {
    let pairs = vec![(1, 'a'), (2, 'b'), (3, 'c')];
    let (nums, chars): (Vec<i32>, Vec<char>) = pairs.into_iter().unzip();
    assert!(nums == vec![1, 2, 3]);
    assert!(chars == vec!['a', 'b', 'c']);

    let (even, odd): (Vec<i32>, Vec<i32>) = (1..10).partition(|&x| x % 2 == 0);
    assert!(even == vec![2, 4, 6, 8]);
    assert!(odd == vec![1, 3, 5, 7, 9]);
}

fn test_fused_iterator_guarantees() {
    let mut raw = Intermittent { state: 0 };
    assert!(raw.next() == Some(10));
    assert!(raw.next().is_none());
    assert!(raw.next() == Some(30));

    let mut fused = Intermittent { state: 0 }.fuse();
    assert!(fused.next() == Some(10));
    assert!(fused.next().is_none());
    assert!(fused.next().is_none());
}

fn test_char_and_str_iterators() {
    let s = "a💖b";
    let mut chars = s.chars();
    assert!(chars.next() == Some('a'));
    assert!(chars.next_back() == Some('b'));
    assert!(chars.next() == Some('💖'));
    assert!(chars.next().is_none());

    let mut indices = s.char_indices();
    assert!(indices.next() == Some((0, 'a')));
    assert!(indices.next() == Some((1, '💖')));
    assert!(indices.next() == Some((5, 'b')));
    assert!(indices.next().is_none());
}

fn test_drop_propagation_in_adapters() {
    unsafe { DROP_TOTAL = 0; }
    {
        let left = [DropToken(1), DropToken(2)];
        let right = [DropToken(10), DropToken(20)];
        let mut zipped = left.into_iter().zip(right.into_iter());

        let first = zipped.next();
        assert!(unsafe { DROP_TOTAL } == 0);

        drop(first);
        assert!(unsafe { DROP_TOTAL } == 11);
    }
    assert!(unsafe { DROP_TOTAL } == 33);

    unsafe { DROP_TOTAL = 0; }
    {
        let left = [DropToken(1), DropToken(2)];
        let right = [DropToken(4), DropToken(8)];
        let mut chained = left.into_iter().chain(right.into_iter());
        let val = chained.next();
        drop(val);
        assert!(unsafe { DROP_TOTAL } == 1);
    }
    assert!(unsafe { DROP_TOTAL } == 15);
}

fn test_by_ref_borrowing_scopes() {
    let mut iter = 0..10;
    let mut sub_ref = iter.by_ref().take(3);
    assert!(sub_ref.next() == Some(0));
    assert!(sub_ref.next() == Some(1));
    assert!(sub_ref.next() == Some(2));
    assert!(sub_ref.next().is_none());

    assert!(iter.next() == Some(3));
}

fn test_step_by_mechanics() {
    let mut steps = (0..10).step_by(3);
    assert!(steps.size_hint() == (4, Some(4)));
    assert!(steps.next() == Some(0));
    assert!(steps.size_hint() == (3, Some(3)));
    assert!(steps.next() == Some(3));
    assert!(steps.next() == Some(6));
    assert!(steps.next() == Some(9));
    assert!(steps.next().is_none());
}

fn test_cloned_copied_semantics() {
    let src = [CloneOnly(5), CloneOnly(15)];
    let mut iter = src.iter().cloned();
    assert!(iter.next().unwrap().0 == 5);
    assert!(iter.next().unwrap().0 == 15);

    let src_copy = [Marker, Marker];
    let mut iter_copy = src_copy.iter().copied();
    assert!(iter_copy.next().is_some());
}

fn test_custom_exact_size_double_ended() {
    let mut range = BidirectionalRange { start: 10, end: 15 };
    assert!(range.len() == 5);
    assert!(range.next() == Some(10));
    assert!(range.next_back() == Some(14));
    assert!(range.len() == 3);
    assert!(range.next_back() == Some(13));
    assert!(range.next() == Some(11));
    assert!(range.next() == Some(12));
    assert!(range.next().is_none());
    assert!(range.next_back().is_none());
    assert!(range.len() == 0);
}

fn test_flatten_double_ended() {
    let mut nested = [[1, 2], [3, 4]].into_iter().flatten();

    // Flatten is DoubleEnded if both outer and inner iterators are DoubleEnded.
    assert!(nested.next() == Some(1));
    assert!(nested.next_back() == Some(4));
    assert!(nested.next_back() == Some(3));
    assert!(nested.next() == Some(2));
    assert!(nested.next().is_none());
    assert!(nested.next_back().is_none());
}

fn test_nested_size_hint_propagation() {
    let a = [1, 2].into_iter();
    let b = [3, 4, 5].into_iter();

    let chained = a.chain(b);
    assert!(chained.size_hint() == (5, Some(5)));

    let filtered = chained.filter(|&x| x % 2 == 0);
    assert!(filtered.size_hint() == (0, Some(5)));

    let mapped = filtered.map(|x| x * 2);
    assert!(mapped.size_hint() == (0, Some(5)));
}

fn test_partial_array_into_iter_drop() {
    unsafe {
        DROP_TOTAL = 0;
    }
    {
        let mut iter = [DropToken(10), DropToken(20), DropToken(30)].into_iter();

        let first = iter.next();
        drop(first);
        assert!(unsafe { DROP_TOTAL } == 10);
    }
    assert!(unsafe { DROP_TOTAL } == 60);
}

fn test_slice_iteration_over_nominal_zsts() {
    let empty = [0_u32; 0];
    let slice: &[[u32; 0]] = &[empty, empty, empty];
    let expected = [&slice[0] as *const _, &slice[1] as *const _, &slice[2] as *const _];

    assert!(slice.len() == 3);
    for (index, value) in slice.iter().enumerate() {
        assert!(value.len() == 0);
        assert!(value as *const _ == expected[index]);
    }
    assert!(slice.iter().nth(2).unwrap() as *const _ == expected[2]);
    assert!(slice.iter().next_back().unwrap() as *const _ == expected[2]);
}

fn test_scan_with_function_item() {
    fn add(state: &mut isize, value: &usize) -> Option<f64> {
        *state += *value as isize;
        Some(*state as f64)
    }

    let source = [0_usize, 1, 2, 3, 4];
    let expected = [0.0, 1.0, 3.0, 6.0, 10.0];
    for (actual, expected) in source.iter().scan(0, add).zip(expected) {
        assert!(actual == expected);
    }
}

fn test_core_iterator_regressions() {
    // These cases mirror iterator compositions used by upstream coretests closely.
    assert!((0_usize..).take(10).collect::<Vec<_>>() == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    assert!(
        (0_usize..).step_by(1).take(10).collect::<Vec<_>>()
            == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    );
    assert!(
        (0_usize..10)
            .filter_map(|value| (value % 2 == 0).then_some(value * value))
            .collect::<Vec<_>>()
            == [0, 4, 16, 36, 64]
    );
    assert!(
        (0_usize..)
            .step_by(1)
            .filter_map(|value| (value % 2 == 0).then_some(value * value))
            .take(5)
            .collect::<Vec<_>>()
            == [0, 4, 16, 36, 64]
    );
    let filtered = (0_usize..)
        .step_by(1)
        .take(10)
        .filter_map(|value| (value % 2 == 0).then_some(value * value))
        .collect::<Vec<_>>();
    assert!(filtered == [0, 4, 16, 36, 64]);

    let values = [0, 1, 2, 3, 4, 5];
    let mut peekable = values.iter().peekable();
    assert!(peekable.peek() == Some(&&0));
    assert!(peekable.count() == 6);

    let windows = "abcd"
        .chars()
        .map_windows(|window: &[_; 2]| *window)
        .collect::<Vec<_>>();
    assert!(windows == [['a', 'b'], ['b', 'c'], ['c', 'd']]);

    assert!([1, 2, 2, 9].iter().is_sorted());
    assert!(![1, 3, 2].iter().is_sorted());
    assert!([1, 2, 2, 9].is_sorted());
    assert!(![1, 3, 2].is_sorted());

    let mut advance = [0, 1, 2, 3].iter();
    assert!(advance.advance_by(2) == Ok(()));
    assert!(advance.as_slice() == &[2, 3]);
    let mut advance_back = [0, 1, 2, 3].iter();
    assert!(advance_back.advance_back_by(2) == Ok(()));
    assert!(advance_back.as_slice() == &[0, 1]);
    assert!([0, 1, 2, 3].iter().copied().max() == Some(3));
    assert!([0, 1, 2, 3].iter().copied().min() == Some(0));
    assert!((0..11).map(Mod3).max().map(|value| value.0) == Some(8));
    assert!((0..11).map(Mod3).min().map(|value| value.0) == Some(0));

    let mut size_hint = MutableSizeHint((5, Some(5)));
    assert!(size_hint.next() == Some(0));
    assert!(size_hint.size_hint() == (4, Some(4)));

    let empty_repeat = std::iter::repeat_n(String::from("unused"), 0);
    assert!(std::format!("{empty_repeat:?}") == "RepeatN { count: 0, element: None }");
}

fn main() {
    test_core_iterator_regressions();
    array_iteration();
    ascii_escape_iterator();
    ascii_char_formatting();
    borrowed_iteration();
    fixed_u32_slice_iteration();
    loop_control();
    iterator_state();
    adapter_chains();
    searching_and_consumers();
    stateful_adapters();
    slice_iterators();
    generator_and_transform_adapters();
    cloned_generator_with_adapter_capture();
    double_ended_and_short_circuiting();
    drop_behavior();
    test_zip_edge_cases();
    test_chain_edge_cases();
    test_peek_mut();
    test_unzip_and_partition();
    test_fused_iterator_guarantees();
    test_char_and_str_iterators();
    test_drop_propagation_in_adapters();
    test_by_ref_borrowing_scopes();
    test_step_by_mechanics();
    test_cloned_copied_semantics();
    test_custom_exact_size_double_ended();
    test_flatten_double_ended();
    test_nested_size_hint_propagation();
    test_partial_array_into_iter_drop();
    test_slice_iteration_over_nominal_zsts();
    test_scan_with_function_item();
}
