use std::convert::Infallible;
use std::hint::black_box;
use std::ops::ControlFlow;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Clone, Debug)]
enum Value<T> {
    Only(T),
}
impl PartialEq for Value<i32> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
impl Value<i32> {
    fn wrapping_add(self, _: Self) -> Self {
        Value::Only(113)
    }
}
#[derive(Debug)]
enum Empty {
    Only(()),
}
impl PartialEq for Empty {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
static DROPS: AtomicUsize = AtomicUsize::new(0);
impl Drop for Empty {
    fn drop(&mut self) {
        DROPS.fetch_add(1, Ordering::Relaxed);
    }
}
static STORED: Result<u64, Infallible> = Ok(0x1234_5678_abcdef);

static EXPECTED_ADDRESS: AtomicUsize = AtomicUsize::new(0);
static PAYLOAD_DROPS: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
struct Tracked(usize);
impl Drop for Tracked {
    fn drop(&mut self) {
        assert_eq!(self.0, 91);
        PAYLOAD_DROPS.fetch_add(1, Ordering::Relaxed);
    }
}
#[derive(Debug)]
enum Dropped {
    Only(Tracked),
}
impl Drop for Dropped {
    fn drop(&mut self) {
        let expected = EXPECTED_ADDRESS.load(Ordering::Relaxed);
        if expected != 0 {
            assert_eq!(self as *mut Self as usize, expected);
        }
        let Self::Only(payload) = self;
        payload.0 = 91;
    }
}

fn drops() {
    let before = PAYLOAD_DROPS.load(Ordering::Relaxed);
    let mut value = Box::new(Dropped::Only(Tracked(17)));
    EXPECTED_ADDRESS.store((&mut *value) as *mut Dropped as usize, Ordering::Relaxed);
    drop(black_box(value));
    EXPECTED_ADDRESS.store(0, Ordering::Relaxed);
    let values = vec![Dropped::Only(Tracked(19)), Dropped::Only(Tracked(23))];
    drop(black_box(values));
    assert_eq!(PAYLOAD_DROPS.load(Ordering::Relaxed), before + 3);
    let value: Box<dyn std::fmt::Debug> = Box::new(Dropped::Only(Tracked(29)));
    drop(black_box(value));
    assert_eq!(PAYLOAD_DROPS.load(Ordering::Relaxed), before + 4);
    // Box slice drop glue must borrow the existing fat pointer.
    let values: Box<[Tracked]> = vec![Tracked(91), Tracked(91)].into_boxed_slice();
    drop(black_box(values));
    assert_eq!(PAYLOAD_DROPS.load(Ordering::Relaxed), before + 6);
    struct Tail<T: ?Sized> {
        header: usize,
        values: T,
    }
    impl<T: ?Sized> Drop for Tail<T> {
        fn drop(&mut self) {
            assert_eq!(self.header, 101);
        }
    }
    let tail: std::sync::Arc<Tail<[Tracked]>> = std::sync::Arc::new(Tail {
        header: 101,
        values: [Tracked(91), Tracked(91)],
    });
    assert_eq!(tail.values.len(), 2);
    drop(black_box(tail));
    assert_eq!(PAYLOAD_DROPS.load(Ordering::Relaxed), before + 8);
    let before = DROPS.load(Ordering::Relaxed);
    drop(black_box(vec![Empty::Only(()), Empty::Only(())]));
    assert_eq!(DROPS.load(Ordering::Relaxed), before + 2);
}

#[inline(never)]
fn success<T>(value: T) -> Result<T, Infallible> {
    Ok(value)
}
#[inline(never)]
fn failure<T>(value: T) -> Result<Infallible, T> {
    Err(value)
}
#[inline(never)]
fn forward<T>(value: T) -> ControlFlow<Infallible, T> {
    ControlFlow::Continue(value)
}
#[inline(never)]
fn unwrap<T>(value: Result<T, Infallible>) -> T {
    match value {
        Ok(value) => value,
        Err(never) => match never {},
    }
}

fn unwrap_ref(value: Result<&u64, Infallible>) -> &u64 {
    unwrap(value)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Zero;

pub fn run() {
    drops();
    const ZERO: Result<Infallible, Zero> = Err(Zero);
    assert_eq!(black_box(ZERO).unwrap_err(), Zero);
    let values: Vec<_> = [3i32, 5].into_iter().map(Value::Only).collect();
    assert!(matches!(values[1], Value::Only(5)));
    assert_eq!(STORED, Ok(0x1234_5678_abcdef));
    assert_eq!(unwrap(success(black_box(31))), 31);
    assert_eq!(failure(black_box(37)).unwrap_err(), 37);
    assert_eq!(forward(black_box(41)).continue_value(), Some(41));
    let item = black_box(String::from("carrier"));
    let value = success(item);
    let cloned = value.clone();
    assert_eq!(unwrap(cloned), "carrier");
    assert_eq!(unwrap(value), "carrier");
    let mut wrapped = Value::Only(black_box(43u64));
    let Value::Only(payload) = &mut wrapped;
    *payload = 47;
    let Value::Only(value) = wrapped;
    assert_eq!(value, 47);
    let bits: u64 = unsafe { std::mem::transmute(black_box(Value::Only(53u64))) };
    assert_eq!(bits, 53);
    let mut result = black_box(Ok::<_, Infallible>(59u64));
    let Ok(payload) = &mut result else {
        unreachable!()
    };
    *payload = 61;
    assert_eq!(
        unsafe { *(&result as *const Result<u64, Infallible>).cast::<u64>() },
        61
    );
    assert!(black_box(Value::Only(3i32)) != black_box(Value::Only(3i32)));
    let Value::Only(wrapped_sum) = black_box(Value::Only(3i32)).wrapping_add(Value::Only(5));
    assert_eq!(wrapped_sum, 113);
    let before = DROPS.load(Ordering::Relaxed);
    {
        let a = black_box(Empty::Only(()));
        let b = black_box(Empty::Only(()));
        assert!(a != b);
    }
    assert_eq!(DROPS.load(Ordering::Relaxed), before + 2);
    let borrow: fn(Result<&u64, Infallible>) -> &u64 = black_box(unwrap_ref);
    assert_eq!(*borrow(black_box(Ok(&value))), 47);
    let slice = black_box([3, 5, 7]);
    assert_eq!(unwrap(success(&slice[1..])), &[5, 7]);
    assert_eq!(
        std::mem::discriminant(&success(3)),
        std::mem::discriminant(&success(5))
    );
}
