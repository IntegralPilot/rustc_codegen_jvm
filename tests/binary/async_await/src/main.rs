use std::future::{Future, IntoFuture, pending, poll_fn, ready};
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

static WAKE_COUNT: AtomicUsize = AtomicUsize::new(0);
static POLL_COUNT: AtomicUsize = AtomicUsize::new(0);

fn dummy_raw_waker() -> RawWaker {
    fn clone(_: *const ()) -> RawWaker {
        dummy_raw_waker()
    }
    fn wake(_: *const ()) {
        WAKE_COUNT.fetch_add(1, Ordering::SeqCst);
    }
    fn no_op(_: *const ()) {}

    let vtable = &RawWakerVTable::new(clone, wake, wake, no_op);
    RawWaker::new(std::ptr::null(), vtable)
}

fn dummy_waker() -> Waker {
    unsafe { Waker::from_raw(dummy_raw_waker()) }
}

fn block_on<F: Future>(mut future: F) -> F::Output {
    let waker = dummy_waker();
    let mut context = Context::from_waker(&waker);
    let mut future = unsafe { Pin::new_unchecked(&mut future) };
    for _ in 0..10_000 {
        match future.as_mut().poll(&mut context) {
            Poll::Ready(output) => return output,
            Poll::Pending => {}
        }
    }
    panic!("test future did not complete");
}

struct Countdown {
    remaining: u32,
    output: i32,
}

impl Future for Countdown {
    type Output = i32;

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        POLL_COUNT.fetch_add(1, Ordering::SeqCst);
        if self.remaining == 0 {
            Poll::Ready(self.output)
        } else {
            self.remaining -= 1;
            context.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

async fn basic() -> i32 {
    42
}

async fn multiple_suspensions(seed: i32) -> i32 {
    let first = basic().await;
    let middle = Countdown {
        remaining: 3,
        output: 10,
    }
    .await;
    let last = basic().await;
    seed + first + middle + last
}

async fn state_across_loop() -> Vec<i32> {
    let mut values = Vec::new();
    for value in 0..5 {
        values.push(value);
        ready(()).await;
    }
    values
}

async fn nested_blocks() -> i32 {
    async {
        let inner = async { basic().await + 10 }.await;
        inner + 100
    }
    .await
}

async fn mutate_borrowed(value: &mut i32) -> i32 {
    let held_across_await = &mut *value;
    ready(()).await;
    *held_across_await += 7;
    *held_across_await
}

async fn generic_map<T, F>(value: T, operation: F) -> T
where
    F: FnOnce(T) -> T,
{
    ready(()).await;
    operation(value)
}

async fn fallible(value: i32) -> Result<i32, &'static str> {
    let doubled = ready(value.checked_mul(2).ok_or("overflow")?).await;
    if doubled < 0 {
        Err("negative")
    } else {
        Ok(doubled + 1)
    }
}

struct Deferred(i32);

impl IntoFuture for Deferred {
    type Output = i32;
    type IntoFuture = std::future::Ready<i32>;

    fn into_future(self) -> Self::IntoFuture {
        ready(self.0)
    }
}

async fn custom_into_future() -> i32 {
    Deferred(40).await + 2
}

trait AsyncCompute {
    async fn compute(&self, input: i32) -> i32;
}

struct Multiplier(i32);

impl AsyncCompute for Multiplier {
    async fn compute(&self, input: i32) -> i32 {
        ready(()).await;
        self.0 * input
    }
}

fn boxed_future(value: i32) -> Pin<Box<dyn Future<Output = i32>>> {
    Box::pin(async move { value + basic().await })
}

fn recursive_sum(n: u64) -> Pin<Box<dyn Future<Output = u64>>> {
    Box::pin(async move {
        if n == 0 {
            0
        } else {
            n + recursive_sum(n - 1).await
        }
    })
}

trait BoxedJob {
    fn run<'a>(&'a self, input: i32) -> Pin<Box<dyn Future<Output = i32> + Send + 'a>>;
}

struct OffsetJob(i32);

impl BoxedJob for OffsetJob {
    fn run<'a>(&'a self, input: i32) -> Pin<Box<dyn Future<Output = i32> + Send + 'a>> {
        Box::pin(async move {
            ready(()).await;
            self.0 + input
        })
    }
}

struct DropFlag(Arc<AtomicUsize>);

impl Drop for DropFlag {
    fn drop(&mut self) {
        self.0.fetch_add(1, Ordering::SeqCst);
    }
}

fn cancellation_drops_state() {
    let drops = Arc::new(AtomicUsize::new(0));
    let captured = drops.clone();
    let mut concrete = Box::pin(async move {
        let _flag = DropFlag(captured);
        pending::<()>().await;
    });
    let waker = dummy_waker();
    let mut context = Context::from_waker(&waker);
    assert!(matches!(
        concrete.as_mut().poll(&mut context),
        Poll::Pending
    ));
    assert!(drops.load(Ordering::SeqCst) == 0);
    drop(concrete);
    assert!(drops.load(Ordering::SeqCst) == 1);

    let captured = drops.clone();
    let mut dynamic: Pin<Box<dyn Future<Output = ()>>> = Box::pin(async move {
        let _flag = DropFlag(captured);
        pending::<()>().await;
    });
    assert!(matches!(
        dynamic.as_mut().poll(&mut context),
        Poll::Pending
    ));
    drop(dynamic);
    assert!(drops.load(Ordering::SeqCst) == 2);
}

fn main() {
    assert!(block_on(basic()) == 42);

    POLL_COUNT.store(0, Ordering::SeqCst);
    WAKE_COUNT.store(0, Ordering::SeqCst);
    assert!(block_on(multiple_suspensions(1)) == 95);
    assert!(POLL_COUNT.load(Ordering::SeqCst) == 4);
    assert!(WAKE_COUNT.load(Ordering::SeqCst) == 3);

    assert!(block_on(state_across_loop()) == [0, 1, 2, 3, 4]);
    assert!(block_on(nested_blocks()) == 152);

    let mut borrowed = 10;
    assert!(block_on(mutate_borrowed(&mut borrowed)) == 17);
    assert!(borrowed == 17);

    let suffix = String::from(" state");
    let moved = async move {
        ready(()).await;
        String::from("async") + &suffix
    };
    assert!(block_on(moved) == "async state");

    assert!(block_on(generic_map(20, |value| value + 22)) == 42);
    assert!(block_on(fallible(20)) == Ok(41));
    assert!(block_on(fallible(-1)) == Err("negative"));
    assert!(block_on(custom_into_future()) == 42);
    assert!(block_on(Multiplier(6).compute(7)) == 42);

    let async_closure = async |left: i32, right: i32| {
        ready(()).await;
        left + right
    };
    assert!(block_on(async_closure(19, 23)) == 42);

    assert!(block_on(boxed_future(8)) == 50);
    assert!(block_on(recursive_sum(32)) == 528);

    let dynamic_futures: Vec<Pin<Box<dyn Future<Output = i32>>>> = vec![
        Box::pin(async { 1 }),
        Box::pin(ready(2)),
        Box::pin(Countdown {
            remaining: 2,
            output: 3,
        }),
    ];
    let mut dynamic_total = 0;
    for future in dynamic_futures {
        dynamic_total += block_on(future);
    }
    assert!(dynamic_total == 6);

    let job = OffsetJob(40);
    assert!(block_on(job.run(2)) == 42);
    let threaded: Pin<Box<dyn Future<Output = i32> + Send>> =
        Box::pin(async move { Countdown { remaining: 1, output: 42 }.await });
    assert!(std::thread::spawn(move || block_on(threaded)).join().unwrap() == 42);

    let mut poll_attempts = 0;
    let from_poll_fn = poll_fn(|context| {
        poll_attempts += 1;
        if poll_attempts == 3 {
            Poll::Ready(42)
        } else {
            context.waker().wake_by_ref();
            Poll::Pending
        }
    });
    assert!(block_on(from_poll_fn) == 42);

    let panic = std::panic::catch_unwind(|| {
        block_on(async {
            ready(()).await;
            panic!("panic after await");
        });
    });
    assert!(panic.is_err());

    cancellation_drops_state();
}
