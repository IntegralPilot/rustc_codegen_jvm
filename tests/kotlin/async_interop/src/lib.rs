use core::future::Future;
use core::pin::Pin;
use core::task::{Context, Poll};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

struct YieldOnce(bool);

impl Future for YieldOnce {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        if self.0 {
            Poll::Ready(())
        } else {
            self.0 = true;
            context.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

struct WakeStorm {
    remaining: u32,
    polls: i32,
}

impl Future for WakeStorm {
    type Output = i32;

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        self.polls += 1;
        if self.remaining == 0 {
            Poll::Ready(self.polls)
        } else {
            self.remaining -= 1;
            // Repeated wakes for one pending poll must collapse into one repoll.
            context.waker().wake_by_ref();
            context.waker().wake_by_ref();
            context.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

struct WakeAndReady(bool);

impl Future for WakeAndReady {
    type Output = i32;

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        assert!(!self.0, "wake-and-ready future was polled after completion");
        self.0 = true;
        context.waker().wake_by_ref();
        Poll::Ready(73)
    }
}

struct DelayedWake {
    ready: Arc<AtomicUsize>,
    spawned: bool,
    value: i32,
}

impl Future for DelayedWake {
    type Output = i32;

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        if self.ready.load(Ordering::Acquire) != 0 {
            return Poll::Ready(self.value);
        }
        if !self.spawned {
            self.spawned = true;
            let ready = self.ready.clone();
            let waker = context.waker().clone();
            thread::Builder::new()
                .name("rust-async-waker".to_string())
                .spawn(move || {
                    thread::sleep(Duration::from_millis(15));
                    ready.store(1, Ordering::Release);
                    waker.wake_by_ref();
                })
                .expect("could not spawn delayed Rust waker");
        }
        Poll::Pending
    }
}

struct ConcurrentWakeState {
    completed: AtomicUsize,
    current_waker: Mutex<Option<core::task::Waker>>,
}

struct ConcurrentWakes {
    state: Arc<ConcurrentWakeState>,
    started: bool,
    workers: usize,
}

impl Future for ConcurrentWakes {
    type Output = i32;

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        *self.state.current_waker.lock().unwrap() = Some(context.waker().clone());
        let completed = self.state.completed.load(Ordering::Acquire);
        if completed == self.workers {
            return Poll::Ready(i32::try_from(completed).unwrap());
        }
        if !self.started {
            self.started = true;
            for worker in 0..self.workers {
                let state = self.state.clone();
                thread::Builder::new()
                    .name(format!("rust-concurrent-waker-{worker}"))
                    .spawn(move || {
                        thread::sleep(Duration::from_millis(4 * (worker as u64 + 1)));
                        state.completed.fetch_add(1, Ordering::AcqRel);
                        if let Some(waker) = state.current_waker.lock().unwrap().as_ref() {
                            waker.wake_by_ref();
                        }
                    })
                    .expect("could not spawn concurrent Rust waker");
            }
        }
        Poll::Pending
    }
}

struct Never;

impl Future for Never {
    type Output = ();

    fn poll(self: Pin<&mut Self>, _context: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Pending
    }
}

struct DropProbe;

impl Drop for DropProbe {
    fn drop(&mut self) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }
}

#[repr(align(64))]
struct AlignedCapture {
    bytes: [u8; 65],
    value: i32,
}

pub struct AsyncReport {
    pub seed: i32,
    pub stages: i32,
    pub checksum: i32,
    pub aligned: bool,
}

pub enum AsyncOutcome {
    Completed(AsyncReport),
    Rejected(i32),
}

pub async fn immediate(value: i32) -> i32 {
    value + 1
}

pub async fn add_after_yield(left: i32, right: i32) -> i32 {
    YieldOnce(false).await;
    left + right
}

pub async fn chained(value: i32) -> i32 {
    let first = add_after_yield(value, 1).await;
    add_after_yield(first, 1).await
}

pub async fn unit_after_yield() {
    YieldOnce(false).await;
}

pub async fn synchronous_wake_storm(rounds: u32) -> i32 {
    WakeStorm {
        remaining: rounds,
        polls: 0,
    }
    .await
}

pub async fn wake_and_ready() -> i32 {
    WakeAndReady(false).await
}

pub async fn delayed_from_rust_thread(value: i32) -> i32 {
    DelayedWake {
        ready: Arc::new(AtomicUsize::new(0)),
        spawned: false,
        value,
    }
    .await
}

pub async fn concurrent_rust_wakers(workers: u32) -> i32 {
    ConcurrentWakes {
        state: Arc::new(ConcurrentWakeState {
            completed: AtomicUsize::new(0),
            current_waker: Mutex::new(None),
        }),
        started: false,
        workers: usize::try_from(workers).unwrap(),
    }
    .await
}

pub async fn aligned_capture(value: i32) -> i32 {
    let mut capture = AlignedCapture {
        bytes: [0; 65],
        value,
    };
    capture.bytes[0] = 3;
    capture.bytes[64] = 7;
    WakeStorm {
        remaining: 3,
        polls: 0,
    }
    .await;
    let address = core::ptr::addr_of!(capture) as usize;
    assert_eq!(address % core::mem::align_of::<AlignedCapture>(), 0);
    capture.value + i32::from(capture.bytes[0]) + i32::from(capture.bytes[64])
}

pub async fn extensive_workflow(seed: i32, reject: bool) -> AsyncOutcome {
    YieldOnce(false).await;
    if reject {
        return AsyncOutcome::Rejected(seed);
    }

    let wake_storm_polls = WakeStorm {
        remaining: 5,
        polls: 0,
    }
    .await;
    let delayed = delayed_from_rust_thread(seed * 3 + 1).await;
    let concurrent = concurrent_rust_wakers(4).await;
    let aligned_value = aligned_capture(seed).await;
    AsyncOutcome::Completed(AsyncReport {
        seed,
        stages: 4,
        checksum: seed + wake_storm_polls + delayed + concurrent + aligned_value,
        aligned: true,
    })
}

pub fn reset_drop_count() {
    DROP_COUNT.store(0, Ordering::SeqCst);
}

pub fn drop_count() -> usize {
    DROP_COUNT.load(Ordering::SeqCst)
}

pub async fn tracked_completion(value: i32) -> i32 {
    let _probe = DropProbe;
    YieldOnce(false).await;
    value
}

pub async fn abandoned_future() {
    let _probe = DropProbe;
    Never.await;
}

pub fn silence_panics() {
    std::panic::set_hook(Box::new(|_| {}));
}

pub async fn panic_after_yield() -> i32 {
    let _probe = DropProbe;
    YieldOnce(false).await;
    panic!("intentional Kotlin async bridge panic")
}
