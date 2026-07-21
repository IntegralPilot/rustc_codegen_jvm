use std::cell::Cell;
use std::collections::HashMap;
use std::hash::{BuildHasherDefault, DefaultHasher};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Barrier, Condvar, Mutex, Once, OnceLock, RwLock, mpsc};
use std::thread;
use std::time::Duration;

static TLS_DROPS: AtomicUsize = AtomicUsize::new(0);
static ONCE: Once = Once::new();
static ONCE_CALLS: AtomicUsize = AtomicUsize::new(0);
static CELL: OnceLock<usize> = OnceLock::new();

struct TlsDrop;

impl Drop for TlsDrop {
    fn drop(&mut self) {
        TLS_DROPS.fetch_add(1, Ordering::SeqCst);
    }
}

thread_local! {
    static TLS_VALUE: (Cell<usize>, TlsDrop) = (Cell::new(7), TlsDrop);
}

fn main() {
    spawn_join_and_names();
    scoped_threads();
    mutex_and_poisoning();
    rwlock_and_once();
    condvar_and_barrier();
    park_and_unpark();
    thread_local_storage();
    hash_maps_across_threads();
}

fn spawn_join_and_names() {
    let main_id = thread::current().id();
    let worker = thread::Builder::new()
        .name("named-rust-worker".into())
        .stack_size(512 * 1024)
        .spawn(move || {
            assert!(thread::current().id() != main_id);
            assert!(thread::current().name() == Some("named-rust-worker"));
            thread::yield_now();
            thread::sleep(Duration::from_millis(1));
            42
        })
        .unwrap();
    assert!(worker.join().unwrap() == 42);
    assert!(thread::available_parallelism().unwrap().get() >= 1);

    let panic = thread::spawn(|| panic!("expected worker panic"));
    let payload = panic.join().unwrap_err();
    assert!(payload.downcast_ref::<&str>() == Some(&"expected worker panic"));
}

fn scoped_threads() {
    let values = [2, 3, 5, 7];
    let sum = Mutex::new(0);
    thread::scope(|scope| {
        for value in &values {
            let sum = &sum;
            scope.spawn(move || *sum.lock().unwrap() += value);
        }
    });
    assert!(*sum.lock().unwrap() == 17);
}

fn mutex_and_poisoning() {
    let counter = Arc::new(Mutex::new(0));
    let mut workers = Vec::new();
    for _ in 0..4 {
        let counter = Arc::clone(&counter);
        workers.push(thread::spawn(move || {
            for _ in 0..250 {
                *counter.lock().unwrap() += 1;
            }
        }));
    }
    for worker in workers {
        worker.join().unwrap();
    }
    assert!(*counter.lock().unwrap() == 1_000);

    let poisoned = Arc::new(Mutex::new(0));
    let worker_value = Arc::clone(&poisoned);
    assert!(
        thread::spawn(move || {
            let mut value = worker_value.lock().unwrap();
            *value = 9;
            panic!("poison the mutex");
        })
        .join()
        .is_err()
    );
    assert!(poisoned.is_poisoned());
    assert!(**poisoned.lock().unwrap_err().get_ref() == 9);
}

fn rwlock_and_once() {
    let values = Arc::new(RwLock::new(vec![1, 2]));
    let writer_values = Arc::clone(&values);
    let writer = thread::spawn(move || writer_values.write().unwrap().push(3));
    writer.join().unwrap();

    let mut readers = Vec::new();
    for _ in 0..3 {
        let values = Arc::clone(&values);
        readers.push(thread::spawn(move || {
            values.read().unwrap().iter().sum::<i32>()
        }));
    }
    for reader in readers {
        assert!(reader.join().unwrap() == 6);
    }

    let mut once_workers = Vec::new();
    for _ in 0..8 {
        once_workers.push(thread::spawn(|| {
            ONCE.call_once(|| {
                ONCE_CALLS.fetch_add(1, Ordering::SeqCst);
            });
            assert!(*CELL.get_or_init(|| 123) == 123);
        }));
    }
    for worker in once_workers {
        worker.join().unwrap();
    }
    assert!(ONCE_CALLS.load(Ordering::SeqCst) == 1);
}

fn condvar_and_barrier() {
    let state = Arc::new((Mutex::new(false), Condvar::new()));
    let worker_state = Arc::clone(&state);
    let worker = thread::spawn(move || {
        let (ready, condition) = &*worker_state;
        let mut ready = ready.lock().unwrap();
        while !*ready {
            ready = condition.wait(ready).unwrap();
        }
        88
    });
    let (ready, condition) = &*state;
    *ready.lock().unwrap() = true;
    condition.notify_one();
    assert!(worker.join().unwrap() == 88);

    let timed = Mutex::new(false);
    let result = Condvar::new()
        .wait_timeout(timed.lock().unwrap(), Duration::from_millis(2))
        .unwrap();
    assert!(result.1.timed_out());

    let barrier = Arc::new(Barrier::new(4));
    let passed = Arc::new(AtomicUsize::new(0));
    let mut workers = Vec::new();
    for _ in 0..3 {
        let barrier = Arc::clone(&barrier);
        let passed = Arc::clone(&passed);
        workers.push(thread::spawn(move || {
            barrier.wait();
            passed.fetch_add(1, Ordering::SeqCst);
        }));
    }
    barrier.wait();
    for worker in workers {
        worker.join().unwrap();
    }
    assert!(passed.load(Ordering::SeqCst) == 3);
}

fn park_and_unpark() {
    let (sender, receiver) = mpsc::channel();
    let resumed = Arc::new(AtomicBool::new(false));
    let worker_resumed = Arc::clone(&resumed);
    let worker = thread::spawn(move || {
        sender.send(thread::current()).unwrap();
        while !worker_resumed.load(Ordering::Acquire) {
            thread::park();
        }
    });
    let worker_thread = receiver.recv().unwrap();
    resumed.store(true, Ordering::Release);
    worker_thread.unpark();
    worker.join().unwrap();
    thread::park_timeout(Duration::from_millis(1));
}

fn thread_local_storage() {
    TLS_VALUE.with(|value| assert!(value.0.get() == 7));
    let worker = thread::spawn(|| {
        TLS_VALUE.with(|value| {
            assert!(value.0.get() == 7);
            value.0.set(99);
        });
        TLS_VALUE.with(|value| assert!(value.0.get() == 99));
    });
    worker.join().unwrap();
    assert!(TLS_DROPS.load(Ordering::SeqCst) == 1);
    TLS_VALUE.with(|value| assert!(value.0.get() == 7));
}

fn hash_maps_across_threads() {
    let mut map = HashMap::<usize, usize, BuildHasherDefault<DefaultHasher>>::default();
    for value in 0..64 {
        map.insert(value, value * value);
    }
    assert!(map.get(&17) == Some(&289));

    let map = Arc::new(map);
    let mut workers = Vec::new();
    for offset in 0..4 {
        let map = Arc::clone(&map);
        workers.push(thread::spawn(move || {
            (offset..64).step_by(4).map(|key| map[&key]).sum::<usize>()
        }));
    }
    let total = workers
        .into_iter()
        .map(|worker| worker.join().unwrap())
        .sum::<usize>();
    assert!(total == (0..64).map(|value| value * value).sum::<usize>());
}
