//! Bounded handoff between MIR construction and independent JVM emission.
use std::{
    sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
    thread::Scope,
};

pub(crate) struct Workers<T, R> {
    producer: Producer<T>,
    results: mpsc::Receiver<R>,
}

pub(crate) struct Producer<T> {
    jobs: mpsc::SyncSender<T>,
    submitted: Arc<AtomicUsize>,
}

impl<T> Clone for Producer<T> {
    fn clone(&self) -> Self {
        Self {
            jobs: self.jobs.clone(),
            submitted: Arc::clone(&self.submitted),
        }
    }
}

pub(crate) fn start<'scope, 'env, T: Send + 'scope, R: Send + 'scope>(
    scope: &'scope Scope<'scope, 'env>,
    count: usize,
    depth: usize,
    work: impl Fn(T) -> R + Send + Sync + 'scope,
) -> Workers<T, R> {
    assert!(count > 0);
    let (jobs, receiver) = mpsc::sync_channel(depth);
    let receiver = Arc::new(Mutex::new(receiver));
    let (sender, results) = mpsc::channel();
    let work = Arc::new(work);
    for _ in 0..count {
        let receiver = Arc::clone(&receiver);
        let sender = sender.clone();
        let work = Arc::clone(&work);
        scope.spawn(move || {
            loop {
                let job = receiver
                    .lock()
                    .expect("emission queue lock poisoned")
                    .recv();
                let Ok(job) = job else {
                    break;
                };
                if sender.send(work(job)).is_err() {
                    break;
                }
            }
        });
    }
    // Only workers may retain these endpoints. Otherwise worker failure can
    // leave a producer blocked forever on an apparently connected full queue.
    drop(receiver);
    drop(sender);
    Workers {
        producer: Producer {
            jobs,
            submitted: Arc::new(AtomicUsize::new(0)),
        },
        results,
    }
}

impl<T, R> Workers<T, R> {
    pub(crate) fn producer(&self) -> Producer<T> {
        self.producer.clone()
    }
    pub(crate) fn submit(&self, job: T) {
        self.producer.submit(job);
    }
    pub(crate) fn finish(self) -> Vec<R> {
        let submitted = Arc::clone(&self.producer.submitted);
        drop(self.producer);
        let results: Vec<_> = self.results.into_iter().collect();
        assert_eq!(
            results.len(),
            submitted.load(Ordering::Relaxed),
            "JVM emission worker stopped without a result"
        );
        results
    }
}

impl<T> Producer<T> {
    pub(crate) fn submit(&self, job: T) {
        assert!(
            self.jobs.send(job).is_ok(),
            "JVM emission workers stopped unexpectedly"
        );
        self.submitted.fetch_add(1, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{panic::catch_unwind, time::Duration};

    #[test]
    fn worker_failure_disconnects_a_full_queue() {
        let (done, result) = mpsc::channel();
        std::thread::spawn(move || {
            let failed = catch_unwind(|| {
                std::thread::scope(|scope| {
                    let workers = start(scope, 1, 1, |_: usize| -> usize {
                        panic!("injected worker failure")
                    });
                    for n in 0..10 {
                        workers.submit(n);
                    }
                    workers.finish();
                })
            })
            .is_err();
            done.send(failed).unwrap();
        });
        assert!(
            result
                .recv_timeout(Duration::from_secs(3))
                .expect("producer blocked after worker failure")
        );
    }

    #[test]
    fn processes_each_job_and_drains_the_queue() {
        let mut results = std::thread::scope(|scope| {
            let workers = start(scope, 3, 1, |n| n * n);
            for n in 0..100 {
                workers.submit(n);
            }
            workers.finish()
        });
        results.sort_unstable();
        assert_eq!(results, (0..100).map(|n| n * n).collect::<Vec<_>>());
    }

    #[test]
    fn concurrent_producers_complete_before_the_final_count_is_checked() {
        let mut results = std::thread::scope(|scope| {
            let workers = start(scope, 3, 1, |n| n);
            for group in 0..4 {
                let producer = workers.producer();
                scope.spawn(move || {
                    for n in 0..100 {
                        producer.submit(group * 100 + n);
                    }
                });
            }
            workers.finish()
        });
        results.sort_unstable();
        assert_eq!(results, (0..400).collect::<Vec<_>>());
    }
}
