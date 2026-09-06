//! Bounded handoff between MIR construction and independent JVM emission.
use std::{
    sync::{Arc, Mutex, mpsc},
    thread::Scope,
};

pub(crate) struct Workers<T, R> {
    jobs: mpsc::SyncSender<T>,
    results: mpsc::Receiver<R>,
    submitted: usize,
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
        jobs,
        results,
        submitted: 0,
    }
}

impl<T, R> Workers<T, R> {
    pub(crate) fn submit(&mut self, job: T) {
        assert!(
            self.jobs.send(job).is_ok(),
            "JVM emission workers stopped unexpectedly"
        );
        self.submitted += 1;
    }

    pub(crate) fn finish(self) -> Vec<R> {
        drop(self.jobs);
        let results: Vec<_> = self.results.into_iter().collect();
        assert_eq!(
            results.len(),
            self.submitted,
            "JVM emission worker stopped without a result"
        );
        results
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
                    let mut workers = start(scope, 1, 1, |_: usize| -> usize {
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
            let mut workers = start(scope, 3, 1, |n| n * n);
            for n in 0..100 {
                workers.submit(n);
            }
            workers.finish()
        });
        results.sort_unstable();
        assert_eq!(results, (0..100).map(|n| n * n).collect::<Vec<_>>());
    }
}
