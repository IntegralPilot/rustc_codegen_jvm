use std::{mem, sync::mpsc, thread};

pub fn run() {
    for detach in [true, false] {
        let (ready, started) = mpsc::channel();
        let worker = thread::spawn(move || {
            ready.send(()).unwrap();
            loop {
                thread::park();
            }
        });
        started.recv().unwrap();

        // Returning from main must terminate even if workers are still alive,
        // whether or not their JoinHandles have been dropped.
        if detach {
            drop(worker);
        } else {
            mem::forget(worker);
        }
    }
}
