package org.rustlang.runtime;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicLong;

/**
 * JVM platform services used by Rust {@code std::thread}, TLS and futexes.
 */
public final class ThreadSupport {
    private ThreadSupport() {}

    private static final String RUST_THREAD_START_CLASS =
            "org.rustlang.runtime.symbols.__jvm_thread_start";
    private static final String RUST_THREAD_START_METHOD = "__jvm_thread_start";
    private static final AtomicLong NEXT_THREAD_ID = new AtomicLong(1);
    private static final AtomicLong NEXT_TLS_KEY = new AtomicLong(1);
    private static final ConcurrentHashMap<Long, ThreadRecord> THREADS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, Object> TLS_DESTRUCTORS =
            new ConcurrentHashMap<>();
    private static final ThreadLocal<Map<Long, Pointer>> TLS =
            ThreadLocal.withInitial(HashMap::new);
    private static volatile MethodHandle rustThreadStart;

    private static final class ThreadRecord {
        private Thread thread;
        private volatile Throwable failure;
        private volatile boolean detached;
    }

    private static String utf8(Pointer bytes, long length) {
        int checkedLength = Math.toIntExact(length);
        byte[] copy = new byte[checkedLength];
        for (int index = 0; index < checkedLength; index++) {
            copy[index] = bytes.add(index).getI8();
        }
        return new String(copy, StandardCharsets.UTF_8);
    }

    private static MethodHandle rustThreadStart() throws ReflectiveOperationException {
        MethodHandle cached = rustThreadStart;
        if (cached != null) {
            return cached;
        }
        synchronized (ThreadSupport.class) {
            cached = rustThreadStart;
            if (cached == null) {
                Class<?> owner = Class.forName(RUST_THREAD_START_CLASS);
                cached = MethodHandles.publicLookup().findStatic(
                        owner,
                        RUST_THREAD_START_METHOD,
                        MethodType.methodType(void.class, Pointer.class));
                rustThreadStart = cached;
            }
            return cached;
        }
    }

    /** Starts one Rust standard-library thread and returns its runtime handle. */
    public static long spawn(Pointer init, long stackSize) {
        if (init == null) {
            throw new NullPointerException("Rust ThreadInit pointer cannot be null");
        }
        if (stackSize < 0) {
            throw new IllegalArgumentException("JVM thread stack size exceeds signed long range");
        }

        final MethodHandle start;
        try {
            start = rustThreadStart();
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not resolve Rust thread entry point", error);
        }

        long id = NEXT_THREAD_ID.getAndIncrement();
        ThreadRecord record = new ThreadRecord();
        Runnable body = () -> {
            try {
                start.invokeExact(init);
            } catch (Throwable failure) {
                boolean report;
                synchronized (record) {
                    report = record.detached;
                    if (!report) {
                        record.failure = failure;
                    }
                }
                if (report) {
                    reportUncaughtFailure(failure);
                }
            } finally {
                TLS.remove();
            }
        };

        try {
            record.thread = stackSize == 0
                    ? new Thread(body, "rust-thread-" + id)
                    : new Thread(null, body, "rust-thread-" + id, stackSize);
            THREADS.put(id, record);
            record.thread.start();
            return id;
        } catch (RuntimeException | Error failure) {
            THREADS.remove(id);
            return 0;
        }
    }

    /** Joins and consumes a Rust standard-library thread handle. */
    public static void join(long id) {
        ThreadRecord record = THREADS.get(id);
        if (record == null) {
            throw new IllegalStateException("unknown or already consumed Rust thread handle " + id);
        }
        boolean interrupted = false;
        for (;;) {
            try {
                record.thread.join();
                break;
            } catch (InterruptedException error) {
                interrupted = true;
            }
        }
        THREADS.remove(id, record);
        if (interrupted) {
            Thread.currentThread().interrupt();
        }
        if (record.failure != null) {
            rethrowWorkerFailure(record.failure);
        }
    }

    /** Detaches a Rust thread whose JoinHandle was dropped. */
    public static void detach(long id) {
        ThreadRecord record = THREADS.remove(id);
        if (record == null) {
            return;
        }
        Throwable failure;
        synchronized (record) {
            record.detached = true;
            failure = record.failure;
            record.failure = null;
        }
        if (failure != null) {
            reportUncaughtFailure(failure);
        }
    }

    public static long currentThreadId() {
        return Thread.currentThread().getId();
    }

    public static int availableProcessors() {
        return Math.max(1, Runtime.getRuntime().availableProcessors());
    }

    public static void yieldCurrent() {
        Thread.yield();
    }

    public static void sleep(long seconds, int nanoseconds) {
        if (seconds < 0 || nanoseconds < 0 || nanoseconds >= 1_000_000_000) {
            throw new IllegalArgumentException("invalid Rust sleep duration");
        }
        long remaining = Math.addExact(Math.multiplyExact(seconds, 1_000_000_000L), nanoseconds);
        boolean interrupted = false;
        while (remaining > 0) {
            long started = System.nanoTime();
            try {
                Thread.sleep(remaining / 1_000_000, (int) (remaining % 1_000_000));
                break;
            } catch (InterruptedException error) {
                interrupted = true;
                long elapsed = System.nanoTime() - started;
                remaining = Math.max(0, remaining - Math.max(0, elapsed));
            }
        }
        if (interrupted) {
            Thread.currentThread().interrupt();
        }
    }

    public static void setCurrentName(Pointer bytes, long length) {
        Thread.currentThread().setName(utf8(bytes, length));
    }

    /** Returns true unless the wait reached its timeout. */
    public static boolean futexWait(Pointer address, int expected, long timeoutNanos) {
        Object monitor = Pointer.atomicMonitor(address);
        boolean interrupted = false;
        synchronized (monitor) {
            if ((int) Pointer.atomicLoad(address, 4, 0) != expected) {
                return true;
            }
            if (timeoutNanos == 0) {
                return false;
            }
            long remaining = timeoutNanos;
            for (;;) {
                long started = System.nanoTime();
                try {
                    if (timeoutNanos < 0) {
                        monitor.wait();
                        if (interrupted) {
                            Thread.currentThread().interrupt();
                        }
                        return true;
                    }
                    if (remaining <= 0) {
                        if (interrupted) {
                            Thread.currentThread().interrupt();
                        }
                        return false;
                    }
                    monitor.wait(remaining / 1_000_000, (int) (remaining % 1_000_000));
                    long elapsed = Math.max(0, System.nanoTime() - started);
                    remaining = Math.max(0, remaining - elapsed);
                    if (remaining == 0
                            && (int) Pointer.atomicLoad(address, 4, 0) == expected) {
                        if (interrupted) {
                            Thread.currentThread().interrupt();
                        }
                        return false;
                    }
                    if (interrupted) {
                        Thread.currentThread().interrupt();
                    }
                    return true;
                } catch (InterruptedException error) {
                    interrupted = true;
                    long elapsed = Math.max(0, System.nanoTime() - started);
                    remaining = Math.max(0, remaining - elapsed);
                }
            }
        }
    }

    public static boolean futexWake(Pointer address) {
        Object monitor = Pointer.atomicMonitor(address);
        synchronized (monitor) {
            monitor.notify();
        }
        // Rust's RwLock uses false to conservatively wake reader fallbacks too.
        return false;
    }

    public static void futexWakeAll(Pointer address) {
        Object monitor = Pointer.atomicMonitor(address);
        synchronized (monitor) {
            monitor.notifyAll();
        }
    }

    public static Pointer tlsGet(long key) {
        Pointer value = TLS.get().get(key);
        return value == null ? Pointer.nullPointer() : value;
    }

    public static long tlsCreate(Pointer destructor) {
        long key = NEXT_TLS_KEY.getAndIncrement();
        if (!Pointer.is_null(destructor)) {
            TLS_DESTRUCTORS.put(key, destructor.directCellValueOrSelf());
        }
        return key;
    }

    public static void tlsSet(long key, Pointer value) {
        if (Pointer.is_null(value)) {
            TLS.get().remove(key);
        } else {
            TLS.get().put(key, value);
        }
    }

    public static void tlsRunDestructors() {
        Map<Long, Pointer> values = TLS.get();
        for (int pass = 0; pass < 5; pass++) {
            boolean ranDestructor = false;
            for (Map.Entry<Long, Object> destructor : TLS_DESTRUCTORS.entrySet()) {
                Pointer value = values.remove(destructor.getKey());
                if (value == null) {
                    continue;
                }
                Pointer.invokeRustFunction(destructor.getValue(), value);
                ranDestructor = true;
            }
            if (!ranDestructor) {
                break;
            }
        }
    }

    public static void runStaticPointerWorkers(
            Pointer ownerBytes,
            long ownerLength,
            Pointer methodBytes,
            long methodLength,
            Pointer context,
            int workerCount) {
        if (workerCount <= 0) {
            throw new IllegalArgumentException("Rust worker count must be positive");
        }

        final MethodHandle worker;
        try {
            Class<?> owner = Class.forName(utf8(ownerBytes, ownerLength));
            worker = MethodHandles.publicLookup().findStatic(
                    owner,
                    utf8(methodBytes, methodLength),
                    MethodType.methodType(void.class, Pointer.class));
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not resolve generated Rust worker", error);
        }

        Thread[] threads = new Thread[workerCount];
        Throwable[] failures = new Throwable[workerCount];
        CountDownLatch ready = new CountDownLatch(workerCount);
        CountDownLatch start = new CountDownLatch(1);
        for (int index = 0; index < workerCount; index++) {
            final int workerIndex = index;
            threads[index] = new Thread(
                    () -> {
                        try {
                            ready.countDown();
                            start.await();
                            worker.invokeExact(context);
                        } catch (Throwable error) {
                            failures[workerIndex] = error;
                        }
                    },
                    "rust-worker-" + index);
            threads[index].start();
        }

        boolean interrupted = false;
        for (;;) {
            try {
                ready.await();
                break;
            } catch (InterruptedException error) {
                interrupted = true;
            }
        }
        start.countDown();
        for (Thread thread : threads) {
            for (;;) {
                try {
                    thread.join();
                    break;
                } catch (InterruptedException error) {
                    interrupted = true;
                }
            }
        }
        if (interrupted) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("interrupted while joining generated Rust workers");
        }
        for (Throwable failure : failures) {
            if (failure != null) {
                rethrowWorkerFailure(failure);
            }
        }
    }

    private static void rethrowWorkerFailure(Throwable failure) {
        if (failure instanceof RuntimeException) {
            throw (RuntimeException) failure;
        }
        if (failure instanceof Error) {
            throw (Error) failure;
        }
        throw new IllegalStateException("generated Rust worker failed", failure);
    }

    private static void reportUncaughtFailure(Throwable failure) {
        Thread thread = Thread.currentThread();
        thread.getUncaughtExceptionHandler().uncaughtException(thread, failure);
    }
}
