package org.rustlang.runtime;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;

/**
 * Runs generated Rust functions on JVM threads for no_std concurrency tests.
 * Needed because we don't support std yet, but still want to check Atomics etc.
 * work correctly across threads in the tests.
 */
public final class ThreadSupport {
    private ThreadSupport() {}

    private static String utf8(Pointer bytes, long length) {
        int checkedLength = Math.toIntExact(length);
        byte[] copy = new byte[checkedLength];
        for (int index = 0; index < checkedLength; index++) {
            copy[index] = bytes.add(index).getI8();
        }
        return new String(copy, StandardCharsets.UTF_8);
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
}
