package org.rustlang.runtime;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;

/** Minimal JVM boundary used by Rust panic handlers. */
public final class PanicSupport {
    private PanicSupport() {}

    private static final ThreadLocal<RustPanic> ACTIVE_PANIC = new ThreadLocal<>();
    private static final Object STACK_OVERFLOW_ABORT_LOCK = new Object();

    public static final class RustPanic extends RuntimeException {
        private Pointer payload;

        private RustPanic(String message) {
            super(message);
        }

        private RustPanic(Pointer payload) {
            super("Rust panic");
            this.payload = payload;
        }

        private Pointer takePayload() {
            if (payload == null) {
                throw new IllegalStateException("Rust panic payload was already consumed");
            }
            Pointer result = payload;
            payload = null;
            return result;
        }
    }

    public static void raise(Pointer message, long length) {
        if (length < 0 || length > Integer.MAX_VALUE) {
            throw new IllegalArgumentException("invalid Rust panic message length: " + length);
        }

        byte[] utf8 = new byte[(int) length];
        for (int index = 0; index < utf8.length; index++) {
            utf8[index] = message.add(index).getI8();
        }
        String detail = new String(utf8, StandardCharsets.UTF_8);
        raiseTracked(new RustPanic(detail.isEmpty() ? "Rust panic" : "Rust panic: " + detail));
    }

    public static void raisePayload(Pointer payload) {
        if (payload == null) {
            throw new NullPointerException("Rust panic payload is null");
        }
        raiseTracked(new RustPanic(payload));
    }

    public static Pointer takePayload(Pointer caughtException) {
        RustPanic panic = (RustPanic) caughtThrowable(caughtException);
        if (ACTIVE_PANIC.get() != panic) {
            throw new IllegalStateException("caught Rust panic is not the active thread panic");
        }
        ACTIVE_PANIC.remove();
        return panic.takePayload();
    }

    public static boolean isRustPanic(Pointer caughtException) {
        return caughtThrowable(caughtException) instanceof RustPanic;
    }

    public static Pointer foreignFailureMessage(Pointer caughtException) {
        byte[] message = describe(caughtThrowable(caughtException)).getBytes(StandardCharsets.UTF_8);
        return Pointer.array(message, 0, 1);
    }

    public static long foreignFailureMessageLength(Pointer caughtException) {
        return describe(caughtThrowable(caughtException)).getBytes(StandardCharsets.UTF_8).length;
    }

    private static Throwable caughtThrowable(Pointer caughtException) {
        Object value = caughtException.directCellValueOrSelf();
        if (!(value instanceof Throwable)) {
            throw new IllegalArgumentException("caught object is not a JVM throwable");
        }
        return (Throwable) value;
    }

    private static void raiseTracked(RustPanic panic) {
        if (ACTIVE_PANIC.get() != null) {
            Runtime.getRuntime().halt(134);
        }
        ACTIVE_PANIC.set(panic);
        throw panic;
    }

    private static String describe(Throwable failure) {
        StringWriter trace = new StringWriter();
        failure.printStackTrace(new PrintWriter(trace));
        return trace.toString();
    }

    private static final class FatalExceptionHandler implements Thread.UncaughtExceptionHandler {
        private final Thread.UncaughtExceptionHandler delegate;

        private FatalExceptionHandler(Thread.UncaughtExceptionHandler delegate) {
            this.delegate = delegate;
        }

        @Override
        public void uncaughtException(Thread thread, Throwable failure) {
            abortIfStackOverflow(failure);
            delegate.uncaughtException(thread, failure);
        }
    }

    /** Installs Rust fatal-error handling at the generated JVM main boundary. */
    public static void installMainThreadHandler() {
        Thread thread = Thread.currentThread();
        Thread.UncaughtExceptionHandler current = thread.getUncaughtExceptionHandler();
        if (!(current instanceof FatalExceptionHandler)) {
            thread.setUncaughtExceptionHandler(new FatalExceptionHandler(current));
        }
    }

    /** Reports a JVM stack overflow using Rust's fatal diagnostic and aborts. */
    public static void abortIfStackOverflow(Throwable failure) {
        if (!(failure instanceof StackOverflowError)) {
            return;
        }
        synchronized (STACK_OVERFLOW_ABORT_LOCK) {
            String name = Thread.currentThread().getName();
            if (name == null || name.isEmpty()) {
                name = "<unnamed>";
            }
            System.err.println("thread '" + name + "' has overflowed its stack");
            System.err.println("fatal runtime error: stack overflow, aborting");
            System.err.flush();
            Runtime.getRuntime().halt(134);
        }
    }

    /** Implements Rust's non-unwinding abort boundary. */
    public static void abort(Throwable failure) {
        abortIfStackOverflow(failure);
        if (failure instanceof RustPanic) {
            Runtime.getRuntime().halt(134);
        }
        if (failure instanceof RuntimeException) {
            throw (RuntimeException) failure;
        }
        if (failure instanceof Error) {
            throw (Error) failure;
        }
        throw new IllegalStateException("foreign JVM throwable crossed a Rust abort boundary", failure);
    }

    /** Implements Rust's explicit abort intrinsic without running JVM shutdown hooks. */
    public static void abortNow() {
        Runtime.getRuntime().halt(134);
    }
}
