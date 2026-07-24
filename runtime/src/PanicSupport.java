package org.rustlang.runtime;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;

/** Minimal JVM boundary used by Rust panic handlers. */
public final class PanicSupport {
    private PanicSupport() {}

    private static final ThreadLocal<RustPanic> ACTIVE_PANIC = new ThreadLocal<>();

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

    /** Implements Rust's non-unwinding abort boundary. */
    public static void abort(Throwable failure) {
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
