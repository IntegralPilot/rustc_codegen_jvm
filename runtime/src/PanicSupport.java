package org.rustlang.runtime;

import java.nio.charset.StandardCharsets;

/** Minimal JVM boundary used by Rust panic handlers. */
public final class PanicSupport {
    private PanicSupport() {}

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
        throw new RustPanic(detail.isEmpty() ? "Rust panic" : "Rust panic: " + detail);
    }

    public static void raisePayload(Pointer payload) {
        if (payload == null) {
            throw new NullPointerException("Rust panic payload is null");
        }
        throw new RustPanic(payload);
    }

    public static Pointer takePayload(Pointer caughtException) {
        Object value = caughtException.directCellValueOrSelf();
        if (!(value instanceof RustPanic)) {
            throw new IllegalArgumentException("caught object is not a Rust panic");
        }
        return ((RustPanic) value).takePayload();
    }

    /** Implements Rust's non-unwinding abort boundary. */
    public static void abort(Throwable ignored) {
        Runtime.getRuntime().halt(134);
    }
}
