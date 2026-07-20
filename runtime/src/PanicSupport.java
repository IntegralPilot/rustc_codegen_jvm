package org.rustlang.runtime;

import java.nio.charset.StandardCharsets;

/** Minimal JVM boundary used by Rust panic handlers. */
public final class PanicSupport {
    private PanicSupport() {}

    public static final class RustPanic extends RuntimeException {
        private RustPanic(String message) {
            super(message);
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
        throw new RustPanic("Rust panic: " + new String(utf8, StandardCharsets.UTF_8));
    }
}
