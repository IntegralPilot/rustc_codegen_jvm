package org.rustlang.core;

/**
 * Shim for {@code core::panicking}. Internal name {@code org/rustlang/core/panicking} — i.e. a
 * class named {@code panicking} in package {@code org.rustlang.core} (the lowercase class name is
 * intentional, mirroring the Rust module path the backend emits).
 */
public final class panicking {
    private panicking() {}

    public static void panic(String arg) {
        throw new RuntimeException("Rust panic: " + arg);
    }

    public static void panic_fmt(Object args) {
        throw new RuntimeException("Rust panic: " + args);
    }
}
