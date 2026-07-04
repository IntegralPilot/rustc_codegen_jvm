package org.rustlang.core;

public final class panicking {
    private panicking() {}

    public static void panic(String arg) {
        throw new RuntimeException("Rust panic: " + arg);
    }

    public static void panic_fmt(Object args) {
        throw new RuntimeException("Rust panic: " + args);
    }
}
