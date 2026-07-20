package org.rustlang.runtime;

/** JVM entry point for Rust drop glue selected through a trait object. */
public interface RustDrop {
    void rustDrop();
}
