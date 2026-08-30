package org.rustlang.runtime;

/** A Rust async value that can be driven by a JVM coroutine adapter. */
public interface RustFuture {
    /**
     * Polls the Rust future once. The returned value is either the completed
     * result or one of the markers exposed by {@link KotlinFutureInterop}.
     */
    Object poll(Runnable wake);
}
