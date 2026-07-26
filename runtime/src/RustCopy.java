package org.rustlang.runtime;

/** Implemented by compiler-generated Rust aggregate carriers with direct copy glue. */
public interface RustCopy {
    Object rustCopy();
}
