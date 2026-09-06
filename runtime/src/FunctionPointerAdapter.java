package org.rustlang.runtime;

/** A function-pointer ABI adapter preserves the identity of its wrapped static code. */
public interface FunctionPointerAdapter {
    Object functionPointerTarget();
}
