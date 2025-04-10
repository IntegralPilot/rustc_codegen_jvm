package org.rustlang.core

/**
 * Core functions needed by the Rust JVM backend stdlib shim.
 */
public object Core {

    @JvmStatic // Ensures static JVM methods are generated
    public fun panic_fmt(message: String?) {
        // note to future self: consider using a more specific Rust exception type if needed
        throw RuntimeException("Rust panic: " + (message ?: "<no message>"))
    }

    @JvmStatic
    public fun arguments_new_const(pieces: Array<String>): String {
        // Concatenate all the string pieces together.
        // This mimics the simplest formatting scenario where pieces are just joined.
        // If the array is empty, it correctly returns an empty string.
        return pieces.joinToString("")
    }
}