package org.rustlang.core

/**
 * Core functions needed by the Rust JVM backend stdlib shim.
 */
public object Core {

    @JvmStatic // Ensures static JVM methods are generated
    public fun rust_panic_fmt(message: String?) {
        // note to future self: consider using a more specific Rust exception type if needed
        throw RuntimeException("Rust panic: " + (message ?: "<no message>"))
    }

    @JvmStatic
    public fun rust_fmt_arguments_new_const_stub(messagePiece: String?): String? {
        return messagePiece
    }
}