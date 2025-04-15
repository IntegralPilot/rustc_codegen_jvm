package org.rustlang.core

/**
 * Core functions needed by the Rust JVM backend stdlib shim.
 */
public object Core {
    @JvmStatic
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

    @JvmStatic
    public fun core_fmt_rt_argument_new_display_i32(value: Int): String {
        // Convert the integer to a string.
        return value.toString()
    }

    @JvmStatic
    public fun core_fmt_rt_argument_new_display(value: Any?): String {
        // Convert the value to a string.
        return value?.toString() ?: "null"
    }

    @JvmStatic
    public fun core_fmt_rt_argument_new_display_bool(value: Boolean): String {
        // Convert the boolean to a string.
        return value.toString()
    }

    @JvmStatic
    public fun core_fmt_rt_argument_new_display_f64(value: Double): String {
        // Convert the double to a string.
        return value.toString()
    }

    @JvmStatic
    public fun arguments_new_v1(pieces: Array<String>, args: Array<Any?>): String {
        val sb = StringBuilder()
        var argIndex = 0
        for (i in pieces.indices) {
            sb.append(pieces[i]) // Append static piece
            if (argIndex < args.size) {
                // Append the corresponding argument (already formatted or convertible via toString)
                sb.append(args[argIndex]?.toString() ?: "null")
                argIndex++
            }
        }
        return sb.toString()
    }

    @JvmStatic 
    public fun to_string(value: Any?): String {
        // Convert the value to a string.
        return value?.toString() ?: "null"
    }

    @JvmStatic
    public fun eq(value1: Any?, value2: Any?): Boolean {
        // Check for equality.
        return value1 == value2
    }

    @JvmStatic
    public fun core_panicking_panic(message: String?) {
        // This is a placeholder for the panic function.
        // In a real implementation, this would handle the panic appropriately.
        throw RuntimeException("Rust panic: " + (message ?: "<no message>"))
    }
}