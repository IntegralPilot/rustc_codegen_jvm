package org.rustlang.core 

public object panicking {
    @JvmStatic
    public fun panic(arg: String?) {
        throw RuntimeException("Rust panic: $arg")
    }

    @JvmStatic
    public fun panic_fmt(args: Any?) {
        throw RuntimeException("Rust panic: $args")
    }
}