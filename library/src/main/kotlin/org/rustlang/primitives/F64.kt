package org.rustlang.primitives

public object F64 {
    @JvmStatic
    public fun eq(a: Double, b: Double): Boolean {
        return a == b
    }
}