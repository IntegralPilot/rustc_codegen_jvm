package org.rustlang.primitives

public object F32 {
    @JvmStatic
    public fun eq(a: Float, b: Float): Boolean {
        return a == b
    }
}