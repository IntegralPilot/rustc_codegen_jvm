package org.rustlang.primitives

public object RustString {
    @JvmStatic 
    public fun eq(a: String?, b: String?): Boolean {
        return a == b
    }
    
    @JvmStatic
    public fun starts_with(s: String, char: Char): Boolean {
        return s.startsWith(char)
    }

    @JvmStatic
    public fun len(s: String): Int {
        return s.length
    }
}
