package org.rustlang.shims

public object FreeFns {
    @JvmStatic
    public fun panic_fmt(args: Any?): Unit {
        throw RuntimeException("Rust panic: $args")
    }

    @JvmStatic
    public fun eq_String_String(a: String?, b: String?): Boolean {
        return a == b
    }

    @JvmStatic
    public fun starts_with_char(s: String, ch: Char): Boolean {
        return s.startsWith(ch)
    }

    @JvmStatic
    public fun eq_Object_Object(a: Any?, b: Any?): Boolean {
        if (a === b) return true
        if (a == null || b == null) return false
        if (a.javaClass != b.javaClass) return false

        val clazz = a.javaClass
        // Structural equality for compiler-generated data classes and enums.
        val fields = clazz.declaredFields
            .filter { !it.isSynthetic && !java.lang.reflect.Modifier.isStatic(it.modifiers) }
            .sortedBy { it.name }

        for (f in fields) {
            f.isAccessible = true
            val av = f.get(a)
            val bv = f.get(b)
            if (!eq_Object_Object(av, bv)) return false
        }
        return true
    }
}
