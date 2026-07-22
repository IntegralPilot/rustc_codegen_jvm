package org.rustlang.runtime.symbols;

import org.rustlang.runtime.F128;
import org.rustlang.runtime.F128Math;

public final class asinhf128 {
    private asinhf128() {}
    public static F128 asinhf128(F128 value) { return F128Math.asinh(value); }
}
