package org.rustlang.runtime.symbols;

import org.rustlang.runtime.F128;
import org.rustlang.runtime.F128Math;

public final class coshf128 {
    private coshf128() {}
    public static F128 coshf128(F128 value) { return F128Math.cosh(value); }
}
