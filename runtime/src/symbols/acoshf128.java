package org.rustlang.runtime.symbols;

import org.rustlang.runtime.F128;
import org.rustlang.runtime.F128Math;

public final class acoshf128 {
    private acoshf128() {}
    public static F128 acoshf128(F128 value) { return F128Math.acosh(value); }
}
