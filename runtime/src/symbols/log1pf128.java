package org.rustlang.runtime.symbols;

import org.rustlang.runtime.F128;
import org.rustlang.runtime.F128Math;

public final class log1pf128 {
    private log1pf128() {}
    public static F128 log1pf128(F128 value) { return F128Math.log1p(value); }
}
