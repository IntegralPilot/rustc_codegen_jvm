package org.rustlang.runtime.symbols;

import org.rustlang.runtime.F128;
import org.rustlang.runtime.F128Math;
import org.rustlang.runtime.Pointer;

public final class lgammaf128_r {
    private lgammaf128_r() {}
    public static F128 lgammaf128_r(F128 value, Pointer sign) {
        return F128Math.logGamma(value, sign);
    }
}
