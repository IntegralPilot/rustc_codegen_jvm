package org.rustlang.runtime.symbols;

import org.rustlang.runtime.F128;
import org.rustlang.runtime.F128Math;

public final class tgammaf128 {
    private tgammaf128() {}
    public static F128 tgammaf128(F128 value) { return F128Math.gamma(value); }
}
