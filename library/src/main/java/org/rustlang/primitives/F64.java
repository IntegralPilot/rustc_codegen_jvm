package org.rustlang.primitives;

public final class F64 {
    private F64() {}

    public static boolean eq(double a, double b) {
        // IEEE primitive comparison (NaN != NaN, +0.0 == -0.0), matching the Kotlin original.
        return a == b;
    }
}
