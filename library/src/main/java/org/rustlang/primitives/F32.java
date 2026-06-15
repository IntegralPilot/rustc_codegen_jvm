package org.rustlang.primitives;

public final class F32 {
    private F32() {}

    public static boolean eq(float a, float b) {
        // IEEE primitive comparison (NaN != NaN, +0.0 == -0.0), matching the Kotlin original.
        return a == b;
    }
}
