package org.rustlang.primitives;

import java.util.Objects;

public final class RustString {
    private RustString() {}

    public static boolean eq(String a, String b) {
        return Objects.equals(a, b);
    }

    public static boolean starts_with(String value, char prefix) {
        return value.startsWith(String.valueOf(prefix));
    }

    public static int len(String value) {
        return value.length();
    }
}
