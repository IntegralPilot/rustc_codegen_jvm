package org.rustlang.primitives;

import java.util.Objects;

public final class RustString {
    private RustString() {}

    public static boolean eq(String a, String b) {
        return Objects.equals(a, b);
    }

    public static boolean starts_with(String s, char c) {
        return s.startsWith(String.valueOf(c));
    }

    public static int len(String s) {
        return s.length();
    }
}
