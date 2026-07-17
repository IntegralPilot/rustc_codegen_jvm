package org.rustlang.runtime;

/** Allocation-free JVM implementations of value-selection Rust intrinsics. */
public final class Intrinsics {
    private Intrinsics() {}

    public static boolean selectUnpredictable(boolean condition, boolean trueValue, boolean falseValue) {
        return condition ? trueValue : falseValue;
    }

    public static byte selectUnpredictable(boolean condition, byte trueValue, byte falseValue) {
        return condition ? trueValue : falseValue;
    }

    public static short selectUnpredictable(boolean condition, short trueValue, short falseValue) {
        return condition ? trueValue : falseValue;
    }

    public static char selectUnpredictable(boolean condition, char trueValue, char falseValue) {
        return condition ? trueValue : falseValue;
    }

    public static int selectUnpredictable(boolean condition, int trueValue, int falseValue) {
        return condition ? trueValue : falseValue;
    }

    public static long selectUnpredictable(boolean condition, long trueValue, long falseValue) {
        return condition ? trueValue : falseValue;
    }

    public static float selectUnpredictable(boolean condition, float trueValue, float falseValue) {
        return condition ? trueValue : falseValue;
    }

    public static double selectUnpredictable(boolean condition, double trueValue, double falseValue) {
        return condition ? trueValue : falseValue;
    }

    public static Object selectUnpredictable(
            boolean condition, Object trueValue, Object falseValue) {
        return condition ? trueValue : falseValue;
    }
}
