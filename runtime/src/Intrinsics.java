package org.rustlang.runtime;

/** Allocation-free JVM implementations of value-selection Rust intrinsics. */
public final class Intrinsics {
    private Intrinsics() {}

    public static byte byteSwap(byte value) { return value; }
    public static short byteSwap(short value) { return Short.reverseBytes(value); }
    public static char byteSwap(char value) { return (char) Short.reverseBytes((short) value); }
    public static int byteSwap(int value) { return Integer.reverseBytes(value); }
    public static long byteSwap(long value) { return Long.reverseBytes(value); }
    public static I128 byteSwap(I128 value) {
        return new I128(Long.reverseBytes(value.low), Long.reverseBytes(value.high));
    }
    public static U128 byteSwap(U128 value) {
        return new U128(Long.reverseBytes(value.low), Long.reverseBytes(value.high));
    }

    public static short floatAbs(short value) { return (short) (value & 0x7fff); }
    public static float floatAbs(float value) {
        return Float.intBitsToFloat(Float.floatToRawIntBits(value) & 0x7fff_ffff);
    }
    public static double floatAbs(double value) {
        return Double.longBitsToDouble(Double.doubleToRawLongBits(value) & Long.MAX_VALUE);
    }
    public static F128 floatAbs(F128 value) {
        return new F128(value.highBits() & Long.MAX_VALUE, value.lowBits());
    }

    public static byte funnelShiftLeft(byte high, byte low, int shift) {
        if (shift == 0) return high;
        int a = high & 0xff;
        int b = low & 0xff;
        return (byte) ((a << shift) | (b >>> (8 - shift)));
    }
    public static char funnelShiftLeft(char high, char low, int shift) {
        if (shift == 0) return high;
        return (char) ((high << shift) | (low >>> (16 - shift)));
    }
    public static int funnelShiftLeft(int high, int low, int shift) {
        return shift == 0 ? high : (high << shift) | (low >>> (32 - shift));
    }
    public static long funnelShiftLeft(long high, long low, int shift) {
        return shift == 0 ? high : (high << shift) | (low >>> (64 - shift));
    }
    public static U128 funnelShiftLeft(U128 high, U128 low, int shift) {
        return shift == 0 ? high : high.shiftLeft(shift).or(low.shiftRight(128 - shift));
    }

    public static byte funnelShiftRight(byte high, byte low, int shift) {
        if (shift == 0) return low;
        int a = high & 0xff;
        int b = low & 0xff;
        return (byte) ((a << (8 - shift)) | (b >>> shift));
    }
    public static char funnelShiftRight(char high, char low, int shift) {
        if (shift == 0) return low;
        return (char) ((high << (16 - shift)) | (low >>> shift));
    }
    public static int funnelShiftRight(int high, int low, int shift) {
        return shift == 0 ? low : (high << (32 - shift)) | (low >>> shift);
    }
    public static long funnelShiftRight(long high, long low, int shift) {
        return shift == 0 ? low : (high << (64 - shift)) | (low >>> shift);
    }
    public static U128 funnelShiftRight(U128 high, U128 low, int shift) {
        return shift == 0 ? low : high.shiftLeft(128 - shift).or(low.shiftRight(shift));
    }

    public static byte carrylessMultiply(byte left, byte right) {
        return (byte) carrylessMultiply32(left & 0xff, right & 0xff, 8);
    }
    public static char carrylessMultiply(char left, char right) {
        return (char) carrylessMultiply32(left, right, 16);
    }
    public static int carrylessMultiply(int left, int right) {
        return carrylessMultiply32(left, right, 32);
    }
    private static int carrylessMultiply32(int left, int right, int bits) {
        int result = 0;
        for (int bit = 0; bit < bits; bit++) {
            if (((right >>> bit) & 1) != 0) result ^= left << bit;
        }
        return result;
    }
    public static long carrylessMultiply(long left, long right) {
        long result = 0;
        for (int bit = 0; bit < 64; bit++) {
            if (((right >>> bit) & 1L) != 0) result ^= left << bit;
        }
        return result;
    }
    public static U128 carrylessMultiply(U128 left, U128 right) {
        U128 result = new U128(0, 0);
        for (int bit = 0; bit < 128; bit++) {
            long word = bit < 64 ? right.low : right.high;
            if (((word >>> (bit & 63)) & 1L) != 0) result = result.xor(left.shiftLeft(bit));
        }
        return result;
    }

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
