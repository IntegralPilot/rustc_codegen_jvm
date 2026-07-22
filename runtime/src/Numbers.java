package org.rustlang.runtime;

import java.math.BigInteger;

public final class Numbers {
    private Numbers() {}

    private static long objectInteger(Object value) {
        if (value == null) return 0L;
        if (value instanceof Character) return ((Character) value).charValue();
        if (value instanceof Pointer) return Pointer.address((Pointer) value);
        return ((Number) value).longValue();
    }

    public static byte objectToI8(Object value) { return (byte) objectInteger(value); }
    public static short objectToI16(Object value) { return (short) objectInteger(value); }
    public static char objectToU16(Object value) { return (char) objectInteger(value); }
    public static int objectToI32(Object value) { return (int) objectInteger(value); }
    public static long objectToI64(Object value) { return objectInteger(value); }
    public static float objectToF32(Object value) { return ((Number) value).floatValue(); }
    public static double objectToF64(Object value) { return ((Number) value).doubleValue(); }

    public static char f16ToBits(short value) { return (char) value; }
    public static short f16FromBits(char value) { return (short) value; }

    public static int bitCount32(int value, int bitWidth) {
        if (bitWidth <= 0 || bitWidth > 32) {
            throw new IllegalArgumentException("invalid integer bit width: " + bitWidth);
        }
        int mask = bitWidth == 32 ? -1 : (1 << bitWidth) - 1;
        return Integer.bitCount(value & mask);
    }

    public static int bitCount64(long value) { return Long.bitCount(value); }

    public static int bitCountI128(I128 value) {
        return Long.bitCount(value.high) + Long.bitCount(value.low);
    }

    public static int bitCountU128(U128 value) {
        return Long.bitCount(value.high) + Long.bitCount(value.low);
    }

    public static int leadingZeros32(int value, int bitWidth) {
        if (bitWidth <= 0 || bitWidth > 32) {
            throw new IllegalArgumentException("invalid integer bit width: " + bitWidth);
        }
        int mask = bitWidth == 32 ? -1 : (1 << bitWidth) - 1;
        return Integer.numberOfLeadingZeros(value & mask) - (32 - bitWidth);
    }

    public static int leadingZeros64(long value) {
        return Long.numberOfLeadingZeros(value);
    }

    public static int leadingZerosI128(I128 value) {
        return value.high == 0
                ? 64 + Long.numberOfLeadingZeros(value.low)
                : Long.numberOfLeadingZeros(value.high);
    }

    public static int leadingZerosU128(U128 value) {
        return value.high == 0
                ? 64 + Long.numberOfLeadingZeros(value.low)
                : Long.numberOfLeadingZeros(value.high);
    }

    public static int trailingZeros32(int value, int bitWidth) {
        if (bitWidth <= 0 || bitWidth > 32) {
            throw new IllegalArgumentException("invalid integer bit width: " + bitWidth);
        }
        int mask = bitWidth == 32 ? -1 : (1 << bitWidth) - 1;
        return Math.min(Integer.numberOfTrailingZeros(value & mask), bitWidth);
    }

    public static int trailingZeros64(long value) {
        return Long.numberOfTrailingZeros(value);
    }

    public static int trailingZerosI128(I128 value) {
        return value.low == 0
                ? 64 + Long.numberOfTrailingZeros(value.high)
                : Long.numberOfTrailingZeros(value.low);
    }

    public static int trailingZerosU128(U128 value) {
        return value.low == 0
                ? 64 + Long.numberOfTrailingZeros(value.high)
                : Long.numberOfTrailingZeros(value.low);
    }

    public static byte rotateLeft(byte value, int distance) {
        int shift = distance & 7;
        int bits = value & 0xff;
        return (byte) ((bits << shift) | (bits >>> (8 - shift)));
    }

    public static short rotateLeft(short value, int distance) {
        int shift = distance & 15;
        int bits = value & 0xffff;
        return (short) ((bits << shift) | (bits >>> (16 - shift)));
    }

    public static char rotateLeft(char value, int distance) {
        return (char) rotateLeft((short) value, distance);
    }

    public static int rotateLeft(int value, int distance) {
        return Integer.rotateLeft(value, distance);
    }

    public static long rotateLeft(long value, int distance) {
        return Long.rotateLeft(value, distance);
    }

    public static I128 rotateLeft(I128 value, int distance) {
        U128 result = rotateLeft(value.toU128(), distance);
        return new I128(result.high, result.low);
    }

    public static U128 rotateLeft(U128 value, int distance) {
        int shift = distance & 127;
        return shift == 0 ? value : value.shiftLeft(shift).or(value.shiftRight(128 - shift));
    }

    public static byte rotateRight(byte value, int distance) {
        return rotateLeft(value, -distance);
    }

    public static short rotateRight(short value, int distance) {
        return rotateLeft(value, -distance);
    }

    public static char rotateRight(char value, int distance) {
        return rotateLeft(value, -distance);
    }

    public static int rotateRight(int value, int distance) {
        return Integer.rotateRight(value, distance);
    }

    public static long rotateRight(long value, int distance) {
        return Long.rotateRight(value, distance);
    }

    public static I128 rotateRight(I128 value, int distance) {
        return rotateLeft(value, -distance);
    }

    public static U128 rotateRight(U128 value, int distance) {
        return rotateLeft(value, -distance);
    }

    public static byte bitReverse(byte value) {
        return (byte) (Integer.reverse(value & 0xff) >>> 24);
    }

    public static short bitReverse(short value) {
        return (short) (Integer.reverse(value & 0xffff) >>> 16);
    }

    public static char bitReverse(char value) {
        return (char) bitReverse((short) value);
    }

    public static int bitReverse(int value) { return Integer.reverse(value); }
    public static long bitReverse(long value) { return Long.reverse(value); }
    public static I128 bitReverse(I128 value) {
        return new I128(Long.reverse(value.low), Long.reverse(value.high));
    }
    public static U128 bitReverse(U128 value) {
        return new U128(Long.reverse(value.low), Long.reverse(value.high));
    }

    private static int carryingTotal(byte a, byte b, byte addend, byte carry) {
        return a * b + addend + carry;
    }

    private static long carryingTotal(short a, short b, short addend, short carry) {
        return (long) a * b + addend + carry;
    }

    private static long carryingTotal(int a, int b, int addend, int carry) {
        return (long) a * b + addend + carry;
    }

    private static BigInteger carryingTotal(long a, long b, long addend, long carry) {
        return BigInteger.valueOf(a).multiply(BigInteger.valueOf(b))
                .add(BigInteger.valueOf(addend)).add(BigInteger.valueOf(carry));
    }

    private static BigInteger carryingTotal(I128 a, I128 b, I128 addend, I128 carry) {
        return a.toBigInteger().multiply(b.toBigInteger())
                .add(addend.toBigInteger()).add(carry.toBigInteger());
    }

    public static byte carryingLowI8(byte a, byte b, byte addend, byte carry) {
        return (byte) carryingTotal(a, b, addend, carry);
    }
    public static byte carryingHighI8(byte a, byte b, byte addend, byte carry) {
        return (byte) (carryingTotal(a, b, addend, carry) >> 8);
    }
    public static char carryingLowI16(short a, short b, short addend, short carry) {
        return (char) carryingTotal(a, b, addend, carry);
    }
    public static short carryingHighI16(short a, short b, short addend, short carry) {
        return (short) (carryingTotal(a, b, addend, carry) >> 16);
    }
    public static int carryingLowI32(int a, int b, int addend, int carry) {
        return (int) carryingTotal(a, b, addend, carry);
    }
    public static int carryingHighI32(int a, int b, int addend, int carry) {
        return (int) (carryingTotal(a, b, addend, carry) >> 32);
    }
    public static long carryingLowI64(long a, long b, long addend, long carry) {
        return carryingTotal(a, b, addend, carry).longValue();
    }
    public static long carryingHighI64(long a, long b, long addend, long carry) {
        return carryingTotal(a, b, addend, carry).shiftRight(64).longValue();
    }
    public static U128 carryingLowI128(I128 a, I128 b, I128 addend, I128 carry) {
        return U128.fromBigInteger(carryingTotal(a, b, addend, carry));
    }
    public static I128 carryingHighI128(I128 a, I128 b, I128 addend, I128 carry) {
        return I128.fromBigInteger(carryingTotal(a, b, addend, carry).shiftRight(128));
    }

    public static float f16ToF32(short value) {
        int half = value & 0xffff;
        int sign = (half & 0x8000) << 16;
        int exponent = (half >>> 10) & 0x1f;
        int significand = half & 0x03ff;
        int bits;

        if (exponent == 0) {
            if (significand == 0) {
                bits = sign;
            } else {
                int shift = Integer.numberOfLeadingZeros(significand) - 21;
                significand = (significand << shift) & 0x03ff;
                int floatExponent = 113 - shift;
                bits = sign | (floatExponent << 23) | (significand << 13);
            }
        } else if (exponent == 0x1f) {
            bits = sign | 0x7f800000 | (significand << 13);
            if (significand != 0) {
                bits |= 0x00400000; // Keep NaNs quiet.
            }
        } else {
            bits = sign | ((exponent + 112) << 23) | (significand << 13);
        }
        return Float.intBitsToFloat(bits);
    }

    /** IEEE-754 round-to-nearest, ties-to-even conversion to binary16. */
    public static short f32ToF16(float value) {
        int bits = Float.floatToRawIntBits(value);
        int sign = (bits >>> 16) & 0x8000;
        int magnitude = bits & 0x7fffffff;

        if (magnitude >= 0x7f800000) {
            if (magnitude == 0x7f800000) {
                return (short) (sign | 0x7c00);
            }
            int payload = (magnitude >>> 13) & 0x03ff;
            return (short) (sign | 0x7c00 | payload | 0x0200);
        }

        int exponent = (magnitude >>> 23) - 127 + 15;
        int significand = magnitude & 0x007fffff;
        if (exponent >= 31) {
            return (short) (sign | 0x7c00);
        }

        if (exponent <= 0) {
            if (exponent < -10) {
                return (short) sign;
            }
            significand |= 0x00800000;
            int shift = 14 - exponent;
            int halfSignificand = significand >>> shift;
            int remainder = significand & ((1 << shift) - 1);
            int halfway = 1 << (shift - 1);
            if (remainder > halfway || (remainder == halfway && (halfSignificand & 1) != 0)) {
                halfSignificand++;
            }
            return (short) (sign | halfSignificand);
        }

        int half = sign | (exponent << 10) | (significand >>> 13);
        int remainder = significand & 0x1fff;
        if (remainder > 0x1000 || (remainder == 0x1000 && (half & 1) != 0)) {
            half++;
        }
        return (short) half;
    }

    public static short f64ToF16(double value) {
        // Correctly rounded binary64 -> binary16 conversion cannot generally use a binary32
        // intermediate because that can double-round.  The two exceptional midpoint cases
        // are avoided by nudging an exact binary32 midpoint toward the original value.
        float narrowed = (float) value;
        if (Double.isFinite(value) && Float.isFinite(narrowed) && (double) narrowed != value) {
            short candidate = f32ToF16(narrowed);
            float candidateValue = f16ToF32(candidate);
            if (candidateValue == narrowed) {
                narrowed = Math.nextAfter(narrowed, value > narrowed
                    ? Double.POSITIVE_INFINITY : Double.NEGATIVE_INFINITY);
            }
        }
        return f32ToF16(narrowed);
    }

    public static short f16Add(short a, short b) { return f32ToF16(f16ToF32(a) + f16ToF32(b)); }
    public static short f16Sub(short a, short b) { return f32ToF16(f16ToF32(a) - f16ToF32(b)); }
    public static short f16Mul(short a, short b) { return f32ToF16(f16ToF32(a) * f16ToF32(b)); }
    public static short f16Div(short a, short b) { return f32ToF16(f16ToF32(a) / f16ToF32(b)); }
    public static short f16Rem(short a, short b) { return f32ToF16(f16ToF32(a) % f16ToF32(b)); }

    public static boolean f16Eq(short a, short b) { return f16ToF32(a) == f16ToF32(b); }
    public static boolean f16Ne(short a, short b) { return f16ToF32(a) != f16ToF32(b); }
    public static boolean f16Lt(short a, short b) { return f16ToF32(a) < f16ToF32(b); }
    public static boolean f16Le(short a, short b) { return f16ToF32(a) <= f16ToF32(b); }
    public static boolean f16Gt(short a, short b) { return f16ToF32(a) > f16ToF32(b); }
    public static boolean f16Ge(short a, short b) { return f16ToF32(a) >= f16ToF32(b); }

    public static float u32ToF32(int value) { return (float) (value & 0xffffffffL); }
    public static double u32ToF64(int value) { return (double) (value & 0xffffffffL); }

    public static double u64ToF64(long value) {
        return value >= 0 ? (double) value : (double) (value >>> 1) * 2.0 + (value & 1L);
    }

    public static float u64ToF32(long value) { return (float) u64ToF64(value); }

    public static int f32ToU32(float value) { return f64ToU32(value); }
    public static int f64ToU32(double value) {
        if (Double.isNaN(value) || value <= 0.0) return 0;
        if (value >= 0x1.0p32) return -1;
        return (int) ((long) value);
    }

    public static long f32ToU64(float value) { return f64ToU64(value); }
    public static long f64ToU64(double value) {
        if (Double.isNaN(value) || value <= 0.0) return 0L;
        if (value >= 0x1.0p64) return -1L;
        if (value < 0x1.0p63) return (long) value;
        return ((long) (value - 0x1.0p63)) ^ Long.MIN_VALUE;
    }

    public static byte saturatingAddI8(byte left, byte right) {
        return (byte) Math.max(Byte.MIN_VALUE, Math.min(Byte.MAX_VALUE, left + right));
    }
    public static byte saturatingSubI8(byte left, byte right) {
        return (byte) Math.max(Byte.MIN_VALUE, Math.min(Byte.MAX_VALUE, left - right));
    }
    public static byte saturatingAddU8(byte left, byte right) {
        return (byte) Math.min(0xff, (left & 0xff) + (right & 0xff));
    }
    public static byte saturatingSubU8(byte left, byte right) {
        return (byte) Math.max(0, (left & 0xff) - (right & 0xff));
    }

    public static short saturatingAddI16(short left, short right) {
        return (short) Math.max(Short.MIN_VALUE, Math.min(Short.MAX_VALUE, left + right));
    }
    public static short saturatingSubI16(short left, short right) {
        return (short) Math.max(Short.MIN_VALUE, Math.min(Short.MAX_VALUE, left - right));
    }
    public static char saturatingAddU16(char left, char right) {
        return (char) Math.min(0xffff, left + right);
    }
    public static char saturatingSubU16(char left, char right) {
        return (char) Math.max(0, left - right);
    }

    public static int saturatingAddI32(int left, int right) {
        long result = (long) left + right;
        return (int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, result));
    }
    public static int saturatingSubI32(int left, int right) {
        long result = (long) left - right;
        return (int) Math.max(Integer.MIN_VALUE, Math.min(Integer.MAX_VALUE, result));
    }
    public static int saturatingAddU32(int left, int right) {
        long result = Integer.toUnsignedLong(left) + Integer.toUnsignedLong(right);
        return result > 0xffff_ffffL ? -1 : (int) result;
    }
    public static int saturatingSubU32(int left, int right) {
        return Integer.compareUnsigned(left, right) < 0 ? 0 : left - right;
    }

    public static long saturatingAddI64(long left, long right) {
        long result = left + right;
        if (((left ^ result) & (right ^ result)) < 0) {
            return left < 0 ? Long.MIN_VALUE : Long.MAX_VALUE;
        }
        return result;
    }
    public static long saturatingSubI64(long left, long right) {
        long result = left - right;
        if (((left ^ right) & (left ^ result)) < 0) {
            return left < 0 ? Long.MIN_VALUE : Long.MAX_VALUE;
        }
        return result;
    }
    public static long saturatingAddU64(long left, long right) {
        long result = left + right;
        return Long.compareUnsigned(result, left) < 0 ? -1L : result;
    }
    public static long saturatingSubU64(long left, long right) {
        return Long.compareUnsigned(left, right) < 0 ? 0L : left - right;
    }

    public static I128 saturatingAddI128(I128 left, I128 right) {
        return left.saturatingAdd(right);
    }
    public static I128 saturatingSubI128(I128 left, I128 right) {
        return left.saturatingSubtract(right);
    }
    public static U128 saturatingAddU128(U128 left, U128 right) {
        return left.saturatingAdd(right);
    }
    public static U128 saturatingSubU128(U128 left, U128 right) {
        return left.saturatingSubtract(right);
    }

    public static byte f32ToI8(float value) { return f64ToI8(value); }
    public static byte f64ToI8(double value) {
        if (Double.isNaN(value)) return 0;
        if (value <= Byte.MIN_VALUE) return Byte.MIN_VALUE;
        if (value >= Byte.MAX_VALUE) return Byte.MAX_VALUE;
        return (byte) value;
    }

    public static short f32ToI16(float value) { return f64ToI16(value); }
    public static short f64ToI16(double value) {
        if (Double.isNaN(value)) return 0;
        if (value <= Short.MIN_VALUE) return Short.MIN_VALUE;
        if (value >= Short.MAX_VALUE) return Short.MAX_VALUE;
        return (short) value;
    }

    public static byte f32ToU8(float value) { return f64ToU8(value); }
    public static byte f64ToU8(double value) {
        if (Double.isNaN(value) || value <= 0.0) return 0;
        if (value >= 255.0) return (byte) 0xff;
        return (byte) ((int) value);
    }

    public static char f32ToU16(float value) { return f64ToU16(value); }
    public static char f64ToU16(double value) {
        if (Double.isNaN(value) || value <= 0.0) return 0;
        if (value >= 65535.0) return (char) 0xffff;
        return (char) ((int) value);
    }
}
