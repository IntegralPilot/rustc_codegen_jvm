package org.rustlang.runtime;

import java.math.BigInteger;

public final class U128 extends Number implements Comparable<U128> {
    private static final long serialVersionUID = 1L;
    private static final BigInteger MASK = BigInteger.ONE.shiftLeft(128).subtract(BigInteger.ONE);

    public final long high;
    public final long low;

    public U128(long high, long low) {
        this.high = high;
        this.low = low;
    }

    public U128(String value) {
        BigInteger bits = new BigInteger(value).and(MASK);
        this.low = bits.longValue();
        this.high = bits.shiftRight(64).longValue();
    }

    public static U128 fromBigInteger(BigInteger value) {
        BigInteger bits = value.and(MASK);
        return new U128(bits.shiftRight(64).longValue(), bits.longValue());
    }

    public static U128 fromI64(long value) {
        return new U128(value < 0 ? -1L : 0L, value);
    }

    public static U128 fromU64(long value) {
        return new U128(0L, value);
    }

    public static U128 fromF32(float value) {
        return fromF64(value);
    }

    public static U128 fromF64(double value) {
        if (Double.isNaN(value) || value <= 0.0) return new U128(0L, 0L);
        if (value >= 0x1.0p128) return new U128(-1L, -1L);
        return fromBigInteger(java.math.BigDecimal.valueOf(value).toBigInteger());
    }

    public I128 toI128() {
        return new I128(high, low);
    }

    private static BigInteger unsignedLong(long value) {
        BigInteger result = BigInteger.valueOf(value & Long.MAX_VALUE);
        return value < 0 ? result.setBit(63) : result;
    }

    public BigInteger toBigInteger() {
        return unsignedLong(high).shiftLeft(64).or(unsignedLong(low));
    }

    public U128 add(U128 other) {
        long resultLow = low + other.low;
        long carry = Long.compareUnsigned(resultLow, low) < 0 ? 1L : 0L;
        return new U128(high + other.high + carry, resultLow);
    }

    public U128 subtract(U128 other) {
        long borrow = Long.compareUnsigned(low, other.low) < 0 ? 1L : 0L;
        return new U128(high - other.high - borrow, low - other.low);
    }

    public U128 saturatingAdd(U128 other) {
        U128 result = add(other);
        return result.compareTo(this) < 0 ? new U128(-1L, -1L) : result;
    }

    public U128 saturatingSubtract(U128 other) {
        return compareTo(other) < 0 ? new U128(0L, 0L) : subtract(other);
    }

    private static long unsignedMultiplyHigh(long left, long right) {
        long mask = 0xffff_ffffL;
        long leftLow = left & mask;
        long leftHigh = left >>> 32;
        long rightLow = right & mask;
        long rightHigh = right >>> 32;
        long lowProduct = leftLow * rightLow;
        long crossLeft = leftHigh * rightLow;
        long crossRight = leftLow * rightHigh;
        long highProduct = leftHigh * rightHigh;
        long middle = (lowProduct >>> 32)
                + (crossLeft & mask)
                + (crossRight & mask);
        return highProduct
                + (crossLeft >>> 32)
                + (crossRight >>> 32)
                + (middle >>> 32);
    }

    public U128 multiply(U128 other) {
        long resultLow = low * other.low;
        long resultHigh = unsignedMultiplyHigh(low, other.low)
                + high * other.low
                + low * other.high;
        return new U128(resultHigh, resultLow);
    }

    private U128 divideOrRemainder(U128 divisor, boolean quotientResult) {
        if ((divisor.high | divisor.low) == 0) {
            throw new ArithmeticException("division by zero");
        }
        long quotientHigh = 0;
        long quotientLow = 0;
        long remainderHigh = 0;
        long remainderLow = 0;
        for (int bit = 127; bit >= 0; bit--) {
            remainderHigh = (remainderHigh << 1) | (remainderLow >>> 63);
            remainderLow <<= 1;
            long inputBit = bit >= 64
                    ? (high >>> (bit - 64)) & 1L
                    : (low >>> bit) & 1L;
            remainderLow |= inputBit;

            int highComparison = Long.compareUnsigned(remainderHigh, divisor.high);
            if (highComparison > 0
                    || (highComparison == 0
                            && Long.compareUnsigned(remainderLow, divisor.low) >= 0)) {
                long borrow = Long.compareUnsigned(remainderLow, divisor.low) < 0 ? 1L : 0L;
                remainderLow -= divisor.low;
                remainderHigh = remainderHigh - divisor.high - borrow;
                if (bit >= 64) {
                    quotientHigh |= 1L << (bit - 64);
                } else {
                    quotientLow |= 1L << bit;
                }
            }
        }
        return quotientResult
                ? new U128(quotientHigh, quotientLow)
                : new U128(remainderHigh, remainderLow);
    }

    public U128 divide(U128 other) {
        return divideOrRemainder(other, true);
    }

    public U128 remainder(U128 other) {
        return divideOrRemainder(other, false);
    }

    public U128 and(U128 other) {
        return new U128(high & other.high, low & other.low);
    }

    public U128 or(U128 other) {
        return new U128(high | other.high, low | other.low);
    }

    public U128 xor(U128 other) {
        return new U128(high ^ other.high, low ^ other.low);
    }

    public U128 not() {
        return new U128(~high, ~low);
    }

    public U128 negate() {
        long resultLow = -low;
        return new U128(~high + (resultLow == 0 ? 1L : 0L), resultLow);
    }

    public U128 shiftLeft(int distance) {
        int shift = distance & 127;
        if (shift == 0) {
            return this;
        }
        if (shift < 64) {
            return new U128((high << shift) | (low >>> (64 - shift)), low << shift);
        }
        return new U128(low << (shift - 64), 0);
    }

    public U128 shiftRight(int distance) {
        int shift = distance & 127;
        if (shift == 0) {
            return this;
        }
        if (shift < 64) {
            return new U128(high >>> shift, (low >>> shift) | (high << (64 - shift)));
        }
        return new U128(0, high >>> (shift - 64));
    }

    public byte byteAt(int index) {
        if (index < 0 || index >= 16) {
            throw new IndexOutOfBoundsException("u128 byte index: " + index);
        }
        long word = index < 8 ? low : high;
        return (byte) (word >>> ((index & 7) * 8));
    }

    public U128 withByte(int index, int value) {
        if (index < 0 || index >= 16) {
            throw new IndexOutOfBoundsException("u128 byte index: " + index);
        }
        int shift = (index & 7) * 8;
        long mask = 0xffL << shift;
        if (index < 8) {
            return new U128(high, (low & ~mask) | (((long) value & 0xffL) << shift));
        }
        return new U128((high & ~mask) | (((long) value & 0xffL) << shift), low);
    }

    @Override
    public int compareTo(U128 other) {
        int highComparison = Long.compareUnsigned(high, other.high);
        return highComparison != 0 ? highComparison : Long.compareUnsigned(low, other.low);
    }

    @Override
    public int intValue() {
        return (int) low;
    }

    @Override
    public long longValue() {
        return low;
    }

    @Override
    public float floatValue() {
        return toBigInteger().floatValue();
    }

    @Override
    public double doubleValue() {
        return toBigInteger().doubleValue();
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof U128
                && high == ((U128) other).high
                && low == ((U128) other).low;
    }

    @Override
    public int hashCode() {
        return (int) (high ^ (high >>> 32) ^ low ^ (low >>> 32));
    }

    @Override
    public String toString() {
        return toBigInteger().toString();
    }
}
