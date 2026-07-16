package org.rustlang.runtime;

import java.math.BigInteger;

public final class I128 extends Number implements Comparable<I128> {
    private static final long serialVersionUID = 1L;
    private static final BigInteger MODULUS = BigInteger.ONE.shiftLeft(128);
    private static final BigInteger MASK = MODULUS.subtract(BigInteger.ONE);

    public final long high;
    public final long low;

    public I128(long high, long low) {
        this.high = high;
        this.low = low;
    }

    public I128(String value) {
        BigInteger bits = new BigInteger(value).and(MASK);
        this.low = bits.longValue();
        this.high = bits.shiftRight(64).longValue();
    }

    public static I128 fromBigInteger(BigInteger value) {
        BigInteger bits = value.and(MASK);
        return new I128(bits.shiftRight(64).longValue(), bits.longValue());
    }

    public static I128 fromI64(long value) {
        return new I128(value < 0 ? -1L : 0L, value);
    }

    public static I128 fromU64(long value) {
        return new I128(0L, value);
    }

    public static I128 fromF32(float value) {
        return fromF64(value);
    }

    public static I128 fromF64(double value) {
        if (Double.isNaN(value)) return new I128(0L, 0L);
        if (value <= -0x1.0p127) return new I128(Long.MIN_VALUE, 0L);
        if (value >= 0x1.0p127) return new I128(Long.MAX_VALUE, -1L);
        return fromBigInteger(java.math.BigDecimal.valueOf(value).toBigInteger());
    }

    public U128 toU128() {
        return new U128(high, low);
    }

    public BigInteger toBigInteger() {
        BigInteger bits = new U128(high, low).toBigInteger();
        return high < 0 ? bits.subtract(MODULUS) : bits;
    }

    public I128 add(I128 other) {
        long resultLow = low + other.low;
        long carry = Long.compareUnsigned(resultLow, low) < 0 ? 1L : 0L;
        return new I128(high + other.high + carry, resultLow);
    }

    public I128 subtract(I128 other) {
        long borrow = Long.compareUnsigned(low, other.low) < 0 ? 1L : 0L;
        return new I128(high - other.high - borrow, low - other.low);
    }

    public I128 saturatingAdd(I128 other) {
        I128 result = add(other);
        if (((high ^ result.high) & (other.high ^ result.high)) < 0) {
            return high < 0
                    ? new I128(Long.MIN_VALUE, 0L)
                    : new I128(Long.MAX_VALUE, -1L);
        }
        return result;
    }

    public I128 saturatingSubtract(I128 other) {
        I128 result = subtract(other);
        if (((high ^ other.high) & (high ^ result.high)) < 0) {
            return high < 0
                    ? new I128(Long.MIN_VALUE, 0L)
                    : new I128(Long.MAX_VALUE, -1L);
        }
        return result;
    }

    public I128 multiply(I128 other) {
        U128 bits = new U128(high, low).multiply(new U128(other.high, other.low));
        return new I128(bits.high, bits.low);
    }

    private U128 magnitude() {
        U128 bits = new U128(high, low);
        return high < 0 ? bits.negate() : bits;
    }

    public I128 divide(I128 other) {
        if ((other.high | other.low) == 0) {
            throw new ArithmeticException("division by zero");
        }
        U128 quotient = magnitude().divide(other.magnitude());
        if ((high < 0) != (other.high < 0)) {
            quotient = quotient.negate();
        }
        return new I128(quotient.high, quotient.low);
    }

    public I128 remainder(I128 other) {
        if ((other.high | other.low) == 0) {
            throw new ArithmeticException("division by zero");
        }
        U128 remainder = magnitude().remainder(other.magnitude());
        if (high < 0) {
            remainder = remainder.negate();
        }
        return new I128(remainder.high, remainder.low);
    }

    public I128 and(I128 other) {
        return new I128(high & other.high, low & other.low);
    }

    public I128 or(I128 other) {
        return new I128(high | other.high, low | other.low);
    }

    public I128 xor(I128 other) {
        return new I128(high ^ other.high, low ^ other.low);
    }

    public I128 not() {
        return new I128(~high, ~low);
    }

    public I128 negate() {
        long resultLow = -low;
        return new I128(~high + (resultLow == 0 ? 1L : 0L), resultLow);
    }

    public I128 shiftLeft(int distance) {
        U128 shifted = new U128(high, low).shiftLeft(distance);
        return new I128(shifted.high, shifted.low);
    }

    public I128 shiftRight(int distance) {
        int shift = distance & 127;
        if (shift == 0) {
            return this;
        }
        if (shift < 64) {
            return new I128(high >> shift, (low >>> shift) | (high << (64 - shift)));
        }
        return new I128(high < 0 ? -1L : 0L, high >> (shift - 64));
    }

    public byte byteAt(int index) {
        return new U128(high, low).byteAt(index);
    }

    public I128 withByte(int index, int value) {
        U128 bits = new U128(high, low).withByte(index, value);
        return new I128(bits.high, bits.low);
    }

    @Override
    public int compareTo(I128 other) {
        int highComparison = Long.compare(high, other.high);
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
        return other instanceof I128
                && high == ((I128) other).high
                && low == ((I128) other).low;
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
