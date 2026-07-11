package org.rustlang.runtime;

import java.math.BigInteger;

/** An immutable IEEE 754 binary128 value, stored without losing payload bits. */
public final class F128 {
    private static final int FRACTION_BITS = 112;
    private static final int EXPONENT_BIAS = 16383;
    private static final int MAX_EXPONENT = 0x7fff;
    private static final BigInteger TWO_112 = BigInteger.ONE.shiftLeft(FRACTION_BITS);
    private static final BigInteger FRACTION_MASK = TWO_112.subtract(BigInteger.ONE);
    private static final BigInteger MASK_64 = BigInteger.ONE.shiftLeft(64).subtract(BigInteger.ONE);
    private static final BigInteger MASK_128 = BigInteger.ONE.shiftLeft(128).subtract(BigInteger.ONE);
    private static final F128 NAN = new F128(0x7fff800000000000L, 0L);

    private final long high;
    private final long low;

    public F128(long high, long low) {
        this.high = high;
        this.low = low;
    }

    public long highBits() {
        return high;
    }

    public long lowBits() {
        return low;
    }

    public BigInteger toBits() {
        return unsignedLong(high).shiftLeft(64).or(unsignedLong(low));
    }

    public BigInteger to_bits() {
        return toBits();
    }

    public static BigInteger to_bits(F128 value) {
        return value.toBits();
    }

    public static F128 fromBits(BigInteger bits) {
        BigInteger raw = bits.and(MASK_128);
        return new F128(raw.shiftRight(64).longValue(), raw.longValue());
    }

    public static F128 from_bits(BigInteger bits) {
        return fromBits(bits);
    }

    public F128 negate() {
        return new F128(high ^ Long.MIN_VALUE, low);
    }

    public F128 add(F128 other) {
        if (isNaN() || other.isNaN()) {
            return NAN;
        }
        if (isInfinite() || other.isInfinite()) {
            if (isInfinite() && other.isInfinite() && isNegative() != other.isNegative()) {
                return NAN;
            }
            return isInfinite() ? this : other;
        }
        if (isZero() && other.isZero()) {
            return zero(isNegative() && other.isNegative());
        }
        Rational a = finiteRational();
        Rational b = other.finiteRational();
        BigInteger numerator = a.numerator.multiply(b.denominator)
                .add(b.numerator.multiply(a.denominator));
        BigInteger denominator = a.denominator.multiply(b.denominator);
        return fromRational(numerator, denominator, false);
    }

    public F128 subtract(F128 other) {
        return add(other.negate());
    }

    public F128 multiply(F128 other) {
        if (isNaN() || other.isNaN()
                || (isInfinite() && other.isZero())
                || (isZero() && other.isInfinite())) {
            return NAN;
        }
        boolean negative = isNegative() ^ other.isNegative();
        if (isInfinite() || other.isInfinite()) {
            return infinity(negative);
        }
        if (isZero() || other.isZero()) {
            return zero(negative);
        }
        Rational a = finiteRational();
        Rational b = other.finiteRational();
        return fromRational(
                a.numerator.multiply(b.numerator),
                a.denominator.multiply(b.denominator),
                negative);
    }

    public F128 divide(F128 other) {
        if (isNaN() || other.isNaN()
                || (isInfinite() && other.isInfinite())
                || (isZero() && other.isZero())) {
            return NAN;
        }
        boolean negative = isNegative() ^ other.isNegative();
        if (isInfinite() || other.isZero()) {
            return infinity(negative);
        }
        if (isZero() || other.isInfinite()) {
            return zero(negative);
        }
        Rational a = finiteRational();
        Rational b = other.finiteRational();
        return fromRational(
                a.numerator.multiply(b.denominator),
                a.denominator.multiply(b.numerator),
                negative);
    }

    public F128 remainder(F128 other) {
        if (isNaN() || other.isNaN() || isInfinite() || other.isZero()) {
            return NAN;
        }
        if (other.isInfinite() || isZero()) {
            return this;
        }
        Rational a = finiteRational();
        Rational b = other.finiteRational();
        BigInteger quotient = a.numerator.abs().multiply(b.denominator)
                .divide(a.denominator.multiply(b.numerator.abs()));
        if (a.numerator.signum() != b.numerator.signum()) {
            quotient = quotient.negate();
        }
        BigInteger numerator = a.numerator.multiply(b.denominator)
                .subtract(quotient.multiply(b.numerator).multiply(a.denominator));
        BigInteger denominator = a.denominator.multiply(b.denominator);
        return fromRational(numerator, denominator, isNegative());
    }

    public boolean eq(F128 other) {
        if (isNaN() || other.isNaN()) {
            return false;
        }
        if (isZero() && other.isZero()) {
            return true;
        }
        return high == other.high && low == other.low;
    }

    public boolean ne(F128 other) {
        return !eq(other);
    }

    public boolean lt(F128 other) {
        Integer comparison = orderedCompare(other);
        return comparison != null && comparison < 0;
    }

    public boolean le(F128 other) {
        Integer comparison = orderedCompare(other);
        return comparison != null && comparison <= 0;
    }

    public boolean gt(F128 other) {
        Integer comparison = orderedCompare(other);
        return comparison != null && comparison > 0;
    }

    public boolean ge(F128 other) {
        Integer comparison = orderedCompare(other);
        return comparison != null && comparison >= 0;
    }

    private Integer orderedCompare(F128 other) {
        if (isNaN() || other.isNaN()) {
            return null;
        }
        if (eq(other)) {
            return 0;
        }
        if (isInfinite()) {
            return isNegative() ? -1 : 1;
        }
        if (other.isInfinite()) {
            return other.isNegative() ? 1 : -1;
        }
        Rational a = finiteRational();
        Rational b = other.finiteRational();
        return a.numerator.multiply(b.denominator)
                .compareTo(b.numerator.multiply(a.denominator));
    }

    private boolean isNegative() {
        return high < 0;
    }

    private boolean isNaN() {
        return exponent() == MAX_EXPONENT && fraction().signum() != 0;
    }

    private boolean isInfinite() {
        return exponent() == MAX_EXPONENT && fraction().signum() == 0;
    }

    private boolean isZero() {
        return exponent() == 0 && fraction().signum() == 0;
    }

    private int exponent() {
        return (int) ((high >>> 48) & 0x7fffL);
    }

    private BigInteger fraction() {
        return toBits().and(FRACTION_MASK);
    }

    private Rational finiteRational() {
        int exponent = exponent();
        BigInteger significand = fraction();
        int power;
        if (exponent == 0) {
            power = 1 - EXPONENT_BIAS - FRACTION_BITS;
        } else {
            significand = significand.add(TWO_112);
            power = exponent - EXPONENT_BIAS - FRACTION_BITS;
        }
        BigInteger numerator = significand;
        BigInteger denominator = BigInteger.ONE;
        if (power >= 0) {
            numerator = numerator.shiftLeft(power);
        } else {
            denominator = denominator.shiftLeft(-power);
        }
        if (isNegative()) {
            numerator = numerator.negate();
        }
        return new Rational(numerator, denominator);
    }

    private static F128 fromRational(
            BigInteger signedNumerator, BigInteger denominator, boolean negativeZero) {
        if (signedNumerator.signum() == 0) {
            return zero(negativeZero);
        }
        boolean negative = signedNumerator.signum() != denominator.signum();
        BigInteger numerator = signedNumerator.abs();
        denominator = denominator.abs();
        int exponent = floorLog2(numerator, denominator);

        if (exponent > EXPONENT_BIAS) {
            return infinity(negative);
        }

        BigInteger significand;
        int encodedExponent;
        if (exponent >= 1 - EXPONENT_BIAS) {
            int scale = FRACTION_BITS - exponent;
            significand = scale >= 0
                    ? roundToEven(numerator.shiftLeft(scale), denominator)
                    : roundToEven(numerator, denominator.shiftLeft(-scale));
            if (significand.bitLength() > FRACTION_BITS + 1) {
                significand = significand.shiftRight(1);
                exponent++;
                if (exponent > EXPONENT_BIAS) {
                    return infinity(negative);
                }
            }
            encodedExponent = exponent + EXPONENT_BIAS;
            significand = significand.subtract(TWO_112);
        } else {
            significand = roundToEven(numerator.shiftLeft(16494), denominator);
            if (significand.signum() == 0) {
                return zero(negative);
            }
            if (significand.compareTo(TWO_112) >= 0) {
                encodedExponent = 1;
                significand = BigInteger.ZERO;
            } else {
                encodedExponent = 0;
            }
        }

        BigInteger bits = BigInteger.valueOf(encodedExponent).shiftLeft(FRACTION_BITS)
                .or(significand.and(FRACTION_MASK));
        if (negative) {
            bits = bits.setBit(127);
        }
        return fromBits(bits);
    }

    private static int floorLog2(BigInteger numerator, BigInteger denominator) {
        int exponent = numerator.bitLength() - denominator.bitLength();
        int comparison = exponent >= 0
                ? numerator.compareTo(denominator.shiftLeft(exponent))
                : numerator.shiftLeft(-exponent).compareTo(denominator);
        return comparison < 0 ? exponent - 1 : exponent;
    }

    private static BigInteger roundToEven(BigInteger numerator, BigInteger denominator) {
        BigInteger[] division = numerator.divideAndRemainder(denominator);
        int comparison = division[1].shiftLeft(1).compareTo(denominator);
        if (comparison > 0 || (comparison == 0 && division[0].testBit(0))) {
            return division[0].add(BigInteger.ONE);
        }
        return division[0];
    }

    private static F128 zero(boolean negative) {
        return new F128(negative ? Long.MIN_VALUE : 0L, 0L);
    }

    private static F128 infinity(boolean negative) {
        return new F128((negative ? Long.MIN_VALUE : 0L) | 0x7fff000000000000L, 0L);
    }

    private static BigInteger unsignedLong(long value) {
        return BigInteger.valueOf(value).and(MASK_64);
    }

    private static final class Rational {
        private final BigInteger numerator;
        private final BigInteger denominator;

        private Rational(BigInteger numerator, BigInteger denominator) {
            this.numerator = numerator;
            this.denominator = denominator;
        }
    }
}
