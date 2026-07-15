package org.rustlang.runtime;

import java.math.BigInteger;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/** An immutable IEEE 754 binary128 value, stored without losing payload bits. */
public final class F128 {
    private static final int FRACTION_BITS = 112;
    private static final int EXPONENT_BIAS = 16383;
    private static final int MAX_EXPONENT = 0x7fff;
    private static final BigInteger TWO_112 = BigInteger.ONE.shiftLeft(FRACTION_BITS);
    private static final BigInteger FRACTION_MASK = TWO_112.subtract(BigInteger.ONE);
    private static final BigInteger MASK_64 = BigInteger.ONE.shiftLeft(64).subtract(BigInteger.ONE);
    private static final BigInteger MASK_128 = BigInteger.ONE.shiftLeft(128).subtract(BigInteger.ONE);
    private static final BigInteger TWO_127 = BigInteger.ONE.shiftLeft(127);
    private static final BigInteger TWO_128 = BigInteger.ONE.shiftLeft(128);
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

    public U128 to_bits() {
        return new U128(high, low);
    }

    public static U128 to_bits(F128 value) {
        return value.to_bits();
    }

    public U128 toU128() {
        return new U128(high, low);
    }

    public static F128 fromBits(BigInteger bits) {
        BigInteger raw = bits.and(MASK_128);
        return new F128(raw.shiftRight(64).longValue(), raw.longValue());
    }

    public static F128 from_bits(BigInteger bits) {
        return fromBits(bits);
    }

    public static F128 from_bits(U128 bits) {
        return new F128(bits.high, bits.low);
    }

    public static F128 fromU128(U128 bits) {
        return new F128(bits.high, bits.low);
    }

    public static F128 fromI64(long value) {
        return fromInteger(BigInteger.valueOf(value));
    }

    public static F128 fromU64(long value) {
        return fromInteger(unsignedLong(value));
    }

    public static F128 fromI128Value(I128 value) {
        return fromInteger(value.toBigInteger());
    }

    public static F128 fromU128Value(U128 value) {
        return fromInteger(value.toBigInteger());
    }

    public static F128 fromF32(float value) {
        return fromF64(value);
    }

    /** Exact widening conversion from an IEEE binary64 value. */
    public static F128 fromF64(double value) {
        long bits = Double.doubleToRawLongBits(value);
        boolean negative = bits < 0;
        int exponent = (int) ((bits >>> 52) & 0x7ffL);
        long fraction = bits & 0x000f_ffff_ffff_ffffL;
        if (exponent == 0x7ff) {
            if (fraction == 0) return infinity(negative);
            long payload = fraction << 12;
            return new F128((negative ? Long.MIN_VALUE : 0L)
                    | 0x7fff_0000_0000_0000L
                    | payload
                    | 0x0000_8000_0000_0000L, 0L);
        }
        if (exponent == 0 && fraction == 0) return zero(negative);

        BigInteger significand;
        int power;
        if (exponent == 0) {
            significand = BigInteger.valueOf(fraction);
            power = 1 - 1023 - 52;
        } else {
            significand = BigInteger.valueOf(fraction | (1L << 52));
            power = exponent - 1023 - 52;
        }
        BigInteger numerator = significand;
        BigInteger denominator = BigInteger.ONE;
        if (power >= 0) numerator = numerator.shiftLeft(power);
        else denominator = denominator.shiftLeft(-power);
        if (negative) numerator = numerator.negate();
        return fromRational(numerator, denominator, negative);
    }

    private static F128 fromInteger(BigInteger value) {
        return fromRational(value, BigInteger.ONE, false);
    }

    private BigInteger clampedInteger(BigInteger minimum, BigInteger maximum) {
        if (isNaN()) return BigInteger.ZERO;
        if (isInfinite()) return isNegative() ? minimum : maximum;
        Rational rational = finiteRational();
        BigInteger integer = rational.numerator.divide(rational.denominator);
        if (integer.compareTo(minimum) < 0) return minimum;
        if (integer.compareTo(maximum) > 0) return maximum;
        return integer;
    }

    public byte castToI8() {
        return clampedInteger(BigInteger.valueOf(Byte.MIN_VALUE), BigInteger.valueOf(Byte.MAX_VALUE)).byteValue();
    }

    public byte castToU8() {
        return clampedInteger(BigInteger.ZERO, BigInteger.valueOf(255)).byteValue();
    }

    public short castToI16() {
        return clampedInteger(BigInteger.valueOf(Short.MIN_VALUE), BigInteger.valueOf(Short.MAX_VALUE)).shortValue();
    }

    public char castToU16() {
        return (char) clampedInteger(BigInteger.ZERO, BigInteger.valueOf(65535)).intValue();
    }

    public int castToI32() {
        return clampedInteger(BigInteger.valueOf(Integer.MIN_VALUE), BigInteger.valueOf(Integer.MAX_VALUE)).intValue();
    }

    public int castToU32() {
        return clampedInteger(BigInteger.ZERO, BigInteger.ONE.shiftLeft(32).subtract(BigInteger.ONE)).intValue();
    }

    public long castToI64() {
        return clampedInteger(BigInteger.valueOf(Long.MIN_VALUE), BigInteger.valueOf(Long.MAX_VALUE)).longValue();
    }

    public long castToU64() {
        return clampedInteger(BigInteger.ZERO, BigInteger.ONE.shiftLeft(64).subtract(BigInteger.ONE)).longValue();
    }

    public I128 castToI128() {
        return I128.fromBigInteger(clampedInteger(TWO_127.negate(), TWO_127.subtract(BigInteger.ONE)));
    }

    public U128 castToU128() {
        return U128.fromBigInteger(clampedInteger(BigInteger.ZERO, TWO_128.subtract(BigInteger.ONE)));
    }

    public float castToF32() {
        if (isNaN()) return Float.NaN;
        if (isInfinite()) return isNegative() ? Float.NEGATIVE_INFINITY : Float.POSITIVE_INFINITY;
        if (isZero()) return isNegative() ? -0.0f : 0.0f;
        Rational rational = finiteRational();
        BigDecimal decimal = new BigDecimal(rational.numerator).divide(
                new BigDecimal(rational.denominator),
                new MathContext(200, RoundingMode.HALF_EVEN));
        return decimal.floatValue();
    }

    public double castToF64() {
        if (isNaN()) return Double.NaN;
        if (isInfinite()) return isNegative() ? Double.NEGATIVE_INFINITY : Double.POSITIVE_INFINITY;
        if (isZero()) return isNegative() ? -0.0d : 0.0d;
        Rational rational = finiteRational();
        BigDecimal decimal = new BigDecimal(rational.numerator).divide(
                new BigDecimal(rational.denominator),
                new MathContext(200, RoundingMode.HALF_EVEN));
        return decimal.doubleValue();
    }

    /** Correctly rounded IEEE binary128 to binary16 conversion. */
    public short castToF16() {
        int sign = isNegative() ? 0x8000 : 0;
        if (isNaN()) return (short) (sign | 0x7e00);
        if (isInfinite()) return (short) (sign | 0x7c00);
        if (isZero()) return (short) sign;

        Rational rational = finiteRational();
        BigInteger numerator = rational.numerator.abs();
        BigInteger denominator = rational.denominator;
        int exponent = floorLog2(numerator, denominator);
        if (exponent > 15) return (short) (sign | 0x7c00);

        BigInteger significand;
        int encodedExponent;
        if (exponent >= -14) {
            int scale = 10 - exponent;
            significand = scale >= 0
                    ? roundToEven(numerator.shiftLeft(scale), denominator)
                    : roundToEven(numerator, denominator.shiftLeft(-scale));
            if (significand.bitLength() > 11) {
                significand = significand.shiftRight(1);
                exponent++;
                if (exponent > 15) return (short) (sign | 0x7c00);
            }
            encodedExponent = exponent + 15;
            significand = significand.subtract(BigInteger.ONE.shiftLeft(10));
        } else {
            // A binary16 subnormal stores value * 2^24 as its fraction.
            significand = roundToEven(numerator.shiftLeft(24), denominator);
            if (significand.signum() == 0) return (short) sign;
            if (significand.compareTo(BigInteger.ONE.shiftLeft(10)) >= 0) {
                encodedExponent = 1;
                significand = BigInteger.ZERO;
            } else {
                encodedExponent = 0;
            }
        }
        return (short) (sign | (encodedExponent << 10) | significand.intValue());
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
