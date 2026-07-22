package org.rustlang.runtime;

/** JVM implementations of the binary128 libm entry points used by std. */
public final class F128Math {
    private static final double LOG_TWO = Math.log(2.0);
    private static final double LOG_MAX = 16384.0 * LOG_TWO;
    private static final double[] LANCZOS = {
        0.99999999999980993,
        676.5203681218851,
        -1259.1392167224028,
        771.32342877765313,
        -176.61502916214059,
        12.507343278686905,
        -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7
    };

    private F128Math() {}

    public static F128 log1p(F128 value) {
        if (value.isNaN() || value.isZero()) return value;
        F128 minusOne = F128.fromF64(-1.0);
        if (value.eq(minusOne)) return F128.fromF64(Double.NEGATIVE_INFINITY);
        if (value.lt(minusOne)) return F128.fromF64(Double.NaN);
        double narrowed = value.castToF64();
        if (Double.isInfinite(narrowed) && !value.isInfinite()) {
            return F128.fromF64(value.naturalLogApprox());
        }
        return F128.fromF64(Math.log1p(narrowed));
    }

    public static F128 asinh(F128 value) {
        if (value.isNaN() || value.isInfinite() || value.isZero()) return value;
        double narrowed = value.castToF64();
        double magnitude = Math.abs(narrowed);
        double result;
        if (Double.isInfinite(magnitude) || magnitude > 1.0e154) {
            result = floatLogMagnitude(value) + LOG_TWO;
        } else {
            result = Math.log1p(
                    magnitude + magnitude * magnitude / (1.0 + Math.sqrt(1.0 + magnitude * magnitude)));
        }
        return F128.fromF64(value.isNegative() ? -result : result);
    }

    public static F128 acosh(F128 value) {
        if (value.isNaN() || value.isNegative() || value.lt(F128.fromF64(1.0))) {
            return F128.fromF64(Double.NaN);
        }
        if (value.eq(F128.fromF64(1.0))) return F128.fromF64(0.0);
        if (value.isInfinite()) return value;
        double narrowed = value.castToF64();
        if (Double.isInfinite(narrowed) || narrowed > 1.0e154) {
            return F128.fromF64(value.naturalLogApprox() + LOG_TWO);
        }
        double offset = narrowed - 1.0;
        return F128.fromF64(Math.log1p(offset + Math.sqrt(offset * (narrowed + 1.0))));
    }

    public static F128 cosh(F128 value) {
        if (value.isNaN() || value.isInfinite()) {
            return value.isNegative() ? value.negate() : value;
        }
        double narrowed = value.castToF64();
        double result = Math.cosh(narrowed);
        return Double.isInfinite(result)
                ? F128.fromNaturalLog(Math.abs(narrowed) - LOG_TWO, false)
                : F128.fromF64(result);
    }

    public static F128 gamma(F128 value) {
        if (value.isNaN() || (value.isInfinite() && value.isNegative())) {
            return F128.fromF64(Double.NaN);
        }
        if (value.isInfinite()) return value;
        if (value.isZero()) {
            return F128.fromF64(value.isNegative()
                    ? Double.NEGATIVE_INFINITY
                    : Double.POSITIVE_INFINITY);
        }

        double narrowed = value.castToF64();
        if (narrowed < 0.0 && narrowed == Math.rint(narrowed)) {
            return F128.fromF64(Double.NaN);
        }
        double logMagnitude = logGammaMagnitude(narrowed);
        boolean negative = narrowed < 0.0 && Math.sin(Math.PI * narrowed) < 0.0;
        if (logMagnitude >= LOG_MAX) {
            return F128.fromF64(negative
                    ? Double.NEGATIVE_INFINITY
                    : Double.POSITIVE_INFINITY);
        }
        return F128.fromNaturalLog(logMagnitude, negative);
    }

    public static F128 logGamma(F128 value, Pointer sign) {
        double narrowed = value.castToF64();
        int signValue = narrowed < 0.0 && Math.sin(Math.PI * narrowed) < 0.0 ? -1 : 1;
        sign.set(Integer.valueOf(signValue));
        return F128.fromF64(logGammaMagnitude(narrowed));
    }

    private static double floatLogMagnitude(F128 value) {
        return value.isNegative() ? value.negate().naturalLogApprox() : value.naturalLogApprox();
    }

    private static double logGammaMagnitude(double value) {
        if (Double.isNaN(value)) return Double.NaN;
        if (value == Double.POSITIVE_INFINITY) return Double.POSITIVE_INFINITY;
        if (value == Double.NEGATIVE_INFINITY || (value < 0.0 && value == Math.rint(value))) {
            return Double.POSITIVE_INFINITY;
        }
        if (value < 0.5) {
            return Math.log(Math.PI)
                    - Math.log(Math.abs(Math.sin(Math.PI * value)))
                    - logGammaMagnitude(1.0 - value);
        }

        double shifted = value - 1.0;
        double sum = LANCZOS[0];
        for (int index = 1; index < LANCZOS.length; index++) {
            sum += LANCZOS[index] / (shifted + index);
        }
        double base = shifted + 7.5;
        return 0.5 * Math.log(2.0 * Math.PI)
                + (shifted + 0.5) * Math.log(base)
                - base
                + Math.log(sum);
    }
}
