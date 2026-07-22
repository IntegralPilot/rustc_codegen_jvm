package org.rustlang.runtime;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/** Allocation-free JVM implementations of value-selection Rust intrinsics. */
public final class Intrinsics {
    private Intrinsics() {}

    private static final class SimdPlan {
        private final Field lanes;
        private final Constructor<?> constructor;

        private SimdPlan(Class<?> vectorClass) {
            try {
                Field laneField = null;
                for (Field field : vectorClass.getFields()) {
                    if (!Modifier.isStatic(field.getModifiers()) && field.getType().isArray()) {
                        if (laneField != null) {
                            throw new IllegalArgumentException(
                                    "SIMD carrier has more than one lane array: "
                                            + vectorClass.getName());
                        }
                        laneField = field;
                    }
                }
                if (laneField == null || !laneField.getType().getComponentType().isPrimitive()) {
                    throw new IllegalArgumentException(
                            "SIMD carrier has no primitive lane array: " + vectorClass.getName());
                }
                lanes = laneField;
                constructor = vectorClass.getConstructor(laneField.getType());
                lanes.setAccessible(true);
                constructor.setAccessible(true);
            } catch (ReflectiveOperationException error) {
                throw new IllegalArgumentException(
                        "invalid generated SIMD carrier " + vectorClass.getName(), error);
            }
        }

        private Object lanes(Object vector) {
            try {
                return lanes.get(vector);
            } catch (IllegalAccessException error) {
                throw new IllegalStateException("could not read SIMD lanes", error);
            }
        }

        private Object vector(Object lanes) {
            try {
                return constructor.newInstance(lanes);
            } catch (ReflectiveOperationException error) {
                throw new IllegalStateException("could not construct SIMD result", error);
            }
        }
    }

    private static final ClassValue<SimdPlan> SIMD_PLANS =
            new ClassValue<SimdPlan>() {
                @Override
                protected SimdPlan computeValue(Class<?> type) {
                    return new SimdPlan(type);
                }
            };

    private static Object simdSplatObject(Object value, String resultClassName, int laneCount) {
        try {
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
            Class<?> resultClass =
                    Class.forName(resultClassName.replace('/', '.'), true, loader);
            SimdPlan plan = SIMD_PLANS.get(resultClass);
            Class<?> arrayType = plan.constructor.getParameterTypes()[0];
            Object lanes = java.lang.reflect.Array.newInstance(
                    arrayType.getComponentType(), laneCount);
            for (int lane = 0; lane < java.lang.reflect.Array.getLength(lanes); lane++) {
                java.lang.reflect.Array.set(lanes, lane, value);
            }
            return plan.vector(lanes);
        } catch (ClassNotFoundException error) {
            throw new IllegalArgumentException(
                    "could not load SIMD result carrier " + resultClassName, error);
        }
    }

    public static Object simdSplat(byte value, String resultClassName, int laneCount) {
        return simdSplatObject(Byte.valueOf(value), resultClassName, laneCount);
    }
    public static Object simdSplat(short value, String resultClassName, int laneCount) {
        return simdSplatObject(Short.valueOf(value), resultClassName, laneCount);
    }
    public static Object simdSplat(char value, String resultClassName, int laneCount) {
        return simdSplatObject(Character.valueOf(value), resultClassName, laneCount);
    }
    public static Object simdSplat(int value, String resultClassName, int laneCount) {
        return simdSplatObject(Integer.valueOf(value), resultClassName, laneCount);
    }
    public static Object simdSplat(long value, String resultClassName, int laneCount) {
        return simdSplatObject(Long.valueOf(value), resultClassName, laneCount);
    }
    public static Object simdSplat(float value, String resultClassName, int laneCount) {
        return simdSplatObject(Float.valueOf(value), resultClassName, laneCount);
    }
    public static Object simdSplat(double value, String resultClassName, int laneCount) {
        return simdSplatObject(Double.valueOf(value), resultClassName, laneCount);
    }

    public static Object simdUnary(String operation, Object vector) {
        SimdPlan plan = SIMD_PLANS.get(vector.getClass());
        Object input = plan.lanes(vector);
        int length = java.lang.reflect.Array.getLength(input);
        Class<?> component = input.getClass().getComponentType();
        Object result = java.lang.reflect.Array.newInstance(component, length);
        for (int lane = 0; lane < length; lane++) {
            if (component == float.class) {
                float value = java.lang.reflect.Array.getFloat(input, lane);
                java.lang.reflect.Array.setFloat(
                        result,
                        lane,
                        operation.equals("simd_neg")
                                ? -value
                                : Float.intBitsToFloat(
                                        Float.floatToRawIntBits(value) & 0x7fff_ffff));
            } else if (component == double.class) {
                double value = java.lang.reflect.Array.getDouble(input, lane);
                java.lang.reflect.Array.setDouble(
                        result,
                        lane,
                        operation.equals("simd_neg")
                                ? -value
                                : Double.longBitsToDouble(
                                        Double.doubleToRawLongBits(value) & Long.MAX_VALUE));
            } else if (operation.equals("simd_fabs") && component == short.class) {
                java.lang.reflect.Array.setShort(
                        result,
                        lane,
                        (short) (java.lang.reflect.Array.getShort(input, lane) & 0x7fff));
            } else if (operation.equals("simd_neg")) {
                setNegatedLane(result, input, component, lane);
            } else {
                throw new IllegalArgumentException(
                        operation + " does not support SIMD lane type " + component.getName());
            }
        }
        return plan.vector(result);
    }

    private static void setNegatedLane(Object result, Object input, Class<?> component, int lane) {
        if (component == byte.class) {
            java.lang.reflect.Array.setByte(
                    result, lane, (byte) -java.lang.reflect.Array.getByte(input, lane));
        } else if (component == short.class) {
            java.lang.reflect.Array.setShort(
                    result, lane, (short) -java.lang.reflect.Array.getShort(input, lane));
        } else if (component == int.class) {
            java.lang.reflect.Array.setInt(
                    result, lane, -java.lang.reflect.Array.getInt(input, lane));
        } else if (component == long.class) {
            java.lang.reflect.Array.setLong(
                    result, lane, -java.lang.reflect.Array.getLong(input, lane));
        } else {
            throw new IllegalArgumentException(
                    "simd_neg does not support SIMD lane type " + component.getName());
        }
    }

    public static Object simdMultiply(Object leftVector, Object rightVector) {
        SimdPlan plan = SIMD_PLANS.get(leftVector.getClass());
        if (leftVector.getClass() != rightVector.getClass()) {
            throw new IllegalArgumentException("SIMD multiplication requires matching vector types");
        }
        Object left = plan.lanes(leftVector);
        Object right = plan.lanes(rightVector);
        int length = java.lang.reflect.Array.getLength(left);
        if (length != java.lang.reflect.Array.getLength(right)) {
            throw new IllegalArgumentException("SIMD multiplication requires matching lane counts");
        }
        Class<?> component = left.getClass().getComponentType();
        Object result = java.lang.reflect.Array.newInstance(component, length);
        for (int lane = 0; lane < length; lane++) {
            if (component == float.class) {
                java.lang.reflect.Array.setFloat(
                        result,
                        lane,
                        java.lang.reflect.Array.getFloat(left, lane)
                                * java.lang.reflect.Array.getFloat(right, lane));
            } else if (component == double.class) {
                java.lang.reflect.Array.setDouble(
                        result,
                        lane,
                        java.lang.reflect.Array.getDouble(left, lane)
                                * java.lang.reflect.Array.getDouble(right, lane));
            } else if (component == byte.class) {
                java.lang.reflect.Array.setByte(
                        result,
                        lane,
                        (byte) (java.lang.reflect.Array.getByte(left, lane)
                                * java.lang.reflect.Array.getByte(right, lane)));
            } else if (component == short.class) {
                java.lang.reflect.Array.setShort(
                        result,
                        lane,
                        (short) (java.lang.reflect.Array.getShort(left, lane)
                                * java.lang.reflect.Array.getShort(right, lane)));
            } else if (component == int.class) {
                java.lang.reflect.Array.setInt(
                        result,
                        lane,
                        java.lang.reflect.Array.getInt(left, lane)
                                * java.lang.reflect.Array.getInt(right, lane));
            } else if (component == long.class) {
                java.lang.reflect.Array.setLong(
                        result,
                        lane,
                        java.lang.reflect.Array.getLong(left, lane)
                                * java.lang.reflect.Array.getLong(right, lane));
            } else {
                throw new IllegalArgumentException(
                        "simd_mul does not support SIMD lane type " + component.getName());
            }
        }
        return plan.vector(result);
    }

    public static Object simdCompare(
            String operation,
            Object leftVector,
            Object rightVector,
            String resultClassName) {
        if (leftVector.getClass() != rightVector.getClass()) {
            throw new IllegalArgumentException("SIMD comparison requires matching vector types");
        }
        SimdPlan inputPlan = SIMD_PLANS.get(leftVector.getClass());
        Object left = inputPlan.lanes(leftVector);
        Object right = inputPlan.lanes(rightVector);
        int length = java.lang.reflect.Array.getLength(left);
        if (length != java.lang.reflect.Array.getLength(right)) {
            throw new IllegalArgumentException("SIMD comparison requires matching lane counts");
        }
        try {
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
            Class<?> resultClass =
                    Class.forName(resultClassName.replace('/', '.'), true, loader);
            SimdPlan resultPlan = SIMD_PLANS.get(resultClass);
            Class<?> resultArrayType = resultPlan.constructor.getParameterTypes()[0];
            Object result = java.lang.reflect.Array.newInstance(
                    resultArrayType.getComponentType(), length);
            Class<?> inputComponent = left.getClass().getComponentType();
            for (int lane = 0; lane < length; lane++) {
                boolean equal;
                if (inputComponent == float.class) {
                    equal = java.lang.reflect.Array.getFloat(left, lane)
                            == java.lang.reflect.Array.getFloat(right, lane);
                } else if (inputComponent == double.class) {
                    equal = java.lang.reflect.Array.getDouble(left, lane)
                            == java.lang.reflect.Array.getDouble(right, lane);
                } else {
                    equal = java.lang.reflect.Array.get(left, lane)
                            .equals(java.lang.reflect.Array.get(right, lane));
                }
                setMaskLane(result, lane, operation.equals("simd_ne") ? !equal : equal);
            }
            return resultPlan.vector(result);
        } catch (ClassNotFoundException error) {
            throw new IllegalArgumentException(
                    "could not load SIMD comparison carrier " + resultClassName, error);
        }
    }

    private static void setMaskLane(Object result, int lane, boolean value) {
        Class<?> component = result.getClass().getComponentType();
        int bits = value ? -1 : 0;
        if (component == byte.class) {
            java.lang.reflect.Array.setByte(result, lane, (byte) bits);
        } else if (component == short.class) {
            java.lang.reflect.Array.setShort(result, lane, (short) bits);
        } else if (component == int.class) {
            java.lang.reflect.Array.setInt(result, lane, bits);
        } else if (component == long.class) {
            java.lang.reflect.Array.setLong(result, lane, (long) bits);
        } else {
            throw new IllegalArgumentException(
                    "SIMD mask has unsupported lane type " + component.getName());
        }
    }

    public static Object simdBitwise(String operation, Object leftVector, Object rightVector) {
        if (leftVector.getClass() != rightVector.getClass()) {
            throw new IllegalArgumentException("SIMD bitwise operation requires matching types");
        }
        SimdPlan plan = SIMD_PLANS.get(leftVector.getClass());
        Object left = plan.lanes(leftVector);
        Object right = plan.lanes(rightVector);
        int length = java.lang.reflect.Array.getLength(left);
        if (length != java.lang.reflect.Array.getLength(right)) {
            throw new IllegalArgumentException("SIMD bitwise operation requires matching lanes");
        }
        Class<?> component = left.getClass().getComponentType();
        Object result = java.lang.reflect.Array.newInstance(component, length);
        for (int lane = 0; lane < length; lane++) {
            if (component == byte.class) {
                int value = bitwise(
                        operation,
                        java.lang.reflect.Array.getByte(left, lane),
                        java.lang.reflect.Array.getByte(right, lane));
                java.lang.reflect.Array.setByte(result, lane, (byte) value);
            } else if (component == short.class) {
                int value = bitwise(
                        operation,
                        java.lang.reflect.Array.getShort(left, lane),
                        java.lang.reflect.Array.getShort(right, lane));
                java.lang.reflect.Array.setShort(result, lane, (short) value);
            } else if (component == char.class) {
                int value = bitwise(
                        operation,
                        java.lang.reflect.Array.getChar(left, lane),
                        java.lang.reflect.Array.getChar(right, lane));
                java.lang.reflect.Array.setChar(result, lane, (char) value);
            } else if (component == int.class) {
                java.lang.reflect.Array.setInt(
                        result,
                        lane,
                        bitwise(
                                operation,
                                java.lang.reflect.Array.getInt(left, lane),
                                java.lang.reflect.Array.getInt(right, lane)));
            } else if (component == long.class) {
                java.lang.reflect.Array.setLong(
                        result,
                        lane,
                        bitwise(
                                operation,
                                java.lang.reflect.Array.getLong(left, lane),
                                java.lang.reflect.Array.getLong(right, lane)));
            } else {
                throw new IllegalArgumentException(
                        operation + " does not support SIMD lane type " + component.getName());
            }
        }
        return plan.vector(result);
    }

    private static int bitwise(String operation, int left, int right) {
        if (operation.equals("simd_and")) return left & right;
        if (operation.equals("simd_or")) return left | right;
        if (operation.equals("simd_xor")) return left ^ right;
        throw new IllegalArgumentException("unknown SIMD bitwise operation " + operation);
    }

    private static long bitwise(String operation, long left, long right) {
        if (operation.equals("simd_and")) return left & right;
        if (operation.equals("simd_or")) return left | right;
        if (operation.equals("simd_xor")) return left ^ right;
        throw new IllegalArgumentException("unknown SIMD bitwise operation " + operation);
    }

    public static boolean simdReduceAll(Object vector) {
        Object lanes = SIMD_PLANS.get(vector.getClass()).lanes(vector);
        int length = java.lang.reflect.Array.getLength(lanes);
        Class<?> component = lanes.getClass().getComponentType();
        for (int lane = 0; lane < length; lane++) {
            boolean set;
            if (component == byte.class) {
                set = java.lang.reflect.Array.getByte(lanes, lane) != 0;
            } else if (component == short.class) {
                set = java.lang.reflect.Array.getShort(lanes, lane) != 0;
            } else if (component == char.class) {
                set = java.lang.reflect.Array.getChar(lanes, lane) != 0;
            } else if (component == int.class) {
                set = java.lang.reflect.Array.getInt(lanes, lane) != 0;
            } else if (component == long.class) {
                set = java.lang.reflect.Array.getLong(lanes, lane) != 0;
            } else {
                throw new IllegalArgumentException(
                        "simd_reduce_all does not support lane type " + component.getName());
            }
            if (!set) return false;
        }
        return true;
    }

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

    private static short half(double value) { return Numbers.f64ToF16(value); }
    private static double halfValue(short value) { return Numbers.f16ToF32(value); }

    public static short copysignf16(short value, short sign) {
        return (short) ((value & 0x7fff) | (sign & 0x8000));
    }
    public static float copysignf32(float value, float sign) { return Math.copySign(value, sign); }
    public static double copysignf64(double value, double sign) { return Math.copySign(value, sign); }
    public static F128 copysignf128(F128 value, F128 sign) { return value.copySign(sign); }

    public static short floorf16(short value) { return half(Math.floor(halfValue(value))); }
    public static float floorf32(float value) { return (float) Math.floor(value); }
    public static double floorf64(double value) { return Math.floor(value); }
    public static F128 floorf128(F128 value) { return value.floor(); }

    public static short ceilf16(short value) { return half(Math.ceil(halfValue(value))); }
    public static float ceilf32(float value) { return (float) Math.ceil(value); }
    public static double ceilf64(double value) { return Math.ceil(value); }
    public static F128 ceilf128(F128 value) { return value.ceil(); }

    private static double trunc(double value) {
        return Math.copySign(Math.floor(Math.abs(value)), value);
    }
    public static short truncf16(short value) { return half(trunc(halfValue(value))); }
    public static float truncf32(float value) { return (float) trunc(value); }
    public static double truncf64(double value) { return trunc(value); }
    public static F128 truncf128(F128 value) { return value.trunc(); }

    private static double roundAway(double value) {
        return Math.copySign(Math.floor(Math.abs(value) + 0.5), value);
    }
    public static short roundf16(short value) { return half(roundAway(halfValue(value))); }
    public static float roundf32(float value) { return (float) roundAway(value); }
    public static double roundf64(double value) { return roundAway(value); }
    public static F128 roundf128(F128 value) { return value.round(); }

    public static short round_ties_even_f16(short value) { return half(Math.rint(halfValue(value))); }
    public static float round_ties_even_f32(float value) { return (float) Math.rint(value); }
    public static double round_ties_even_f64(double value) { return Math.rint(value); }
    public static F128 round_ties_even_f128(F128 value) { return value.roundTiesEven(); }

    public static short sqrtf16(short value) { return half(Math.sqrt(halfValue(value))); }
    public static float sqrtf32(float value) { return (float) Math.sqrt(value); }
    public static double sqrtf64(double value) { return Math.sqrt(value); }
    public static F128 sqrtf128(F128 value) {
        if (value.isZero() || value.isNaN()) return value;
        if (value.isNegative()) return F128.fromF64(Double.NaN);
        if (value.isInfinite()) return value;
        return F128.fromF64(Math.sqrt(value.castToF64()));
    }

    public static short expf16(short value) { return half(Math.exp(halfValue(value))); }
    public static float expf32(float value) { return (float) Math.exp(value); }
    public static double expf64(double value) { return Math.exp(value); }
    public static F128 expf128(F128 value) { return F128.fromF64(Math.exp(value.castToF64())); }

    public static short exp2f16(short value) { return half(Math.pow(2.0, halfValue(value))); }
    public static float exp2f32(float value) { return (float) Math.pow(2.0, value); }
    public static double exp2f64(double value) { return Math.pow(2.0, value); }
    public static F128 exp2f128(F128 value) {
        return F128.fromF64(Math.pow(2.0, value.castToF64()));
    }

    private static double log2(double value) { return Math.log(value) / Math.log(2.0); }
    public static short logf16(short value) { return half(Math.log(halfValue(value))); }
    public static float logf32(float value) { return (float) Math.log(value); }
    public static double logf64(double value) { return Math.log(value); }
    public static F128 logf128(F128 value) { return F128.fromF64(value.naturalLogApprox()); }
    public static short log2f16(short value) { return half(log2(halfValue(value))); }
    public static float log2f32(float value) { return (float) log2(value); }
    public static double log2f64(double value) { return log2(value); }
    public static F128 log2f128(F128 value) {
        return F128.fromF64(value.naturalLogApprox() / Math.log(2.0));
    }
    public static short log10f16(short value) { return half(Math.log10(halfValue(value))); }
    public static float log10f32(float value) { return (float) Math.log10(value); }
    public static double log10f64(double value) { return Math.log10(value); }
    public static F128 log10f128(F128 value) {
        return F128.fromF64(value.naturalLogApprox() / Math.log(10.0));
    }

    public static short sinf16(short value) { return half(Math.sin(halfValue(value))); }
    public static float sinf32(float value) { return (float) Math.sin(value); }
    public static double sinf64(double value) { return Math.sin(value); }
    public static F128 sinf128(F128 value) { return F128.fromF64(Math.sin(value.castToF64())); }
    public static short cosf16(short value) { return half(Math.cos(halfValue(value))); }
    public static float cosf32(float value) { return (float) Math.cos(value); }
    public static double cosf64(double value) { return Math.cos(value); }
    public static F128 cosf128(F128 value) { return F128.fromF64(Math.cos(value.castToF64())); }

    public static short powf16(short value, short power) {
        return half(Math.pow(halfValue(value), halfValue(power)));
    }
    public static float powf32(float value, float power) { return (float) Math.pow(value, power); }
    public static double powf64(double value, double power) { return Math.pow(value, power); }
    public static F128 powf128(F128 value, F128 power) {
        return F128.fromF64(Math.pow(value.castToF64(), power.castToF64()));
    }

    public static short powif16(short value, int power) {
        return half(Math.pow(halfValue(value), power));
    }
    public static float powif32(float value, int power) { return (float) Math.pow(value, power); }
    public static double powif64(double value, int power) { return Math.pow(value, power); }
    public static F128 powif128(F128 value, int power) {
        long remaining = power;
        boolean reciprocal = remaining < 0;
        if (reciprocal) remaining = -remaining;
        F128 factor = value;
        F128 result = F128.fromF64(1.0);
        while (remaining != 0) {
            if ((remaining & 1L) != 0) result = result.multiply(factor);
            remaining >>>= 1;
            if (remaining != 0) factor = factor.multiply(factor);
        }
        return reciprocal ? F128.fromF64(1.0).divide(result) : result;
    }

    private static float fusedMultiplyAdd(float left, float right, float addend) {
        if (!Float.isFinite(left) || !Float.isFinite(right) || !Float.isFinite(addend)) {
            return left * right + addend;
        }
        java.math.BigDecimal exact = new java.math.BigDecimal(left)
                .multiply(new java.math.BigDecimal(right))
                .add(new java.math.BigDecimal(addend));
        return exact.floatValue();
    }

    private static double fusedMultiplyAdd(double left, double right, double addend) {
        if (!Double.isFinite(left) || !Double.isFinite(right) || !Double.isFinite(addend)) {
            return left * right + addend;
        }
        java.math.BigDecimal exact = new java.math.BigDecimal(left)
                .multiply(new java.math.BigDecimal(right))
                .add(new java.math.BigDecimal(addend));
        return exact.doubleValue();
    }

    public static short fmaf16(short left, short right, short addend) {
        return half(fusedMultiplyAdd(
                (double) halfValue(left), (double) halfValue(right), (double) halfValue(addend)));
    }
    public static float fmaf32(float left, float right, float addend) {
        return fusedMultiplyAdd(left, right, addend);
    }
    public static double fmaf64(double left, double right, double addend) {
        return fusedMultiplyAdd(left, right, addend);
    }
    public static F128 fmaf128(F128 left, F128 right, F128 addend) {
        return left.fusedMultiplyAdd(right, addend);
    }
    public static short fmuladdf16(short left, short right, short addend) {
        return half(halfValue(left) * halfValue(right) + halfValue(addend));
    }
    public static float fmuladdf32(float left, float right, float addend) {
        return left * right + addend;
    }
    public static double fmuladdf64(double left, double right, double addend) {
        return left * right + addend;
    }
    public static F128 fmuladdf128(F128 left, F128 right, F128 addend) {
        return left.multiply(right).add(addend);
    }

    private static float maximum(float left, float right) {
        if (Float.isNaN(left) || Float.isNaN(right)) return Float.NaN;
        return Math.max(left, right);
    }
    private static double maximum(double left, double right) {
        if (Double.isNaN(left) || Double.isNaN(right)) return Double.NaN;
        return Math.max(left, right);
    }
    private static float minimum(float left, float right) {
        if (Float.isNaN(left) || Float.isNaN(right)) return Float.NaN;
        return Math.min(left, right);
    }
    private static double minimum(double left, double right) {
        if (Double.isNaN(left) || Double.isNaN(right)) return Double.NaN;
        return Math.min(left, right);
    }
    public static short maximumf16(short left, short right) {
        return half(maximum((float) halfValue(left), (float) halfValue(right)));
    }
    public static float maximumf32(float left, float right) { return maximum(left, right); }
    public static double maximumf64(double left, double right) { return maximum(left, right); }
    public static F128 maximumf128(F128 left, F128 right) {
        if (left.isNaN() || right.isNaN()) return F128.fromF64(Double.NaN);
        if (left.isZero() && right.isZero()) return left.isNegative() ? right : left;
        return left.lt(right) ? right : left;
    }
    public static short minimumf16(short left, short right) {
        return half(minimum((float) halfValue(left), (float) halfValue(right)));
    }
    public static float minimumf32(float left, float right) { return minimum(left, right); }
    public static double minimumf64(double left, double right) { return minimum(left, right); }
    public static F128 minimumf128(F128 left, F128 right) {
        if (left.isNaN() || right.isNaN()) return F128.fromF64(Double.NaN);
        if (left.isZero() && right.isZero()) return left.isNegative() ? left : right;
        return left.gt(right) ? right : left;
    }

    public static short maximum_number_nsz_f16(short left, short right) {
        float a = (float) halfValue(left), b = (float) halfValue(right);
        return half(Float.isNaN(a) ? b : Float.isNaN(b) ? a : Math.max(a, b));
    }
    public static float maximum_number_nsz_f32(float left, float right) {
        return Float.isNaN(left) ? right : Float.isNaN(right) ? left : Math.max(left, right);
    }
    public static double maximum_number_nsz_f64(double left, double right) {
        return Double.isNaN(left) ? right : Double.isNaN(right) ? left : Math.max(left, right);
    }
    public static F128 maximum_number_nsz_f128(F128 left, F128 right) {
        return left.isNaN() ? right : right.isNaN() ? left : maximumf128(left, right);
    }
    public static short minimum_number_nsz_f16(short left, short right) {
        float a = (float) halfValue(left), b = (float) halfValue(right);
        return half(Float.isNaN(a) ? b : Float.isNaN(b) ? a : Math.min(a, b));
    }
    public static float minimum_number_nsz_f32(float left, float right) {
        return Float.isNaN(left) ? right : Float.isNaN(right) ? left : Math.min(left, right);
    }
    public static double minimum_number_nsz_f64(double left, double right) {
        return Double.isNaN(left) ? right : Double.isNaN(right) ? left : Math.min(left, right);
    }
    public static F128 minimum_number_nsz_f128(F128 left, F128 right) {
        return left.isNaN() ? right : right.isNaN() ? left : minimumf128(left, right);
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
