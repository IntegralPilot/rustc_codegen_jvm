package org.rustlang.runtime;

/** Shared little-endian codecs for primitive arrays, independent of Rust length. */
final class ArrayMemoryCodec {
    private ArrayMemoryCodec() { }

    static int elementSize(Class<?> type) {
        if (type == byte.class || type == boolean.class) return 1;
        if (type == short.class || type == char.class) return 2;
        if (type == int.class || type == float.class) return 4;
        if (type == long.class || type == double.class) return 8;
        throw new IllegalArgumentException("not a primitive array element: " + type);
    }

    static void write(Object source, byte[] target, int offset) {
        if (source instanceof byte[]) {
            byte[] values = (byte[]) source;
            System.arraycopy(values, 0, target, offset, values.length);
        } else if (source instanceof boolean[]) {
            for (boolean value : (boolean[]) source) target[offset++] = (byte) (value ? 1 : 0);
        } else if (source instanceof short[]) {
            for (short value : (short[]) source) { MemoryBytes.write(target, offset, 2, value); offset += 2; }
        } else if (source instanceof char[]) {
            for (char value : (char[]) source) { MemoryBytes.write(target, offset, 2, value); offset += 2; }
        } else if (source instanceof int[]) {
            for (int value : (int[]) source) { MemoryBytes.write(target, offset, 4, value); offset += 4; }
        } else if (source instanceof float[]) {
            for (float value : (float[]) source) { MemoryBytes.write(target, offset, 4, Float.floatToRawIntBits(value)); offset += 4; }
        } else if (source instanceof long[]) {
            for (long value : (long[]) source) { MemoryBytes.write(target, offset, 8, value); offset += 8; }
        } else if (source instanceof double[]) {
            for (double value : (double[]) source) { MemoryBytes.write(target, offset, 8, Double.doubleToRawLongBits(value)); offset += 8; }
        } else throw new IllegalArgumentException("not a primitive array");
    }

    static void read(byte[] source, int offset, Object target) {
        if (target instanceof byte[]) {
            byte[] values = (byte[]) target;
            System.arraycopy(source, offset, values, 0, values.length);
        } else if (target instanceof boolean[]) {
            boolean[] values = (boolean[]) target;
            for (int i = 0; i < values.length; i++) values[i] = source[offset++] != 0;
        } else if (target instanceof short[]) {
            short[] values = (short[]) target;
            for (int i = 0; i < values.length; i++, offset += 2) values[i] = (short) MemoryBytes.read(source, offset, 2);
        } else if (target instanceof char[]) {
            char[] values = (char[]) target;
            for (int i = 0; i < values.length; i++, offset += 2) values[i] = (char) MemoryBytes.read(source, offset, 2);
        } else if (target instanceof int[]) {
            int[] values = (int[]) target;
            for (int i = 0; i < values.length; i++, offset += 4) values[i] = (int) MemoryBytes.read(source, offset, 4);
        } else if (target instanceof float[]) {
            float[] values = (float[]) target;
            for (int i = 0; i < values.length; i++, offset += 4) values[i] = Float.intBitsToFloat((int) MemoryBytes.read(source, offset, 4));
        } else if (target instanceof long[]) {
            long[] values = (long[]) target;
            for (int i = 0; i < values.length; i++, offset += 8) values[i] = MemoryBytes.read(source, offset, 8);
        } else if (target instanceof double[]) {
            double[] values = (double[]) target;
            for (int i = 0; i < values.length; i++, offset += 8) values[i] = Double.longBitsToDouble(MemoryBytes.read(source, offset, 8));
        } else throw new IllegalArgumentException("not a primitive array");
    }
}
