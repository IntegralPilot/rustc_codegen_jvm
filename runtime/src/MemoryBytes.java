package org.rustlang.runtime;

/** Little-endian scalar access shared by exact-layout codecs and pointer views. */
public final class MemoryBytes {
    private MemoryBytes() {}

    public static void write(byte[] bytes, int offset, int size, long value) {
        for (int index = 0; index < size; index++) {
            bytes[offset + index] = (byte) (value >>> (index * 8));
        }
    }

    public static long read(byte[] bytes, int offset, int size) {
        long value = 0;
        for (int index = 0; index < size; index++) {
            value |= ((long) bytes[offset + index] & 0xffL) << (index * 8);
        }
        return value;
    }

    public static void writeI128(byte[] bytes, int offset, I128 value) {
        write(bytes, offset, 8, value.low);
        write(bytes, offset + 8, 8, value.high);
    }

    public static void writeU128(byte[] bytes, int offset, U128 value) {
        write(bytes, offset, 8, value.low);
        write(bytes, offset + 8, 8, value.high);
    }

    public static I128 readI128(byte[] bytes, int offset) {
        return new I128(read(bytes, offset + 8, 8), read(bytes, offset, 8));
    }

    public static U128 readU128(byte[] bytes, int offset) {
        return new U128(read(bytes, offset + 8, 8), read(bytes, offset, 8));
    }
}
