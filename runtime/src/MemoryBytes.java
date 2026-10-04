package org.rustlang.runtime;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.nio.ByteOrder;

/** Little-endian scalar access shared by exact-layout codecs and pointer views. */
public final class MemoryBytes {
    private static final MethodHandle READ_LONG = reader(long[].class);
    private static final MethodHandle READ_INT = reader(int[].class);

    private MemoryBytes() {}

    // Java 9 adds unaligned scalar views. Link once and retain the Java 8 fallback.
    private static MethodHandle reader(Class<?> type) {
        try {
            Class<?> handle = Class.forName("java.lang.invoke.VarHandle");
            Class<?> mode = Class.forName("java.lang.invoke.VarHandle$AccessMode");
            Object view = MethodHandles.class.getMethod("byteArrayViewVarHandle", Class.class, ByteOrder.class)
                    .invoke(null, type, ByteOrder.LITTLE_ENDIAN);
            return (MethodHandle) handle.getMethod("toMethodHandle", mode)
                    .invoke(view, mode.getField("GET").get(null));
        } catch (ClassNotFoundException | NoSuchMethodException unavailable) {
            return null;
        } catch (ReflectiveOperationException error) {
            throw new ExceptionInInitializerError(error);
        }
    }

    /** Copy an embedded Latin-1 byte chunk without allocating a temporary array. */
    public static void fillConstant(byte[] target, int offset, String chunk) {
        for (int index = 0; index < chunk.length(); index++) {
            target[offset + index] = (byte) chunk.charAt(index);
        }
    }

    public static void clear(byte[] bytes, int offset, int size) {
        java.util.Arrays.fill(bytes, offset, offset + size, (byte) 0);
    }

    public static void write(byte[] bytes, int offset, int size, long value) {
        switch (size) {
            case 8:
                writeInt(bytes, offset, (int) value);
                writeInt(bytes, offset + 4, (int) (value >>> 32));
                return;
            case 4:
                writeInt(bytes, offset, (int) value);
                return;
            case 2:
                bytes[offset] = (byte) value;
                bytes[offset + 1] = (byte) (value >>> 8);
                return;
            case 1:
                bytes[offset] = (byte) value;
                return;
        }
        for (int index = 0; index < size; index++) {
            bytes[offset + index] = (byte) (value >>> (index * 8));
        }
    }

    public static long read(byte[] bytes, int offset, int size) {
        switch (size) {
            case 8:
                return readLong(bytes, offset);
            case 4:
                return readInt(bytes, offset) & 0xffffffffL;
            case 2:
                return (bytes[offset] & 255L) | ((bytes[offset + 1] & 255L) << 8);
            case 1:
                return bytes[offset] & 255L;
        }
        long value = 0;
        for (int index = 0; index < size; index++) {
            value |= ((long) bytes[offset + index] & 0xffL) << (index * 8);
        }
        return value;
    }

    private static int readInt(byte[] bytes, int offset) {
        if (READ_INT != null) {
            try {
                return (int) READ_INT.invokeExact(bytes, offset);
            } catch (RuntimeException | Error error) {
                throw error;
            } catch (Throwable error) {
                throw new IllegalStateException(error);
            }
        }
        return (bytes[offset] & 255) | ((bytes[offset + 1] & 255) << 8)
                | ((bytes[offset + 2] & 255) << 16) | (bytes[offset + 3] << 24);
    }

    private static long readLong(byte[] bytes, int offset) {
        if (READ_LONG != null) {
            try {
                return (long) READ_LONG.invokeExact(bytes, offset);
            } catch (RuntimeException | Error error) {
                throw error;
            } catch (Throwable error) {
                throw new IllegalStateException(error);
            }
        }
        return (readInt(bytes, offset) & 0xffffffffL) | ((long) readInt(bytes, offset + 4) << 32);
    }

    private static void writeInt(byte[] bytes, int offset, int value) {
        bytes[offset] = (byte) value;
        bytes[offset + 1] = (byte) (value >>> 8);
        bytes[offset + 2] = (byte) (value >>> 16);
        bytes[offset + 3] = (byte) (value >>> 24);
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
