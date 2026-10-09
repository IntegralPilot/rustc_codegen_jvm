package org.rustlang.runtime;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/** Lazy binary constants. Payload sharing never shares the destination allocation. */
public final class ConstantData {
    private ConstantData() {}

    private static final int MAX_BYTES = 4 * 1024 * 1024;
    private static final int MAX_ENTRIES = 256;
    // Entries retain only the per-owner map, never its Class or ClassLoader.
    private static final ClassValue<Map<String, Block>> OWNERS = new ClassValue<Map<String, Block>>() {
        protected Map<String, Block> computeValue(Class<?> owner) { return new HashMap<>(); }
    };
    private static final Map<Block, Boolean> BLOCKS = new LinkedHashMap<Block, Boolean>(16, 0.75f, true);
    private static int retainedBytes;

    private static final class Block {
        final Map<String, Block> owner;
        final String name;
        final ByteBuffer data;
        Block(Map<String, Block> owner, String name, byte[] bytes) {
            this.owner = owner;
            this.name = name;
            data = ByteBuffer.wrap(bytes).asReadOnlyBuffer();
        }
    }

    private static ByteBuffer block(String name, Class<?> owner, int size) {
        if (size < 0 || size > 65536 || !name.startsWith("META-INF/rust-data/")
                || name.indexOf("..") >= 0) throw new IllegalArgumentException("constant block");
        Map<String, Block> owned = OWNERS.get(owner);
        synchronized (BLOCKS) {
            Block cached = owned.get(name);
            if (cached != null) {
                if (cached.data.capacity() != size) throw new IllegalStateException("constant block size changed");
                BLOCKS.get(cached);
                return cached.data;
            }
        }
        // Resource I/O does not serialize unrelated constructor threads.
        Block loaded = new Block(owned, name, readBlock(name, owner, size));
        synchronized (BLOCKS) {
            Block existing = owned.get(name);
            if (existing != null) return existing.data;
            owned.put(name, loaded);
            BLOCKS.put(loaded, Boolean.TRUE);
            retainedBytes += size;
            while (retainedBytes > MAX_BYTES || BLOCKS.size() > MAX_ENTRIES) {
                java.util.Iterator<Block> oldest = BLOCKS.keySet().iterator();
                Block evicted = oldest.next();
                retainedBytes -= evicted.data.capacity();
                evicted.owner.remove(evicted.name);
                oldest.remove();
            }
        }
        return loaded.data;
    }

    /** Independent cursor over immutable, bounded, loader-local constant data. */
    public static ByteBuffer buffer(String name, Class<?> owner, int size) {
        return block(name, owner, size).duplicate().order(ByteOrder.LITTLE_ENDIAN);
    }

    private static byte[] readBlock(String name, Class<?> owner, int size) {
        byte[] bytes = new byte[size];
        try (InputStream input = owner.getResourceAsStream("/" + name)) {
            if (input == null) throw new IOException("missing Rust constant " + name);
            for (int i = 0; i < size;) {
                int read = input.read(bytes, i, size - i);
                if (read < 0) throw new EOFException("truncated Rust constant");
                if (read == 0) {
                    int value = input.read();
                    if (value < 0) throw new EOFException("truncated Rust constant");
                    bytes[i++] = (byte) value;
                } else i += read;
            }
            if (input.read() != -1) throw new IOException("oversized Rust constant");
        } catch (IOException error) {
            throw new IllegalStateException("could not load Rust constant " + name, error);
        }
        return bytes;
    }

    public static void readArray(Object target, ByteBuffer input) {
        readArray(target, 0, java.lang.reflect.Array.getLength(target), input);
    }

    public static void fill(Object target, int start, int count, String name, Class<?> owner) {
        int width;
        if (target instanceof byte[] || target instanceof boolean[]) width = 1;
        else if (target instanceof short[] || target instanceof char[]) width = 2;
        else if (target instanceof int[] || target instanceof float[]) width = 4;
        else if (target instanceof long[] || target instanceof double[]) width = 8;
        else throw new IllegalArgumentException("not a primitive constant array");
        check(start, count, java.lang.reflect.Array.getLength(target));
        ByteBuffer input = buffer(name, owner, Math.multiplyExact(count, width));
        readArray(target, start, count, input);
    }

    private static void readArray(Object target, int start, int count, ByteBuffer input) {
        int end = start + count;
        if (target instanceof byte[]) input.get((byte[]) target, start, count);
        else if (target instanceof short[]) {
            short[] a = (short[]) target; for (int i = start; i < end; i++) a[i] = input.getShort();
        } else if (target instanceof char[]) {
            char[] a = (char[]) target; for (int i = start; i < end; i++) a[i] = input.getChar();
        } else if (target instanceof int[]) {
            int[] a = (int[]) target; for (int i = start; i < end; i++) a[i] = input.getInt();
        } else if (target instanceof long[]) {
            long[] a = (long[]) target; for (int i = start; i < end; i++) a[i] = input.getLong();
        } else if (target instanceof float[]) {
            float[] a = (float[]) target; for (int i = start; i < end; i++) a[i] = input.getFloat();
        } else if (target instanceof double[]) {
            double[] a = (double[]) target; for (int i = start; i < end; i++) a[i] = input.getDouble();
        } else if (target instanceof boolean[]) {
            boolean[] a = (boolean[]) target; for (int i = start; i < end; i++) a[i] = input.get() != 0;
        } else throw new IllegalArgumentException("not a primitive constant array");
    }

    private static void check(int start, int count, int length) {
        if (start < 0 || count < 0 || start > length - count) {
            throw new IndexOutOfBoundsException("constant array range");
        }
    }
}
