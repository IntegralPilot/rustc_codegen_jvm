package org.rustlang.runtime;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.ConcurrentHashMap;

/** Rust byte offsets mapped to authoritative JVM scalar fields. */
final class StorageLayout {
    private static final ClassValue<ConcurrentHashMap<String, StorageLayout>> PLANS =
            new ClassValue<ConcurrentHashMap<String, StorageLayout>>() {
                protected ConcurrentHashMap<String, StorageLayout> computeValue(Class<?> type) {
                    return new ConcurrentHashMap<>();
                }
            };

    static StorageLayout of(Class<?> type, String descriptor) {
        return PLANS.get(type).computeIfAbsent(descriptor, key -> new StorageLayout(type, key));
    }

    private final Leaf[] leaves;
    private final BorrowedFieldPath[] borrows;

    private StorageLayout(Class<?> type, String descriptor) {
        String[] entries = descriptor.split("\n", -1);
        ArrayList<Leaf> scalars = new ArrayList<>();
        ArrayList<BorrowedFieldPath> borrowed = new ArrayList<>();
        try {
            for (int i = 0; i < entries.length; i++) {
                String[] parts = entries[i].split(",", -1);
                if (parts.length == 4 && (parts[3].startsWith("p:") || parts[3].startsWith("v:"))) {
                    int lines = Integer.parseInt(parts[3].substring(2));
                    if (lines < 1 || lines > entries.length - i - 1)
                        throw new IllegalArgumentException("invalid borrowed storage codec");
                    String codec = String.join("\n", Arrays.copyOfRange(entries, i + 1, i + 1 + lines));
                    borrowed.add(new BorrowedFieldPath(type, Integer.parseInt(parts[0]),
                            Integer.parseInt(parts[1]), parts[2], parts[3].charAt(0) == 'v', codec));
                    i += lines;
                    continue;
                }
                scalars.add(new Leaf(type, Integer.parseInt(parts[0]),
                        Integer.parseInt(parts[1]), parts[2],
                        parts.length == 4 ? Integer.parseInt(parts[3]) : 0));
            }
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("invalid typed Rust storage layout", error);
        }
        leaves = scalars.toArray(new Leaf[0]);
        borrows = borrowed.toArray(new BorrowedFieldPath[0]);
        Arrays.sort(leaves, Comparator.comparingInt(leaf -> leaf.offset));
        Arrays.sort(borrows, Comparator.comparingInt(leaf -> leaf.offset));
    }

    BorrowedFieldPath borrowedAt(long offset, boolean view) {
        int low = 0, high = borrows.length;
        while (low < high) {
            int middle = (low + high) >>> 1;
            BorrowedFieldPath path = borrows[middle];
            if (path.offset < offset) low = middle + 1;
            else if (path.offset > offset) high = middle;
            else return path.view == view && path.field().borrowedShape(view) ? path : null;
        }
        return null;
    }

    Leaf at(long offset, int size) {
        if (offset < 0 || size <= 0 || size > 8) return null;
        int low = 0, high = leaves.length;
        while (low < high) {
            int middle = (low + high) >>> 1;
            if (leaves[middle].offset <= offset) low = middle + 1;
            else high = middle;
        }
        if (low == 0) return null;
        Leaf leaf = leaves[low - 1];
        long relative = offset - leaf.offset;
        long extent = (long) leaf.size * (leaf.count == 0 ? 1 : leaf.count);
        return relative < extent && relative % leaf.size <= leaf.size - size ? leaf : null;
    }

    static final class Leaf {
        final int offset, size, count;
        private final MethodHandle read, write;

        Leaf(Class<?> type, int offset, int size, String path, int count) throws ReflectiveOperationException {
            this.offset = offset;
            this.size = size;
            this.count = count;
            String[] names = path.isEmpty() ? new String[0] : path.split("/");
            MethodHandle owner = MethodHandles.identity(Object.class);
            int ownerPath = count == 0 ? names.length - 1 : names.length;
            for (int i = 0; i < ownerPath; i++) {
                RustField field = RustField.find(type, names[i]);
                owner = MethodHandles.filterReturnValue(owner,
                        field.getter().asType(MethodType.methodType(Object.class, Object.class)));
                type = field.getType();
            }
            MethodHandle getter, setter;
            Class<?> scalar;
            if (count == 0) {
                RustField field = RustField.find(type, names[names.length - 1]);
                getter = field.getter();
                setter = field.setter();
                scalar = field.getType();
            } else {
                getter = MethodHandles.arrayElementGetter(type);
                setter = MethodHandles.arrayElementSetter(type);
                scalar = type.getComponentType();
            }
            if (scalar == float.class || scalar == double.class) {
                boolean single = scalar == float.class;
                Class<?> wrapper = single ? Float.class : Double.class;
                Class<?> bits = single ? int.class : long.class;
                MethodHandle encode = MethodHandles.lookup().findStatic(wrapper,
                        single ? "floatToRawIntBits" : "doubleToRawLongBits",
                        MethodType.methodType(bits, scalar));
                MethodHandle decode = MethodHandles.lookup().findStatic(wrapper,
                        single ? "intBitsToFloat" : "longBitsToDouble",
                        MethodType.methodType(scalar, bits));
                getter = MethodHandles.filterReturnValue(getter, encode);
                setter = MethodHandles.filterArguments(setter, count == 0 ? 1 : 2, decode);
            }
            read = MethodHandles.filterArguments(MethodHandles.explicitCastArguments(getter,
                    count == 0 ? MethodType.methodType(long.class, Object.class)
                            : MethodType.methodType(long.class, Object.class, int.class)), 0, owner);
            write = MethodHandles.filterArguments(MethodHandles.explicitCastArguments(setter,
                    count == 0 ? MethodType.methodType(void.class, Object.class, long.class)
                            : MethodType.methodType(void.class, Object.class, int.class, long.class)), 0, owner);
        }

        long read(Object root, long position, int count) {
            try {
                return (bits(root, position) >>> (((position - offset) % size) * 8)) & mask(count);
            } catch (RuntimeException | Error error) {
                throw error;
            } catch (Throwable error) {
                throw new IllegalStateException("could not read typed Rust field", error);
            }
        }

        void write(Object root, long position, long bits, int count) {
            try {
                int within = (int) ((position - offset) % size);
                if (within != 0 || count != size) {
                    int shift = within * 8;
                    long mask = mask(count) << shift;
                    bits = (bits(root, position) & ~mask) | ((bits << shift) & mask);
                }
                if (this.count == 0) write.invokeExact(root, bits);
                else write.invokeExact(root, Math.toIntExact((position - offset) / size), bits);
            } catch (RuntimeException | Error error) {
                throw error;
            } catch (Throwable error) {
                throw new IllegalStateException("could not write typed Rust field", error);
            }
        }

        private long bits(Object root, long position) throws Throwable {
            return count == 0 ? (long) read.invokeExact(root)
                    : (long) read.invokeExact(root, Math.toIntExact((position - offset) / size));
        }

        private static long mask(int size) {
            return size == 8 ? -1L : (1L << (size * 8)) - 1;
        }
    }
}
