package org.rustlang.runtime;

/** Keeps allocator storage separate from address offsets. */
public final class Heap {
    private Heap() {}

    public static Object allocate(long byteCount, long alignment) {
        try {
            int size = Pointer.checkedArrayLength(byteCount);
            int checkedAlignment = Pointer.checkedAlignment(alignment);
            byte[] bytes = new byte[size];
            Pointer.registerHeapAllocation(bytes, checkedAlignment);
            return bytes;
        } catch (IllegalArgumentException | ArithmeticException | OutOfMemoryError failure) {
            return null;
        }
    }

    public static Object reallocate(Object root, long offset, long oldByteCount,
            long alignment, long newByteCount) {
        if (root == null) throw new NullPointerException("Rust realloc requires a non-null pointer");
        byte[] destination = null;
        try {
            int oldSize = Pointer.checkedArrayLength(oldByteCount);
            int newSize = Pointer.checkedArrayLength(newByteCount);
            destination = (byte[]) allocate(newSize, alignment);
            if (destination == null) return null;
            Pointer.copyHeapAllocation(root, offset, destination, Math.min(oldSize, newSize));
            deallocate(root, offset);
            return destination;
        } catch (IllegalArgumentException | ArithmeticException | OutOfMemoryError failure) {
            if (destination != null) Pointer.releaseHeapAllocation(destination);
            return null;
        }
    }

    public static void deallocate(Object root, long offset) {
        Pointer.releaseHeapAllocation(root);
    }
}
