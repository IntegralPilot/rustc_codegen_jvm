import org.rustlang.runtime.Pointer;

/** Location comparisons preserve unsigned ordering without borrowing wrappers. */
public final class AddressOrder {
    public static final class Empty {}
    public static final class Pair { public int first, second; }

    private static void compare(Pointer left, long x, Pointer right, long y) {
        long a = Pointer.addr(left) + x, b = Pointer.addr(right) + y;
        int expected = Long.compareUnsigned(a, b);
        if (Pointer.compareLocations(left, x, right, y) != expected) {
            throw new AssertionError("location ordering lost its address or origin");
        }
    }

    public static void check() {
        byte[] bytes = new byte[32];
        Pointer start = Pointer.array(bytes, 0, 1), end = start.add(16);
        for (long x : new long[] {Long.MIN_VALUE, -32, -1, 0, 16, Long.MAX_VALUE}) {
            for (long y : new long[] {Long.MIN_VALUE, -1, 0, 1, Long.MAX_VALUE}) {
                compare(start, x, end, y);
            }
            encoded(Pointer.wrapping_byte_offset(start, x));
        }
        if (Pointer.compareLocations(bytes, 7, bytes, 17) != -1
                || Pointer.compareLocations(null, -1, null, 0) != 1) {
            throw new AssertionError("array or unprovenanced ordering");
        }
        Object storage = Pointer.storageAligned(17, 4, null, 4);
        if (Pointer.compareLocations(storage, 0, storage, 4) != -1) {
            throw new AssertionError("typed storage ordering");
        }
        // A typed ZST can have a dangling address. Positive offsets can wrap across unsigned zero.
        Pointer zst = Pointer.withoutProvenance(-2L, 1)
                .retype(0, "@zero-sized:" + Empty.class.getName());
        Pointer before = Pointer.wrapping_byte_offset(zst.retype(1), 1);
        Pointer after = Pointer.wrapping_byte_offset(zst.retype(1), 3);
        compare(before, 0, after, 0);
        if (before.compareAddress(after) <= 0 || before.addr() != -1L || after.addr() != 1L) {
            throw new AssertionError("dangling ZST address wrap");
        }
        encoded(before);
        encoded(after);
        Pointer parent = Pointer.cell(new Pair(), 8, null);
        Pointer field = parent.projectStructField(Pair.class.getName(), "second", 4, 4, null);
        encoded(Pointer.wrapping_byte_offset(field, Long.MAX_VALUE));
        // Also cover the first publication, before any base-address cache hit.
        encoded(Pointer.wrapping_byte_offset(Pointer.array(new byte[1], 0, 1), Long.MAX_VALUE));
    }

    private static void encoded(Pointer pointer) {
        if (Pointer.encodedAddress(pointer, new byte[8]) != pointer.addr()) {
            throw new AssertionError("encoding changed a wrapping address");
        }
    }
}
