import java.lang.reflect.Field;
import org.rustlang.runtime.Pointer;

/** Address observations must preserve provenance without forcing boundary carriers. */
public final class LocationQueries {
    public static final class Pair { public int first, second; }

    public static void check() throws Exception {
        Object storage = Pointer.storageAligned(17, 4, null, 64);
        long address = Pointer.locationAddr(storage, 0);
        if ((address & 63) != 0 || Pointer.locationAddr(storage, 3) != address + 3
                || Pointer.offsetLocations(storage, 12, storage, 0, 4) != 3
                || Pointer.alignLocation(storage, 1, 1, 16) != 15) {
            throw new AssertionError("storage address query changed its layout");
        }
        Field boundary = storage.getClass().getDeclaredField("boundary");
        boundary.setAccessible(true);
        if (boundary.get(storage) != null) throw new AssertionError("address query materialized a pointer");
        Pointer materialized = Pointer.fromStorageLocation(storage, 0);
        if (materialized.addr() != address || Pointer.byteOffsetLocations(storage, 3, materialized, 0) != 3) {
            throw new AssertionError("materialization changed allocation identity");
        }
        int[] values = new int[16];
        long base = Pointer.locationAddr(values, 0);
        Pointer middle = Pointer.array(values, 4, 4);
        if (base + 16 != middle.addr() || Pointer.offsetLocations(middle, 8, values, 4, 4) != 5
                || Pointer.byteOffsetLocations(values, 4, middle, 8) != -20
                || Pointer.locationAddr(values, Long.MAX_VALUE) != base + Long.MAX_VALUE) {
            throw new AssertionError("array address query changed displacement");
        }
        Pointer pair = Pointer.cell(new Pair(), 8, null);
        Pointer second = pair.projectStructField(Pair.class.getName(), "second", 4, 4, null);
        if (Pointer.locationAddr(second, 3) != pair.addr() + 7
                || Pointer.byteOffsetLocations(second, -4, pair, 0) != 0) {
            throw new AssertionError("projected field lost its parent provenance");
        }
        expect(IllegalArgumentException.class, () -> Pointer.byteOffsetLocations(values, 0, new int[16], 0));
        expect(ArithmeticException.class, () -> Pointer.offsetLocations(values, 3, values, 0, 4));
        expect(ArithmeticException.class, () -> Pointer.offsetLocations(values, 0, values, 0, 0));
        expect(ArithmeticException.class, () -> Pointer.byteOffsetLocationsUnsigned(values, 0, values, 1));
        expect(IllegalArgumentException.class, () -> Pointer.alignLocation(values, 0, 4, 3));
        for (long alignment = 1; alignment <= 256; alignment *= 2) {
            for (long stride = 0; stride < 40; stride++) {
                for (long bits : new long[] {0, 1, 3, 12, 255, -1, Long.MIN_VALUE, Long.MAX_VALUE}) {
                    long expected = -1;
                    for (long n = 0; n < alignment; n++) {
                        if ((bits + n * stride) % alignment == 0) { expected = n; break; }
                    }
                    long result = Pointer.alignLocation(null, bits, stride, alignment);
                    if (result != expected || Pointer.withoutProvenance(bits, stride).align_offset(alignment) != expected) {
                        throw new AssertionError("incorrect modular alignment solution");
                    }
                }
            }
        }
        // The previous linear search cannot feasibly handle this alignment.
        if (Pointer.alignLocation(null, 1, 3, 1L << 62) != 1537228672809129301L
                || Pointer.alignLocation(null, 1, 2, 1L << 62) != -1) {
            throw new AssertionError("large modular alignment solution");
        }
    }

    private static void expect(Class<? extends Throwable> type, Runnable action) {
        try { action.run(); } catch (Throwable error) {
            if (type.isInstance(error)) return;
            throw error;
        }
        throw new AssertionError("missing " + type.getSimpleName());
    }
}
