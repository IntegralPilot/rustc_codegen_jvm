import org.rustlang.runtime.Pointer;
import org.rustlang.runtime.MemoryBytes;

/** Scalar and aggregate slice reads retain aliases without temporary addresses. */
public final class SliceLocations {
    public static void check() throws Exception {
        trackedArrayCarriers();
        String[] values = {"zero", "one", "two"};
        Pointer objects = Pointer.array(values, 1, 8);
        if (Pointer.sliceGetObject(objects, 1) != values[2]) throw new AssertionError("object slice displacement");
        int[] ints = {17, 23, 29};
        Pointer words = Pointer.array(ints, 1, 4);
        Pointer.sliceSetI32(words, 1, 37);
        if (Pointer.sliceGetI32(words, -1) != 17 || ints[2] != 37) throw new AssertionError("scalar slice displacement");
        Pointer local = Pointer.withMetadata(Pointer.cell(43, 4, null), 19);
        if (Pointer.scalarSliceRoot(local, 4) != local || local.metadata() != 19) {
            throw new AssertionError("normalizing scalar storage changed its metadata");
        }
        Object root = Pointer.locationSliceBacking(local, 0, 4);
        int start = Pointer.locationSliceOffset(local, 0, 4);
        Pointer.sliceSetI32(root, start, 47);
        if (local.getI32() != 47) throw new AssertionError("scalar slice lost write-through storage");
        String codec = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";
        byte[] bytes = new byte[16];
        Pointer pairs = Pointer.array(bytes, 0, 1).retype(8, codec);
        MemoryViews.Pair first = (MemoryViews.Pair) Pointer.sliceGetObject(pairs, 0);
        MemoryViews.Pair second = (MemoryViews.Pair) Pointer.sliceGetObject(pairs, 1);
        first.first = 53;
        second.second = 59;
        for (int i = 0; i < 10; i++) {
            if (Pointer.sliceGetObject(pairs, 0) != first || Pointer.sliceGetObject(pairs, 1) != second) {
                throw new AssertionError("decoded slice identity changed");
            }
        }
        if (Pointer.loadLocationBits(bytes, 0, 4) != 53 || Pointer.loadLocationBits(bytes, 12, 4) != 59) {
            throw new AssertionError("decoded slice lost its memory origin");
        }
        Pointer.storeLocationBits(bytes, 8, 61, 4);
        MemoryViews.Pair replaced = (MemoryViews.Pair) Pointer.sliceGetObject(pairs, 1);
        if (replaced == second || replaced.first != 61 || replaced.second != 59) {
            throw new AssertionError("decoded slice reused an invalidated view");
        }
        Pointer scalar = pairs.retype(4, null);
        Pointer.sliceSetI32(scalar, 3, 67);
        if (Pointer.sliceGetI32(scalar, 2) != 61 || MemoryBytes.read(bytes, 12, 4) != 67) {
            throw new AssertionError("scalar slice bypassed aggregate invalidation");
        }
        int[][] rows = {new int[] {71, 73}, new int[] {79, 83}};
        Pointer row = Pointer.array(rows, 0, 8, "@array:i32:2");
        // Nested array reads must still register the original allocation.
        Object inner = Pointer.sliceGetObject(row, 1);
        if (inner != rows[1] || Pointer.array(inner, 0, 4).addr() != row.addr() + 8) {
            throw new AssertionError("nested slice array lost origin");
        }
    }

    private static void trackedArrayCarriers() throws Exception {
        for (String filterName : new String[] {
                "MEMORY_VIEW_FILTER", "ENCODED_POINTER_FILTER", "ENCODED_REFERENCE_FILTER"}) {
            java.lang.reflect.Field field = Pointer.class.getDeclaredField(filterName);
            field.setAccessible(true);
            Object filter = field.get(null);
            java.lang.reflect.Method mark = Pointer.class.getDeclaredMethod(
                    "markIdentityFilter", filter.getClass(), Object.class);
            mark.setAccessible(true);
            for (Object sample : new Object[] {new byte[4], new short[4], new int[4], new long[128]}) {
                int width = sample instanceof byte[] ? 1 : sample instanceof short[] ? 2
                        : sample instanceof int[] ? 4 : 8;
                int size = java.lang.reflect.Array.getLength(sample) * width;
                String codec = "org/rustlang/runtime/ArrayMemoryCodec#array#"
                        + sample.getClass().getName() + "#" + size;
                byte[] bytes = new byte[size];
                Pointer owner = Pointer.fromTypedStorageLocation(bytes, 0, size, codec);
                Object view = owner.getObject();
                // False filter matches must not change which array receives the write.
                mark.invoke(null, filter, view);
                Pointer.storeLocationBits(view, width, 17, width);
                if (((Number) java.lang.reflect.Array.get(view, 1)).longValue() != 17
                        || Pointer.loadLocationBits(view, width, width) != 17
                        || Pointer.loadLocationBits(bytes, width, width) != 17) {
                    throw new AssertionError("tracked scalar array detached from its carrier: " + filterName);
                }
                if (view instanceof long[]) {
                    view = owner.getObject();
                    mark.invoke(null, filter, view);
                    Pointer.sliceSetI64(view, 11, 23);
                    if (((long[]) view)[11] != 23 || Pointer.sliceGetI64(view, 11) != 23
                            || Pointer.loadLocationBits(bytes, 88, 8) != 23)
                        throw new AssertionError("tracked slice array detached from its carrier");
                }
                Pointer.storeLocationBits(bytes, width, 29, width);
                Object refreshed = owner.getObject();
                if (refreshed == view || Pointer.loadLocationBits(refreshed, width, width) != 29)
                    throw new AssertionError("raw byte alias did not invalidate the decoded array");
            }
        }
    }
}
