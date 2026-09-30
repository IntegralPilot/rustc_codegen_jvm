import java.lang.reflect.Field;
import java.util.Map;
import org.rustlang.runtime.Heap;
import org.rustlang.runtime.Pointer;

public final class HeapStorage {
    public static void check() throws Exception {
        if (Heap.allocate(-1, 8) != null || Heap.allocate(16, 3) != null) {
            throw new AssertionError("invalid allocation did not report failure");
        }
        Object root = Heap.allocate(32, 512);
        if (!(root instanceof byte[]) || Pointer.loadLocationBits(root, 0, 8) != 0) {
            throw new AssertionError("allocator did not return zeroed storage directly");
        }
        Pointer alias = Pointer.fromLocation(root, 8, 8);
        if ((alias.addr() - 8) % 512 != 0 || !Pointer.sameLocation(root, 8, alias, 0)) {
            throw new AssertionError("component allocation lost alignment or identity");
        }
        Pointer.storeLocationBits(root, 8, 0x123456789abcdefL, 8);
        if (alias.getI64() != 0x123456789abcdefL) throw new AssertionError("alias detached");
        if (Heap.reallocate(root, 0, 32, 512, -1) != null || alias.getI64() != 0x123456789abcdefL) {
            throw new AssertionError("failed realloc changed the old allocation");
        }
        Object grown = Heap.reallocate(root, 0, 32, 512, 64);
        if (!(grown instanceof byte[]) || Pointer.loadLocationBits(grown, 8, 8) != 0x123456789abcdefL
                || Pointer.loadLocationBits(grown, 56, 8) != 0) {
            throw new AssertionError("growing allocator storage changed its contents");
        }
        Object shrunk = Heap.reallocate(grown, 0, 64, 512, 16);
        if (Pointer.loadLocationBits(shrunk, 8, 8) != 0x123456789abcdefL) {
            throw new AssertionError("shrinking allocator storage changed its prefix");
        }
        long exposed = Pointer.fromLocation(shrunk, 0, 1).expose_provenance();
        Field field = Pointer.class.getDeclaredField("EXPOSED_ADDRESSES");
        field.setAccessible(true);
        Map<?, ?> addresses = (Map<?, ?>) field.get(null);
        if (!addresses.containsKey(exposed)) throw new AssertionError("address was not exposed");
        Heap.deallocate(shrunk, 0);
        if (addresses.containsKey(exposed)) throw new AssertionError("freed provenance was retained");

        // Reallocation must flush decoded writes and retain encoded references.
        String codec = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";
        root = Heap.allocate(8, 8);
        Pointer object = Pointer.fromLocation(root, 0, 1).retype(8, codec);
        object.set(new MemoryViews.Pair(3, 5));
        MemoryViews.Pair view = (MemoryViews.Pair) object.getObject();
        view.second = 19;
        grown = Heap.reallocate(root, 0, 8, 8, 16);
        if (Pointer.loadLocationBits(grown, 4, 4) != 19) throw new AssertionError("realloc missed a live view");
        Heap.deallocate(grown, 0);

        root = Heap.allocate(8, 8);
        Pointer target = Pointer.cell(73L, 8, null);
        Pointer.fromLocation(root, 0, 8).retype(8, "@raw-pointer").set(target);
        grown = Heap.reallocate(root, 0, 8, 8, 16);
        Pointer stored = (Pointer) Pointer.fromLocation(grown, 0, 8).retype(8, "@raw-pointer").getObject();
        if (!stored.samePointer(target) || stored.getI64() != 73) {
            throw new AssertionError("realloc lost an encoded reference");
        }
        Heap.deallocate(grown, 0);
    }
}
