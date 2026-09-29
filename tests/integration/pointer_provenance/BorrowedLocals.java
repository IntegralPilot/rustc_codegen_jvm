import java.lang.reflect.Field;
import org.rustlang.runtime.Pointer;
import org.rustlang.runtime.SliceView;

/** Decomposed local borrows become ordinary coherent cells at byte boundaries. */
public final class BorrowedLocals {
    private static final String ADDRESS = "@raw-pointer\n4\n\n";
    private static final String VIEW = "@slice-pointer\norg/rustlang/runtime/SliceView\n1\n";

    private static void unboxed(Object storage) throws Exception {
        Field boundary = storage.getClass().getSuperclass().getDeclaredField("boundary");
        boundary.setAccessible(true);
        if (boundary.get(storage) != null) throw new AssertionError("borrow allocated a boundary");
    }

    public static void check() throws Exception {
        int[] words = {11, 13, 17};
        long[] metadata = new long[2];
        Object slot = Pointer.borrowedStorageAligned(null, 8, ADDRESS, 8, 0);
        if (Pointer.loadBorrowedAddress(slot, 0, metadata) != null || metadata[0] != 0)
            throw new AssertionError("initial null borrow changed");
        for (int i = 0; i < words.length; i++) {
            Pointer.storeBorrowedAddress(slot, 0, words, i * 4, 4);
            if (Pointer.loadBorrowedAddress(slot, 0, metadata) != words || metadata[0] != i * 4)
                throw new AssertionError("local address components changed");
        }
        unboxed(slot);
        Pointer.storeBorrowedAddress(slot, 0, words, 4, 4);
        Pointer escaped = Pointer.addressFromParts(slot, 0, 128);
        Pointer value = (Pointer) escaped.getObject();
        value.set(29);
        if (words[1] != 29) throw new AssertionError("stored borrow lost its pointee");
        Pointer.storeBorrowedAddress(slot, 0, words, 8, 4);
        if (((Pointer) escaped.getObject()).getI32() != 17 || value.getI32() != 29)
            throw new AssertionError("replacing a borrow changed its prior value");
        escaped.set(Pointer.array(words, 0, 4));
        Object root = Pointer.loadBorrowedAddress(slot, 0, metadata);
        if (Pointer.loadLocationBits(root, metadata[0], 4) != 11)
            throw new AssertionError("opaque store was not authoritative");
        Pointer.storeBorrowedAddress(slot, 0, null, 0, 4);
        if (escaped.getObject() != null) throw new AssertionError("escaped null store changed");

        byte[] bytes = {3, 5, 7, 11};
        Object viewSlot = Pointer.borrowedStorageAligned(null, 16, VIEW, 8, 1);
        Pointer.storeBorrowedView(viewSlot, 0, bytes, 1, 3);
        if (Pointer.loadBorrowedView(viewSlot, 0, metadata) != bytes || metadata[0] != 1 || metadata[1] != 3)
            throw new AssertionError("local slice components changed");
        unboxed(viewSlot);
        Pointer raw = Pointer.fromStorageLocation(viewSlot, 0);
        SliceView saved = (SliceView) raw.getObject();
        raw.byte_offset(8).retype(8, null).set(2L);
        if (((SliceView) raw.getObject()).rustLength != 2)
            throw new AssertionError("byte store did not update materialized metadata");
        Pointer.storeBorrowedView(viewSlot, 0, bytes, 2, 1);
        if (Pointer.loadBorrowedView(viewSlot, 0, metadata) != bytes || metadata[0] != 2 || metadata[1] != 1)
            throw new AssertionError("view components ignored escaped storage");
        if (saved.offset != 1) throw new AssertionError("borrow replacement mutated prior view");

        // Byte access must use the codec even before a Pointer escapes.
        Object addressBytes = Pointer.borrowedStorageAligned(null, 8, ADDRESS, 8, 0);
        Pointer.storeBorrowedAddress(addressBytes, 0, words, 8, 4);
        long address = Pointer.loadLocationBits(addressBytes, 0, 8);
        if (Pointer.fromEncodedAddress(address, 4, null, ADDRESS).getI32() != 17)
            throw new AssertionError("deferred carrier lost exposed provenance");

        Object self = Pointer.borrowedStorageAligned(null, 8, ADDRESS, 8, 0);
        Pointer.storeBorrowedAddress(self, 0, self, 0, 0);
        Pointer selfBoundary = Pointer.fromStorageLocation(self, 0);
        if (!Pointer.sameLocation(selfBoundary.getObject(), 0, self, 0))
            throw new AssertionError("self-referential borrow lost its identity");
        Object other = Pointer.borrowedStorageAligned(null, 8, ADDRESS, 8, 0);
        Object cycle = Pointer.borrowedStorageAligned(null, 8, ADDRESS, 8, 0);
        Pointer.storeBorrowedAddress(other, 0, cycle, 0, 0);
        Pointer.storeBorrowedAddress(cycle, 0, other, 0, 0);
        Pointer left = Pointer.fromStorageLocation(other, 0);
        Pointer right = Pointer.fromStorageLocation(cycle, 0);
        if (left.getObject() != right || right.getObject() != left)
            throw new AssertionError("mutually referential storage lost its identity");
    }
}
