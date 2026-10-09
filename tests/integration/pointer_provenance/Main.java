import java.lang.reflect.Field;
import java.util.Map;
import org.rustlang.runtime.Pointer;

public class Main {
    public static void main(String[] args) throws Exception {
        ArrayViews.check();
        CellArrayViews.check();
        SliceAliases.check();
        MemoryViews.check();
        if (pointer_provenance.pointer_provenance.owned_slice_elements() != 26) {
            throw new AssertionError("slice element copies changed their source or each other");
        }
        AtomicViews.check();
        AtomicLocations.check();
        RangeCodecs.check();
        PrimitiveArrayCodecs.check();
        TypedFields.check();
        FieldProjections.check();
        CyclicFieldViews.check();
        AddressOrder.check();
        LocationQueries.check();
        SliceLocations.check();
        OwnedFields.check();
        IoCopies.check();
        MemoryCopies.check();
        MemoryFills.check();
        BorrowedFields.check();
        BorrowedLocals.check();
        HeapStorage.check();
        CodecAdapters.check();
        ConstantBlocks.check();
        MetadataFilters.check();
        StructuralViews.check();
        Field field = Pointer.class.getDeclaredField("EXPOSED_ADDRESSES");
        field.setAccessible(true);
        Map<?, ?> exposed = (Map<?, ?>) field.get(null);
        int before = exposed.size();
        for (int index = 0; index < 1000; index++) {
            if (pointer_provenance.pointer_provenance.address_bits(false) == 0) {
                throw new AssertionError("pointer transmute lost its address bits");
            }
        }
        if (exposed.size() != before) {
            throw new AssertionError("pointer transmute exposed temporary storage");
        }
        pointer_provenance.pointer_provenance.address_bits(true);
        if (exposed.size() != before + 1) {
            throw new AssertionError("pointer cast did not expose provenance");
        }
        // Warm static metadata before checking that repeated formatting retains no arguments.
        pointer_provenance.pointer_provenance.format_many(10);
        before = exposed.size();
        if (pointer_provenance.pointer_provenance.format_many(10000) != 38890) {
            throw new AssertionError("formatting produced incorrect output");
        }
        if (exposed.size() != before) {
            throw new AssertionError("formatting leaked exposed argument arrays");
        }
        System.out.println("Pointer provenance passed");
    }
}
