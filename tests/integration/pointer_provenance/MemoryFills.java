import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Map;
import org.rustlang.runtime.Pointer;

public final class MemoryFills {
    private static final String PAIR = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;#8";

    public static void check() throws Exception {
        for (Object array : new Object[] {new byte[5], new boolean[5], new short[5], new char[5],
                new int[5], new long[5], new float[5], new double[5]}) {
            int width = array instanceof byte[] || array instanceof boolean[] ? 1
                    : array instanceof short[] || array instanceof char[] ? 2
                    : array instanceof int[] || array instanceof float[] ? 4 : 8;
            int fill = array instanceof boolean[] ? 1 : 0xa5;
            for (boolean carrier : new boolean[] {false, true}) {
                for (int count : new int[] {1, width + 2, 3 * width}) {
                    byte[] initial = new byte[5 * width];
                    Arrays.fill(initial, (byte) (array instanceof boolean[] ? 0 : 0x33));
                    Pointer.decodeArrayMemory(initial, 0, array, width, null);
                    byte[] expected = initial.clone();
                    Arrays.fill(expected, width - 1, width - 1 + count, (byte) fill);
                    if (carrier) Pointer.writeBytes(Pointer.array(array, 1, width).byte_offset(-1), fill, count);
                    else Pointer.writeBytes(array, width - 1, fill, count);
                    byte[] actual = new byte[initial.length];
                    Pointer.encodeArrayMemory(array, actual, 0, width, null);
                    if (!Arrays.equals(actual, expected)) throw new AssertionError("unaligned fill: " + array.getClass());
                }
            }
        }
        MemoryViews.Pair value = new MemoryViews.Pair(0x11223344, 0x55667788);
        Object storage = Pointer.storageAligned(value, 8, PAIR, 4, "0,4,first\n4,4,second");
        Pointer.writeBytes(storage, 2, 0xa5, 4);
        if (value.first != 0xa5a53344 || value.second != 0x5566a5a5) throw new AssertionError("scalar leaf fill");
        Field boundary = storage.getClass().getDeclaredField("boundary");
        boundary.setAccessible(true);
        if (boundary.get(storage) != null) throw new AssertionError("scalar fill materialized a boundary");
        Object scalar = Pointer.storage(Integer.valueOf(0x11223344), 4, null);
        Pointer.writeBytes(scalar, 1, 0xff, 2);
        if (!Pointer.loadStorageLocation(scalar, 0, null).equals(0x11ffff44) || boundary.get(scalar) != null)
            throw new AssertionError("scalar cell fill materialized or changed neighboring bytes");
        Pointer.writeBytes(storage, Long.MAX_VALUE, 1, 0);
        Pointer.writeBytes(null, Long.MAX_VALUE, 1, 0);
        Pointer.writeBytes((Pointer) null, 1, 0L);
        Pointer.writeElements(Pointer.withoutProvenance(-1L, 0), 1, Long.MAX_VALUE);
        for (long count : new long[] {-1, (long) Integer.MAX_VALUE + 1}) {
            try { Pointer.writeBytes(storage, 0, 0, count); throw new AssertionError("invalid fill count"); }
            catch (IllegalArgumentException expected) { }
        }
        if (boundary.get(storage) != null) throw new AssertionError("empty or invalid fill materialized storage");

        Pointer aggregate = Pointer.cell(new MemoryViews.Pair(0x11223344, 0x55667788), 8, PAIR);
        Pointer byteView = aggregate.retype(1, "org/rustlang/runtime/ArrayMemoryCodec#array#[B#1");
        Pointer.writeBytes(byteView, 1L, 0xa5, 4L);
        MemoryViews.Pair filled = (MemoryViews.Pair) aggregate.getObject();
        if (filled.first != 0xa5a5a544 || filled.second != 0x556677a5)
            throw new AssertionError("fill used the transient view codec instead of the allocation codec");

        byte[] bytes = new byte[16];
        Pointer owner = Pointer.fromTypedStorageLocation(bytes, 4, 8, PAIR);
        MemoryViews.Pair view = (MemoryViews.Pair) owner.getObject();
        view.first = 17; view.second = 19;
        Pointer.writeBytes(bytes, 4, 0x11, 4);
        view = (MemoryViews.Pair) owner.getObject();
        if (view.first != 0x11111111 || view.second != 19 || bytes[3] != 0 || bytes[12] != 0)
            throw new AssertionError("fill lost decoded writes or neighboring bytes");
        byte[] before = bytes.clone();
        try { Pointer.writeBytes(bytes, 15, 0, 2); throw new AssertionError("invalid fill range"); }
        catch (IndexOutOfBoundsException expected) { }
        if (!Arrays.equals(bytes, before)) throw new AssertionError("invalid fill modified storage");
        encodedReferences();
    }

    private static void encodedReferences() throws Exception {
        byte[] bytes = new byte[16];
        String codec = "@raw-pointer\n4\n\n";
        int[] values = {17, 19};
        Pointer.fromTypedStorageLocation(bytes, 0, 8, codec).set(Pointer.array(values, 0, 4));
        Pointer.fromTypedStorageLocation(bytes, 8, 8, codec).set(Pointer.array(values, 1, 4));
        Pointer.writeBytes(bytes, 0, 0, 8);
        if (!Pointer.is_null((Pointer) Pointer.fromTypedStorageLocation(bytes, 0, 8, codec).getObject()))
            throw new AssertionError("fill retained overwritten pointer metadata");
        ((Pointer) Pointer.fromTypedStorageLocation(bytes, 8, 8, codec).getObject()).set(23);
        if (values[1] != 23) throw new AssertionError("partial fill erased neighboring provenance");
        Pointer.writeBytes(bytes, 0, 0, 16);
        Field references = Pointer.class.getDeclaredField("ENCODED_REFERENCES");
        references.setAccessible(true);
        Method index = Pointer.class.getDeclaredMethod("stateStripeIndex", Object.class);
        index.setAccessible(true);
        Map<?, ?> stripe = ((Map<?, ?>[]) references.get(null))[(int) index.invoke(null, (Object) bytes)];
        if (stripe.get(bytes) != null) throw new AssertionError("complete fill retained overwritten GC dependencies");
    }
}
