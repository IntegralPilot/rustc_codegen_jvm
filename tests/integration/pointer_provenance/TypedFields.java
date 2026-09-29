import java.lang.reflect.Field;
import org.rustlang.runtime.Pointer;

public final class TypedFields {
    public static final class Nested {
        public MemoryViews.Pair pair;
        public float single;
        public double wide;
        public boolean flag;
        public char small;
        public int[] words;
        public float[] floats;
    }

    public static void check() throws Exception {
        originViews();
        String codec = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";
        Object owner = Pointer.storageAligned(new MemoryViews.Pair(17, 31), 8, codec, 4,
                "0,4,first\n4,4,second");
        Object field = Pointer.storageFieldRoot(owner, 0, MemoryViews.Pair.class.getName(), "second", 4, 4, null);
        if (field != owner || Pointer.storageFieldOffset(field, owner, 4) != 4) {
            throw new AssertionError("typed field allocated an address carrier");
        }
        Pointer.storeLocationBits(field, 4, 73, 4);
        if (((MemoryViews.Pair) Pointer.loadStorageLocation(owner, 0, null)).second != 73) {
            throw new AssertionError("field write missed authoritative storage");
        }
        Pointer.storeStorageLocation(owner, 0, new MemoryViews.Pair(101, 0x12345678));
        Pointer.storeLocationBits(field, 5, 0xab, 1);
        if (Pointer.loadLocationBits(field, 4, 4) != 0x1234ab78L) {
            throw new AssertionError("field retained the replaced parent or lost neighboring bytes");
        }
        Field boundary = owner.getClass().getDeclaredField("boundary");
        boundary.setAccessible(true);
        if (boundary.get(owner) != null) throw new AssertionError("typed scalar access materialized Pointer");
        Pointer raw = Pointer.fromLocation(owner, 4, 4);
        raw.set(113);
        if (Pointer.loadLocationBits(field, 4, 4) != 113) throw new AssertionError("byte boundary split storage identity");
        Pointer.storeLocationBits(field, 4, 127, 4);
        if (raw.getI32() != 127) throw new AssertionError("old alias missed a typed write");

        Nested arrays = new Nested();
        arrays.words = new int[] {0x12345678, 17, 23};
        arrays.floats = new float[] {-0.0f, Float.intBitsToFloat(0x7fc01234)};
        Object arraysOwner = Pointer.storageAligned(arrays, 20, null, 4,
                "0,4,words,3\n12,4,floats,2");
        Pointer.storeLocationBits(arraysOwner, 5, 0xab, 1);
        if (arrays.words[1] != 0xab11 || Pointer.loadLocationBits(arraysOwner, 16, 4) != 0x7fc01234L) {
            throw new AssertionError("inline array layout lost bits or index");
        }
        arrays.words = new int[] {31, 37, 41};
        Pointer.storeLocationBits(arraysOwner, 8, 43, 4);
        Pointer.storeLocationBits(arraysOwner, 12, 0x7fc05678L, 4);
        if (arrays.words[2] != 43 || Float.floatToRawIntBits(arrays.floats[0]) != 0x7fc05678
                || Pointer.loadLocationBits(arraysOwner, 4, 4) != 37
                || boundary.get(arraysOwner) != null) {
            throw new AssertionError("inline array replacement detached or materialized storage");
        }
        Object bareArray = Pointer.storageAligned(new long[] {47, 53}, 16, null, 8, "0,8,,2");
        Pointer.storeLocationBits(bareArray, 8, 59, 8);
        if (Pointer.loadLocationBits(bareArray, 8, 8) != 59 || boundary.get(bareArray) != null) {
            throw new AssertionError("root inline array materialized storage");
        }

        Nested nested = new Nested();
        nested.pair = new MemoryViews.Pair(3, 5);
        nested.single = Float.intBitsToFloat(0x7fc01234);
        nested.wide = -0.0;
        nested.flag = true;
        nested.small = 60000;
        Object aggregate = Pointer.storageAligned(nested, 32, null, 8,
                "0,4,pair/first\n4,4,pair/second\n8,4,single\n16,8,wide\n24,1,flag\n26,2,small");
        if (Pointer.loadLocationBits(aggregate, 8, 4) != 0x7fc01234L
                || Pointer.loadLocationBits(aggregate, 16, 8) != Long.MIN_VALUE
                || Pointer.loadLocationBits(aggregate, 24, 1) != 1
                || Pointer.loadLocationBits(aggregate, 26, 2) != 60000) {
            throw new AssertionError("typed scalar bit conversion changed its value");
        }
        nested.pair = new MemoryViews.Pair(11, 13);
        Pointer.storeLocationBits(aggregate, 4, 19, 4);
        Pointer.storeLocationBits(aggregate, 8, 0x80000000L, 4);
        Pointer.storeLocationBits(aggregate, 16, 0x7ff8000000004567L, 8);
        Pointer.storeLocationBits(aggregate, 24, 0, 1);
        Pointer.storeLocationBits(aggregate, 26, 50000, 2);
        if (nested.pair.second != 19 || Float.floatToRawIntBits(nested.single) != 0x80000000
                || Double.doubleToRawLongBits(nested.wide) != 0x7ff8000000004567L
                || nested.flag || nested.small != 50000 || boundary.get(aggregate) != null) {
            throw new AssertionError("nested scalar store materialized or detached storage");
        }
    }

    private static void originViews() {
        String codec = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";
        Pointer root = Pointer.cellAligned(new MemoryViews.Pair(17, 31), 8, codec, 4);
        Pointer field = root.projectStructField(MemoryViews.Pair.class.getName(), "second", 4, 4, null);
        Pointer bytes = field.retype(1, null);
        long address = field.addr();
        for (int displacement = -4; displacement <= 3; displacement++) {
            Pointer shifted = bytes.byte_offset(displacement);
            Pointer restored = shifted.byte_offset(-displacement).retype(4, null);
            if (shifted.addr() != address + displacement || restored.addr() != address
                    || bytes.addr() != address || field.addr() != root.addr() + 4) {
                throw new AssertionError("deriving an offset view changed another view's origin");
            }
        }
        root.set(new MemoryViews.Pair(37, 41));
        bytes.byte_offset(1).set((byte) 0x55);
        if (field.getI32() != 0x5529 || ((MemoryViews.Pair) root.getObject()).first != 37) {
            throw new AssertionError("derived origin lost replacement or adjacent-field coherence");
        }
        root = null;
        field = null;
        System.gc();
        if (bytes.getI8() != 41 || bytes.byte_offset(1).getI8() != 0x55 || bytes.addr() != address) {
            throw new AssertionError("shared origin did not keep backing storage alive");
        }
    }
}
