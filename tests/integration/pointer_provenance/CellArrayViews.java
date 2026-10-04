import org.rustlang.runtime.Pointer;

public final class CellArrayViews {
    private static final String ELEMENT = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;#8";
    private static final String ARRAY = "CellArrayViews$Codec#pairs#[LMemoryViews$Pair;#16";

    public static final class Codec {
        public static int s$pairs() { return 8; }
        public static String c$pairs() { return ELEMENT; }
        public static byte[] e$pairs(MemoryViews.Pair[] values) {
            byte[] bytes = new byte[16];
            Pointer.encodeArrayMemory(values, bytes, 0, 8, ELEMENT);
            return bytes;
        }
        public static MemoryViews.Pair[] d$pairs(byte[] bytes) {
            MemoryViews.Pair[] values = new MemoryViews.Pair[2];
            Pointer.decodeArrayMemory(bytes, 0, values, 8, ELEMENT);
            return values;
        }
    }

    public static final class Holder {
        public MemoryViews.Pair[] pairs;
        public Holder(MemoryViews.Pair[] pairs) { this.pairs = pairs; }
    }

    public static final class HolderCodec {
        public static byte[] e$holder(Holder value) { return Codec.e$pairs(value.pairs); }
        public static Holder d$holder(byte[] bytes) { return new Holder(Codec.d$pairs(bytes)); }
    }

    public static void check() {
        check(false);
        check(true);
        String codec = "CellArrayViews$HolderCodec#holder#LCellArrayViews$Holder;#16";
        Pointer bytes = Pointer.array(new byte[16], 0, 1);
        Holder decoded = (Holder) bytes.retype(16, codec).getObject();
        Pointer field = Pointer.field(decoded, "pairs", 16, ARRAY).retype(8, ELEMENT);
        direct(field, 8).second = 41;
        Pointer.commitStorageLocation(field, 8);
        if (bytes.byte_offset(12).retype(4, null).getI32() != 41)
            throw new AssertionError("field array write did not reach its decoded owner");
    }

    private static void check(boolean field) {
        MemoryViews.Pair[] values = {new MemoryViews.Pair(3, 5), new MemoryViews.Pair(7, 11)};
        Pointer parent = Pointer.cell(new Holder(values), 16, null);
        Pointer whole = field
                ? parent.projectStructField(Holder.class.getName(), "pairs", 0, 16, ARRAY)
                : Pointer.cell(values, 16, ARRAY);
        Pointer elements = whole.retype(8, ELEMENT);
        if (direct(elements, 0) != values[0] || direct(elements, 8) != values[1])
            throw new AssertionError("cell array element decoded instead of borrowing its carrier");
        direct(elements, 8).second = 13;
        Pointer.commitStorageLocation(elements, 8);
        if (whole.byte_offset(12).retype(4, null).getI32() != 13)
            throw new AssertionError("direct element field write was not visible through bytes");
        MemoryViews.Pair[] replacement = {new MemoryViews.Pair(17, 19), new MemoryViews.Pair(23, 29)};
        if (field) parent.set(new Holder(replacement));
        else whole.set(replacement);
        if (direct(elements, 0) != replacement[0] || direct(elements, 8) != replacement[1])
            throw new AssertionError("element lookup retained a replaced whole array");
        Pointer decoded = whole.byte_offset(8).retype(8, ELEMENT);
        MemoryViews.Pair pending = (MemoryViews.Pair) decoded.getObject();
        pending.second = 31;
        if (direct(elements, 8).second != 31)
            throw new AssertionError("element lookup missed pending decoded writes");
        whole.byte_offset(8).retype(4, null).set(37);
        MemoryViews.Pair refreshed = direct(elements, 8);
        if (refreshed.first != 37 || refreshed.second != 31 || direct(elements, 0).first != 17)
            throw new AssertionError("byte alias did not update the selected element");
        if (Pointer.directStorageAggregate(elements, 4, MemoryViews.Pair.class) != null
                || Pointer.directStorageAggregate(whole, 0, MemoryViews.Pair.class) != null
                || Pointer.directStorageAggregate(whole.retype(4, ELEMENT), 0, MemoryViews.Pair.class) != null
                || Pointer.directStorageAggregate(whole.retype(8, null), 0, MemoryViews.Pair.class) != null
                || Pointer.directStorageAggregate(elements, 0, String.class) != null)
            throw new AssertionError("incompatible array layout accepted");
        Pointer shortArray = Pointer.cell(new MemoryViews.Pair[] {new MemoryViews.Pair()}, 16, ARRAY)
                .retype(8, ELEMENT);
        if (Pointer.directStorageAggregate(shortArray, 0, MemoryViews.Pair.class) != null)
            throw new AssertionError("array extent mismatch accepted");
    }

    private static MemoryViews.Pair direct(Pointer root, long offset) {
        return (MemoryViews.Pair) Pointer.directStorageAggregate(root, offset, MemoryViews.Pair.class);
    }
}
