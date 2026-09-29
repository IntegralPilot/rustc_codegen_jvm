import java.util.Arrays;
import org.rustlang.runtime.MemoryBytes;
import org.rustlang.runtime.Pointer;

public final class MemoryViews {
    public static final class Pair {
        public int first;
        public int second;

        public Pair() { }

        public Pair(int first, int second) {
            this.first = first;
            this.second = second;
        }
    }

    public static final class PairCodec {
        static int encodes;
        static int rangeEncodes;
        static byte[] decodedStorage;

        public static byte[] e$pair(Pair value) {
            encodes++;
            byte[] bytes = new byte[8];
            MemoryBytes.write(bytes, 0, 4, value.first);
            MemoryBytes.write(bytes, 4, 4, value.second);
            return bytes;
        }

        public static Pair d$pair(byte[] bytes) {
            return a$pair(bytes, 0);
        }

        public static Pair a$pair(byte[] bytes, int offset) {
            decodedStorage = bytes;
            Pair value = new Pair();
            value.first = (int) MemoryBytes.read(bytes, offset, 4);
            value.second = (int) MemoryBytes.read(bytes, offset + 4, 4);
            return value;
        }

        public static void w$pair(Pair value, byte[] bytes, int offset) {
            rangeEncodes++;
            MemoryBytes.write(bytes, offset, 4, value.first);
            MemoryBytes.write(bytes, offset + 4, 4, value.second);
        }
    }

    private static final String PAIR_CODEC =
            "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";

    public static void main(String[] args) {
        check();
    }

    private static Pair view(Pointer bytes) {
        return (Pair) bytes.retype(8, PAIR_CODEC).getObject();
    }

    public static void check() {
        byte[] storage = new byte[16];
        Pointer bytes = Pointer.array(storage, 0, 1);
        Pair original = view(bytes);
        original.first = 17;
        original.second = 23;
        int before = PairCodec.encodes;
        bytes.retype(8, null).set(0x0000003300000022L);
        if (PairCodec.encodes != before) {
            throw new AssertionError("complete overwrite serialized the discarded view");
        }
        Pair replaced = view(bytes);
        if (replaced.first != 0x22 || replaced.second != 0x33) {
            throw new AssertionError("complete overwrite left a stale view");
        }

        replaced.first = 41;
        replaced.second = 43;
        bytes.retype(4, null).set(47);
        Pair partial = view(bytes);
        if (partial.first != 47 || partial.second != 43) {
            throw new AssertionError("partial overwrite lost the untouched field");
        }

        partial.first = 51;
        partial.second = 53;
        Pair following = view(bytes.byte_offset(8));
        following.first = 57;
        following.second = 59;
        bytes.byte_offset(4).retype(8, null).set(0x0000006500000063L);
        Pair left = view(bytes);
        Pair right = view(bytes.byte_offset(8));
        if (left.first != 51 || left.second != 99 || right.first != 101 || right.second != 59) {
            throw new AssertionError("overlapping write lost adjacent live fields");
        }

        left.first = 71;
        Pointer copiedPointer = bytes.retype(8, PAIR_CODEC);
        Pair snapshot = (Pair) copiedPointer.getObjectCopyAs(Pair.class.getName());
        if (PairCodec.decodedStorage != storage) {
            throw new AssertionError("plain aggregate read copied its source buffer");
        }
        if (snapshot.first != 71 || snapshot.second != 99) {
            throw new AssertionError("value copy ignored pending mutations");
        }
        snapshot.first = 73;
        if (view(bytes).first != 71) {
            throw new AssertionError("value copy retained a live alias");
        }
        Pair offsetCopy = (Pair) bytes.byte_offset(8).retype(8, PAIR_CODEC)
                .getObjectCopyAs(Pair.class.getName());
        if (offsetCopy.first != 101 || offsetCopy.second != 59
                || PairCodec.decodedStorage != storage) {
            throw new AssertionError("plain aggregate read used the wrong source window");
        }

        Pair pending = view(bytes);
        pending.second = 83;
        byte[] image = new byte[storage.length];
        Pointer.encodeArrayMemory(storage, image, 0, 1, null);
        if (MemoryBytes.read(image, 4, 4) != 83) {
            throw new AssertionError("byte array encoding ignored a live aggregate view");
        }

        byte[] encoded = {9, 9, 9, 9, 9, 9};
        Pointer.encodeArrayMemory(new byte[] {1, 2, 3, 4}, encoded, 1, 1, null);
        if (!Arrays.equals(encoded, new byte[] {9, 1, 2, 3, 4, 9})) {
            throw new AssertionError("byte array encoding changed surrounding bytes");
        }
        byte[] decoded = new byte[4];
        Pointer.decodeArrayMemory(encoded, 1, decoded, 1, null);
        if (!Arrays.equals(decoded, new byte[] {1, 2, 3, 4})) {
            throw new AssertionError("byte array decoding read the wrong range");
        }

        Pair fields = new Pair();
        fields.first = 17;
        fields.second = 23;
        Pointer root = Pointer.cell(fields, 8, PAIR_CODEC);
        Pointer first = root.projectStructField(Pair.class.getName(), "first", 0, 4, null);
        if (first.add(0).offset_from(first) != 0 || first.add(1).getI32() != 23) {
            throw new AssertionError("pointer arithmetic lost the containing allocation");
        }
    }
}
