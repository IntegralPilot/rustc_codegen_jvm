import java.util.Arrays;
import org.rustlang.runtime.Pointer;

/** Owned projected fields remain snapshots across managed and byte aliases. */
public final class OwnedFields {
    private static final String CODEC = "org/rustlang/runtime/ArrayMemoryCodec#array#[B#4";

    public static final class Pixel {
        public byte[] channels;
        public Pixel(byte... channels) { this.channels = channels; }
    }

    private static byte[] read(Object root, long offset) {
        return (byte[]) Pointer.loadStorageFieldCopy(root, offset,
                Pixel.class.getName(), "channels", 4, 4, CODEC, "[B");
    }

    private static void write(Object root, long offset, byte[] value) {
        Pointer.storeStorageField(root, offset, Pixel.class.getName(), "channels", 4, 4, CODEC, value);
    }

    private static void equal(byte[] actual, int... values) {
        byte[] expected = new byte[values.length];
        for (int i = 0; i < values.length; i++) expected[i] = (byte) values[i];
        if (!Arrays.equals(actual, expected)) throw new AssertionError(Arrays.toString(actual));
    }

    public static void check() {
        byte[] bytes = new byte[32];
        System.arraycopy(new byte[] {11, 13, 17, 19}, 0, bytes, 12, 4);
        Pointer base = Pointer.array(bytes, 0, 1);
        for (Object root : new Object[] {bytes, base}) {
            byte[] snapshot = read(root, 8);
            snapshot[0] = 23;
            equal(read(root, 8), 11, 13, 17, 19);
            bytes[13] = 29;
            equal(snapshot, 23, 13, 17, 19);
            bytes[13] = 13;
            byte[] replacement = {73, 79, 83, 89};
            write(root, 8, replacement);
            equal(read(root, 8), 73, 79, 83, 89);
            replacement[0] = 97;
            equal(read(root, 8), 73, 79, 83, 89);
            if (bytes[11] != 0 || bytes[16] != 0) throw new AssertionError("store crossed field boundary");
            write(root, 8, new byte[] {11, 13, 17, 19});
            try {
                read(root, 26);
                throw new AssertionError("field copy accepted an invalid byte window");
            } catch (IndexOutOfBoundsException expected) { }
        }
        equal(read(base.byte_offset(4), 4), 11, 13, 17, 19);
        byte[] live = (byte[]) base.byte_offset(12).retype(4, CODEC).getObject();
        live[2] = 31;
        equal(read(base, 8), 11, 13, 31, 19);
        equal(read(bytes, 8), 11, 13, 31, 19);
        base.addr();
        equal(read(base, 8), 11, 13, 31, 19);
        write(base, 8, new byte[] {11, 13, 17, 19});
        equal(read(bytes, 8), 11, 13, 17, 19);

        Pixel pixel = new Pixel((byte) 37, (byte) 41, (byte) 43, (byte) 47);
        for (Object root : new Object[] {Pointer.cell(pixel, 8, null),
                Pointer.storageAligned(pixel, 8, null, 4)}) {
            byte[] snapshot = read(root, 0);
            pixel.channels[0] = 53;
            equal(snapshot, 37, 41, 43, 47);
            equal(read(root, 0), 53, 41, 43, 47);
            Pointer.storeStorageLocation(root, 0, new Pixel((byte) 59, (byte) 61, (byte) 67, (byte) 71));
            equal(read(root, 0), 59, 61, 67, 71);
            equal(snapshot, 37, 41, 43, 47);
            write(root, 0, new byte[] {73, 79, 83, 89});
            equal(read(root, 0), 73, 79, 83, 89);
            equal(snapshot, 37, 41, 43, 47);
            pixel.channels[0] = 37;
        }
    }
}
