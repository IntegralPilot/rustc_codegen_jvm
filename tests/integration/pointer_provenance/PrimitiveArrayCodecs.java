import java.lang.reflect.Array;
import java.util.Arrays;
import org.rustlang.runtime.Pointer;

/** Shared codecs preserve exact lengths, offsets and scalar bit patterns. */
public final class PrimitiveArrayCodecs {
    public static void check() {
        byteWidths();
        roundTrip(new boolean[] {true, false, true}, 1);
        roundTrip(new byte[] {-128, -1, 0, 127}, 1);
        roundTrip(new short[] {Short.MIN_VALUE, -1, 0, Short.MAX_VALUE}, 2);
        roundTrip(new char[] {0, 255, 256, 65535}, 2);
        roundTrip(new int[] {Integer.MIN_VALUE, -1, 0, Integer.MAX_VALUE}, 4);
        roundTrip(new long[] {Long.MIN_VALUE, -1, 0, Long.MAX_VALUE}, 8);
        roundTrip(new float[] {-0.0f, Float.intBitsToFloat(0x7fc01234), 7.25f}, 4);
        roundTrip(new double[] {-0.0, Double.longBitsToDouble(0x7ff8123456789abcL), 7.25}, 8);
        roundTrip(new byte[0], 1);
        roundTrip(new int[0], 4);
        roundTrip(new int[] {197}, 4);
        byte[] overlap = {3, 5, 7, 11, 13};
        Pointer.encodeArrayMemory(overlap, overlap, 0, 1, null);
        if (!Arrays.equals(overlap, new byte[] {3, 5, 7, 11, 13}))
            throw new AssertionError("in-place byte encoding changed data");
    }

    private static void byteWidths() {
        for (long bits : new long[] {0, -1, Long.MIN_VALUE, 0x81726354a5b6c7d8L}) {
            byte[] encoded = java.nio.ByteBuffer.allocate(8)
                    .order(java.nio.ByteOrder.LITTLE_ENDIAN).putLong(bits).array();
            for (int width = 0; width <= 8; width++) {
                for (int offset = 0; offset <= 8; offset++) {
                    byte[] expected = new byte[16], actual = new byte[16];
                    Arrays.fill(expected, (byte) 37);
                    Arrays.fill(actual, (byte) 37);
                    System.arraycopy(encoded, 0, expected, offset, width);
                    org.rustlang.runtime.MemoryBytes.write(actual, offset, width, bits);
                    byte[] scalar = Arrays.copyOf(Arrays.copyOf(encoded, width), 8);
                    long value = java.nio.ByteBuffer.wrap(scalar)
                            .order(java.nio.ByteOrder.LITTLE_ENDIAN).getLong();
                    if (!Arrays.equals(actual, expected)
                            || org.rustlang.runtime.MemoryBytes.read(actual, offset, width) != value)
                        throw new AssertionError("scalar byte width or alignment changed");
                }
            }
        }
    }

    private static void roundTrip(Object source, int width) {
        int size = Array.getLength(source) * width;
        String recipe = "org/rustlang/runtime/ArrayMemoryCodec#array#"
                + source.getClass().getName() + "#" + size;
        byte[] storage = new byte[size + 10];
        Arrays.fill(storage, (byte) 37);
        Pointer.storeTypedStorage(storage, 5, size, recipe, source);
        Object decoded = Pointer.array(storage, 0, 1).byte_offset(5).retype(size, recipe)
                .getObjectCopyAs(source.getClass().getName());
        if (decoded == source || decoded.getClass() != source.getClass()
                || Array.getLength(decoded) != Array.getLength(source))
            throw new AssertionError("shared codec changed ownership, type or length");
        byte[] original = new byte[size], restored = new byte[size];
        Pointer.encodeArrayMemory(source, original, 0, width, null);
        Pointer.encodeArrayMemory(decoded, restored, 0, width, null);
        if (!Arrays.equals(original, restored)
                || !Arrays.equals(original, Arrays.copyOfRange(storage, 5, 5 + size)))
            throw new AssertionError("shared codec changed scalar bits");
        for (int i = 0; i < 5; i++) {
            if (storage[i] != 37 || storage[size + 5 + i] != 37)
                throw new AssertionError("shared codec wrote outside its range");
        }
    }
}
