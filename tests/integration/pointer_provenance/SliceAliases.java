import org.rustlang.runtime.Pointer;

/** Scalar slices keep observing aggregate views created after decomposition. */
public final class SliceAliases {
    public static void check() {
        Object[] arrays = { new byte[8], new short[4], new char[4], new int[2],
                new long[1], new float[2], new double[1] };
        int[] sizes = {1, 2, 2, 4, 8, 4, 8};
        for (int index = 0; index < arrays.length; index++) {
            Object array = arrays[index];
            int size = sizes[index];
            Pointer data = Pointer.array(array, 0, size);
            Object backing = Pointer.locationSliceBacking(data, 0, size);
            int start = Pointer.locationSliceOffset(data, 0, size);
            Pointer aggregate = data.retype(8, "MemoryViews$PairCodec#pair#LMemoryViews$Pair;");
            MemoryViews.Pair view = (MemoryViews.Pair) aggregate.getObject();
            view.first = 0x44332211;
            view.second = 0;
            if (array instanceof int[]
                    && pointer_provenance.pointer_provenance.first_word((int[]) array) != 0x44332211) {
                throw new AssertionError("stale generated array read");
            }
            long mask = size == 8 ? -1L : (1L << (8 * size)) - 1;
            long value;
            if (array instanceof byte[]) value = Pointer.sliceGetI8(backing, start) & 255L;
            else if (array instanceof short[]) value = Pointer.sliceGetI16(backing, start) & 65535L;
            else if (array instanceof char[]) value = Pointer.sliceGetU16(backing, start);
            else if (array instanceof int[]) value = Pointer.sliceGetI32(backing, start) & 0xffffffffL;
            else if (array instanceof long[]) value = Pointer.sliceGetI64(backing, start);
            else if (array instanceof float[]) value = Float.floatToRawIntBits(Pointer.sliceGetF32(backing, start)) & 0xffffffffL;
            else value = Double.doubleToRawLongBits(Pointer.sliceGetF64(backing, start));
            if (value != (0x44332211L & mask)) throw new AssertionError("stale slice read " + array.getClass());

            if (array instanceof byte[]) Pointer.sliceSetI8(backing, start, (byte) 0x55);
            else if (array instanceof short[]) Pointer.sliceSetI16(backing, start, (short) 0x55);
            else if (array instanceof char[]) Pointer.sliceSetU16(backing, start, (char) 0x55);
            else if (array instanceof int[]) Pointer.sliceSetI32(backing, start, 0x55);
            else if (array instanceof long[]) Pointer.sliceSetI64(backing, start, 0x55);
            else if (array instanceof float[]) Pointer.sliceSetF32(backing, start, Float.intBitsToFloat(0x55));
            else Pointer.sliceSetF64(backing, start, Double.longBitsToDouble(0x55));
            MemoryViews.Pair result = (MemoryViews.Pair) aggregate.getObject();
            long expected = (0x44332211L & ~mask) | 0x55;
            if ((result.first & 0xffffffffL) != expected) throw new AssertionError("lost slice write " + array.getClass());

            if (array instanceof byte[]) Pointer.fillArray((byte[]) array, (byte) 0x33);
            else if (array instanceof short[]) Pointer.fillArray((short[]) array, (short) 0x33);
            else if (array instanceof char[]) Pointer.fillArray((char[]) array, (char) 0x33);
            else if (array instanceof int[]) Pointer.fillArray((int[]) array, 0x33);
            else if (array instanceof long[]) Pointer.fillArray((long[]) array, 0x33L);
            else if (array instanceof float[]) Pointer.fillArray((float[]) array, Float.intBitsToFloat(0x33));
            else Pointer.fillArray((double[]) array, Double.longBitsToDouble(0x33));
            result = (MemoryViews.Pair) aggregate.getObject();
            long word = size == 1 ? 0x33333333L : size == 2 ? 0x00330033L : 0x33L;
            if ((result.first & 0xffffffffL) != word || (result.second & 0xffffffffL) != (size == 8 ? 0 : word)) {
                throw new AssertionError("lost array fill " + array.getClass());
            }
        }
    }
}
