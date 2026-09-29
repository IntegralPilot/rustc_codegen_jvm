import java.lang.management.ManagementFactory;
import java.util.Arrays;
import org.rustlang.runtime.Pointer;
import org.rustlang.runtime.SliceView;

public final class ArrayViews {
    private static volatile Object sink;

    public static void main(String[] args) {
        check();
    }

    public static void check() {
        byte[] bytes = {0, 1, 2, 3, 4, 5, 6, 7};
        SliceView window = new SliceView(Pointer.array(bytes, 1, 1), 2, 3);
        equal(new byte[] {3, 4, 5}, Pointer.arrayCarrier(window, "[B"));
        equal(new byte[] {3, 4, 5}, Pointer.arrayCarrier(window));
        equal(new byte[] {3, 4, 5}, Pointer.arrayCarrier(new SliceView(bytes, 3, 3), "[B"));
        equal(new byte[] {3, 4, 5}, Pointer.arrayCarrier(
                new SliceView(Pointer.cell(bytes, bytes.length), 3, 3), "[B"));
        equal(new byte[0], Pointer.arrayCarrier(
                new SliceView(Pointer.array(bytes, bytes.length, 1), 0, 0), "[B"));
        if (Pointer.arrayCarrier(new SliceView(Pointer.array(bytes, 0, 1), 0, 8), "[B") != bytes) {
            throw new AssertionError("whole-array view lost its backing");
        }
        byte[] copy = (byte[]) Pointer.arrayCarrier(window, "[B");
        copy[0] = 99;
        if (bytes[3] != 3) throw new AssertionError("array window did not copy its elements");

        int[] words = {0x44332211, 0x88776655};
        Pointer unaligned = Pointer.array(words, 0, 4).byte_offset(1).retype(1, null);
        equal(new byte[] {0x33, 0x44, 0x55}, Pointer.arrayCarrier(
                new SliceView(unaligned, 1, 3), "[B"));
        int[] selected = (int[]) Pointer.arrayCarrier(
                new SliceView(Pointer.array(words, 1, 4), 0, 1), "[I");
        if (!Arrays.equals(selected, new int[] {0x88776655})) {
            throw new AssertionError("pointer offset used bytes instead of elements");
        }
        String[] strings = {"zero", "one", "two", "three"};
        Object[] objects = (Object[]) Pointer.arrayCarrier(
                new SliceView(Pointer.array(strings, 1, 8), 1, 2), "[Ljava.lang.Object;");
        if (objects.getClass() != Object[].class || !Arrays.equals(objects, new Object[] {"two", "three"})) {
            throw new AssertionError("covariant array window has the wrong type or contents");
        }
        Pointer target = Pointer.array(new int[] {17, 23}, 1, 4);
        byte[] encoded = new byte[32];
        Pointer.array(encoded, 12, 1).retype(8, "@raw-pointer").set(target);
        byte[] pointerWindow = (byte[]) Pointer.arrayCarrier(
                new SliceView(Pointer.array(encoded, 8, 1), 4, 8), "[B");
        Pointer decoded = (Pointer) Pointer.array(pointerWindow, 0, 1)
                .retype(8, "@raw-pointer").getObject();
        if (decoded.retype(4, null).getI32() != 23) {
            throw new AssertionError("array window lost pointer provenance");
        }
        boundedAllocation();
    }

    private static void equal(byte[] expected, Object actual) {
        if (!(actual instanceof byte[]) || !Arrays.equals(expected, (byte[]) actual)) {
            throw new AssertionError("incorrect fixed-array window");
        }
    }

    private static void boundedAllocation() {
        java.lang.management.ThreadMXBean bean = ManagementFactory.getThreadMXBean();
        if (!(bean instanceof com.sun.management.ThreadMXBean)) return;
        com.sun.management.ThreadMXBean allocations = (com.sun.management.ThreadMXBean) bean;
        if (!allocations.isThreadAllocatedMemorySupported()) return;
        allocations.setThreadAllocatedMemoryEnabled(true);
        SliceView tiny = new SliceView(Pointer.array(new byte[1024 * 1024], 1, 1), 3, 4);
        for (int index = 0; index < 32; index++) sink = Pointer.arrayCarrier(tiny, "[B");
        long thread = Thread.currentThread().getId();
        long before = allocations.getThreadAllocatedBytes(thread);
        for (int index = 0; index < 128; index++) sink = Pointer.arrayCarrier(tiny, "[B");
        long allocated = allocations.getThreadAllocatedBytes(thread) - before;
        // Copying four bytes must not first duplicate the remaining megabyte.
        if (allocated > 1024 * 1024) {
            throw new AssertionError("tiny array reads allocated " + allocated + " bytes");
        }
    }
}
