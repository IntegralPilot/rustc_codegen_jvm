import java.util.Arrays;
import org.rustlang.runtime.Pointer;

public final class MemoryCopies {
    private static final String PAIR = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";
    private static final String ADDRESS = "@raw-pointer\n4\n\n";

    public static final class ReferenceBox { public Pointer value = Pointer.withoutProvenance(0L, 4); }
    public static final class ReferenceCodec {
        public static byte[] e$reference(ReferenceBox value) {
            byte[] bytes = new byte[8];
            Pointer.fromTypedStorageLocation(bytes, 0, 8, ADDRESS).set(value.value);
            return bytes;
        }
        public static ReferenceBox d$reference(byte[] bytes) {
            ReferenceBox result = new ReferenceBox();
            result.value = (Pointer) Pointer.fromTypedStorageLocation(bytes, 0, 8, ADDRESS).getObject();
            return result;
        }
    }

    public static void check() throws Exception {
        zeroLengthCopies();
        scalarLocations();
        encodedScalarLocations();
        for (boolean boxed : new boolean[] {false, true}) {
            byte[] bytes = {2, 3, 5, 7, 11, 13, 17, 19};
            Object root = boxed ? Pointer.array(bytes, 1, 1) : bytes;
            long base = boxed ? -1 : 0;
            Pointer.copyStorage(root, base, 1, null, root, base + 2, 1, null, 6, false);
            if (!Arrays.equals(bytes, new byte[] {2, 3, 2, 3, 5, 7, 11, 13}))
                throw new AssertionError("forward overlap");
            Pointer.copyStorage(root, base + 2, 1, null, root, base, 1, null, 6, false);
            if (!Arrays.equals(bytes, new byte[] {2, 3, 5, 7, 11, 13, 11, 13}))
                throw new AssertionError("backward overlap");
            try {
                Pointer.copyStorage(root, base, 1, null, root, base + 1, 1, null, 6, true);
                throw new AssertionError("nonoverlap constraint lost");
            } catch (IllegalArgumentException expected) { }
            Pointer.copyStorage(root, base + 8, 1, null, root, base + 8, 1, null, 0, true);
            try {
                Pointer.copyStorage(root, base, 1, null, root, base + 7, 1, null, 2, false);
                throw new AssertionError("bounds not checked");
            } catch (IndexOutOfBoundsException expected) { }
        }
        int[] source = {0x01020304, 0x05060708, 0x11121314};
        int[] actual = {0, 0, 0}, expected = {0, 0, 0};
        Pointer.copyStorage(source, 4, 4, null, actual, 0, 4, null, 8, true);
        if (!Arrays.equals(actual, new int[] {source[1], source[2], 0}))
            throw new AssertionError("aligned primitive array copy");
        Pointer.copy(Pointer.array(source, 0, 4).byte_offset(1),
                Pointer.array(expected, 0, 4).byte_offset(2), 7);
        Arrays.fill(actual, 0);
        Pointer.copyStorage(source, 1, 4, null, actual, 2, 4, null, 7, false);
        if (!Arrays.equals(actual, expected)) throw new AssertionError("unaligned copy");

        byte[] from = new byte[16], to = new byte[16];
        Pointer owner = Pointer.array(from, 4, 1).retype(8, PAIR);
        MemoryViews.Pair pair = (MemoryViews.Pair) owner.getObject();
        pair.first = 29; pair.second = 31;
        Pointer.copyStorage(owner, 0, 8, PAIR, to, 4, 8, PAIR, 8, false);
        MemoryViews.Pair copied = (MemoryViews.Pair) Pointer.array(to, 4, 1).retype(8, PAIR).getObject();
        if (copied.first != 29 || copied.second != 31) throw new AssertionError("pending source writes");
        copied.first = 37;
        Pointer.copyStorage(owner, 4, 4, null, to, 8, 4, null, 4, false);
        copied = (MemoryViews.Pair) Pointer.array(to, 4, 1).retype(8, PAIR).getObject();
        if (copied.first != 37 || copied.second != 31) throw new AssertionError("destination aliases");

        int[] pointee = {41, 43};
        Pointer.array(from, 0, 1).retype(8, ADDRESS).set(Pointer.array(pointee, 1, 4));
        Pointer.copyStorage(from, 0, 8, ADDRESS, to, 0, 8, ADDRESS, 8, true);
        Pointer restored = (Pointer) Pointer.array(to, 0, 1).retype(8, ADDRESS).getObject();
        restored.set(47);
        if (pointee[1] != 47) throw new AssertionError("encoded pointer provenance lost");
        for (int displacement : new int[] {0, 3}) {
            byte[] storage = new byte[16];
            Pointer root = Pointer.array(storage, displacement, 1);
            Pointer original = Pointer.array(pointee, 1, 4);
            Pointer.fromTypedStorageLocation(root, 2, 8, ADDRESS).set(original);
            Pointer loaded = (Pointer) Pointer.loadTypedStorage(root, 2, 8, ADDRESS, Pointer.class.getName());
            loaded.set(53);
            if (pointee[1] != 53 || loaded.addr() != original.addr())
                throw new AssertionError("direct pointer read lost offset or provenance");
            Pointer.withMetadata(loaded, 17);
            Pointer again = (Pointer) Pointer.loadTypedStorage(root, 2, 8, ADDRESS, Pointer.class.getName());
            try {
                again.metadata();
                throw new AssertionError("decoded pointer metadata leaked to storage");
            } catch (IllegalStateException absent) { }
            long replacement = Pointer.array(pointee, 0, 4).address();
            org.rustlang.runtime.MemoryBytes.write(storage, displacement + 2, 8, replacement);
            loaded = (Pointer) Pointer.loadTypedStorage(root, 2, 8, ADDRESS, Pointer.class.getName());
            loaded.set(61);
            if (pointee[0] != 61 || pointee[1] != 53)
                throw new AssertionError("changed address reused stale provenance");
        }
        byte[] self = new byte[16];
        Pointer selfRoot = Pointer.array(self, 0, 1);
        Pointer.fromTypedStorageLocation(selfRoot, 0, 8, ADDRESS).set(selfRoot.byte_offset(8).retype(4));
        Pointer.copyStorage(self, 0, 8, ADDRESS, to, 0, 8, ADDRESS, 8, true);
        for (Pointer data : new Pointer[] {selfRoot, Pointer.array(to, 0, 1)}) {
            Pointer value = (Pointer) Pointer.loadTypedStorage(data, 0, 8, ADDRESS, Pointer.class.getName());
            value.set(59);
            if (Pointer.loadLocationBits(self, 8, 4) != 59)
                throw new AssertionError("copied self-pointer lost its allocation");
            Pointer.storeLocationBits(self, 8, 0, 4);
        }
    }

    private static void scalarLocations() throws Exception {
        for (int count = 1; count <= 8; count++) {
            long[] source = {0x123456789abcdef0L, 0x1122334455667788L};
            byte[] bytes = new byte[16];
            Pointer.copyStorage(source, 1, 8, null, bytes, 3, 1, null, count, true);
            if (Pointer.loadLocationBits(source, 1, count) != Pointer.loadLocationBits(bytes, 3, count))
                throw new AssertionError("cross-representation scalar copy: " + count);
        }
        MemoryViews.Pair value = new MemoryViews.Pair(17, 19);
        Object storage = Pointer.storageAligned(value, 8, PAIR, 4,
                "0,4,first\n4,4,second");
        byte[] bytes = new byte[8];
        Pointer.copyStorage(storage, 0, 4, null, bytes, 0, 1, null, 4, true);
        Pointer.copyStorage(bytes, 0, 1, null, storage, 4, 4, null, 4, true);
        java.lang.reflect.Field boundary = storage.getClass().getDeclaredField("boundary");
        boundary.setAccessible(true);
        if (value.second != 17 || boundary.get(storage) != null)
            throw new AssertionError("scalar copy materialized typed storage");

        Pointer aggregate = Pointer.fromTypedStorageLocation(bytes, 0, 8, PAIR);
        MemoryViews.Pair view = (MemoryViews.Pair) aggregate.getObject();
        view.first = 23; view.second = 29;
        Pointer.copyStorage(aggregate, 0, 4, null, aggregate, 4, 4, null, 4, false);
        view = (MemoryViews.Pair) aggregate.getObject();
        if (view.first != 23 || view.second != 23)
            throw new AssertionError("decoded scalar copy lost pending writes or invalidation");
        // Distinct carriers into the same tracked allocation still overlap.
        Pointer alias = Pointer.fromLocation(bytes, 1, 1);
        try {
            Pointer.copyStorage(aggregate, 0, 1, null, alias, 0, 1, null, 7, true);
            throw new AssertionError("tracked byte aliases lost overlap validation");
        } catch (IllegalArgumentException expected) { }
        byte[] expected = bytes.clone();
        System.arraycopy(expected, 0, expected, 1, 7);
        Pointer.copyStorage(aggregate, 0, 1, null, alias, 0, 1, null, 7, false);
        if (!Arrays.equals(bytes, expected)) throw new AssertionError("tracked overlapping scalar copy");
    }

    private static void encodedScalarLocations() throws Exception {
        byte[] source = new byte[16], destination = new byte[8];
        int[] pointee = {41};
        String codec = "MemoryCopies$ReferenceCodec#reference#LMemoryCopies$ReferenceBox;#8";
        Pointer whole = Pointer.fromTypedStorageLocation(source, 0, 8, codec);
        ReferenceBox decoded = (ReferenceBox) whole.getObject();
        decoded.value = Pointer.array(pointee, 0, 4);
        Pointer direct = (Pointer) Pointer.loadTypedStorage(Pointer.array(source, 0, 1),
                0, 8, ADDRESS, Pointer.class.getName());
        if (direct.getI32() != 41) throw new AssertionError("direct pointer read missed pending view writes");
        decoded = (ReferenceBox) whole.getObject();
        decoded.value = Pointer.array(pointee, 0, 4);
        // Flushing first publishes this provenance. A metadata check before the read cannot detect it.
        Pointer.copyStorage(whole, 0, 1, null, destination, 0, 1, null, 8, true);
        Pointer copied = (Pointer) Pointer.fromTypedStorageLocation(destination, 0, 8, ADDRESS).getObject();
        copied.set(43);
        if (pointee[0] != 43) throw new AssertionError("newly encoded copy lost pointer provenance");
        Pointer.copyStorage(source, 0, 1, null, source, 4, 1, null, 8, false);
        copied = (Pointer) Pointer.fromTypedStorageLocation(source, 4, 8, ADDRESS).getObject();
        copied.set(47);
        if (pointee[0] != 47) throw new AssertionError("overlapping encoded copy lost provenance");

        byte[] referenceSource = new byte[8], referenceTarget = new byte[8];
        Object dependency = new Object();
        java.lang.reflect.Method retain = Pointer.class.getDeclaredMethod("retainEncodedReference", Object.class, Object.class);
        retain.setAccessible(true);
        retain.invoke(null, referenceSource, dependency);
        Pointer.copyStorage(referenceSource, 0, 1, null, referenceTarget, 0, 1, null, 8, true);
        java.lang.reflect.Method index = Pointer.class.getDeclaredMethod("stateStripeIndex", Object.class);
        index.setAccessible(true);
        java.lang.reflect.Field references = Pointer.class.getDeclaredField("ENCODED_REFERENCES");
        references.setAccessible(true);
        java.util.Map<?, ?> stripe = ((java.util.Map<?, ?>[]) references.get(null))[(int) index.invoke(null, referenceTarget)];
        if (stripe.get(referenceTarget) != dependency) throw new AssertionError("scalar copy lost GC dependency");
    }

    private static void zeroLengthCopies() throws Exception {
        Object source = Pointer.storageAligned(new MemoryViews.Pair(5, 7), 8, PAIR, 4);
        Object destination = Pointer.storageAligned(new MemoryViews.Pair(11, 13), 8, PAIR, 4);
        Pointer.copyStorage(source, 0, 8, PAIR, destination, 0, 8, PAIR, 0, true);
        java.lang.reflect.Field boundary = source.getClass().getDeclaredField("boundary");
        boundary.setAccessible(true);
        if (boundary.get(source) != null || boundary.get(destination) != null) {
            throw new AssertionError("zero-byte copy materialized storage boundaries");
        }
        // Zero-sized copies need no offset arithmetic, even with dangling or maximal addresses.
        Pointer.copyStorage(null, Long.MAX_VALUE, 0, "@zero-sized:unused", null, -1L,
                0, "@zero-sized:unused", 0, false);
        Pointer dangling = Pointer.withoutProvenance(-1L, 0);
        Pointer.copyElements(dangling, dangling, Long.MAX_VALUE);
        for (long count : new long[] {-1L, (long) Integer.MAX_VALUE + 1, Long.MAX_VALUE}) {
            try {
                Pointer.copyStorage(source, 0, 8, PAIR, destination, 0, 8, PAIR, count, false);
                throw new AssertionError("invalid byte count accepted");
            } catch (IllegalArgumentException expected) { }
        }
        if (boundary.get(source) != null || boundary.get(destination) != null) {
            throw new AssertionError("invalid copy count materialized storage boundaries");
        }
    }
}
