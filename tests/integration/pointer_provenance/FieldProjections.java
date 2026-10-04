import org.rustlang.runtime.Pointer;

/** Repeated field borrows retain a live place without rebuilding its wrapper. */
public final class FieldProjections {
    private static final java.lang.invoke.MethodHandle SECOND;
    static {
        try {
            SECOND = org.rustlang.runtime.FieldProjections.project(java.lang.invoke.MethodHandles.lookup(),
                    "project", java.lang.invoke.MethodType.methodType(Pointer.class, Pointer.class),
                    Pair.class, "second", 4, 4, "").dynamicInvoker();
        } catch (ReflectiveOperationException error) {
            throw new ExceptionInInitializerError(error);
        }
    }

    public static class Pair {
        public int first, second;
        Pair(int first, int second) { this.first = first; this.second = second; }
    }

    public static final class Outer {
        public Pair inner;
        Outer(Pair inner) { this.inner = inner; }
    }

    public static final class Derived extends Pair {
        Derived(int first, int second) { super(first, second); }
    }

    public static final class NestedCodec {
        public static byte[] e$words(pointer_provenance.NestedWords value) {
            return java.nio.ByteBuffer.allocate(32).order(java.nio.ByteOrder.LITTLE_ENDIAN)
                    .putLong(value.prefix[0]).putLong(value.prefix[1])
                    .putLong(value.pair.first).putLong(value.pair.second).array();
        }
        public static pointer_provenance.NestedWords d$words(byte[] image) {
            java.nio.ByteBuffer bytes = java.nio.ByteBuffer.wrap(image).order(java.nio.ByteOrder.LITTLE_ENDIAN);
            return new pointer_provenance.NestedWords(new long[] {bytes.getLong(), bytes.getLong()},
                    new pointer_provenance.NestedPair(bytes.getLong(), bytes.getLong()));
        }
    }

    private static Pointer second(Pointer root) {
        try {
            return (Pointer) SECOND.invokeExact(root);
        } catch (Throwable error) {
            throw new AssertionError(error);
        }
    }

    private static boolean thin(Pointer pointer) {
        try {
            pointer.metadata();
            return false;
        } catch (IllegalStateException expected) {
            return true;
        }
    }

    private static java.lang.ref.WeakReference<Pointer> abandonedProjection() {
        Pointer root = Pointer.cellAligned(new Pair(107, 109), 8, null, 4);
        // Exercise address bookkeeping as well as the owner/projection cycle.
        second(root).addr();
        return new java.lang.ref.WeakReference<>(root);
    }

    public static void check() throws Exception {
        Pointer words = Pointer.cell(pointer_provenance.pointer_provenance.nested_words(), 32,
                "FieldProjections$NestedCodec#words#Lpointer_provenance/NestedWords;#32");
        byte[] image = new byte[32];
        java.nio.ByteBuffer.wrap(image).order(java.nio.ByteOrder.LITTLE_ENDIAN)
                .putLong(0, 13).putLong(8, 17).putLong(16, 19).putLong(24, (23L << 32) | 29);
        Pointer bytes = Pointer.array(image, 0, 1);
        for (Pointer value : new Pointer[] {words, bytes}) {
            if (pointer_provenance.pointer_provenance.replace_nested_word(value, 31) != 23
                    || pointer_provenance.pointer_provenance.replace_nested_word(value, 37) != 31
                    || pointer_provenance.pointer_provenance.read_nested_word(value) != ((37L << 32) | 29))
                throw new AssertionError("nested scalar projection used the wrong offset");
        }
        if (java.nio.ByteBuffer.wrap(image).order(java.nio.ByteOrder.LITTLE_ENDIAN).getLong(24)
                != ((37L << 32) | 29))
            throw new AssertionError("nested scalar write changed adjacent bytes");
        Pair fixedOwner = new Pair(5, 7);
        Pointer fixedFirst = Pointer.field(fixedOwner, "second", 4, null);
        Pointer fixedSecond = Pointer.field(fixedOwner, "second", 4, null);
        Pointer fixedWide = Pointer.withMetadata(fixedFirst, 13);
        if (fixedWide.metadata() != 13 || !thin(fixedSecond)) throw new AssertionError("fixed field metadata alias");
        Pointer fixedTrait = Pointer.attachTraitObjectCarrier(fixedFirst, "fixed", 4, 4);
        if (!"fixed".equals(fixedTrait.getObject()) || fixedSecond.getI32() != 7) {
            throw new AssertionError("fixed field trait alias");
        }
        Pointer.field(fixedOwner, "second", 1, null);
        Pointer oldWide = Pointer.withMetadata(fixedFirst, 17);
        if (oldWide.metadata() != 17 || !thin(fixedSecond)) throw new AssertionError("evicted fixed field metadata alias");

        Pointer root = Pointer.cellAligned(new Pair(17, 31), 8, null, 4);
        Pointer field = second(root);
        if (second(root) != field) throw new AssertionError("unchanged field projection was rebuilt");
        field.set(43);
        if (second(root).getI32() != 43) throw new AssertionError("field write");
        root.set(new Pair(53, 59));
        if (field.getI32() != 59) throw new AssertionError("parent replacement");
        Pointer priorCached = second(root);
        Pointer narrow = root.projectStructField(Pair.class.getName(), "second", 4, 1, null);
        Pointer detached = Pointer.withMetadata(priorCached, 5);
        if (detached.metadata() != 5 || !thin(field)) throw new AssertionError("evicted cache metadata alias");
        Pointer trait = Pointer.attachTraitObjectCarrier(priorCached, "carrier", 4, 4);
        if (!"carrier".equals(trait.getObject()) || !Integer.valueOf(59).equals(priorCached.getObject())) {
            throw new AssertionError("trait attachment changed an existing field borrow");
        }
        if (second(root).getI32() != 59 || narrow.getI8() != 59) {
            throw new AssertionError("projection width changed another view");
        }
        Pointer earlier = second(root);
        field = Pointer.withMetadata(field, 7);
        if (!thin(earlier)) throw new AssertionError("late metadata changed an existing borrow");
        Pointer other = second(root);
        if (!thin(other) || field.metadata() != 7) throw new AssertionError("projection metadata");
        Pointer.withMetadata(root, 11);
        Pointer metadata = second(root);
        if (metadata.metadata() != 11 || !thin(other)) throw new AssertionError("parent metadata");

        Pointer outer = Pointer.cellAligned(new Outer(new Pair(67, 71)), 8, null, 4);
        Pointer inner = outer.projectStructField(Outer.class.getName(), "inner", 0, 8, null);
        Pointer nested = second(inner);
        outer.set(new Outer(new Pair(73, 79)));
        if (nested.getI32() != 79) throw new AssertionError("nested parent replacement");
        long address = nested.addr();
        if (second(inner).addr() != address || nested.byte_offset(-4).addr() != address - 4) {
            throw new AssertionError("projection address identity");
        }
        inner = null;
        outer = null;
        System.gc();
        if (nested.getI32() != 79 || nested.addr() != address) {
            throw new AssertionError("projection backing lifetime");
        }
        Pointer inherited = Pointer.cellAligned(new Derived(97, 101), 8, null, 4);
        Pointer baseField = second(inherited);
        Pointer derivedField = inherited.projectStructField(
                Derived.class.getName(), "second", 4, 4, null);
        if (baseField != derivedField) throw new AssertionError("inherited field lost storage identity");
        derivedField.set(103);
        if (baseField.getI32() != 103) throw new AssertionError("inherited field alias");
        Pointer concurrent = Pointer.cellAligned(new Pair(83, 89), 8, null, 4);
        java.util.concurrent.atomic.AtomicReference<Throwable> failure = new java.util.concurrent.atomic.AtomicReference<>();
        Thread[] threads = new Thread[4];
        for (int i = 0; i < threads.length; i++) {
            threads[i] = new Thread(() -> {
                try {
                    for (int j = 0; j < 5000; j++) {
                        if (second(concurrent).getI32() != 89) throw new AssertionError("concurrent read");
                        Pointer first = concurrent.projectStructField(
                                Pair.class.getName(), "first", 0, 4, null);
                        if (first.getI32() != 83) throw new AssertionError("concurrent second field");
                    }
                } catch (Throwable error) {
                    failure.set(error);
                }
            });
        }
        for (Thread thread : threads) thread.start();
        for (Thread thread : threads) thread.join();
        if (failure.get() != null) throw new AssertionError(failure.get());
        java.lang.ref.WeakReference<Pointer> abandoned = abandonedProjection();
        for (int i = 0; i < 10; i++) {
            System.gc();
            if (abandoned.get() == null) return;
            Thread.sleep(20);
        }
        throw new AssertionError("field cache retained an abandoned allocation");
    }
}
