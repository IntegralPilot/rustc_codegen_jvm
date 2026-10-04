import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import org.rustlang.runtime.MemoryBytes;
import org.rustlang.runtime.Pointer;

/** An aggregate decode must not publish bytes superseded by an interior atomic write. */
public final class AtomicViews {
    public static final class Pair {
        public int payload;
        public int state;
    }

    public static final class Codec {
        static volatile CountDownLatch decoding;
        static volatile CountDownLatch resume;

        public static byte[] e$pair(Pair value) {
            byte[] bytes = new byte[8];
            MemoryBytes.write(bytes, 0, 4, value.payload);
            MemoryBytes.write(bytes, 4, 4, value.state);
            return bytes;
        }

        public static Pair d$pair(byte[] bytes) {
            Pair value = new Pair();
            value.payload = (int) MemoryBytes.read(bytes, 0, 4);
            value.state = (int) MemoryBytes.read(bytes, 4, 4);
            CountDownLatch paused = decoding;
            if (paused != null) {
                paused.countDown();
                await(resume);
            }
            return value;
        }
    }

    private static void await(CountDownLatch latch) {
        try {
            if (!latch.await(10, TimeUnit.SECONDS)) throw new AssertionError("worker stalled");
        } catch (InterruptedException error) {
            throw new AssertionError(error);
        }
    }

    public static void check() throws Exception {
        check(false);
        check(true);
        absenceEpoch(false);
        absenceEpoch(true);
    }

    private static void absenceEpoch(boolean initialView) throws InterruptedException {
        byte[] storage = new byte[8];
        Pointer bytes = Pointer.array(storage, 0, 1).retype(4, null);
        Pointer whole = bytes.retype(8, "AtomicViews$Codec#pair#LAtomicViews$Pair;#8");
        if (initialView) ((Pair) whole.getObject()).payload = 1;
        int expected = initialView ? 1 : 0;
        if (bytes.getI32() != expected || bytes.getI32() != expected)
            throw new AssertionError("initial view was not flushed");
        Thread writer = new Thread(() -> ((Pair) whole.getObject()).payload = 73);
        writer.start();
        writer.join();
        if (bytes.getI32() != 73) throw new AssertionError("cached absence hid a new thread's view");
    }

    private static void check(boolean components) throws Exception {
        byte[] storage = new byte[8];
        MemoryBytes.write(storage, 4, 4, 1);
        Pointer whole = Pointer.array(storage, 0, 1).retype(8, "AtomicViews$Codec#pair#LAtomicViews$Pair;#8");
        Pointer atomic = Pointer.array(storage, 4, 1).retype(4, null);
        AtomicReference<Pair> decoded = new AtomicReference<>();
        AtomicReference<Throwable> failed = new AtomicReference<>();
        Codec.decoding = new CountDownLatch(1);
        Codec.resume = new CountDownLatch(1);
        Thread reader = new Thread(() -> {
            try { decoded.set((Pair) whole.getObject()); }
            catch (Throwable error) { failed.set(error); }
        });
        Thread writer = new Thread(() -> {
            try {
                if (components) Pointer.atomicStore(storage, 4L, 0, 4, 0);
                else Pointer.atomicStore(atomic, 0, 4, 0);
            }
            catch (Throwable error) { failed.set(error); }
        });
        reader.start();
        await(Codec.decoding);
        writer.start();
        // Wait until the writer finishes or blocks on the overlapping decode.
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
        while (writer.isAlive() && writer.getState() != Thread.State.BLOCKED) {
            if (System.nanoTime() > deadline) throw new AssertionError("writer stalled");
            Thread.yield();
        }
        Codec.resume.countDown();
        reader.join(10000);
        writer.join(10000);
        Codec.decoding = null;
        if (reader.isAlive() || writer.isAlive()) throw new AssertionError("workers stalled");
        if (failed.get() != null) throw new AssertionError(failed.get());
        decoded.get().payload = 73;
        // Flushing the aggregate must not restore stale state over the atomic store.
        if (Pointer.atomicLoad(atomic, 4, 0) != 0) {
            throw new AssertionError("decoded aggregate restored a completed atomic state");
        }
    }
}
