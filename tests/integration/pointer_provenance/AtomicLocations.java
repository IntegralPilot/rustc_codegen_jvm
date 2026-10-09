import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;
import org.rustlang.runtime.Pointer;

/** The carrier and component APIs must use the same allocation monitor. */
public final class AtomicLocations {
    public static final class Counter { public long value; }

    public static void check() throws Exception {
        for (int width : new int[] {1, 2, 4, 8}) {
            byte[] bytes = new byte[24];
            Pointer pointer = Pointer.array(bytes, 8, 1).retype(width, null);
            Pointer.atomicStore(bytes, 8L, 17, width, 4);
            if (Pointer.atomicAdd(pointer, 3, width, 0) != 17
                    || Pointer.atomicSubtract(bytes, 8L, 2, width, 0) != 20
                    || Pointer.atomicExchange(bytes, 8L, 7, width, 0) != 18
                    || Pointer.atomicAnd(bytes, 8L, 6, width, 0) != 7
                    || Pointer.atomicOr(bytes, 8L, 9, width, 0) != 6
                    || Pointer.atomicXor(bytes, 8L, 3, width, 0) != 15
                    || Pointer.atomicNand(bytes, 8L, 10, width, 0) != 12) {
                throw new AssertionError("atomic component RMW");
            }
            long mask = width == 8 ? -1L : (1L << (width * 8)) - 1;
            if (Pointer.atomicLoad(pointer, width, 0) != (~8L & mask)
                    || Pointer.atomicCompareExchange(bytes, 8L, 1, 2, width, 0, 0) != (~8L & mask)
                    || Pointer.atomicCompareExchange(pointer, ~8L, 33, width, 4, 0) != (~8L & mask)
                    || Pointer.atomicLoad(bytes, 8L, width, 0) != 33) {
                throw new AssertionError("atomic compare exchange or truncation");
            }
            Pointer.atomicStore(pointer, -2, width, 0);
            if (Pointer.atomicMax(bytes, 8L, 1, width, 0) != (-2L & mask)
                    || Pointer.atomicMin(bytes, 8L, -3, width, 0) != 1
                    || Pointer.atomicUnsignedMin(bytes, 8L, 3, width, 0) != (-3L & mask)
                    || Pointer.atomicUnsignedMax(bytes, 8L, -4, width, 0) != 3
                    || Pointer.atomicLoad(pointer, width, 0) != (-4L & mask)) {
                throw new AssertionError("atomic signed comparison");
            }
        }
        byte[] bytes = new byte[24];
        mixed(bytes, 8, Pointer.array(bytes, 8, 1).retype(8, null));
        long[] words = new long[3];
        mixed(words, 8, Pointer.array(words, 1, 8));
        Object local = Pointer.storageAligned(0L, 8, null, 8);
        mixed(local, 0, Pointer.fromLocation(local, 0, 8));
        Pointer field = Pointer.field(new Counter(), "value", 8, null);
        mixed(field, 0, field.retype(8, null));
    }

    private static void mixed(Object root, long offset, Pointer pointer) throws Exception {
        Pointer.atomicStore(root, offset, 0, 8, 0);
        CountDownLatch start = new CountDownLatch(1);
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread[] workers = new Thread[4];
        for (int i = 0; i < workers.length; i++) {
            final boolean component = (i & 1) != 0;
            final int ordering = i < 2 ? 0 : 4;
            workers[i] = new Thread(() -> {
                try {
                    start.await();
                    for (int n = 0; n < 10000; n++) {
                        if (component) Pointer.atomicAdd(root, offset, 1, 8, ordering);
                        else Pointer.atomicAdd(pointer, 1, 8, ordering);
                    }
                } catch (Throwable error) { failure.compareAndSet(null, error); }
            });
            workers[i].start();
        }
        start.countDown();
        for (Thread worker : workers) {
            worker.join(10000);
            if (worker.isAlive()) throw new AssertionError("atomic worker stalled");
        }
        if (failure.get() != null) throw new AssertionError(failure.get());
        if (Pointer.atomicLoad(root, offset, 8, 4) != 40000
                || Pointer.atomicLoad(pointer, 8, 0) != 40000) {
            throw new AssertionError("component and carrier atomics did not synchronize");
        }
    }
}
