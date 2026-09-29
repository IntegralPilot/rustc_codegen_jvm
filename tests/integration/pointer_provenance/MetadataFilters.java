import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import org.rustlang.runtime.Pointer;

/** A cache rebuild must not hide a newly published allocation dependency. */
final class MetadataFilters {
    private static Object field(Object owner, String name) throws Exception {
        Class<?> type = owner instanceof Class<?> ? (Class<?>) owner : owner.getClass();
        Field field = type.getDeclaredField(name);
        field.setAccessible(true);
        return field.get(owner instanceof Class<?> ? null : owner);
    }

    private static Method method(String name, Class<?>... arguments) throws Exception {
        Method method = Pointer.class.getDeclaredMethod(name, arguments);
        method.setAccessible(true);
        return method;
    }

    @SuppressWarnings("unchecked")
    static void check() throws Exception {
        locationOriginFalsePositive();
        Object source = new byte[16], target = new byte[16], copied = new byte[16];
        Map<Object, Object>[] stripes = (Map<Object, Object>[]) field(Pointer.class, "ENCODED_REFERENCES");
        Method stripeIndex = method("stateStripeIndex", Object.class);
        Map<Object, Object> sourceStripe = stripes[(int) stripeIndex.invoke(null, source)];
        Method retain = method("retainEncodedReference", Object.class, Object.class);
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread writer = new Thread(() -> {
            try { retain.invoke(null, source, target); }
            catch (Throwable error) { failure.set(error); }
        });
        synchronized (sourceStripe) {
            writer.start();
            // Rebuild after the writer marks the old filter but before it publishes the entry.
            long deadline = System.nanoTime() + 5_000_000_000L;
            while (writer.getState() != Thread.State.BLOCKED) {
                if (failure.get() != null || !writer.isAlive() || System.nanoTime() >= deadline)
                    throw new AssertionError("writer did not reach metadata publication", failure.get());
                Thread.onSpinWait();
            }
            Object filter = field(Pointer.class, "ENCODED_REFERENCE_FILTER");
            ((AtomicLong) field(filter, "marks")).set(Long.MAX_VALUE);
            method("maybeRebuildIdentityFilter", filter.getClass(), Map[].class).invoke(null, filter, stripes);
        }
        writer.join(5000);
        if (writer.isAlive() || failure.get() != null)
            throw new AssertionError("metadata publication failed", failure.get());
        method("transferEncodedReferences", Object.class, Object.class).invoke(null, source, copied);
        Map<Object, Object> copyStripe = stripes[(int) stripeIndex.invoke(null, copied)];
        synchronized (copyStripe) {
            if (copyStripe.get(copied) != target)
                throw new AssertionError("filter rebuild lost the copied allocation's strong dependency");
        }
        method("discardEncodedReferences", Object.class).invoke(null, source);
        method("discardEncodedReferences", Object.class).invoke(null, copied);
    }

    private static void locationOriginFalsePositive() throws Exception {
        long[] values = new long[] {17, 23};
        Object filter = field(Pointer.class, "MEMORY_VIEW_ORIGIN_FILTER");
        method("markIdentityFilter", filter.getClass(), Object.class).invoke(null, filter, values);
        Object normalized = method("normalizeLocationOrigin", Object.class).invoke(null, values);
        if (normalized != values) throw new AssertionError("false origin mark allocated an address");
        long address = Pointer.locationAddr(values, 8);
        Pointer pointer = Pointer.array(values, 1, 8);
        if (pointer.addr() != address || Pointer.byteOffsetLocations(values, 8, pointer, 0) != 0
                || Pointer.atomicAdd(values, 8L, 5, 8, 0) != 23
                || Pointer.atomicLoad(pointer, 8, 4) != 28) {
            throw new AssertionError("false origin mark changed address or atomic identity");
        }
    }

}
