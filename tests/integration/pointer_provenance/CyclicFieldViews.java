import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.util.Map;
import org.rustlang.runtime.Pointer;

/** A decoded field may contain a reference back to its enclosing allocation. */
public final class CyclicFieldViews {
    public static final class Holder { public long bits = 41; }
    public static final class View {
        public Pointer parent;
        View(Pointer parent) { this.parent = parent; }
    }
    private static Pointer decodingParent;
    public static final class Codec {
        public static byte[] e$view(View value) { return new byte[8]; }
        public static View d$view(byte[] bytes) { return new View(decodingParent); }
    }

    private static WeakReference<Pointer> abandoned() {
        Pointer root = Pointer.cell(new Holder(), 8, null);
        Pointer field = root.projectStructField(Holder.class.getName(), "bits", 0, 8, null);
        // Only the cached decoded view retains this parent after return.
        decodingParent = root;
        try {
            View view = (View) field.retype(8,
                    "CyclicFieldViews$Codec#view#LCyclicFieldViews$View;").getObject();
            if (view.parent != root) throw new AssertionError("decoded parent");
        } finally {
            decodingParent = null;
        }
        return new WeakReference<>(root);
    }

    public static void check() throws Exception {
        WeakReference<Pointer> root = abandoned();
        for (int i = 0; i < 20; i++) {
            System.gc();
            Thread.sleep(20);
            // Drain the weak maps so the field key and then its decoded parent can be collected.
            for (String name : new String[] {"MEMORY_VIEWS", "MEMORY_VIEW_ORIGINS"}) {
                Field field = Pointer.class.getDeclaredField(name);
                field.setAccessible(true);
                for (Map<?, ?> stripe : (Map<?, ?>[]) field.get(null)) {
                    synchronized (stripe) { stripe.size(); }
                }
            }
            if (root.get() == null) return;
        }
        throw new AssertionError("decoded field cache retained an abandoned parent");
    }
}
