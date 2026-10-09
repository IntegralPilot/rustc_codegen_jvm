import java.lang.reflect.Field;
import java.util.Map;
import org.rustlang.runtime.Pointer;

/** Adding a layout must not generate a fresh JVM class for each operation. */
public final class CodecAdapters {
    public static final class OtherCodec {
        public static byte[] e$other(MemoryViews.Pair value) { return MemoryViews.PairCodec.e$pair(value); }
        public static MemoryViews.Pair d$other(byte[] bytes) { return MemoryViews.PairCodec.d$pair(bytes); }
        public static MemoryViews.Pair a$other(byte[] bytes, int offset) { return MemoryViews.PairCodec.a$pair(bytes, offset); }
        public static void w$other(MemoryViews.Pair value, byte[] bytes, int offset) { MemoryViews.PairCodec.w$pair(value, bytes, offset); }
    }

    public static final class RangeOnly {
        public static MemoryViews.Pair a$range(byte[] bytes, int offset) { return MemoryViews.PairCodec.a$pair(bytes, offset); }
        public static void w$range(MemoryViews.Pair value, byte[] bytes, int offset) { MemoryViews.PairCodec.w$pair(value, bytes, offset); }
    }

    public static void check() throws Exception {
        String first = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";
        String second = "CodecAdapters$OtherCodec#other#LMemoryViews$Pair;";
        for (String recipe : new String[] {first, second}) {
            Pointer memory = Pointer.array(new byte[8], 0, 1).retype(8, recipe);
            memory.set(new MemoryViews.Pair(37, 41));
            MemoryViews.Pair copy = (MemoryViews.Pair) memory.getObjectCopyAs(MemoryViews.Pair.class.getName());
            if (copy.first != 37 || copy.second != 41) throw new AssertionError("codec changed its value");
        }
        Field cache = Pointer.class.getDeclaredField("CODEC_METHODS");
        cache.setAccessible(true);
        Map<?, ?> plans = (Map<?, ?>) cache.get(null);
        Object left = plans.get(first), right = plans.get(second);
        Field initialized = right.getClass().getDeclaredField("initialized");
        initialized.setAccessible(true);
        int used = initialized.getInt(right);
        if ((used & (1 << 3)) == 0 || (used & (1 << 2)) != 0) {
            throw new AssertionError("range decode resolved an unused whole-buffer decoder");
        }
        for (String name : new String[] {"encode", "encodeAt", "decode", "decodeAt"}) {
            java.lang.reflect.Method method = left.getClass().getDeclaredMethod(name);
            method.setAccessible(true);
            if (method.invoke(left).getClass() != method.invoke(right).getClass()) {
                throw new AssertionError("codec operation " + name + " generated a class per layout");
            }
        }
        String range = "CodecAdapters$RangeOnly#range#LMemoryViews$Pair;#8";
        Pointer typed = Pointer.cell(new MemoryViews.Pair(19, 23), 8, range);
        // Neither the whole encoder nor decoder has a generated wrapper.
        if (typed.retype(4, null).getI32() != 19) throw new AssertionError("shared whole encoder");
        typed.retype(4, null).set(29);
        MemoryViews.Pair value = (MemoryViews.Pair) typed.getObjectCopyAs(MemoryViews.Pair.class.getName());
        if (value.first != 29 || value.second != 23) throw new AssertionError("range codec round trip");
        Object plan = plans.get(range);
        java.lang.reflect.Method encode = plan.getClass().getDeclaredMethod("encode");
        java.lang.reflect.Method decode = plan.getClass().getDeclaredMethod("decode");
        encode.setAccessible(true); decode.setAccessible(true);
        Object encoder = encode.invoke(plan), decoder = decode.invoke(plan);
        java.lang.reflect.Method call = encoder.getClass().getDeclaredMethod("encode", Object.class);
        call.setAccessible(true);
        byte[] bytes = (byte[]) call.invoke(encoder, new MemoryViews.Pair(31, 37));
        if (bytes.length != 8) throw new AssertionError("exact whole encoder byte size");
        call = decoder.getClass().getDeclaredMethod("decode", byte[].class);
        call.setAccessible(true);
        value = (MemoryViews.Pair) call.invoke(decoder, bytes);
        if (value.first != 31 || value.second != 37) throw new AssertionError("shared whole decoder");
    }
}
