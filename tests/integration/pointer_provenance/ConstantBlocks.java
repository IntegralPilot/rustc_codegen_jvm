import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.ReadOnlyBufferException;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.rustlang.runtime.ConstantData;

public final class ConstantBlocks {
    public static final class Anchor { }
    private static final String PREFIX = "META-INF/rust-data/";

    private static final class Loader extends ClassLoader {
        final int marker;
        final AtomicInteger opens = new AtomicInteger();
        Loader(int marker) { super(ConstantBlocks.class.getClassLoader()); this.marker = marker; }
        Class<?> anchor(byte[] code) { return defineClass(null, code, 0, code.length); }
        public InputStream getResourceAsStream(String name) {
            if (!name.startsWith(PREFIX)) return super.getResourceAsStream(name);
            opens.incrementAndGet();
            int size = Integer.parseInt(name.substring(name.lastIndexOf('-') + 1));
            if (name.contains("truncated")) size--;
            if (name.contains("oversized")) size++;
            byte[] bytes = new byte[size];
            for (int i = 0; i < size; i++) bytes[i] = (byte) (marker + i);
            return new ByteArrayInputStream(bytes);
        }
    }

    public static void check() throws Exception {
        byte[] code;
        try (InputStream input = ConstantBlocks.class.getResourceAsStream("/ConstantBlocks$Anchor.class")) {
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            byte[] scratch = new byte[4096];
            for (int n; (n = input.read(scratch)) >= 0;) output.write(scratch, 0, n);
            code = output.toByteArray();
        }
        Loader first = new Loader(17), second = new Loader(43);
        Class<?> a = first.anchor(code), b = second.anchor(code);
        String name = PREFIX + "cursor-8";
        ByteBuffer left = ConstantData.buffer(name, a, 8);
        ByteBuffer right = ConstantData.buffer(name, a, 8);
        if (left.getInt() != 0x14131211 || right.position() != 0
                || right.getInt() != 0x14131211 || first.opens.get() != 1) {
            throw new AssertionError("constant blocks reread data or shared cursor state");
        }
        try {
            right.put(0, (byte) 0);
            throw new AssertionError("shared constant data is mutable");
        } catch (ReadOnlyBufferException expected) { }
        if (ConstantData.buffer(name, b, 8).getInt() != 0x2e2d2c2b) {
            throw new AssertionError("constant data crossed loader boundaries");
        }
        byte[] target = new byte[12];
        Arrays.fill(target, (byte) 9);
        ConstantData.fill(target, 2, 8, name, a);
        target[3] = 0;
        ConstantData.fill(target, 2, 8, name, a);
        if (target[1] != 9 || target[2] != 17 || target[3] != 18 || target[10] != 9) {
            throw new AssertionError("constant destination range is shared or incorrect");
        }
        long[] words = new long[3];
        ConstantData.fill(words, 1, 1, name, a);
        if (words[0] != 0 || words[1] != 0x1817161514131211L || words[2] != 0) {
            throw new AssertionError("constant byte order or scalar range");
        }
        float[] floats = new float[2];
        ByteBuffer bits = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN);
        bits.putInt(0x7fc01234).putInt(0x80000000).flip();
        ConstantData.readArray(floats, bits);
        if (Float.floatToRawIntBits(floats[0]) != 0x7fc01234
                || Float.floatToRawIntBits(floats[1]) != 0x80000000 || bits.position() != 8) {
            throw new AssertionError("constant floating-point bits or cursor");
        }
        for (String invalid : new String[] {"truncated-8", "oversized-8"}) {
            try {
                ConstantData.buffer(PREFIX + invalid, a, 8);
                throw new AssertionError("malformed constant block accepted");
            } catch (IllegalStateException expected) { }
        }
        try {
            ConstantData.buffer(name, a, 4);
            throw new AssertionError("cached constant ignored its expected size");
        } catch (IllegalStateException expected) { }
        int reads = first.opens.get();
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread[] threads = new Thread[4];
        for (int i = 0; i < threads.length; i++) {
            threads[i] = new Thread(() -> {
                try {
                    for (int n = 0; n < 200; n++) {
                        if (ConstantData.buffer(name, a, 8).getLong() != 0x1817161514131211L) {
                            throw new AssertionError("concurrent constant cursor");
                        }
                    }
                } catch (Throwable error) { failure.set(error); }
            });
            threads[i].start();
        }
        for (Thread thread : threads) thread.join();
        if (failure.get() != null || first.opens.get() != reads) {
            throw new AssertionError("constant cache concurrency", failure.get());
        }
        for (int i = 0; i < 80; i++) ConstantData.buffer(PREFIX + "large" + i + "-65536", a, 65536);
        reads = first.opens.get();
        ConstantData.buffer(name, a, 8);
        if (first.opens.get() == reads) throw new AssertionError("constant byte cache is unbounded");
        for (int i = 0; i < 300; i++) ConstantData.buffer(PREFIX + "tiny" + i + "-1", a, 1);
        reads = first.opens.get();
        ConstantData.buffer(name, a, 8);
        if (first.opens.get() == reads) throw new AssertionError("constant entry cache is unbounded");
    }
}
