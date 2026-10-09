import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;
import org.rustlang.runtime.Pointer;
import org.rustlang.runtime.RuntimeSupport;

/** Host I/O shares the runtime's bulk-copy alias and byte-window semantics. */
public final class IoCopies {
    private static final String CODEC = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";

    public static void check() {
        PrintStream previousOut = System.out;
        InputStream previousIn = System.in;
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        System.setOut(new PrintStream(output));
        try {
            RuntimeSupport.writeStdout(null, 0);
            if (RuntimeSupport.readStdin(null, 0) != 0) throw new AssertionError("empty read");
            byte[] bytes = {11, 13, 17, 19, 23, 29};
            RuntimeSupport.writeStdout(Pointer.array(bytes, 1, 1), 4);
            if (!Arrays.equals(output.toByteArray(), new byte[] {13, 17, 19, 23}))
                throw new AssertionError("offset output");
            output.reset();
            try {
                RuntimeSupport.writeStdout(Pointer.array(new int[] {31, 37, 255}, 0, 4), 3);
                throw new AssertionError("wide output view accepted");
            } catch (IllegalStateException expected) { }
            System.setIn(new ByteArrayInputStream(new byte[] {41, 43, 47}));
            if (RuntimeSupport.readStdin(Pointer.array(bytes, 1, 1), 4) != 3
                    || !Arrays.equals(bytes, new byte[] {11, 41, 43, 47, 23, 29}))
                throw new AssertionError("short read changed neighboring bytes");
            int[] strided = {0, 0, 0};
            System.setIn(new ByteArrayInputStream(new byte[] {53, 59, -1}));
            RuntimeSupport.readStdin(Pointer.array(strided, 0, 4), 3);
            if (!Arrays.equals(strided, new int[] {53, 59, -1})) throw new AssertionError("strided input");

            byte[] storage = new byte[16];
            Pointer owner = Pointer.array(storage, 4, 1).retype(8, CODEC);
            MemoryViews.Pair view = (MemoryViews.Pair) owner.getObject();
            view.first = 61; view.second = 67;
            output.reset();
            RuntimeSupport.writeStdout(owner.retype(1, null), 8);
            if (!Arrays.equals(output.toByteArray(), new byte[] {61, 0, 0, 0, 67, 0, 0, 0}))
                throw new AssertionError("output missed pending managed writes");
            view = (MemoryViews.Pair) owner.getObject();
            view.second = 71;
            owner.commitMemoryView();
            System.setIn(new ByteArrayInputStream(new byte[] {73, 0, 0, 0}));
            RuntimeSupport.readStdin(owner.retype(1, null), 4);
            view = (MemoryViews.Pair) owner.getObject();
            if (view.first != 73 || view.second != 71 || storage[3] != 0 || storage[12] != 0)
                throw new AssertionError("input lost alias coherence or neighboring bytes: "
                        + view.first + "," + view.second + " " + Arrays.toString(storage));
            try {
                RuntimeSupport.writeStdout(Pointer.array(bytes, 4, 1), 4);
                throw new AssertionError("invalid output range accepted");
            } catch (IndexOutOfBoundsException expected) { }
        } finally {
            System.setOut(previousOut);
            System.setIn(previousIn);
        }
    }
}
