import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import org.rustlang.runtime.Pointer;
import pointer_provenance.Pixel;

/** Generated array codecs must snapshot before touching overlapping storage. */
public final class RangeCodecs {
    public static void check() throws Exception {
        Pointer seed = pointer_provenance.pointer_provenance.pixel_storage();
        try {
            Field field = Pointer.class.getDeclaredField("viewCodecClassName");
            field.setAccessible(true);
            String recipe = (String) field.get(seed);
            String[] parts = recipe.split("#");
            Class<?> owner = Class.forName(parts[0].replace('/', '.'));
            // Require range access to prevent a return to allocating whole snapshots.
            Method encode = owner.getMethod("w$" + parts[1], Pixel.class, byte[].class, int.class);
            byte[] bytes = {11, 13, 17, 19, 23, 29};
            encode.invoke(null, new Pixel(bytes), bytes, 1);
            byte[] expected = {11, 11, 13, 17, 19, 29};
            if (!Arrays.equals(bytes, expected)) {
                throw new AssertionError("range codec overwrote an unread array element");
            }
            bytes = new byte[] {31, 37, 41, 43, 47, 53};
            Pointer destination = Pointer.array(bytes, 0, 1).retype(4, recipe);
            Pointer.storeStorageLocation(destination, 1, new Pixel(bytes));
            if (!Arrays.equals(bytes, new byte[] {31, 31, 37, 41, 43, 53})) {
                throw new AssertionError("storage cleared the source before its range codec ran");
            }
        } finally {
            pointer_provenance.pointer_provenance.free_pixel(seed);
        }
    }
}
