package org.rustlang.runtime;

import java.lang.reflect.Array;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

/** Carries slices across Java and opaque boundaries. Rust calls use components. */
public class SliceView {
    public final Object array;
    public final int offset;
    public final int length;
    public final long rustLength;

    public SliceView(Object array, int offset, int length) {
        this(array, offset, (long) length);
    }

    public SliceView(Object array, int offset, long length) {
        this.array = array;
        this.offset = offset;
        this.length = (int) length;
        this.rustLength = length;
    }

    static SliceView create(String className, Object array, int offset, long length) {
        if (className.equals("org/rustlang/runtime/SliceView")
                || className.equals("org.rustlang.runtime.SliceView")) {
            return new SliceView(array, offset, length);
        }
        if (className.equals("org/rustlang/runtime/Utf8View")
                || className.equals("org.rustlang.runtime.Utf8View")) {
            return new Utf8View(array, offset, length);
        }
        throw new IllegalArgumentException("unknown Rust view carrier " + className);
    }

    static SliceView create(Class<?> type, Object array, int offset, long length) {
        if (type == SliceView.class) return new SliceView(array, offset, length);
        if (type == Utf8View.class) return new Utf8View(array, offset, length);
        // An explicit Java subclass is an interop boundary, not a Rust carrier.
        try {
            return (SliceView) type.getConstructor(Object.class, int.class, long.class)
                    .newInstance(array, offset, length);
        } catch (ReflectiveOperationException failure) {
            throw new IllegalArgumentException("invalid Rust view carrier " + type, failure);
        }
    }

    public final Object toArray() {
        Object result = Array.newInstance(array.getClass().getComponentType(), length);
        System.arraycopy(array, offset, result, 0, length);
        return result;
    }

    public static SliceView fromString(String value) {
        return (SliceView) Pointer.stringView(value, "org/rustlang/runtime/SliceView");
    }

    public static String toUtf8String(SliceView value) {
        return new String(Pointer.sliceToByteArray(value.array, value.offset, value.length),
                StandardCharsets.UTF_8);
    }

    public static Utf8View encodeUtf8(int value, SliceView target) {
        SliceView bytes = fromString(String.valueOf(Character.toChars(value)));
        System.arraycopy(bytes.array, bytes.offset, target.array, target.offset, bytes.length);
        return new Utf8View(target.array, target.offset, bytes.length);
    }

    public static boolean startsWith(SliceView value, SliceView prefix) {
        if (prefix.length > value.length) return false;
        for (int i = 0; i < prefix.length; i++) {
            if (!Objects.equals(Pointer.sliceGetObject(value.array, value.offset + i),
                    Pointer.sliceGetObject(prefix.array, prefix.offset + i))) return false;
        }
        return true;
    }

    public static boolean startsWithI8(SliceView value, SliceView prefix) {
        if (prefix.length > value.length) return false;
        for (int i = 0; i < prefix.length; i++) {
            if (Pointer.sliceGetI8(value.array, value.offset + i)
                    != Pointer.sliceGetI8(prefix.array, prefix.offset + i)) return false;
        }
        return true;
    }

    public static boolean startsWithI32(SliceView value, SliceView prefix) {
        if (prefix.length > value.length) return false;
        for (int i = 0; i < prefix.length; i++) {
            if (Pointer.sliceGetI32(value.array, value.offset + i)
                    != Pointer.sliceGetI32(prefix.array, prefix.offset + i)) return false;
        }
        return true;
    }

    public static Object $part$array(SliceView value) { return value == null ? null : value.array; }
    public static int $part$offset(SliceView value) { return value == null ? 0 : value.offset; }
    public static long $part$rustLength(SliceView value) { return value == null ? 0 : value.rustLength; }
}
