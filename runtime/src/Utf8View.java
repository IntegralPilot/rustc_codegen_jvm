package org.rustlang.runtime;

/** A UTF-8-valid boundary view. Length and offsets are measured in bytes. */
public final class Utf8View extends SliceView {
    public Utf8View(Object array, int offset, int length) { super(array, offset, length); }
    public Utf8View(Object array, int offset, long length) { super(array, offset, length); }

    public static Utf8View fromJavaString(String value) {
        return (Utf8View) Pointer.stringView(value, "org/rustlang/runtime/Utf8View");
    }

    public static String toJavaString(Utf8View value) { return toUtf8String(value); }
    public static SliceView asSlice(Utf8View value) { return value; }
    public static Utf8View fromSlice(SliceView value) {
        return new Utf8View(value.array, value.offset, value.rustLength);
    }
    public static long len(Utf8View value) { return value.rustLength; }
    public static boolean startsWith(Utf8View value, Utf8View prefix) {
        return startsWithI8(value, prefix);
    }
    public static boolean equals(Utf8View value, Utf8View other) {
        return value.rustLength == other.rustLength && startsWith(value, other);
    }
    public static boolean startsWithChar(Utf8View value, int character) {
        return toJavaString(value).startsWith(String.valueOf(Character.toChars(character)));
    }
}
