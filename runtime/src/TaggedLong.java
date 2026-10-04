package org.rustlang.runtime;

/** Immutable boundary carrier for a scalar payload and independent Rust tag. */
public final class TaggedLong {
    public final long value;
    public final long tag;

    public TaggedLong(long value, long tag) {
        this.value = value;
        this.tag = tag;
    }

    public static TaggedLong of(long value, long tag) { return new TaggedLong(value, tag); }

    public static boolean optionEquals(TaggedLong left, TaggedLong right) {
        long tag = tag(left);
        return tag == tag(right) && (tag == 0 || value(left) == value(right));
    }

    public static long value(TaggedLong value) { return value == null ? 0 : value.value; }
    public static long tag(TaggedLong value) { return value == null ? 0 : value.tag; }
}
