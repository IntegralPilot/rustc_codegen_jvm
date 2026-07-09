package org.rustlang.core.ptr.non_null;

public final class NonNull_i16 {
    public short[] pointer;

    public NonNull_i16(short[] pointer) {
        this.pointer = pointer;
    }

    public final boolean eq(NonNull_i16 other) {
        return other != null && pointer == other.pointer;
    }
}
