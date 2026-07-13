package org.rustlang.core.ptr.non_null;

import org.rustlang.runtime.Pointer;

public final class NonNull_i16 {
    public Pointer pointer;

    public NonNull_i16(Pointer pointer) {
        this.pointer = pointer;
    }

    public NonNull_i16(short[] pointer) {
        this(Pointer.array(pointer, 0));
    }

    public final boolean eq(NonNull_i16 other) {
        return other != null && pointer != null && pointer.sameAddress(other.pointer);
    }
}
