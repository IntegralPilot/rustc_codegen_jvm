package org.rustlang.core.ptr.non_null;

import org.rustlang.runtime.Pointer;

public final class NonNull_u8 {
    public Pointer pointer;

    public NonNull_u8(Pointer pointer) {
        this.pointer = pointer;
    }

    public NonNull_u8(byte[] pointer) {
        this(Pointer.array(pointer, 0));
    }

    public final boolean eq(NonNull_u8 other) {
        return other != null && pointer != null && pointer.sameAddress(other.pointer);
    }
}
