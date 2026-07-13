package org.rustlang.core.ptr.non_null;

import org.rustlang.core.fmt.rt.Argument__;
import org.rustlang.runtime.Pointer;

public final class NonNull_Argument__ {
    public Pointer pointer;

    public NonNull_Argument__(Pointer pointer) {
        this.pointer = pointer;
    }

    public NonNull_Argument__(Argument__[] pointer) {
        this(Pointer.array(pointer, 0));
    }

    public final boolean eq(NonNull_Argument__ other) {
        return other != null && pointer != null && pointer.sameAddress(other.pointer);
    }
}
