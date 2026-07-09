package org.rustlang.core.ptr.non_null;

import org.rustlang.core.fmt.rt.Argument__;

public final class NonNull_Argument__ {
    public Argument__[] pointer;

    public NonNull_Argument__(Argument__[] pointer) {
        this.pointer = pointer;
    }

    public final boolean eq(NonNull_Argument__ other) {
        return other != null && pointer == other.pointer;
    }
}
