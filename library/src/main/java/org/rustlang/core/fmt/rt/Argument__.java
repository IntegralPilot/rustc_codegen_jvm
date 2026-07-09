package org.rustlang.core.fmt.rt;

import org.rustlang.core.Core;

public final class Argument__ {
    public Object value;
    public ArgumentType__ ty;

    public Argument__(Object value) {
        this.value = value;
    }

    public Argument__(ArgumentType__ ty) {
        this.ty = ty;
        this.value = Core.formatArgumentType(ty);
    }

    public static Argument__ new_display(Object value) {
        return Core.new_display(value);
    }

    @Override
    public String toString() {
        return value == null ? "" : value.toString();
    }
}
