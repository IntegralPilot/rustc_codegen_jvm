package org.rustlang.core.fmt.rt;

public abstract class ArgumentType__ {
    public ArgumentType__() {}

    public abstract int getVariantIdx();

    public final boolean is_none() {
        return getVariantIdx() != 0;
    }

    public final boolean is_some() {
        return getVariantIdx() == 0;
    }

    public final boolean eq(ArgumentType__ other) {
        return other != null && getVariantIdx() == other.getVariantIdx();
    }
}
