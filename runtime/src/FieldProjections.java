package org.rustlang.runtime;

import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.ref.WeakReference;

/** Reuse repeated field borrows without keeping their owners alive. */
public final class FieldProjections {
    private static final MethodHandle FIELD;
    private volatile WeakReference<Pointer> previous;

    static {
        try {
            FIELD = MethodHandles.lookup().findVirtual(FieldProjections.class, "field",
                    MethodType.methodType(Pointer.class, Object.class, String.class, long.class, String.class));
        } catch (ReflectiveOperationException error) {
            throw new ExceptionInInitializerError(error);
        }
    }

    private FieldProjections() {}

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String name, MethodType type) {
        return new ConstantCallSite(FIELD.bindTo(new FieldProjections()).asType(type));
    }

    private Pointer field(Object owner, String name, long size, String codec) {
        WeakReference<Pointer> reference = previous;
        Pointer pointer = reference == null ? null : reference.get();
        if (Pointer.cachedFieldMatches(pointer, owner, name, size, codec)) return pointer;
        pointer = Pointer.field(owner, name, size, codec);
        previous = new WeakReference<>(pointer);
        return pointer;
    }
}
