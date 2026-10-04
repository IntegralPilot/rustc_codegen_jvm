package org.rustlang.runtime;

import java.lang.invoke.CallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.MutableCallSite;

/** Links owned reads to exact decoders while retaining general storage fallbacks. */
public final class OwnedReads {
    private static final MethodHandle FALLBACK, MATCHES, BYTES, OFFSET, GENERIC;

    static {
        try {
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            FALLBACK = lookup.findVirtual(Site.class, "read",
                    MethodType.methodType(Object.class, Object.class, int.class));
            MATCHES = lookup.findStatic(Pointer.class, "ownedSliceMatches",
                    MethodType.methodType(boolean.class, String.class, int.class, Object.class));
            BYTES = lookup.findStatic(Pointer.class, "ownedSliceBytes",
                    MethodType.methodType(byte[].class, Object.class));
            OFFSET = lookup.findStatic(Pointer.class, "ownedSliceOffset",
                    MethodType.methodType(int.class, Object.class, int.class));
            GENERIC = lookup.findStatic(Pointer.class, "sliceGetObjectCopy",
                    MethodType.methodType(Object.class, Object.class, int.class, String.class));
        } catch (ReflectiveOperationException error) {
            throw new ExceptionInInitializerError(error);
        }
    }

    private OwnedReads() {}

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String name, MethodType type) {
        return new Site(type);
    }

    private static final class Site extends MutableCallSite {
        private final MethodHandle generic;
        private int depth;

        Site(MethodType type) {
            super(type);
            String target = type.returnType().getName();
            generic = MethodHandles.insertArguments(GENERIC, 2, target).asType(type);
            setTarget(FALLBACK.bindTo(this).asType(type));
        }

        Object read(Object backing, int index) throws Throwable {
            link(backing);
            return getTarget().invoke(backing, index);
        }

        private synchronized void link(Object backing) throws Throwable {
            String recipe = Pointer.ownedSliceRecipe(backing, type().returnType());
            MethodHandle decoder = recipe == null ? null
                    : Pointer.codecPlan(recipe).directRangeDecoder();
            if (decoder == null || depth >= 4) {
                setTarget(generic);
            } else {
                int size = Pointer.ownedSliceSize(backing);
                MethodHandle decode = MethodHandles.filterArguments(decoder, 0, BYTES);
                decode = MethodHandles.collectArguments(decode, 1, OFFSET);
                decode = MethodHandles.permuteArguments(decode, type(), 0, 0, 1);
                MethodHandle test = MethodHandles.insertArguments(MATCHES, 0, recipe, size);
                setTarget(MethodHandles.guardWithTest(test, decode, getTarget()));
                depth++;
            }
            MutableCallSite.syncAll(new MutableCallSite[] {this});
        }
    }
}
