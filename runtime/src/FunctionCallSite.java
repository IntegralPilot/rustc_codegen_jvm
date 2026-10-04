package org.rustlang.runtime;

import java.lang.invoke.CallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.MutableCallSite;
import java.lang.invoke.WrongMethodTypeException;
import java.util.ArrayList;

/** A few executed targets become direct calls without a class per cold target. */
public final class FunctionCallSite extends MutableCallSite {
    private static final int LIMIT = 4;
    private static final MethodHandle SAME, LINK;
    static {
        try {
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            SAME = lookup.findStatic(FunctionCallSite.class, "same",
                    MethodType.methodType(boolean.class, MethodHandle.class, MethodHandle.class));
            LINK = lookup.findVirtual(FunctionCallSite.class, "link",
                    MethodType.methodType(Object.class, MethodHandle.class, Object[].class));
        } catch (ReflectiveOperationException error) {
            throw new ExceptionInInitializerError(error);
        }
    }
    private final ArrayList<MethodHandle> targets = new ArrayList<>(LIMIT);
    private final MethodHandle linker, generic;

    private FunctionCallSite(MethodType type) {
        super(type);
        generic = MethodHandles.exactInvoker(type.dropParameterTypes(0, 1));
        linker = LINK.bindTo(this).asCollector(Object[].class, type.parameterCount() - 1).asType(type);
        setTarget(linker);
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String name, MethodType type) {
        return new FunctionCallSite(type);
    }

    private static boolean same(MethodHandle expected, MethodHandle actual) {
        return expected == actual;
    }

    private Object link(MethodHandle implementation, Object[] arguments) throws Throwable {
        if (!implementation.type().equals(type().dropParameterTypes(0, 1))) {
            throw new WrongMethodTypeException("Rust function ABI does not match its target");
        }
        synchronized (this) {
            if (targets.size() < LIMIT && !targets.contains(implementation)) {
                targets.add(implementation);
                MethodHandle chain = targets.size() == LIMIT ? generic : linker;
                for (MethodHandle target : targets) {
                    chain = MethodHandles.guardWithTest(SAME.bindTo(target),
                            MethodHandles.dropArguments(target, 0, MethodHandle.class), chain);
                }
                setTarget(chain);
                MutableCallSite.syncAll(new MutableCallSite[] {this});
            }
        }
        // Cached calls use the exact ABI and do not allocate an argument array.
        return implementation.invokeWithArguments(arguments);
    }
}
