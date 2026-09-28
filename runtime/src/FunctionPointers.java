package org.rustlang.runtime;

import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.LambdaMetafactory;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandleInfo;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;

/** Shares code identity between constant adapters and invokedynamic reifications. */
public final class FunctionPointers {
    private static final Map<Object, Object> IDENTITIES = new IdentityHashMap<>();
    private static final ClassValue<Map<String, Object>> TARGETS =
            new ClassValue<Map<String, Object>>() {
                protected Map<String, Object> computeValue(Class<?> owner) {
                    return new HashMap<>();
                }
            };

    private FunctionPointers() {}

    private static Object target(Class<?> owner, String method) {
        Map<String, Object> methods = TARGETS.get(owner);
        synchronized (methods) {
            return methods.computeIfAbsent(method, key -> new Object());
        }
    }

    static Object identity(Object value) {
        synchronized (IDENTITIES) {
            Object identity = IDENTITIES.get(value);
            if (identity != null) {
                return identity;
            }
        }
        if (value instanceof StaticFunctionPointer) {
            String name = ((StaticFunctionPointer) value).functionPointerIdentity();
            int separator = name.indexOf("::");
            try {
                Class<?> owner = Class.forName(name.substring(0, separator).replace('/', '.'),
                        false, value.getClass().getClassLoader());
                return target(owner, name.substring(separator + 2));
            } catch (ClassNotFoundException error) {
                throw new IllegalStateException("unknown function pointer target " + name, error);
            }
        }
        return value;
    }

    public static CallSite metafactory(MethodHandles.Lookup lookup, String name,
            MethodType factoryType, MethodType interfaceType, MethodHandle implementation,
            MethodType instantiatedType) throws Throwable {
        CallSite site = LambdaMetafactory.metafactory(lookup, name, factoryType,
                interfaceType, implementation, instantiatedType);
        Object function = site.getTarget().invoke();
        MethodHandleInfo info = lookup.revealDirect(implementation);
        Object identity = target(info.getDeclaringClass(),
                info.getName() + ":" + info.getMethodType().toMethodDescriptorString());
        synchronized (IDENTITIES) {
            IDENTITIES.put(function, identity);
        }
        return new ConstantCallSite(MethodHandles.constant(factoryType.returnType(), function));
    }
}
