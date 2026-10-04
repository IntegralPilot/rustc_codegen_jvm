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

    private static final ClassValue<Map<Object, Object>> CONSTANTS =
            new ClassValue<Map<Object, Object>>() {
                protected Map<Object, Object> computeValue(Class<?> signature) {
                    return new IdentityHashMap<>();
                }
            };

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
        if (value instanceof MethodHandle) {
            MethodHandleInfo info = MethodHandles.lookup().revealDirect((MethodHandle) value);
            return target(info.getDeclaringClass(), info.getName() + ":" + info.getMethodType().toMethodDescriptorString());
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

    /** Share the invocation class per exact ABI instead of per code target. */
    private static final ClassValue<Factory> FACTORIES = new ClassValue<Factory>() {
        protected Factory computeValue(Class<?> signature) { return new Factory(); }
    };

    private static final class Factory {
        private MethodHandle factory;
        private boolean initialized;

        synchronized MethodHandle get(MethodHandles.Lookup lookup, Class<?> signature,
                MethodType type) throws Throwable {
            if (!initialized) {
                if (signature.getName().startsWith("org.rustlang.runtime.FnPtr_")) {
                    try {
                        MethodHandle bridge = lookup.findStatic(signature, "$rust$invoke",
                                type.insertParameterTypes(0, MethodHandle.class));
                        factory = LambdaMetafactory.metafactory(lookup, "call",
                                MethodType.methodType(signature, MethodHandle.class), type,
                                bridge, type).getTarget().asType(
                                        MethodType.methodType(Object.class, MethodHandle.class));
                    } catch (NoSuchMethodException absent) {
                        // Foreign/older interfaces retain their own Java SAM contract.
                    }
                }
                initialized = true;
            }
            return factory;
        }
    }

    public static Object bind(MethodHandles.Lookup lookup, Class<?> signature, MethodHandle implementation) {
        try {
            return constant(lookup, signature, implementation);
        } catch (RuntimeException | Error failure) {
            throw failure;
        } catch (Throwable failure) {
            throw new IllegalStateException("could not link constant Rust function", failure);
        }
    }

    private static Object constant(MethodHandles.Lookup lookup, Class<?> signature,
            MethodHandle implementation) throws Throwable {
        MethodType type = implementation.type();
        MethodHandleInfo info = lookup.revealDirect(implementation);
        Object identity = target(info.getDeclaringClass(),
                info.getName() + ":" + info.getMethodType().toMethodDescriptorString());
        Map<Object, Object> constants = CONSTANTS.get(signature);
        synchronized (constants) {
            Object function = constants.get(identity);
            if (function != null) return function;
            MethodHandle factory = FACTORIES.get(signature).get(lookup, signature, type);
            if (factory != null) {
                function = (Object) factory.invokeExact(implementation);
            } else {
                CallSite site = LambdaMetafactory.metafactory(lookup, "call",
                        MethodType.methodType(signature), type, implementation, type);
                function = site.getTarget().invoke();
            }
            synchronized (IDENTITIES) { IDENTITIES.put(function, identity); }
            constants.put(identity, function);
            return function;
        }
    }

    public static CallSite metafactory(MethodHandles.Lookup lookup, String name,
            MethodType factoryType, MethodType interfaceType, MethodHandle implementation,
            MethodType instantiatedType) throws Throwable {
        if (!name.equals("call") || factoryType.parameterCount() != 0) {
            return LambdaMetafactory.metafactory(lookup, name, factoryType,
                    interfaceType, implementation, instantiatedType);
        }
        // Executed reifications use direct JVM calls. Constant vtable entries use
        // shared adapters in bind() to avoid hidden classes for unused targets.
        CallSite site = LambdaMetafactory.metafactory(lookup, name, factoryType,
                interfaceType, implementation, instantiatedType);
        Object function = site.getTarget().invoke();
        MethodHandleInfo info = lookup.revealDirect(implementation);
        Object identity = target(info.getDeclaringClass(),
                info.getName() + ":" + info.getMethodType().toMethodDescriptorString());
        synchronized (IDENTITIES) { IDENTITIES.put(function, identity); }
        return new ConstantCallSite(MethodHandles.constant(factoryType.returnType(), function));
    }
}
