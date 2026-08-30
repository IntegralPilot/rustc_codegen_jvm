package org.rustlang.runtime;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.util.concurrent.ConcurrentHashMap;

/** Runtime support for awaiting generated Rust futures from Kotlin. */
public final class KotlinFutureInterop {
    /** Returned by {@link RustFuture#poll(Runnable)} while the future is pending. */
    public static final Object PENDING = new Object();

    /** Represents Rust's zero-sized unit result without depending on kotlin.Unit. */
    public static final Object UNIT = new Object();

    private static final ConcurrentHashMap<Class<?>, PollAdapter> POLL_ADAPTERS =
            new ConcurrentHashMap<Class<?>, PollAdapter>();

    private KotlinFutureInterop() {}

    /** Called by generated RawWaker functions. */
    public static void wake(Pointer data) {
        Object value = data.getObject();
        if (!(value instanceof Runnable)) {
            throw new IllegalStateException("Rust async waker does not contain a Runnable");
        }
        ((Runnable) value).run();
    }

    /** Invoked by the small method generated on every Rust coroutine class. */
    public static Object pollRustFuture(
            Object future,
            Runnable wake,
            int futureSize,
            String futureCodec,
            int futureAlignment,
            int wakerVtableSize,
            String wakerVtableCodec,
            int wakerVtableAlignment) {
        Class<?> futureClass = future.getClass();
        PollAdapter adapter = POLL_ADAPTERS.get(futureClass);
        if (adapter == null) {
            PollAdapter created = new PollAdapter(
                    futureClass,
                    futureSize,
                    futureCodec,
                    futureAlignment,
                    wakerVtableSize,
                    wakerVtableCodec,
                    wakerVtableAlignment);
            PollAdapter existing = POLL_ADAPTERS.putIfAbsent(futureClass, created);
            adapter = existing == null ? created : existing;
        }
        return adapter.poll(future, wake);
    }

    /** Drops a completed or abandoned Rust async state machine exactly once. */
    public static void dropRustFuture(RustFuture future) {
        Pointer.dropRustValue(future);
    }

    private static void throwUnchecked(Throwable failure) {
        KotlinFutureInterop.<RuntimeException>throwAny(failure);
    }

    @SuppressWarnings("unchecked")
    private static <T extends Throwable> void throwAny(Throwable failure) throws T {
        throw (T) failure;
    }

    /** Cached reflection needed to enter a monomorphized Rust coroutine. */
    private static final class PollAdapter {
        private final Constructor<?> rawWakerConstructor;
        private final Constructor<?> wakerConstructor;
        private final Constructor<?> contextConstructor;
        private final Constructor<?> pinConstructor;
        private final Method resume;
        private final Pointer vtablePointer;
        private final int futureSize;
        private final String futureCodec;
        private final int futureAlignment;

        private PollAdapter(
                Class<?> futureClass,
                int futureSize,
                String futureCodec,
                int futureAlignment,
                int wakerVtableSize,
                String wakerVtableCodec,
                int wakerVtableAlignment) {
            try {
                ClassLoader loader = futureClass.getClassLoader();
                Class<?> rawWakerClass = Class.forName(
                        "org.rustlang.core.task.wake.RawWaker", true, loader);
                Class<?> rawWakerVTableClass = Class.forName(
                        "org.rustlang.core.task.wake.RawWakerVTable", true, loader);
                Class<?> wakerClass = Class.forName(
                        "org.rustlang.core.task.wake.Waker", true, loader);
                Class<?> contextClass = Class.forName(
                        "org.rustlang.core.task.wake.Context", true, loader);
                Class<?> pinClass = Class.forName(
                        "org.rustlang.core.pin.Pin_MutRef" + futureClass.getSimpleName(),
                        true,
                        loader);

                rawWakerConstructor = rawWakerClass.getConstructor(Pointer.class, Pointer.class);
                Constructor<?> vtableConstructor = constructorWithArity(rawWakerVTableClass, 4);
                Class<?>[] vtableParameters = vtableConstructor.getParameterTypes();
                final Pointer[] vtableHolder = new Pointer[1];

                Object cloneFunction = Proxy.newProxyInstance(
                        loader,
                        new Class<?>[] {vtableParameters[0]},
                        new InvocationHandler() {
                            @Override
                            public Object invoke(Object proxy, Method method, Object[] args)
                                    throws Throwable {
                                Object objectMethod = handleObjectMethod(proxy, method, args);
                                if (objectMethod != NOT_AN_OBJECT_METHOD) {
                                    return objectMethod;
                                }
                                return rawWakerConstructor.newInstance(args[0], vtableHolder[0]);
                            }
                        });
                InvocationHandler wakeHandler =
                        new InvocationHandler() {
                            @Override
                            public Object invoke(Object proxy, Method method, Object[] args) {
                                Object objectMethod = handleObjectMethod(proxy, method, args);
                                if (objectMethod != NOT_AN_OBJECT_METHOD) {
                                    return objectMethod;
                                }
                                KotlinFutureInterop.wake((Pointer) args[0]);
                                return null;
                            }
                        };
                Object wakeFunction = Proxy.newProxyInstance(
                        loader, new Class<?>[] {vtableParameters[1]}, wakeHandler);
                Object wakeByRefFunction = Proxy.newProxyInstance(
                        loader, new Class<?>[] {vtableParameters[2]}, wakeHandler);
                Object noopFunction = Proxy.newProxyInstance(
                        loader,
                        new Class<?>[] {vtableParameters[3]},
                        new InvocationHandler() {
                            @Override
                            public Object invoke(Object proxy, Method method, Object[] args) {
                                Object objectMethod = handleObjectMethod(proxy, method, args);
                                return objectMethod == NOT_AN_OBJECT_METHOD ? null : objectMethod;
                            }
                        });
                Object vtable = vtableConstructor.newInstance(
                        cloneFunction, wakeFunction, wakeByRefFunction, noopFunction);
                vtablePointer = Pointer.cellAligned(
                        vtable,
                        wakerVtableSize,
                        wakerVtableCodec,
                        wakerVtableAlignment);
                vtableHolder[0] = vtablePointer;

                wakerConstructor = wakerClass.getConstructor(rawWakerClass);
                contextConstructor = rustConstructor(contextClass);
                pinConstructor = pinClass.getConstructor(Pointer.class);
                resume = findResumeMethod(futureClass, pinClass);
                this.futureSize = futureSize;
                this.futureCodec = futureCodec;
                this.futureAlignment = futureAlignment;
            } catch (ReflectiveOperationException error) {
                throw new IllegalStateException(
                        "Could not prepare Rust async adapter for " + futureClass.getName(), error);
            }
        }

        private Object poll(Object future, Runnable wake) {
            try {
                Pointer wakePointer = Pointer.cell(wake);
                Object rawWaker = rawWakerConstructor.newInstance(wakePointer, vtablePointer);
                Object waker = wakerConstructor.newInstance(rawWaker);
                Pointer wakerPointer = Pointer.cell(waker);
                Class<?>[] contextParameters = contextConstructor.getParameterTypes();
                Object[] contextArguments = new Object[contextParameters.length];
                contextArguments[0] = wakerPointer;
                contextArguments[1] = wakerPointer;
                for (int index = 2; index < contextParameters.length; index++) {
                    contextArguments[index] = defaultRustValue(contextParameters[index]);
                }
                Object context = contextConstructor.newInstance(contextArguments);
                Pointer futurePointer = Pointer.receiverCellAligned(
                        future, futureSize, futureCodec, futureAlignment);
                Object pin = pinConstructor.newInstance(futurePointer);
                Object result = resume.invoke(null, pin, Pointer.cell(context));

                if (result.getClass().getName().endsWith("$Pending")) {
                    return PENDING;
                }
                try {
                    Field output = result.getClass().getField("field0");
                    return output.get(result);
                } catch (NoSuchFieldException noOutput) {
                    return UNIT;
                }
            } catch (InvocationTargetException error) {
                throwUnchecked(PanicSupport.detachForForeignBoundary(error.getCause()));
                return null;
            } catch (ReflectiveOperationException error) {
                throw new IllegalStateException("Could not poll Rust async value", error);
            }
        }

        private static Constructor<?> constructorWithArity(Class<?> type, int arity)
                throws NoSuchMethodException {
            for (Constructor<?> constructor : type.getConstructors()) {
                if (constructor.getParameterTypes().length == arity) {
                    return constructor;
                }
            }
            throw new NoSuchMethodException(type.getName() + " constructor with arity " + arity);
        }

        private static Constructor<?> rustConstructor(Class<?> type) throws NoSuchMethodException {
            for (Constructor<?> constructor : type.getConstructors()) {
                Class<?>[] parameters = constructor.getParameterTypes();
                if (parameters.length >= 2
                        && parameters[0] == Pointer.class
                        && parameters[1] == Pointer.class
                        && (parameters.length == 2 || parameters[2] != long.class)) {
                    return constructor;
                }
            }
            throw new NoSuchMethodException(type.getName() + " Rust-value constructor");
        }

        private static Object defaultRustValue(Class<?> type) throws ReflectiveOperationException {
            if (type.isInterface()) {
                Class<?> none = Class.forName(
                        type.getName() + "$None", true, type.getClassLoader());
                return none.getConstructor().newInstance();
            }
            try {
                return type.getConstructor().newInstance();
            } catch (NoSuchMethodException noDefaultConstructor) {
                Constructor<?> constructor = constructorWithArity(type, 1);
                return constructor.newInstance(defaultRustValue(constructor.getParameterTypes()[0]));
            }
        }

        private static Method findResumeMethod(Class<?> futureClass, Class<?> pinClass)
                throws ReflectiveOperationException {
            String packageName = futureClass.getPackage().getName();
            String crateName = packageName.contains(".")
                    ? packageName.substring(0, packageName.indexOf('.'))
                    : packageName;
            Class<?> moduleClass = Class.forName(
                    crateName + "." + crateName, true, futureClass.getClassLoader());
            for (Method method : moduleClass.getMethods()) {
                Class<?>[] parameters = method.getParameterTypes();
                if (Modifier.isStatic(method.getModifiers())
                        && parameters.length == 2
                        && parameters[0] == pinClass
                        && parameters[1] == Pointer.class
                        && method.getReturnType().getName().contains(".task.poll.Poll_")) {
                    return method;
                }
            }
            throw new NoSuchMethodException(
                    "Rust coroutine resume method for " + futureClass.getName());
        }
    }

    private static final Object NOT_AN_OBJECT_METHOD = new Object();

    private static Object handleObjectMethod(Object proxy, Method method, Object[] args) {
        if (method.getDeclaringClass() != Object.class) {
            return NOT_AN_OBJECT_METHOD;
        }
        String name = method.getName();
        if ("toString".equals(name)) {
            return "Rust async RawWaker function";
        }
        if ("hashCode".equals(name)) {
            return System.identityHashCode(proxy);
        }
        if ("equals".equals(name)) {
            return proxy == args[0];
        }
        return NOT_AN_OBJECT_METHOD;
    }
}
