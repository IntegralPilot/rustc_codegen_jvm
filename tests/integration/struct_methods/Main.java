import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public class Main {
    public static void main(String[] args) throws Exception {
        assertNoFieldedNoArgsConstructor();
        assertFieldConstructor();
        assertStaticAssociatedFunctions();
        assertInstanceAndStaticSelfMethods();
        assertRustFinalizeIsNotJavaFinalizer();
        assertFieldlessNoArgsConstructorRemains();
        assertConstantStructUsesDeclarationOrder();
        assertPrivateCarrierHasNoReceiverBridges();
        assertOptionalViewBridges();

        System.out.println("Struct method mapping test passed!");
    }

    private static void assertOptionalViewBridges() {
        struct_methods.OptionalViewBridge bridge = new struct_methods.OptionalViewBridge();
        org.rustlang.runtime.Utf8View empty = org.rustlang.runtime.Utf8View.fromJavaString("");
        if (bridge.has_value(null) || bridge.has_value(bridge.round_trip(null))
                || !bridge.has_value(empty) || !bridge.has_value(bridge.round_trip(empty))) {
            throw new AssertionError("optional view bridges must distinguish None from empty borrows");
        }
    }

    private static void assertNoFieldedNoArgsConstructor() {
        try {
            struct_methods.NamedCounter.class.getConstructor();
            throw new AssertionError("NamedCounter must not expose a no-args constructor");
        } catch (NoSuchMethodException expected) {
            // Expected: fielded Rust structs need every field.
        }
    }

    private static void assertFieldConstructor() throws Exception {
        Constructor<struct_methods.NamedCounter> constructor =
                struct_methods.NamedCounter.class.getConstructor(
                        org.rustlang.runtime.Utf8View.class,
                        int.class,
                        int.class,
                        boolean.class);
        struct_methods.NamedCounter counter = constructor.newInstance(
                org.rustlang.runtime.Utf8View.fromJavaString("Michael"), 2, 99, true);

        if (!org.rustlang.runtime.Utf8View.toJavaString(counter.name).equals("Michael")) {
            throw new AssertionError("field constructor should initialize name");
        }
        if (counter.count != 2L || counter.limit != 99L || !counter.enabled) {
            throw new AssertionError("field constructor should initialize every field in Rust order");
        }
    }

    private static void assertStaticAssociatedFunctions() throws Exception {
        Method newMethod = struct_methods.NamedCounter.class.getMethod(
                "new", org.rustlang.runtime.Utf8View.class, int.class);
        if (!Modifier.isStatic(newMethod.getModifiers())) {
            throw new AssertionError("NamedCounter.new must be static");
        }

        struct_methods.NamedCounter counter = (struct_methods.NamedCounter) newMethod.invoke(
                null, org.rustlang.runtime.Utf8View.fromJavaString("Michael"), 99);
        if (!org.rustlang.runtime.Utf8View.toJavaString(counter.name).equals("Michael")
                || counter.count != 0L || counter.limit != 99L || !counter.enabled) {
            throw new AssertionError("static NamedCounter.new should initialize the counter");
        }

        struct_methods.NamedCounter disabled = struct_methods.NamedCounter.new_disabled(
                org.rustlang.runtime.Utf8View.fromJavaString("Offline"), 7);
        if (!org.rustlang.runtime.Utf8View.toJavaString(disabled.name).equals("Offline")
                || disabled.count != 0L || disabled.limit != 7L || disabled.enabled) {
            throw new AssertionError("other no-self associated functions should also be static");
        }
    }

    private static void assertInstanceAndStaticSelfMethods() throws Exception {
        Method newMethod = struct_methods.NamedCounter.class.getMethod(
                "new", org.rustlang.runtime.Utf8View.class, int.class);
        struct_methods.NamedCounter counter = (struct_methods.NamedCounter) newMethod.invoke(
                null, org.rustlang.runtime.Utf8View.fromJavaString("Michael"), 99);

        if (counter.get_limit() != 99L) {
            throw new AssertionError("self methods should be callable as instance methods");
        }
        if (struct_methods.NamedCounter.get_limit(counter) != 99L) {
            throw new AssertionError("self methods should also have static receiver bridges");
        }
        if (!counter.increment() || counter.get_count() != 1L || struct_methods.NamedCounter.get_count(counter) != 1L) {
            throw new AssertionError("mutable self methods should update the receiver");
        }
    }

    private static void assertFieldlessNoArgsConstructorRemains() throws Exception {
        struct_methods.EmptyMarker marker = new struct_methods.EmptyMarker();
        if (marker == null) {
            throw new AssertionError("fieldless Rust structs should keep a no-args constructor");
        }
    }

    private static void assertRustFinalizeIsNotJavaFinalizer() throws Exception {
        for (Method method : struct_methods.NamedCounter.class.getDeclaredMethods()) {
            if (method.getName().equals("finalize") && method.getParameterCount() == 0
                    && method.getReturnType() == void.class) {
                throw new AssertionError("Rust finalize must not override Object.finalize");
            }
        }

        struct_methods.NamedCounter counter = new struct_methods.NamedCounter(
                org.rustlang.runtime.Utf8View.fromJavaString("Finalizer"), 0, 99, true);
        struct_methods.NamedCounter.class.getMethod("finalize$rust").invoke(counter);
        struct_methods.NamedCounter.class.getMethod("finalize$rust", struct_methods.NamedCounter.class)
                .invoke(null, counter);
        counter.finish();
        if (counter.count != 3) {
            throw new AssertionError("renamed finalize must work through instance, static and Rust calls");
        }
        if (struct_methods.struct_methods.finish_number() != 8) {
            throw new AssertionError("renamed finalize must work through a Rust trait adapter");
        }
        if (struct_methods.Finalize.class.getMethod("finalize$rust").getReturnType() != void.class) {
            throw new AssertionError("exported traits must use the same renamed finalize method");
        }
    }

    private static void assertConstantStructUsesDeclarationOrder() {
        struct_methods.OrderedConstant profile = struct_methods.struct_methods.default_profile();
        if (profile.z_value != 36L || !profile.a_flag) {
            throw new AssertionError("constant structs should use declaration-order constructor arguments");
        }
    }

    private static void assertPrivateCarrierHasNoReceiverBridges() throws Exception {
        if (struct_methods.struct_methods.private_counter() != 12) {
            throw new AssertionError("private Rust calls and destruction must still run");
        }
        Class<?> carrier;
        try {
            carrier = Class.forName("struct_methods.PrivateCounter");
        } catch (ClassNotFoundException eliminatedCarrier) {
            return;
        }
        for (Method method : carrier.getDeclaredMethods()) {
            if (method.getName().equals("advance") || method.getName().equals("drop")
                    || method.getName().equals("rustDrop")) {
                throw new AssertionError("private carrier should not have an unused receiver bridge: " + method);
            }
        }
    }
}
