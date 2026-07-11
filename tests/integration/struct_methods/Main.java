import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public class Main {
    public static void main(String[] args) throws Exception {
        assertNoFieldedNoArgsConstructor();
        assertFieldConstructor();
        assertStaticAssociatedFunctions();
        assertInstanceAndStaticSelfMethods();
        assertFieldlessNoArgsConstructorRemains();
        assertConstantStructUsesDeclarationOrder();

        System.out.println("Struct method mapping test passed!");
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
                        long.class,
                        long.class,
                        boolean.class);
        struct_methods.NamedCounter counter = constructor.newInstance(
                org.rustlang.runtime.Utf8View.fromJavaString("Michael"), 2L, 99L, true);

        if (!org.rustlang.runtime.Utf8View.toJavaString(counter.name).equals("Michael")) {
            throw new AssertionError("field constructor should initialize name");
        }
        if (counter.count != 2L || counter.limit != 99L || !counter.enabled) {
            throw new AssertionError("field constructor should initialize every field in Rust order");
        }
    }

    private static void assertStaticAssociatedFunctions() throws Exception {
        Method newMethod = struct_methods.NamedCounter.class.getMethod(
                "new", org.rustlang.runtime.Utf8View.class, long.class);
        if (!Modifier.isStatic(newMethod.getModifiers())) {
            throw new AssertionError("NamedCounter.new must be static");
        }

        struct_methods.NamedCounter counter = (struct_methods.NamedCounter) newMethod.invoke(
                null, org.rustlang.runtime.Utf8View.fromJavaString("Michael"), 99L);
        if (!org.rustlang.runtime.Utf8View.toJavaString(counter.name).equals("Michael")
                || counter.count != 0L || counter.limit != 99L || !counter.enabled) {
            throw new AssertionError("static NamedCounter.new should initialize the counter");
        }

        struct_methods.NamedCounter disabled = struct_methods.NamedCounter.new_disabled(
                org.rustlang.runtime.Utf8View.fromJavaString("Offline"), 7L);
        if (!org.rustlang.runtime.Utf8View.toJavaString(disabled.name).equals("Offline")
                || disabled.count != 0L || disabled.limit != 7L || disabled.enabled) {
            throw new AssertionError("other no-self associated functions should also be static");
        }
    }

    private static void assertInstanceAndStaticSelfMethods() throws Exception {
        Method newMethod = struct_methods.NamedCounter.class.getMethod(
                "new", org.rustlang.runtime.Utf8View.class, long.class);
        struct_methods.NamedCounter counter = (struct_methods.NamedCounter) newMethod.invoke(
                null, org.rustlang.runtime.Utf8View.fromJavaString("Michael"), 99L);

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

    private static void assertConstantStructUsesDeclarationOrder() {
        struct_methods.OrderedConstant profile = struct_methods.struct_methods.default_profile();
        if (profile.z_value != 36L || !profile.a_flag) {
            throw new AssertionError("constant structs should use declaration-order constructor arguments");
        }
    }
}
