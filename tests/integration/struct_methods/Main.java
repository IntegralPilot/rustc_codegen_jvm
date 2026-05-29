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
            NamedCounter.class.getConstructor();
            throw new AssertionError("NamedCounter must not expose a no-args constructor");
        } catch (NoSuchMethodException expected) {
            // Expected: fielded Rust structs need every field.
        }
    }

    private static void assertFieldConstructor() throws Exception {
        Constructor<NamedCounter> constructor =
                NamedCounter.class.getConstructor(String.class, long.class, long.class, boolean.class);
        NamedCounter counter = constructor.newInstance("Michael", 2L, 99L, true);

        if (!counter.name.equals("Michael")) {
            throw new AssertionError("field constructor should initialize name");
        }
        if (counter.count != 2L || counter.limit != 99L || !counter.enabled) {
            throw new AssertionError("field constructor should initialize every field in Rust order");
        }
    }

    private static void assertStaticAssociatedFunctions() throws Exception {
        Method newMethod = NamedCounter.class.getMethod("new", String.class, long.class);
        if (!Modifier.isStatic(newMethod.getModifiers())) {
            throw new AssertionError("NamedCounter.new must be static");
        }

        NamedCounter counter = (NamedCounter) newMethod.invoke(null, "Michael", 99L);
        if (!counter.name.equals("Michael") || counter.count != 0L || counter.limit != 99L || !counter.enabled) {
            throw new AssertionError("static NamedCounter.new should initialize the counter");
        }

        NamedCounter disabled = NamedCounter.new_disabled("Offline", 7L);
        if (!disabled.name.equals("Offline") || disabled.count != 0L || disabled.limit != 7L || disabled.enabled) {
            throw new AssertionError("other no-self associated functions should also be static");
        }
    }

    private static void assertInstanceAndStaticSelfMethods() throws Exception {
        Method newMethod = NamedCounter.class.getMethod("new", String.class, long.class);
        NamedCounter counter = (NamedCounter) newMethod.invoke(null, "Michael", 99L);

        if (counter.get_limit() != 99L) {
            throw new AssertionError("self methods should be callable as instance methods");
        }
        if (NamedCounter.get_limit(counter) != 99L) {
            throw new AssertionError("self methods should also have static receiver bridges");
        }
        if (!counter.increment() || counter.get_count() != 1L || NamedCounter.get_count(counter) != 1L) {
            throw new AssertionError("mutable self methods should update the receiver");
        }
    }

    private static void assertFieldlessNoArgsConstructorRemains() throws Exception {
        EmptyMarker marker = new EmptyMarker();
        if (marker == null) {
            throw new AssertionError("fieldless Rust structs should keep a no-args constructor");
        }
    }

    private static void assertConstantStructUsesDeclarationOrder() {
        OrderedConstant profile = struct_methods.default_profile();
        if (profile.z_value != 36L || !profile.a_flag) {
            throw new AssertionError("constant structs should use declaration-order constructor arguments");
        }
    }
}
