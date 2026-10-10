import foreign_interfaces.Counter;
import foreign_interfaces.foreign_interfaces;
import java.util.function.IntUnaryOperator;

public class Main {
    public interface Base { int bias(); }
    public interface Wide extends Base {
        long applyLong(long value, double scale);
        int offset();
        default long twice(long value) { return applyLong(value, 2.0) * 2; }
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Exception {
        Counter counter = new Counter(4);
        Runnable runnable = counter;
        runnable.run();
        foreign_interfaces.run_rust(counter);
        if (counter.value != 6) throw new AssertionError("mutable interface receiver");
        IntUnaryOperator operator = counter;
        if (operator.applyAsInt(3) != 9 || operator.andThen(x -> x * 2).applyAsInt(3) != 18)
            throw new AssertionError("renamed method or Java default method");
        if (foreign_interfaces.apply_rust(counter, 7) != 13
                || foreign_interfaces.apply_rust(x -> x * 3, 7) != 21
                || foreign_interfaces.rust_dispatch() != 12)
            throw new AssertionError("Rust and Java trait dispatch");
        Wide wide = counter;
        if (wide.bias() != 6 || wide.applyLong(4_000_000_000L, 2.0) != 8_000_000_006L
                || wide.offset() != 8 || wide.twice(5) != 32
                || foreign_interfaces.wide_rust(wide) != 24)
            throw new AssertionError("inherited, wide, or Rust default method");
        if (!(counter instanceof java.io.Serializable)) throw new AssertionError("marker interface");
        var generic = foreign_interfaces.make_generic();
        IntUnaryOperator genericOperator = generic;
        if (genericOperator.applyAsInt(6) != 42 || !(generic instanceof java.io.Serializable))
            throw new AssertionError("uncalled methods on an upstream generic implementor");
        Object noncopy = foreign_interfaces.make_noncopy();
        if (noncopy instanceof IntUnaryOperator || noncopy instanceof java.io.Serializable)
            throw new AssertionError("unsatisfied generic impl bounds");
        java.util.function.UnaryOperator<Object> identity = counter;
        Object token = new Object();
        if (identity.apply(token) != token || foreign_interfaces.object_rust(counter, token) != token)
            throw new AssertionError("erased Java object parameter and return");
        try (var jar = new java.util.jar.JarFile(new java.io.File(
                Counter.class.getProtectionDomain().getCodeSource().getLocation().toURI()))) {
            for (String name : new String[] { "java/lang/Runnable.class", "Main$Wide.class",
                    "foreign_interface_api/Runnable.class" }) {
                if (jar.getEntry(name) != null) throw new AssertionError("emitted foreign interface " + name);
            }
        }
        for (Class<?> type : new Class<?>[] { Runnable.class, IntUnaryOperator.class, Wide.class }) {
            if (!type.isAssignableFrom(Counter.class)) throw new AssertionError(type.getName());
        }
    }
}
