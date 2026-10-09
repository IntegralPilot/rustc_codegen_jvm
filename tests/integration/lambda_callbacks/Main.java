import org.rustlang.runtime.FnPtr_int_to_int;

public class Main {
    public static int addFromHandle(int value) { return value + 1; }
    public static int addTwo(int value) { return value + 2; }
    public static int addThree(int value) { return value + 3; }
    public static int addFour(int value) { return value + 4; }
    public static int addFive(int value) { return value + 5; }

    public static int throwFromHandle(int value) {
        throw new IllegalArgumentException("shared handle " + value);
    }

    public static void main(String[] args) {
        if (lambda_callbacks.lambda_callbacks.rust_capturing_closure(5).call(37) != 42) {
            throw new AssertionError("returned Rust closure must remain callable from Java");
        }
        if (lambda_callbacks.lambda_callbacks.rust_closure_test(10) != 42) {
            throw new AssertionError("Rust closure callback test failed");
        }

        // A Java lambda can be passed directly at the Rust API boundary.
        if (lambda_callbacks.lambda_callbacks.apply_i32(value -> value + 1, 41) != 42) {
            throw new AssertionError("direct Java lambda failed");
        }

        FnPtr_int_to_int triple = value -> value * 3;
        if (lambda_callbacks.lambda_callbacks.apply_twice(triple, 2) != 18) {
            throw new AssertionError("reused Java lambda failed");
        }

        int[] calls = {0};
        int mutableResult = lambda_callbacks.lambda_callbacks.apply_mut_i32(value -> {
            calls[0]++;
            return value + calls[0];
        }, 9);
        if (mutableResult != 10 || calls[0] != 1) {
            throw new AssertionError("capturing/mutating Java lambda failed");
        }

        if (lambda_callbacks.lambda_callbacks.combine_i32((left, right) -> left * 10 + right, 4, 2) != 42) {
            throw new AssertionError("two-argument Java lambda failed");
        }
        if (lambda_callbacks.lambda_callbacks.choose_i32(value -> value > 0, 7, 42, -1) != 42) {
            throw new AssertionError("boolean-returning Java lambda failed");
        }
        if (lambda_callbacks.lambda_callbacks.supply_i32(() -> 42) != 42) {
            throw new AssertionError("zero-argument Java lambda failed");
        }
        if (lambda_callbacks.lambda_callbacks.apply_f64(value -> value * 2.0, 21.0) != 42.0) {
            throw new AssertionError("double Java lambda failed");
        }
        int[] unitCalls = {0};
        lambda_callbacks.lambda_callbacks.call_unit(() -> unitCalls[0]++);
        if (unitCalls[0] != 1) {
            throw new AssertionError("unit/void Java lambda failed");
        }
        if (lambda_callbacks.lambda_callbacks.rust_fn_mut_test(5) != 35) {
            throw new AssertionError("Rust FnMut callback bridge failed");
        }
        if (lambda_callbacks.lambda_callbacks.rust_fn_pointer_dyn_test(39) != 42) {
            throw new AssertionError("Rust function-pointer dyn Fn bridge failed");
        }
        if (lambda_callbacks.lambda_callbacks.rust_fn_pointer_dyn_mut_test(39) != 42
                || lambda_callbacks.lambda_callbacks.rust_fn_pointer_dyn_reload_test(13) != 42) {
            throw new AssertionError("Rust function-pointer callable adapters failed");
        }

        FnPtr_int_to_int rustFunction = lambda_callbacks.lambda_callbacks.rust_function_pointer();
        FnPtr_int_to_int rustClosure = lambda_callbacks.lambda_callbacks.rust_non_capturing_closure_pointer();
        if (rustFunction.call(39) != 42 || rustClosure.call(21) != 42) {
            throw new AssertionError("Rust invokedynamic function pointers failed");
        }
        if (!rustFunction.getClass().isSynthetic() || !rustClosure.getClass().isSynthetic()) {
            throw new AssertionError("stateless Rust callables should use JVM lambda classes");
        }
        if (rustFunction != lambda_callbacks.lambda_callbacks.rust_function_pointer()) {
            throw new AssertionError("reifying the same code target must retain its canonical callable");
        }
        try {
            java.lang.invoke.MethodHandles.Lookup lookup = java.lang.invoke.MethodHandles.lookup();
            java.lang.invoke.MethodHandle target = lookup.findStatic(Main.class, "throwFromHandle",
                    java.lang.invoke.MethodType.methodType(int.class, int.class));
            FnPtr_int_to_int throwing = (FnPtr_int_to_int) org.rustlang.runtime.FunctionPointers.bind(
                    lookup, FnPtr_int_to_int.class, target);
            FnPtr_int_to_int adding = (FnPtr_int_to_int) org.rustlang.runtime.FunctionPointers.bind(
                    lookup, FnPtr_int_to_int.class, lookup.findStatic(Main.class, "addFromHandle",
                            java.lang.invoke.MethodType.methodType(int.class, int.class)));
            if (adding.getClass() != throwing.getClass() || adding.call(41) != 42) {
                throw new AssertionError("constant targets with one ABI should share their invocation class");
            }
            if (throwing != org.rustlang.runtime.FunctionPointers.bind(lookup, FnPtr_int_to_int.class, target)) {
                throw new AssertionError("constant target should retain its canonical callable");
            }
            String[] names = {"addFromHandle", "addTwo", "addThree", "addFour", "addFive"};
            FnPtr_int_to_int[] functions = new FnPtr_int_to_int[names.length];
            for (int i = 0; i < names.length; i++) {
                functions[i] = (FnPtr_int_to_int) org.rustlang.runtime.FunctionPointers.bind(
                        lookup, FnPtr_int_to_int.class, lookup.findStatic(Main.class, names[i],
                                java.lang.invoke.MethodType.methodType(int.class, int.class)));
            }
            for (int repeat = 0; repeat < 2; repeat++) {
                for (int i = 0; i < functions.length; i++) {
                    if (functions[i].call(10) != 11 + i) throw new AssertionError("constant call target changed");
                }
            }
            try {
                throwing.call(17);
                throw new AssertionError("shared invocation swallowed an exception");
            } catch (IllegalArgumentException expected) {
                if (!expected.getMessage().equals("shared handle 17")) throw expected;
            }
        } catch (ReflectiveOperationException error) {
            throw new AssertionError(error);
        }
    }
}
