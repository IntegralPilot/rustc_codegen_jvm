import org.rustlang.runtime.FnPtr_int_to_int;

public class Main {
    public static void main(String[] args) {
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

        FnPtr_int_to_int rustFunction = lambda_callbacks.lambda_callbacks.rust_function_pointer();
        FnPtr_int_to_int rustClosure = lambda_callbacks.lambda_callbacks.rust_non_capturing_closure_pointer();
        if (rustFunction.call(39) != 42 || rustClosure.call(21) != 42) {
            throw new AssertionError("Rust invokedynamic function pointers failed");
        }
        if (!rustFunction.getClass().isSynthetic() || !rustClosure.getClass().isSynthetic()) {
            throw new AssertionError("stateless Rust callables should use JVM lambda classes");
        }
    }
}
