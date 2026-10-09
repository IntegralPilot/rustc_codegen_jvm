public class Main {
    public static void main(String[] args) {
        if (java_exports.java_exports.answer() != 42) throw new AssertionError();
        java_exports.Counter counter = new java_exports.Counter(40);
        if (counter.add(2) != 42 || counter.value != 42) throw new AssertionError();
        if (!(java_exports.api.calls.choose(true) instanceof java_exports.api.Choice.First)) {
            throw new AssertionError();
        }
        export_provider.Shared shared = java_exports.java_exports.upstream(new export_provider.Shared(38));
        if (shared.value != 40) throw new AssertionError();
        if (shared.add(2) != 42 || shared.value != 42) throw new AssertionError();
        org.rustlang.runtime.Pointer internal = java_exports.java_exports.internal_drop_value();
        if (internal.getObject() instanceof org.rustlang.runtime.RustDrop) {
            throw new AssertionError("unexported Rust type should not acquire a Java drop callback");
        }
        java_exports.java_exports.destroy_internal(internal);
        if (java_exports.java_exports.drop_count() != 2) {
            throw new AssertionError("typed and trait-object destruction must still run");
        }
        new java_exports.ExportedDrop(7.0).rustDrop();
        if (java_exports.java_exports.drop_count() != 3) {
            throw new AssertionError("exported Java destruction callback must remain available");
        }
        for (var method : java_exports.java_exports.class.getDeclaredMethods()) {
            if (method.getName().equals("internal_function")) throw new AssertionError();
        }
    }
}
