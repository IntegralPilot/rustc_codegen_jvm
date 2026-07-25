public class Main {
    public static void main(String[] args) {
        int result = cargo_jvm_workflow.cargo_jvm_workflow.triple(14);
        if (result != 42) {
            throw new AssertionError("packaged Rust library returned " + result);
        }
        System.out.println("cargo-jvm library package: 42");
    }
}
