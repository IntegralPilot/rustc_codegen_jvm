public class Main {
    public static void main(String[] args) {
        int product = cdylib_interop.cdylib_interop.multiply(6, 7);
        int sum = org.rustlang.runtime.symbols.exported_add.exported_add(20, 22);
        if (product != 42 || sum != 42) {
            throw new AssertionError(
                    "cdylib interop failed: product=" + product + ", sum=" + sum);
        }
    }
}
