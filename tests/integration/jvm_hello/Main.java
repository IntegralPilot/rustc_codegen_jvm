import org.rustlang.runtime.Utf8View;

public class Main {
    public static void main(String[] args) {
        jvm_hello.Ciallo data = new jvm_hello.Ciallo(233, Utf8View.fromJavaString("wooooooooo"));
        String result = Utf8View.toJavaString(jvm_hello.jvm_hello.ciallo(data));
        if (result.equals("Hello from Rust!")) {
            System.out.println("Test passed: " + result);
        } else {
            throw new AssertionError("Test failed: expected 'Hello from Rust!' but got '" + result + "'");
        }
    }
}
