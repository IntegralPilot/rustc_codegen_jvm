public class Main {
    public static void main(String[] args) {
        Ciallo data = new Ciallo(233, "wooooooooo");
        String result = jvm_hello.ciallo(data);     
        if (result.equals("Hello from Rust!")) {
            System.out.println("Test passed: " + result);
        } else {
            throw new AssertionError("Test failed: expected 'Hello from Rust!' but got '" + result + "'");
        }
    }
}
