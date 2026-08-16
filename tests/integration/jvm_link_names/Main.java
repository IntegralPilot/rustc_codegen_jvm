public class Main {
    public static long shared = 21;
    public static State sharedState;
    public static org.rustlang.runtime.Pointer sharedPointer;

    public static final class State {
        public int value;
        public long wide;
        public State next;
        public org.rustlang.runtime.Pointer pointer;

        public State(int value, long wide) {
            this.value = value;
            this.wide = wide;
        }
    }

    public static void main(String[] args) {
        long result = jvm_link_names.jvm_link_names.exercise();
        if (result != 58 || shared != 34 || sharedState == null || sharedState.value != 13) {
            throw new AssertionError(
                    "JVM link-name field/constructor interop failed: result=" + result);
        }
    }
}
