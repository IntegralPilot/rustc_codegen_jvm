public class Main {
    public static long shared = 21;
    public static State sharedState;

    public static final class State {
        public int value;
        public long wide;
        public State next;

        public State(int value, long wide) {
            this.value = value;
            this.wide = wide;
        }

        public static int twice(int value) {
            return value * 2;
        }
    }

    public static void main(String[] args) {
        long result = jvm_macros.jvm_macros.exercise();
        if (result != 101 || shared != 34 || sharedState == null || sharedState.value != 13) {
            throw new AssertionError(
                    "JVM macro interop failed: result=" + result);
        }
    }
}
