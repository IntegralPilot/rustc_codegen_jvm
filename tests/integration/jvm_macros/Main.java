public class Main {
    public static long shared = 21;
    public static State sharedState;

    public interface Measure {
        int base = 3;

        long measure(long value, double scale);

        default long doubled(long value, double scale) {
            return 2 * measure(value, scale);
        }

        static Measure create(int bias) {
            return new Measurement(bias);
        }

        static int constant() {
            return 42;
        }
    }

    public interface StaticOnly {
        static long value(long value) {
            return value * 3;
        }
    }

    public interface StaticDirect {
        static int value(int value) {
            return value + 5;
        }
    }

    public static final class Measurement implements Measure {
        private final int bias;

        public Measurement(int bias) {
            this.bias = bias;
        }

        public long measure(long value, double scale) {
            return (long) (value * scale) + bias;
        }
    }

    public static Measure makeMeasure(int bias) {
        return Measure.create(bias);
    }

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
