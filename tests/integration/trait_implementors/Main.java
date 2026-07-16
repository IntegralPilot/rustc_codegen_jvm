public class Main {
    private static final class JavaAccumulator implements trait_implementors.Accumulator {
        private int value;

        JavaAccumulator(int initial) {
            value = initial;
        }

        @Override
        public int add(int amount) {
            value += amount;
            return value;
        }

        @Override
        public int current() {
            return value;
        }
    }

    private static final class JavaProjector
            implements trait_implementors.Projector_Dyn_Output_i64 {
        private long bias;

        JavaProjector(long bias) {
            this.bias = bias;
        }

        @Override
        public long project(int value) {
            bias += 2;
            return value * 100L + bias;
        }
    }

    public static void main(String[] args) {
        JavaAccumulator accumulator = new JavaAccumulator(10);
        int exercised = trait_implementors.trait_implementors.exercise_accumulator(accumulator);
        if (exercised != 1543 || accumulator.current() != 13) {
            throw new AssertionError("Java accumulator did not dispatch through &mut dyn Accumulator");
        }

        JavaAccumulator left = new JavaAccumulator(1);
        JavaAccumulator right = new JavaAccumulator(10);
        int combined = trait_implementors.trait_implementors.combine_accumulators(left, right);
        if (combined != 38 || left.current() != 4 || right.current() != 15) {
            throw new AssertionError("Multiple Java trait objects lost identity or mutable state");
        }

        JavaProjector projector = new JavaProjector(5);
        long projected = trait_implementors.trait_implementors.exercise_i64_projector(projector);
        if (projected != 516L) {
            throw new AssertionError("Associated-type trait object used the wrong specialized ABI");
        }

        if (trait_implementors.trait_implementors.rust_projector_smoke() != 57L) {
            throw new AssertionError("Rust and Java implementors do not share the same trait-object ABI");
        }
    }
}
