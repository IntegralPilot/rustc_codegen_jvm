public class Main {
    public static void main(String[] args) {
        for (int n = 0; n <= 10; n++) {
            int sum = 0, filter = 0, zip = 0, reverse = 0, nested = 0;
            for (int x = 0; x < n; x++) {
                sum += x;
                if (x % 3 == 0) filter += x * 2;
                zip += x * 3 + 10;
                for (int y = 0; y < x; y += 2) nested += y;
            }
            for (int x = n - 3; x >= Math.max(0, n - 6); x--) reverse = reverse * 10 + x;
            check(iterator_codegen.iterator_codegen.range_sum(n), sum);
            check(iterator_codegen.iterator_codegen.filter_map(n), filter);
            check(iterator_codegen.iterator_codegen.zip_enumerate(n), zip);
            check(iterator_codegen.iterator_codegen.reverse(n), reverse);
            check(iterator_codegen.iterator_codegen.windows(n), n*(n+1)+(n+1)*(n+2)+(n+2)*(n+3));
            check(iterator_codegen.iterator_codegen.nested(n), nested);
            check(iterator_codegen.iterator_codegen.mutable(n), 4*n+12);
            check(iterator_codegen.iterator_codegen.custom(n), sum*3);
            int chain = 0, limited = 0;
            for (int x = 0; x < n + 3; x += 2) chain += x;
            for (int x = 0; x < Math.min(n, 5); x++) limited += x;
            check(iterator_codegen.iterator_codegen.chain(n), chain);
            check(iterator_codegen.iterator_codegen.short_circuit(n), limited + (n > 5 ? 5 : -1));
            check(iterator_codegen.iterator_codegen.chunks(n), 6*n+15);
            check(iterator_codegen.iterator_codegen.captured(n), sum);
            check(iterator_codegen.iterator_codegen.drops(n), Math.min(n, 2));
            check(iterator_codegen.iterator_codegen.peekable(n), sum);
            int scanned = 0, partial = 0, flattened = 0;
            for (int x = 0; x < n; x++) {
                partial += x;
                scanned += partial;
                if (x % 2 == 0) flattened += x;
            }
            check(iterator_codegen.iterator_codegen.scan(n), scanned);
            check(iterator_codegen.iterator_codegen.flatten(n), flattened);
            int max = 0;
            for (int x = 0; x < n; x++) max = Math.max(max, x * 7 % 11);
            if (iterator_codegen.iterator_codegen.bounds(n) != max) throw new AssertionError("bounds");
        }
        if (!iterator_codegen.iterator_codegen.caller_location()) throw new AssertionError("caller location");
        if (!iterator_codegen.iterator_codegen.erased_arrays()) throw new AssertionError("erased arrays");
        check(iterator_codegen.iterator_codegen.discarded_provenance(), 43);
        check(iterator_codegen.iterator_codegen.borrowed_choice(true), 105);
        check(iterator_codegen.iterator_codegen.borrowed_choice(false), 42);
        if (!iterator_codegen.iterator_codegen.string_pattern()) throw new AssertionError("string pattern");
        check(iterator_codegen.iterator_codegen.union_buffer(49), 49);
        check(iterator_codegen.iterator_codegen.union_buffer(255), 255);
        System.out.println("iterator codegen OK");
    }
    private static void check(int actual, int expected) {
        if (actual != expected) throw new AssertionError(actual + " != " + expected);
    }
}
