public class Main {
    public static void main(String[] args) {
        inner_classes.TrafficLight red = new inner_classes.TrafficLight.Red();
        inner_classes.TrafficLight yellow = new inner_classes.TrafficLight.Yellow();
        inner_classes.TrafficLight green = new inner_classes.TrafficLight.Green();

        String redAction = org.rustlang.runtime.Utf8View.toJavaString(
                inner_classes.inner_classes.get_light_action(red));
        if (!redAction.equals("Stop")) {
             throw new AssertionError("Test failed for Red: expected 'Stop' but got '" + redAction + "'");
        }

        String yellowAction = org.rustlang.runtime.Utf8View.toJavaString(
                inner_classes.inner_classes.get_light_action(yellow));
        if (!yellowAction.equals("Caution")) {
             throw new AssertionError("Test failed for Yellow: expected 'Caution' but got '" + yellowAction + "'");
        }

        String greenAction = org.rustlang.runtime.Utf8View.toJavaString(
                inner_classes.inner_classes.get_light_action(green));
        if (!greenAction.equals("Go")) {
             throw new AssertionError("Test failed for Green: expected 'Go' but got '" + greenAction + "'");
        }

        inner_classes.MaybeNumber seven = inner_classes.inner_classes.make_some(7);
        inner_classes.MaybeNumber anotherSeven = inner_classes.inner_classes.make_some(7);
        inner_classes.MaybeNumber eight = inner_classes.inner_classes.make_some(8);

        if (!inner_classes.MaybeNumber.eq(seven, anotherSeven)) {
             throw new AssertionError("Enum equality should accept matching payload fields");
        }
        if (inner_classes.MaybeNumber.eq(seven, eight)) {
             throw new AssertionError("Enum equality should reject different payload fields");
        }

        inner_classes.MaybeNumber pairTrue = inner_classes.inner_classes.make_pair(7, true);
        inner_classes.MaybeNumber pairFalse = inner_classes.inner_classes.make_pair(7, false);

        if (inner_classes.MaybeNumber.eq(pairTrue, pairFalse)) {
             throw new AssertionError("Enum equality should compare every payload field");
        }
        if (inner_classes.MaybeNumber.eq(pairTrue, seven)) {
             throw new AssertionError("Enum equality should reject different variants");
        }

        inner_classes.NestedEquality nestedSeven = inner_classes.inner_classes.wrap_maybe(inner_classes.inner_classes.make_some(7));
        inner_classes.NestedEquality anotherNestedSeven = inner_classes.inner_classes.wrap_maybe(inner_classes.inner_classes.make_some(7));
        inner_classes.NestedEquality nestedEight = inner_classes.inner_classes.wrap_maybe(inner_classes.inner_classes.make_some(8));

        if (!inner_classes.NestedEquality.eq(nestedSeven, anotherNestedSeven)) {
             throw new AssertionError("Enum equality should recurse into nested enum payloads");
        }
        if (inner_classes.NestedEquality.eq(nestedSeven, nestedEight)) {
             throw new AssertionError("Nested enum payloads with different fields should not compare equal");
        }

        inner_classes.NestedEquality boxedSeven = inner_classes.inner_classes.wrap_boxed(inner_classes.inner_classes.make_number_box(7));
        inner_classes.NestedEquality anotherBoxedSeven = inner_classes.inner_classes.wrap_boxed(inner_classes.inner_classes.make_number_box(7));
        inner_classes.NestedEquality boxedEight = inner_classes.inner_classes.wrap_boxed(inner_classes.inner_classes.make_number_box(8));

        if (!inner_classes.NestedEquality.eq(boxedSeven, anotherBoxedSeven)) {
             throw new AssertionError("Enum equality should recurse into nested struct payloads");
        }
        if (inner_classes.NestedEquality.eq(boxedSeven, boxedEight)) {
             throw new AssertionError("Nested struct payloads with different fields should not compare equal");
        }

        inner_classes.LeafEvent leaf = inner_classes.inner_classes.make_leaf_number(41);
        inner_classes.Event promoted = inner_classes.inner_classes.promote_leaf(leaf);
        if (promoted.getClass() != leaf.getClass()) {
             throw new AssertionError("Transparent enum subtype promotion must not create a wrapper class");
        }
        if (!(promoted instanceof inner_classes.LeafEvent)) {
             throw new AssertionError("Nested enum interface must extend the outer enum interface");
        }
        if (inner_classes.inner_classes.read_event(promoted) != 41) {
             throw new AssertionError("Rust matching through a transparent enum subtype failed");
        }
        if (inner_classes.Event.variantIndex(promoted) != 0) {
             throw new AssertionError("Outer enum variant tagging failed for transparent subtype");
        }
        if (!inner_classes.Event.eq(promoted, leaf)) {
             throw new AssertionError("Outer enum equality failed for transparent subtype");
        }
        if (!(promoted instanceof inner_classes.EventScore)) {
             throw new AssertionError("An enum trait implementation must extend the generated trait interface");
        }
        if (promoted.category() != 70) {
             throw new AssertionError("Java dispatch must select the most-specific inner enum default method");
        }
        if (inner_classes.inner_classes.event_category(promoted) != 7) {
             throw new AssertionError("Rust inherent dispatch must retain the nominal outer enum implementation");
        }
        if (inner_classes.inner_classes.leaf_category(leaf) != 70) {
             throw new AssertionError("Rust inherent dispatch must select the inner enum implementation");
        }
        if (promoted.score() != 201) {
             throw new AssertionError("Java trait dispatch must select the most-specific inner enum implementation");
        }
        if (((inner_classes.EventScore) promoted).score() != 201) {
             throw new AssertionError("Java trait-interface dispatch must reach the inner enum implementation");
        }
        if (inner_classes.Event.score(promoted) != 101) {
             throw new AssertionError("The outer enum static trait implementation must remain owner-qualified");
        }
        if (inner_classes.LeafEvent.score(leaf) != 201) {
             throw new AssertionError("The inner enum static trait implementation must remain owner-qualified");
        }
        if (inner_classes.inner_classes.event_score_direct(promoted) != 101) {
             throw new AssertionError("Direct Rust trait dispatch selected the wrong enum implementation");
        }
        if (inner_classes.inner_classes.leaf_score_direct(leaf) != 201) {
             throw new AssertionError("Direct Rust trait dispatch selected the wrong inner enum implementation");
        }
        if (inner_classes.inner_classes.event_score_generic(promoted) != 101) {
             throw new AssertionError("Generic Rust trait dispatch selected the wrong enum implementation");
        }
        if (inner_classes.inner_classes.event_score_dyn(promoted) != 101) {
             throw new AssertionError("Rust dyn-trait dispatch selected the wrong enum implementation");
        }

        System.out.println("Inner class access test passed!");
    }
}
