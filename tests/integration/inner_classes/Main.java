public class Main {
    public static void main(String[] args) {
        TrafficLight red = new TrafficLight.Red();
        TrafficLight yellow = new TrafficLight.Yellow();
        TrafficLight green = new TrafficLight.Green();

        String redAction = inner_classes.get_light_action(red);
        if (!redAction.equals("Stop")) {
             throw new AssertionError("Test failed for Red: expected 'Stop' but got '" + redAction + "'");
        }

        String yellowAction = inner_classes.get_light_action(yellow);
        if (!yellowAction.equals("Caution")) {
             throw new AssertionError("Test failed for Yellow: expected 'Caution' but got '" + yellowAction + "'");
        }

        String greenAction = inner_classes.get_light_action(green);
        if (!greenAction.equals("Go")) {
             throw new AssertionError("Test failed for Green: expected 'Go' but got '" + greenAction + "'");
        }

        MaybeNumber seven = inner_classes.make_some(7);
        MaybeNumber anotherSeven = inner_classes.make_some(7);
        MaybeNumber eight = inner_classes.make_some(8);

        if (!seven.eq(anotherSeven)) {
             throw new AssertionError("Enum equality should accept matching payload fields");
        }
        if (seven.eq(eight)) {
             throw new AssertionError("Enum equality should reject different payload fields");
        }

        MaybeNumber pairTrue = inner_classes.make_pair(7, true);
        MaybeNumber pairFalse = inner_classes.make_pair(7, false);

        if (pairTrue.eq(pairFalse)) {
             throw new AssertionError("Enum equality should compare every payload field");
        }
        if (pairTrue.eq(seven)) {
             throw new AssertionError("Enum equality should reject different variants");
        }

        NestedEquality nestedSeven = inner_classes.wrap_maybe(inner_classes.make_some(7));
        NestedEquality anotherNestedSeven = inner_classes.wrap_maybe(inner_classes.make_some(7));
        NestedEquality nestedEight = inner_classes.wrap_maybe(inner_classes.make_some(8));

        if (!nestedSeven.eq(anotherNestedSeven)) {
             throw new AssertionError("Enum equality should recurse into nested enum payloads");
        }
        if (nestedSeven.eq(nestedEight)) {
             throw new AssertionError("Nested enum payloads with different fields should not compare equal");
        }

        NestedEquality boxedSeven = inner_classes.wrap_boxed(inner_classes.make_number_box(7));
        NestedEquality anotherBoxedSeven = inner_classes.wrap_boxed(inner_classes.make_number_box(7));
        NestedEquality boxedEight = inner_classes.wrap_boxed(inner_classes.make_number_box(8));

        if (!boxedSeven.eq(anotherBoxedSeven)) {
             throw new AssertionError("Enum equality should recurse into nested struct payloads");
        }
        if (boxedSeven.eq(boxedEight)) {
             throw new AssertionError("Nested struct payloads with different fields should not compare equal");
        }

        System.out.println("Inner class access test passed!");
    }
}
