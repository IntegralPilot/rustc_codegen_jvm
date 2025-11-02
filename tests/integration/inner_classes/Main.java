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

        System.out.println("Inner class access test passed!");
    }
}