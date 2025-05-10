trait Calculator {
    // Performs a primary calculation
    fn calculate(&self, a: i32, b: i32) -> i32;

    // Modifies internal state based on input
    fn accumulate(&mut self, value: i32);

    // Returns the current internal state
    fn get_value(&self) -> i32;

    // Returns an identifier for the calculator type
    fn id(&self) -> u8;
}

// --- First Implementation ---
struct SimpleAdder {
    current_total: i32,
}

impl Calculator for SimpleAdder {
    fn calculate(&self, a: i32, b: i32) -> i32 {
        a + b // Simple addition
    }

    fn accumulate(&mut self, value: i32) {
        self.current_total += value;
    }

    fn get_value(&self) -> i32 {
        self.current_total
    }

    fn id(&self) -> u8 {
        1 // Identifier for SimpleAdder
    }
}

// --- Second Implementation ---
struct Multiplier {
    current_product: i32,
}

impl Calculator for Multiplier {
    fn calculate(&self, a: i32, b: i32) -> i32 {
        a * b // Multiplication
    }

fn accumulate(&mut self, value: i32) {
        if value == 0 {
            // Explicitly do nothing if value is 0, preserving the current product.
            return;
        }

        if self.current_product == 0 {
            // If the current product is 0, accumulating a non-zero value should
            // just set the product to that value. Avoids 0 * value = 0.
             self.current_product = value;
        } else {
            // Otherwise (current product is non-zero and value is non-zero), multiply.
             self.current_product *= value;
        }
    }


    fn get_value(&self) -> i32 {
        self.current_product
    }

    fn id(&self) -> u8 {
        6 // Identifier for Multiplier
    }
}

// Takes an immutable trait object reference
fn perform_calculation(calc: &dyn Calculator, x: i32, y: i32) -> i32 {
    calc.calculate(x, y)
}

// Takes a mutable trait object reference
fn update_state(calc: &mut dyn Calculator, val: i32) {
    calc.accumulate(val);
}

// Checks properties via immutable trait object
fn check_properties(calc: &dyn Calculator) -> (i32, u8) {
    (calc.get_value(), calc.id())
}


fn main() {
    let mut adder = SimpleAdder { current_total: 10 };

    // Direct calls
    assert!(adder.calculate(5, 3) == 8);
    assert!(adder.get_value() == 10);
    assert!(adder.id() == 1);

    adder.accumulate(5);
    assert!(adder.get_value() == 15);

    // Immutable Trait Object (&dyn Calculator)
    let adder_ref: &dyn Calculator = &adder;
    assert!(adder_ref.calculate(10, 20) == 30);
    assert!(adder_ref.get_value() == 15); // State reflects previous mutation
    assert!(adder_ref.id() == 1);

    // Pass immutable trait object to function
    let result1 = perform_calculation(&adder, 100, 50);
    assert!(result1 == 150);
    let (val1, id1) = check_properties(&adder);
    assert!(val1 == 15);
    assert!(id1 == 1);


    // Mutable Trait Object (&mut dyn Calculator)
    let adder_mut_ref: &mut dyn Calculator = &mut adder;
    adder_mut_ref.accumulate(-7);
    // Check state change via original variable AFTER mutable borrow ends
    assert!(adder.get_value() == 8); // 15 - 7 = 8

    // Pass mutable trait object to function
    update_state(&mut adder, 2);
    assert!(adder.get_value() == 10); // 8 + 2 = 10


    let mut multiplier = Multiplier { current_product: 2 };

    // Direct calls
    assert!(multiplier.calculate(5, 3) == 15);
    assert!(multiplier.get_value() == 2);
    assert!(multiplier.id() == 6);

    multiplier.accumulate(4); // state becomes 2 * 4 = 8
    assert!(multiplier.get_value() == 8);

    // Immutable Trait Object (&dyn Calculator)
    let multiplier_ref: &dyn Calculator = &multiplier;
    assert!(multiplier_ref.calculate(6, 7) == 42);
    assert!(multiplier_ref.get_value() == 8); // State reflects previous mutation
    assert!(multiplier_ref.id() == 6);

    // Pass immutable trait object to function
    let result2 = perform_calculation(&multiplier, -2, 9);
    assert!(result2 == -18);
    let (val2, id2) = check_properties(&multiplier);
    assert!(val2 == 8);
    assert!(id2 == 6);

    // Mutable Trait Object (&mut dyn Calculator)
    let multiplier_mut_ref: &mut dyn Calculator = &mut multiplier;
    multiplier_mut_ref.accumulate(3);
     // Check state change via original variable AFTER mutable borrow ends
    assert!(multiplier.get_value() == 24); // 8 * 3 = 24

    // Pass mutable trait object to function
    update_state(&mut multiplier, -2);
    assert!(multiplier.get_value() == -48); // 24 * -2 = -48

    // Check zero accumulation behaviour
    update_state(&mut multiplier, 0);
    assert!(multiplier.get_value() == -48); // Should not change when multiplying by 0

    // Final check: use different trait objects in sequence
    let calc1: &dyn Calculator = &SimpleAdder { current_total: 100 };
    let calc2: &dyn Calculator = &Multiplier { current_product: 10 };

    assert!(perform_calculation(calc1, 1, 1) == 2);
    assert!(check_properties(calc1) == (100, 1));

    assert!(perform_calculation(calc2, 2, 3) == 6);
    assert!(check_properties(calc2) == (10, 6));

    // If we reach here without panic, the test passes
}