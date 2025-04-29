trait Calculator {
    fn calculate(&self, a: i32, b: i32) -> i32;
    fn accumulate(&mut self, value: i32);
    fn get_value(&self) -> i32;
    fn id(&self) -> u8;
    fn is_advanced(&self) -> bool;
}

struct SimpleAdder {
    current_total: i32,
}

impl Calculator for SimpleAdder {
    fn calculate(&self, a: i32, b: i32) -> i32 {
        a + b
    }

    fn accumulate(&mut self, value: i32) {
        self.current_total += value;
    }

    fn get_value(&self) -> i32 {
        self.current_total
    }

    fn id(&self) -> u8 {
        1
    }
    fn is_advanced(&self) -> bool {
        false
    }
}

struct Multiplier {
    current_product: i32,
}

impl Calculator for Multiplier {
    fn calculate(&self, a: i32, b: i32) -> i32 {
        a * b
    }

    fn accumulate(&mut self, value: i32) {
        if value != 0 {
            if self.current_product == 0 {
                self.current_product = value;
            } else {
                self.current_product *= value;
            }
        }
    }

    fn get_value(&self) -> i32 {
        self.current_product
    }

    fn id(&self) -> u8 {
        6
    }

    fn is_advanced(&self) -> bool {
        true
    }
}

macro_rules! check_properties {
    ($calc:expr) => {
        ($calc.get_value(), $calc.id(), $calc.is_advanced())
    };
}

fn main() {
    let mut adder = SimpleAdder { current_total: 10 };

    assert!(adder.calculate(5, 3) == 8);
    assert!(adder.get_value() == 10);
    assert!(adder.id() == 1);
    assert!(!adder.is_advanced());

    adder.accumulate(5);
    assert!(adder.get_value() == 15);

    let result1 = adder.calculate(100, 50);
    assert!(result1 == 150);
    let (val1, id1, adv1) = check_properties!(&adder);
    assert!(val1 == 15);
    assert!(id1 == 1);
    assert!(adv1 == false);

    adder.accumulate(-7);
    assert!(adder.get_value() == 8);

    adder.accumulate(2);
    assert!(adder.get_value() == 10);

    let mut multiplier = Multiplier { current_product: 2 };

    assert!(multiplier.calculate(5, 3) == 15);
    assert!(multiplier.get_value() == 2);
    assert!(multiplier.id() == 6);
    assert!(multiplier.is_advanced());

    multiplier.accumulate(4);
    assert!(multiplier.get_value() == 8);

    let result2 = multiplier.calculate(-2, 9);
    assert!(result2 == -18);
    let (val2, id2, adv2) = check_properties!(&multiplier);
    assert!(val2 == 8);
    assert!(id2 == 6);
    assert!(adv2 == true);

    multiplier.accumulate(3);
    assert!(multiplier.get_value() == 24);

    multiplier.accumulate(-2);
    assert!(multiplier.get_value() == -48);

    multiplier.accumulate(0);
    assert!(multiplier.get_value() == -48);

    let calc1 = SimpleAdder { current_total: 100 };
    let calc2 = Multiplier { current_product: 10 };

    assert!(calc1.calculate(1, 1) == 2);
    let (val1, id1, adv1) = check_properties!(&calc1);
    assert!(val1 == 100);
    assert!(id1 == 1);
    assert!(adv1 == false);

    assert!(calc2.calculate(2, 3) == 6);
    let (val2, id2, adv2) = check_properties!(&calc2);
    assert!(val2 == 10);
    assert!(id2 == 6);
    assert!(adv2 == true);
}
