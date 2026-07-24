struct ConfigData {
    id: u32,
    enabled: bool,
    params: (f32, f32), // Nested tuple
}

enum ComplexEnum<'a> {
    SimpleVariant,                      // No data
    Count(i64),                         // Single primitive data
    Coords((i32, i32, i32)),            // Tuple data
    UserData { name: &'a str, age: u8 }, // Struct-like variant
    RawData([u8; 8]),                   // Array data
    Settings(ConfigData),               // Struct data
}

#[repr(u8)]
enum ConstantArrayEnum {
    Zero = 0,
    One = 1,
    Seven = 7,
}

enum RecursiveFormatItem<'a> {
    Literal(&'a [u8]),
    Compound(&'a [RecursiveFormatItem<'a>]),
    Optional(&'a RecursiveFormatItem<'a>),
    First(&'a [RecursiveFormatItem<'a>]),
}

const CONSTANT_ENUMS: [ConstantArrayEnum; 4] = [
    ConstantArrayEnum::Zero,
    ConstantArrayEnum::Seven,
    ConstantArrayEnum::One,
    ConstantArrayEnum::Seven,
];

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

static ENUM_WRAPPER_DROPS: AtomicUsize = AtomicUsize::new(0);
static ENUM_PAYLOAD_DROPS: AtomicUsize = AtomicUsize::new(0);
static ENUM_WRAPPER_RAN: AtomicBool = AtomicBool::new(false);

struct DropTracer(u8);

impl Drop for DropTracer {
    fn drop(&mut self) {
        assert!(ENUM_WRAPPER_RAN.swap(false, Ordering::SeqCst));
        ENUM_PAYLOAD_DROPS.fetch_add(self.0 as usize, Ordering::SeqCst);
    }
}

enum CustomDropEnum {
    First(DropTracer),
    Second(DropTracer),
    Empty,
}

impl Drop for CustomDropEnum {
    fn drop(&mut self) {
        assert!(!ENUM_WRAPPER_RAN.swap(true, Ordering::SeqCst));
        ENUM_WRAPPER_DROPS.fetch_add(1, Ordering::SeqCst);
    }
}

fn main() {
    drop(CustomDropEnum::First(DropTracer(1)));
    assert!(ENUM_WRAPPER_DROPS.load(Ordering::SeqCst) == 1);
    assert!(ENUM_PAYLOAD_DROPS.load(Ordering::SeqCst) == 1);

    {
        let _second = CustomDropEnum::Second(DropTracer(10));
    }
    assert!(ENUM_WRAPPER_DROPS.load(Ordering::SeqCst) == 2);
    assert!(ENUM_PAYLOAD_DROPS.load(Ordering::SeqCst) == 11);

    drop(CustomDropEnum::Empty);
    assert!(ENUM_WRAPPER_RAN.swap(false, Ordering::SeqCst));
    assert!(ENUM_WRAPPER_DROPS.load(Ordering::SeqCst) == 3);
    assert!(ENUM_PAYLOAD_DROPS.load(Ordering::SeqCst) == 11);

    // Initialize with one variant
    let mut current_state = ComplexEnum::UserData {
        name: "Alice",
        age: 30,
    };

   // Access initial values using match
    match current_state {
        ComplexEnum::UserData { ref name, age } => {
            assert!(*name == "Alice", "Initial name should be Alice");
            assert!(age == 30, "Initial age should be 30");
        }
        _ => panic!("Initial state should be UserData!"),
    } 

    // Access initial values using if let
    if let ComplexEnum::UserData { name, .. } = current_state {
        assert!(name.starts_with('A'), "Name should start with A");
    } else {
        panic!("Still expect UserData here!");
    } 

    // Mutate the enum variable to a different variant (Settings)
    current_state = ComplexEnum::Settings(ConfigData {
        id: 123,
        enabled: true,
        params: (1.0, -0.5),
    });

    // Access nested values in the new variant
     match current_state {
        ComplexEnum::Settings(config) => {
            assert!(config.id == 123, "Settings ID should be 123");
            assert!(config.enabled, "Settings should be enabled");
            assert!(config.params.0 == 1.0, "Settings params.0 should be 1.0");
            assert!(config.params.1 == -0.5, "Settings params.1 should be -0.5");
        }
        _ => panic!("State should now be Settings!"),
    } 

    // Mutate the enum variable to a different variant (RawData)
    current_state = ComplexEnum::RawData([0, 1, 2, 3, 4, 5, 6, 7]);

    // Mutate data *inside* the RawData variant
    let mut mutated_internally = false;
    match &mut current_state {
        // Use mutable borrow (&mut) to modify internal data
        ComplexEnum::RawData(data_array) => {
            assert!(data_array[0] == 0, "RawData[0] should be 0 initially");
            assert!(data_array[7] == 7, "RawData[7] should be 7 initially");

            // Modify elements
            data_array[0] = 100;
            data_array[7] = 200;
            data_array[1] *= 5; // Modify based on existing value

            mutated_internally = true;
        }
        _ => {  /*No mutation needed for other branches in this step*/  }
    }
    assert!(mutated_internally, "Internal mutation should have happened"); 

    // Assert internal mutations in RawData
     if let ComplexEnum::RawData(data_array) = current_state {
        assert!(data_array[0] == 100, "RawData[0] should now be 100");
        assert!(data_array[1] == 5, "RawData[1] should now be 5 (1*5)");
        assert!(data_array[7] == 200, "RawData[7] should now be 200");
    } else {
        panic!("State should still be RawData after internal mutation!");
    } 

    // Mutate data *inside* the nested ConfigData struct within Settings variant
   current_state = ComplexEnum::Settings(ConfigData {
        // Reset state
        id: 999,
        enabled: false,
        params: (0.0, 0.0),
    });

    match &mut current_state {
        ComplexEnum::Settings(config) => {
            config.enabled = true; // Mutate bool field
            config.params.1 = config.params.0 + 10.0; // Mutate tuple field
            config.id += 1; // Mutate id field
        }
        _ => panic!("State should be Settings for nested mutation!"),
    }

    // Assert internal nested mutations
    match current_state {
        ComplexEnum::Settings(config) => {
            assert!(config.id == 1000, "ConfigData id should be 1000");
            assert!(config.enabled, "ConfigData enabled should be true");
            assert!(
                config.params.0 == 0.0,
                "ConfigData params.0 should be unchanged"
            );
            assert!(config.params.1 == 10.0, "ConfigData params.1 should be 10.0");
        }
        _ => panic!("State should still be Settings after nested mutation!"),
    }

    // Test remaining variants
    current_state = ComplexEnum::Count(5000);
    if let ComplexEnum::Count(c) = current_state {
        assert!(c == 5000);
    } else {
        panic!("State should be Count");
    }

    current_state = ComplexEnum::Coords((-10, 0, 20));
    if let ComplexEnum::Coords((x, y, z)) = current_state {
        assert!(x == -10);
        assert!(y == 0);
        assert!(z == 20);
    } else {
        panic!("State should be Coords");
    }
    if let ComplexEnum::Coords((x, y, z)) = current_state {
        assert!(x == -10);
        assert!(y == 0);
        assert!(z == 20);
    } else {
        panic!("State should be Coords");
    }

    current_state = ComplexEnum::SimpleVariant;
    if let ComplexEnum::SimpleVariant = current_state {
        // Do nothing, this is the expected state
    } else {
        panic!("State should be SimpleVariant");
    }

    assert!(matches!(CONSTANT_ENUMS[0], ConstantArrayEnum::Zero));
    assert!(matches!(CONSTANT_ENUMS[1], ConstantArrayEnum::Seven));
    assert!(matches!(CONSTANT_ENUMS[2], ConstantArrayEnum::One));
    assert!(matches!(CONSTANT_ENUMS[3], ConstantArrayEnum::Seven));

    let literal = RecursiveFormatItem::Literal(b"time");
    let optional_literal = RecursiveFormatItem::Literal(b"time");
    let optional = RecursiveFormatItem::Optional(&optional_literal);
    let compound_items = [literal, optional];
    let compound = RecursiveFormatItem::Compound(&compound_items);
    let first_items = [compound];
    let first = RecursiveFormatItem::First(&first_items);
    match first {
        RecursiveFormatItem::First([RecursiveFormatItem::Compound(items)]) => {
            assert!(matches!(items[0], RecursiveFormatItem::Literal(b"time")));
            assert!(matches!(
                items[1],
                RecursiveFormatItem::Optional(RecursiveFormatItem::Literal(b"time"))
            ));
        }
        _ => panic!("recursive format item shape was not preserved"),
    }
}
