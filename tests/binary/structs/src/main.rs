#[derive(Debug)]
struct NestedStruct {
    // A nested struct that holds an array.
    values: [i32; 3],
}

#[derive(Debug)]
struct TestStruct {
    field1: i32,
    field2: String,
    // Nested struct inside the main struct.
    nested: NestedStruct,
}

fn main() {
    // Initialize the nested struct with an array.
    let nested = NestedStruct { values: [10, 20, 30] };

    // Initialize the main structure with two simple fields and one nested struct.
    let test_struct = TestStruct {
        field1: 180,
        field2: "rust".to_string(),
        nested,
    };

    // Test simple field accesses.
    assert!(test_struct.field1 == 180, "Field1 should be 180");
    assert!(test_struct.field2 == "rust", "Field2 should be 'rust'");

    // Test nested field access (accessing a field from the nested struct).
    assert!(test_struct.nested.values[0] == 10, "First element in nested.values should be 10");
    assert!(test_struct.nested.values[1] == 20, "Second element in nested.values should be 20");
    assert!(test_struct.nested.values[2] == 30, "Third element in nested.values should be 30");

    // If you want to test even deeper nesting, for instance, an array containing nested structs:
    let more_nested: [TestStruct; 2] = [
        TestStruct {
            field1: 100,
            field2: "foo".to_string(),
            nested: NestedStruct { values: [1, 2, 3] },
        },
        TestStruct {
            field1: 200,
            field2: "bar".to_string(),
            nested: NestedStruct { values: [4, 5, 6] },
        },
    ];

    // Test access into the array and then the nested struct.
    assert!(more_nested[0].field1 == 100, "First element's field1 should be 100");
    assert!(more_nested[1].nested.values[2] == 6, "Second element's nested.values[2] should be 6");
}
