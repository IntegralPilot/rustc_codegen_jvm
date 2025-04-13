struct TestStruct {
    field1: i32,
    field2: String,
}

fn main() {
    let test_struct = TestStruct {
        field1: 180,
        field2: "rust".to_string(),
    };
    assert!(test_struct.field1 == 180, "Field1 should be 180");
    assert!(test_struct.field2 == "rust", "Field2 should be 'rust'");
}
