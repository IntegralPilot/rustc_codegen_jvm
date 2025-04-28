struct Inner {
    x: i32,
    y: (i32, i32),
}

struct Outer<'a> {
    label: &'a str,
    inner_struct: Inner,
    data: [i32; 3],
}

fn main() {
    // === Nested STRUCT + TUPLE + ARRAY ===
    let mut outer = Outer {
        label: "start",
        inner_struct: Inner {
            x: 100,
            y: (5, 10),
        },
        data: [1, 2, 3],
    };

    // === Access nested values ===
    assert!(outer.label == "start");
    assert!(outer.inner_struct.x == 100, "Inner x should be 100");
    assert!(outer.inner_struct.y.0 == 5, "Inner tuple y.0 should be 5");
    assert!(outer.inner_struct.y.1 == 10, "Inner tuple y.1 should be 10");
    assert!(outer.data[0] == 1, "Array element at index 0 should be 1");

    // === Mutate nested values ===
    outer.label = "updated";
    outer.inner_struct.x = 200;
    outer.inner_struct.y.1 = 999;
    outer.data[1] = 42;

    // === Assert mutations ===
    assert!(outer.label == "updated", "Outer label should be updated");
    assert!(outer.inner_struct.x == 200, "Inner x should now be 200");
    assert!(outer.inner_struct.y.1 == 999, "Inner tuple y.1 should now be 999");
    assert!(outer.data[1] == 42, "Array element at index 1 should now be 42");

    // === Tuple nesting test ===
    let mut big_tuple = (
        (10, 20),
        Inner { x: 50, y: (7, 8) },
        ["a", "b", "c"],
    );

    // Access nested tuple values
    assert!((big_tuple.0).1 == 20, "First tuple's second element should be 20");
    assert!(big_tuple.1.y.0 == 7, "Nested tuple inside struct should be 7");
    assert!(big_tuple.2[2] == "c", "Array inside tuple should contain 'c' at index 2");

    // Mutate nested values
    (big_tuple.0).0 = 99;
    big_tuple.1.x = 123;
    big_tuple.2[1] = "z";

    // Assert changes
    assert!((big_tuple.0).0 == 99, "Tuple value mutated to 99");
    assert!(big_tuple.1.x == 123, "Inner struct field x mutated to 123");
    assert!(big_tuple.2[1] == "z", "Tuple's array element at index 1 mutated to 'z'");
}
