// Simple struct
struct Point {
    x: i32,
    y: i32,
}

#[derive(Copy, Clone)]
struct RepeatPoint {
    x: i32,
    y: i32,
}

// Function taking a mutable borrow of a Point field
fn make_y_negative(p_y: &mut i32) {
    if *p_y > 0 {
        *p_y = -(*p_y);
    }
    assert!(*p_y <= 0);
}

// Function taking a shared borrow of a Point and reading
fn get_x_coord(p: &Point) -> i32 {
    p.x // Just read the x field
}

// Function taking a mutable borrow of the whole Point
fn shift_point(p: &mut Point) {
    p.x += 10; // Modify field directly via mutable borrow of struct
    make_y_negative(&mut p.y); // Re-borrow field mutably and pass to another function
    p.x += 5; // Modify field again after inner borrow ended
}

fn check_arrays_of_mutable_references() {
    let mut first = 1;
    let mut second = 2;
    {
        let refs = [&mut first, &mut second];
        *refs[0] += 10;
        *refs[1] += 20;
        assert!(*refs[0] == 11);
        assert!(*refs[1] == 22);
    }

    let mut left = Point { x: 3, y: 4 };
    let mut right = Point { x: 5, y: 6 };
    {
        let refs = [&mut left, &mut right];
        refs[0].x += 30;
        refs[1].y += 60;
        assert!(refs[0].x == 33);
        assert!(refs[1].y == 66);
    }
}

fn pass_i32(value: &mut i32) -> &mut i32 {
    value
}

fn pass_point(value: &mut Point) -> &mut Point {
    value
}

fn adjust_floats(single: &mut f32, double: &mut f64) {
    *single += 1.25;
    *double -= 2.5;
}

fn adjust_nested(value: &mut &mut i32) {
    **value += 7;
}

fn check_reference_boundaries() {
    let mut integer = 5;
    let returned = pass_i32(&mut integer);
    *returned *= 3;
    assert!(integer == 15);

    let mut point = Point { x: 2, y: 4 };
    let returned = pass_point(&mut point);
    returned.x += 8;
    returned.y += 6;
    assert!(point.x == 10);
    assert!(point.y == 10);

    let mut single = 1.5_f32;
    let mut double = 10.0_f64;
    adjust_floats(&mut single, &mut double);
    assert!(single == 2.75);
    assert!(double == 7.5);

    let mut nested_value = 20;
    let mut first_reference = &mut nested_value;
    adjust_nested(&mut first_reference);
    assert!(nested_value == 27);

    let mut captured = 30;
    {
        let captured_reference = &mut captured;
        let mut closure = || {
            *captured_reference += 12;
        };
        closure();
    }
    assert!(captured == 42);
}

fn check_maybe_uninit_pointer_write_through() {
    let mut integer = std::mem::MaybeUninit::new(11_i32);
    unsafe {
        *integer.as_mut_ptr() = 42;
        assert!(integer.assume_init() == 42);
    }

    let mut text = std::mem::MaybeUninit::new("old");
    unsafe {
        *text.as_mut_ptr() = "new";
        assert!(text.assume_init() == "new");
    }

    let mut bytes = [std::mem::MaybeUninit::new(7_u8); 64];
    unsafe {
        *bytes[0].as_mut_ptr() = 11;
        *bytes[63].as_mut_ptr() = 99;
        assert!(bytes[0].assume_init() == 11);
        assert!(bytes[1].assume_init() == 7);
        assert!(bytes[62].assume_init() == 7);
        assert!(bytes[63].assume_init() == 99);
    }

    let mut points = [RepeatPoint { x: 3, y: 4 }; 64];
    points[0].x = 30;
    points[63].y = 40;
    assert!(points[0].x == 30 && points[0].y == 4);
    assert!(points[1].x == 3 && points[1].y == 4);
    assert!(points[63].x == 3 && points[63].y == 40);
}

fn main() {
    // 1. Initial setup
    let mut point = Point { x: 1, y: 5 };
    assert!(point.x == 1 && point.y == 5);

    // 2. Test shared borrow
    let current_x = get_x_coord(&point); // Pass shared borrow
    assert!(current_x == 1);
    // Point should be unchanged after shared borrow
    assert!(point.x == 1 && point.y == 5);

    // 3. Test mutable borrow of the whole struct
    shift_point(&mut point); // Pass mutable borrow

    // 4. Assert final state after mutable borrow
    // Inside shift_point:
    // p.x = 1 + 10 = 11
    // p.y = -(5) = -5 (via make_y_negative(&mut p.y))
    // p.x = 11 + 5 = 16
    assert!(point.x == 16);
    assert!(point.y == -5);

    check_arrays_of_mutable_references();
    check_reference_boundaries();
    check_maybe_uninit_pointer_write_through();
}
