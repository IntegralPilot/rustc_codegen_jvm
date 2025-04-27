// Simple struct
#[derive(Debug, Clone, Copy)]
struct Point {
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
}