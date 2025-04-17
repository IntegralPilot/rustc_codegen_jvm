enum Test {
    A,
    B,
    C,
}

fn main() {
    let mut test_enum = Test::A;

    match test_enum {
        Test::A => { /* correct */},
        Test::B => panic!("Enum should be A, not B"),
        Test::C => panic!("Enum shuld be A, not C"),
    }
}
