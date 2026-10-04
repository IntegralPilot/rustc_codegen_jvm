#[repr(i64)]
enum Sparse<T> {
    Negative = -5,
    Payload(T) = 17,
    Largest = i64::MAX,
}

#[inline(never)]
fn inspect(value: &Sparse<u8>) -> i64 {
    match value {
        Sparse::Negative => -5,
        Sparse::Payload(value) => i64::from(*value),
        Sparse::Largest => i64::MAX,
    }
}

pub fn run() {
    for value in [Sparse::Negative, Sparse::Payload(31), Sparse::Largest] {
        let expected = match &value {
            Sparse::Negative => -5,
            Sparse::Payload(_) => 31,
            Sparse::Largest => i64::MAX,
        };
        assert_eq!(inspect(std::hint::black_box(&value)), expected);
    }
}
