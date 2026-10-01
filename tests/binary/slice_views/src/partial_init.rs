// Tuple::clone needs default physical components until each field is initialized.
pub fn run() {
    let bytes = [3, 5, 7, 11];
    let pairs = vec![(vec![13_u8], &bytes[1..3]), (vec![], &bytes[..0])];
    let cloned = std::hint::black_box(&pairs).clone();
    assert_eq!(cloned, pairs);
    let strings = vec![(vec![17_u8], "éclair"), (vec![], "")];
    assert_eq!(std::hint::black_box(&strings).clone(), strings);
}
