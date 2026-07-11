fn sum(values: &[i32]) -> i32 {
    let mut total = 0;
    let mut index = 0;
    while index < values.len() {
        total += values[index];
        index += 1;
    }
    total
}

fn check_edges(whole: &[i32]) {
    let [first, rest @ ..] = whole else {
        panic!()
    };
    assert!(*first == 10);
    assert!(rest.len() == 4);
    assert!(sum(rest) == 140);

    let [prefix @ .., last] = whole else {
        panic!()
    };
    assert!(prefix.len() == 4);
    assert!(sum(prefix) == 100);
    assert!(*last == 50);
}

fn check_middle_and_nested(whole: &[i32]) {
    let [first, middle @ .., last] = whole else {
        panic!()
    };
    assert!(*first == 10);
    assert!(middle.len() == 3);
    assert!(middle[0] == 20);
    assert!(middle[2] == 40);
    assert!(*last == 50);

    let [_, tail @ ..] = whole else {
        panic!()
    };
    let [_, nested @ .., _] = tail else {
        panic!()
    };
    assert!(nested.len() == 2);
    assert!(sum(nested) == 70);
}

fn check_empty_views() {
    let pair = [7, 9];
    let [_, empty_middle @ .., _] = &pair;
    assert!(empty_middle.len() == 0);

    let singleton = [11];
    let [empty_prefix @ .., _] = &singleton;
    let [_, empty_suffix @ ..] = &singleton;
    assert!(empty_prefix.len() == 0);
    assert!(empty_suffix.len() == 0);

    let empty: &[i32] = &[];
    let [empty_view @ ..] = empty;
    assert!(empty_view.len() == 0);
}

fn check_other_element_types() {
    let words = ["zero", "one", "two", "three"];
    let [_, middle @ .., _] = &words;
    assert!(middle.len() == 2);
    assert!(middle[0] == "one");
    assert!(middle[1] == "two");
}

fn check_mutation() {
    let mut values = [1, 2, 3, 4, 5];
    {
        let whole: &mut [i32] = &mut values;
        let [_, middle @ .., _] = whole else {
            panic!()
        };
        middle[0] = 20;
        middle[2] = 40;
        let [_, nested @ ..] = middle else {
            panic!()
        };
        nested[0] = 30;
    }
    assert!(values[0] == 1);
    assert!(values[1] == 20);
    assert!(values[2] == 30);
    assert!(values[3] == 40);
    assert!(values[4] == 5);
}

fn mutate_reborrowed_slice(values: &mut [i32]) {
    let reborrow: &mut [i32] = &mut *values;
    reborrow[0] += 10;

    let [_, middle @ .., _] = reborrow else {
        panic!()
    };
    let nested_reborrow: &mut [i32] = &mut *middle;
    nested_reborrow[0] += 20;
}

fn mutate_array_reference(values: &mut [i32; 4]) {
    values[3] += 40;
}

fn check_mutable_reborrows() {
    let mut values = [1, 2, 3, 4];
    mutate_reborrowed_slice(&mut values);
    mutate_array_reference(&mut values);
    assert!(values[0] == 11);
    assert!(values[1] == 22);
    assert!(values[2] == 3);
    assert!(values[3] == 44);
}

fn check_shim_boundary_match() {
    let bytes = [1_u8, 2, 3, 4, 5];
    let [_, middle @ .., _] = &bytes;
    assert!(middle.starts_with(&[2, 3]));
}

#[inline(never)]
fn utf8_byte_len(value: &str) -> usize {
    value.as_bytes().len()
}

fn check_utf8_string_bridge() {
    assert!(utf8_byte_len("é") == 2);
}

fn check_utf8_char_prefix() {
    assert!("éclair".starts_with('é'));
}

#[inline(never)]
fn utf8_bytes(value: &str) -> &[u8] {
    value.as_bytes()
}

fn check_utf8_byte_contents() {
    let bytes = utf8_bytes("é");
    assert!(bytes.len() == 2);
    assert!(bytes[0] == 0xc3);
    assert!(bytes[1] == 0xa9);
}

#[inline(never)]
fn unchecked_utf8(bytes: &[u8]) -> &str {
    unsafe { core::str::from_utf8_unchecked(bytes) }
}

fn check_slice_to_str_view() {
    let bytes = [0xc3_u8, 0xa9];
    assert!(unchecked_utf8(&bytes) == "é");
}

fn main() {
    let values = [10, 20, 30, 40, 50];
    let whole: &[i32] = &values;
    check_edges(whole);
    check_middle_and_nested(whole);
    check_empty_views();
    check_other_element_types();
    check_mutation();
    check_mutable_reborrows();
    check_shim_boundary_match();
    check_utf8_string_bridge();
    check_utf8_char_prefix();
    check_utf8_byte_contents();
    check_slice_to_str_view();
}
