struct Words([u32; 2]);

const ELEMENT_FIRST: &Words = &Words([17, 29]);
const ELEMENT_FIRST_ARRAY: &[Words; 1] = core::array::from_ref(ELEMENT_FIRST);
const ARRAY_FIRST: &Words = &Words([41, 53]);
const ARRAY_FIRST_ARRAY: &[Words; 1] = core::array::from_ref(ARRAY_FIRST);

#[inline(never)]
fn element_first() -> &'static Words {
    ELEMENT_FIRST
}

#[inline(never)]
fn element_first_array() -> &'static [Words] {
    ELEMENT_FIRST_ARRAY
}

#[inline(never)]
fn array_first() -> &'static [Words] {
    ARRAY_FIRST_ARRAY
}

#[inline(never)]
fn array_first_element() -> &'static Words {
    ARRAY_FIRST
}

pub fn run() {
    let element = core::hint::black_box(element_first());
    let array = core::hint::black_box(element_first_array());
    assert_eq!(element.0, [17, 29]);
    assert_eq!(array[0].0, element.0);
    assert!(core::ptr::eq(element, &array[0]));

    let array = core::hint::black_box(array_first());
    let element = core::hint::black_box(array_first_element());
    assert_eq!(element.0, [41, 53]);
    assert_eq!(array[0].0, element.0);
    assert!(core::ptr::eq(element, &array[0]));
}
