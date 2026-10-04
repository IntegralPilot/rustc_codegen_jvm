use std::hint::black_box;

trait Value {
    fn value(&self) -> u32;
}

trait Text {
    fn text(&self) -> &'static str;
}

impl Text for u32 {
    fn text(&self) -> &'static str {
        "second option payload"
    }
}

#[inline(never)]
unsafe fn optional_text<'a>(pointer: *const dyn Text) -> Option<&'a dyn Text> {
    unsafe { pointer.as_ref() }
}
impl Value for u32 {
    fn value(&self) -> u32 {
        *self
    }
}

#[inline(never)]
unsafe fn optional<'a>(pointer: *const dyn Value) -> Option<&'a dyn Value> {
    unsafe { pointer.as_ref() }
}

pub fn run() {
    let value = black_box(73u32);
    let pointer = &value as &dyn Value as *const dyn Value;
    assert_eq!(unsafe { optional(black_box(pointer)) }.unwrap().value(), 73);
    let null =
        std::ptr::from_raw_parts::<dyn Value>(std::ptr::null::<()>(), std::ptr::metadata(pointer));
    assert!(unsafe { optional(black_box(null)) }.is_none());
    // Shared Option tags must preserve each trait's dispatch payload.
    let text = &value as &dyn Text as *const dyn Text;
    assert_eq!(
        unsafe { optional_text(black_box(text)) }.unwrap().text(),
        "second option payload"
    );
    let null_text =
        std::ptr::from_raw_parts::<dyn Text>(std::ptr::null::<()>(), std::ptr::metadata(text));
    assert!(unsafe { optional_text(black_box(null_text)) }.is_none());
}
