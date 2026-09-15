//! JVM names shared by body selection and generated representation schemas.
pub const SLICE_VIEW_CLASS: &str = "org/rustlang/runtime/SliceView";
pub const UTF8_VIEW_CLASS: &str = "org/rustlang/runtime/Utf8View";
pub const POINTER_CLASS: &str = "org/rustlang/runtime/Pointer";
pub const RELATIVE_POINTER_METHOD_SUFFIX: &str = "$relative";
pub const RELATIVE_POINTER_ELEMENT_OFFSET_SUFFIX: &str = "$rcj$elementOffset";
pub const RELATIVE_POINTER_BYTE_OFFSET_SUFFIX: &str = "$rcj$byteOffset";

pub fn relative_pointer_element_offset_field(field: &str) -> String {
    format!("{field}{RELATIVE_POINTER_ELEMENT_OFFSET_SUFFIX}")
}

pub fn relative_pointer_byte_offset_field(field: &str) -> String {
    format!("{field}{RELATIVE_POINTER_BYTE_OFFSET_SUFFIX}")
}
