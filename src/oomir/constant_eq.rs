//! Structural constant identity preserves floating-point bit patterns.
use super::Constant;

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;
    #[test]
    fn constant_identity_and_hash_preserve_float_bits_recursively() {
        let nan = Constant::F64(f64::from_bits(0x7ff8_0000_0000_0042));
        assert_eq!(nan, nan.clone());
        let values = HashSet::from([
            Constant::F64(0.0),
            Constant::F64(-0.0),
            nan.clone(),
            nan.clone(),
        ]);
        assert_eq!(values.len(), 3);
        assert!(values.contains(&nan));
        let array = Constant::Array(Box::new(super::super::Type::F64), vec![nan]);
        assert_eq!(array, array.clone());
        assert!(HashSet::from([array.clone()]).contains(&array));
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (
                Self::StaticRef {
                    owner_class: a0,
                    field_name: a1,
                    ty: a2,
                },
                Self::StaticRef {
                    owner_class: b0,
                    field_name: b1,
                    ty: b2,
                },
            ) => a0 == b0 && a1 == b1 && a2 == b2,
            (
                Self::FunctionPointer {
                    adapter_class: a0,
                    interface_name: a1,
                },
                Self::FunctionPointer {
                    adapter_class: b0,
                    interface_name: b1,
                },
            ) => a0 == b0 && a1 == b1,
            (
                Self::FactoryCall {
                    owner_class: a0,
                    method_name: a1,
                    ty: a2,
                },
                Self::FactoryCall {
                    owner_class: b0,
                    method_name: b1,
                    ty: b2,
                },
            ) => a0 == b0 && a1 == b1 && a2 == b2,
            (
                Self::StaticCall {
                    owner_class: a0,
                    method_name: a1,
                    args: a2,
                    param_types: a3,
                    ty: a4,
                },
                Self::StaticCall {
                    owner_class: b0,
                    method_name: b1,
                    args: b2,
                    param_types: b3,
                    ty: b4,
                },
            ) => a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4,
            (
                Self::PointerAddress {
                    address: a0,
                    view_size: a1,
                    pointee: a2,
                },
                Self::PointerAddress {
                    address: b0,
                    view_size: b1,
                    pointee: b2,
                },
            ) => a0 == b0 && a1 == b1 && a2 == b2,
            (
                Self::RepeatedBytePointer {
                    identity: a0,
                    byte: a1,
                    length: a2,
                    offset: a3,
                    view_size: a4,
                    alignment: a5,
                    view_codec: a6,
                    pointee: a7,
                },
                Self::RepeatedBytePointer {
                    identity: b0,
                    byte: b1,
                    length: b2,
                    offset: b3,
                    view_size: b4,
                    alignment: b5,
                    view_codec: b6,
                    pointee: b7,
                },
            ) => {
                a0 == b0
                    && a1 == b1
                    && a2 == b2
                    && a3 == b3
                    && a4 == b4
                    && a5 == b5
                    && a6 == b6
                    && a7 == b7
            }
            (
                Self::ByteArrayPointer {
                    identity: a0,
                    bytes: a1,
                    offset: a2,
                    view_size: a3,
                    alignment: a4,
                    view_codec: a5,
                    pointee: a6,
                },
                Self::ByteArrayPointer {
                    identity: b0,
                    bytes: b1,
                    offset: b2,
                    view_size: b3,
                    alignment: b4,
                    view_codec: b5,
                    pointee: b6,
                },
            ) => a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6,
            (
                Self::InternedPointer {
                    identity: a0,
                    value: a1,
                    array_backed: a2,
                    allocation_size: a3,
                    offset: a4,
                    view_size: a5,
                    alignment: a6,
                    view_codec: a7,
                    pointee: a8,
                },
                Self::InternedPointer {
                    identity: b0,
                    value: b1,
                    array_backed: b2,
                    allocation_size: b3,
                    offset: b4,
                    view_size: b5,
                    alignment: b6,
                    view_codec: b7,
                    pointee: b8,
                },
            ) => {
                a0 == b0
                    && a1 == b1
                    && a2 == b2
                    && a3 == b3
                    && a4 == b4
                    && a5 == b5
                    && a6 == b6
                    && a7 == b7
                    && a8 == b8
            }
            (Self::Null(a0), Self::Null(b0)) => a0 == b0,
            (Self::I8(a0), Self::I8(b0)) => a0 == b0,
            (Self::U8(a0), Self::U8(b0)) => a0 == b0,
            (Self::I16(a0), Self::I16(b0)) => a0 == b0,
            (Self::U16(a0), Self::U16(b0)) => a0 == b0,
            (Self::I32(a0), Self::I32(b0)) => a0 == b0,
            (Self::U32(a0), Self::U32(b0)) => a0 == b0,
            (Self::I64(a0), Self::I64(b0)) => a0 == b0,
            (Self::U64(a0), Self::U64(b0)) => a0 == b0,
            (Self::F16(a0), Self::F16(b0)) => a0 == b0,
            (Self::F32(a0), Self::F32(b0)) => a0.to_bits() == b0.to_bits(),
            (Self::F64(a0), Self::F64(b0)) => a0.to_bits() == b0.to_bits(),
            (Self::Boolean(a0), Self::Boolean(b0)) => a0 == b0,
            (Self::Char(a0), Self::Char(b0)) => a0 == b0,
            (Self::Str(a0), Self::Str(b0)) => a0 == b0,
            (Self::String(a0), Self::String(b0)) => a0 == b0,
            (Self::Array(a0, a1), Self::Array(b0, b1)) => a0 == b0 && a1 == b1,
            (Self::Slice(a0, a1), Self::Slice(b0, b1)) => a0 == b0 && a1 == b1,
            (
                Self::SliceRef {
                    backing: a0,
                    element_type: a1,
                    offset: a2,
                    length: a3,
                },
                Self::SliceRef {
                    backing: b0,
                    element_type: b1,
                    offset: b2,
                    length: b3,
                },
            ) => a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3,
            (
                Self::Instance {
                    class_name: a0,
                    fields: a1,
                    params: a2,
                    param_types: a3,
                },
                Self::Instance {
                    class_name: b0,
                    fields: b1,
                    params: b2,
                    param_types: b3,
                },
            ) => a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3,
            _ => false,
        }
    }
}
