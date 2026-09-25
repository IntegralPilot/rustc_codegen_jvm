//! Structural constant identity, shared by generation and class merging.
use super::{self as jvm, Constant};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ConstantKey {
    Utf8(jvm::JavaString),
    Integer(i32),
    Float(u32),
    Long(i64),
    Double(u64),
    Class(u16),
    String(u16),
    FieldRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    MethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    InterfaceMethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    NameAndType {
        name_index: u16,
        descriptor_index: u16,
    },
    MethodHandle {
        reference_kind: u8,
        reference_index: u16,
    },
    MethodType(u16),
    Dynamic {
        bootstrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
    InvokeDynamic {
        bootstrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
    Module(u16),
    Package(u16),
}

impl From<&Constant<'_>> for ConstantKey {
    fn from(constant: &Constant<'_>) -> Self {
        match constant {
            Constant::Utf8(value) => ConstantKey::Utf8(value.as_ref().to_owned()),
            Constant::Integer(value) => ConstantKey::Integer(*value),
            Constant::Float(value) => ConstantKey::Float(value.to_bits()),
            Constant::Long(value) => ConstantKey::Long(*value),
            Constant::Double(value) => ConstantKey::Double(value.to_bits()),
            Constant::Class(name_index) => ConstantKey::Class(*name_index),
            Constant::String(string_index) => ConstantKey::String(*string_index),
            Constant::FieldRef {
                class_index,
                name_and_type_index,
            } => ConstantKey::FieldRef {
                class_index: *class_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::MethodRef {
                class_index,
                name_and_type_index,
            } => ConstantKey::MethodRef {
                class_index: *class_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::InterfaceMethodRef {
                class_index,
                name_and_type_index,
            } => ConstantKey::InterfaceMethodRef {
                class_index: *class_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::NameAndType {
                name_index,
                descriptor_index,
            } => ConstantKey::NameAndType {
                name_index: *name_index,
                descriptor_index: *descriptor_index,
            },
            Constant::MethodHandle {
                reference_kind,
                reference_index,
            } => ConstantKey::MethodHandle {
                reference_kind: reference_kind.kind(),
                reference_index: *reference_index,
            },
            Constant::MethodType(descriptor_index) => ConstantKey::MethodType(*descriptor_index),
            Constant::Dynamic {
                bootstrap_method_attr_index,
                name_and_type_index,
            } => ConstantKey::Dynamic {
                bootstrap_method_attr_index: *bootstrap_method_attr_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::InvokeDynamic {
                bootstrap_method_attr_index,
                name_and_type_index,
            } => ConstantKey::InvokeDynamic {
                bootstrap_method_attr_index: *bootstrap_method_attr_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::Module(name_index) => ConstantKey::Module(*name_index),
            Constant::Package(name_index) => ConstantKey::Package(*name_index),
        }
    }
}
