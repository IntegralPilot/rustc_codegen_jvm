//! Representation types used by semantic lowering and JVM schemas.
use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)] /* Reference variant currently unused */
pub enum Type {
    Void,
    /// Rust's inhabited, zero-sized unit value. It has no JVM stack value or local slot.
    Unit,
    Boolean,
    Char,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F16,
    F32,
    F64,
    Pointer(Box<Type>), // A sized Rust reference or raw pointer.
    MutableReference(Box<Type>),
    Reference(Box<Type>), // Representing references, not currently constructed but might be useful in future for more complex things.
    Array(Box<Type>),     // Representing arrays
    Slice(Box<Type>),     // A view over an array with an offset and length.
    Str,                  // A borrowed UTF-8 byte view.
    Class(String),        // For structs, enums, and potentially Objects
    Interface(String),    // dyn TraitName
}

pub fn is_non_null_class_name(class_name: &str) -> bool {
    class_name
        .rsplit('/')
        .next()
        .is_some_and(|leaf| leaf.starts_with("NonNull_"))
}

impl Type {
    /// A readable, descriptor-stable token for generated JVM ABI helper names.
    pub fn jvm_abi_name_token(&self) -> String {
        fn identifier(raw: &str) -> String {
            let mut result = String::with_capacity(raw.len());
            let mut separator = false;
            for ch in raw.chars() {
                if ch.is_ascii_alphanumeric() || ch == '_' {
                    result.push(ch);
                    separator = false;
                } else if !separator && !result.is_empty() {
                    result.push('_');
                    separator = true;
                }
            }
            while result.ends_with('_') {
                result.pop();
            }
            if result.is_empty() {
                "Object".to_string()
            } else {
                result
            }
        }

        match self {
            Type::Void | Type::Unit => "void".to_string(),
            Type::Boolean => "boolean".to_string(),
            Type::I8 | Type::U8 => "byte".to_string(),
            Type::I16 => "short".to_string(),
            Type::Char | Type::U16 => "char".to_string(),
            Type::I32 | Type::U32 => "int".to_string(),
            Type::I64 | Type::U64 => "long".to_string(),
            Type::F16 => "binary16".to_string(),
            Type::F32 => "float".to_string(),
            Type::F64 => "double".to_string(),
            Type::Pointer(_) => "Pointer".to_string(),
            Type::MutableReference(inner) | Type::Array(inner) => {
                format!("Array_{}", inner.jvm_abi_name_token())
            }
            Type::Reference(inner) => inner.jvm_abi_name_token(),
            Type::Slice(_) => "SliceView".to_string(),
            Type::Str => "Utf8View".to_string(),
            Type::Class(name) | Type::Interface(name) => identifier(name),
        }
    }

    /// The JVM's own immutable string class, used only for JVM ABI values.
    pub fn java_string() -> Self {
        Self::Class(JAVA_STRING_CLASS.to_string())
    }

    /// Returns the JVM type descriptor string (e.g., "I", "Ljava/lang/String;", "[I").
    pub fn to_jvm_descriptor(&self) -> String {
        let mut descriptor = String::new();
        self.write_jvm_descriptor(&mut descriptor);
        descriptor
    }

    /// Appends this type's JVM descriptor without allocating intermediate
    /// descriptors for nested array/reference types or signature components.
    pub fn write_jvm_descriptor(&self, descriptor: &mut String) {
        match self {
            Type::Void => descriptor.push('V'),
            // Unit is only descriptor-compatible as a method return. Parameters and fields
            // omit it before descriptors are built.
            Type::Unit => descriptor.push('V'),
            Type::Boolean => descriptor.push('Z'),
            Type::Char => descriptor.push('C'),
            Type::I8 | Type::U8 => descriptor.push('B'),
            Type::I16 => descriptor.push('S'),
            Type::U16 => descriptor.push('C'),
            Type::I32 | Type::U32 => descriptor.push('I'),
            Type::I64 | Type::U64 => descriptor.push('J'),
            // Binary16 is stored as its raw 16-bit IEEE representation.
            Type::F16 => descriptor.push('S'),
            Type::F32 => descriptor.push('F'),
            Type::F64 => descriptor.push('D'),
            Type::Pointer(_) => {
                descriptor.push('L');
                descriptor.push_str(POINTER_CLASS);
                descriptor.push(';');
            }
            Type::Str => {
                descriptor.push('L');
                descriptor.push_str(UTF8_VIEW_CLASS);
                descriptor.push(';');
            }
            Type::Class(name) | Type::Interface(name) => {
                descriptor.push('L');
                for character in name.chars() {
                    descriptor.push(if character == '.' { '/' } else { character });
                }
                descriptor.push(';');
            }
            Type::Reference(inner) => inner.write_jvm_descriptor(descriptor),
            Type::MutableReference(inner) => {
                if inner.has_jvm_value() {
                    descriptor.push('[');
                    inner.write_jvm_descriptor(descriptor);
                } else {
                    descriptor.push_str("Ljava/lang/Object;");
                }
            }
            Type::Array(element_type) => {
                if element_type.has_jvm_value() {
                    descriptor.push('[');
                    element_type.write_jvm_descriptor(descriptor);
                } else {
                    descriptor.push_str("[Ljava/lang/Object;");
                }
            }
            Type::Slice(_) => {
                descriptor.push('L');
                descriptor.push_str(SLICE_VIEW_CLASS);
                descriptor.push(';');
            }
        }
    }

    pub fn to_jvm_return_descriptor(&self) -> String {
        let mut descriptor = String::new();
        self.write_jvm_return_descriptor(&mut descriptor);
        descriptor
    }

    pub fn write_jvm_return_descriptor(&self, descriptor: &mut String) {
        if matches!(self, Type::Unit | Type::Void) {
            descriptor.push('V');
        } else {
            self.write_jvm_descriptor(descriptor);
        }
    }

    pub fn has_jvm_value(&self) -> bool {
        !matches!(self, Type::Unit | Type::Void)
    }

    /// Returns the JVM internal name for class/interface types used by anewarray.
    /// Returns None for primitive types.
    pub fn to_jvm_internal_name(&self) -> Option<String> {
        match self {
            Type::Str => Some(UTF8_VIEW_CLASS.to_string()),
            Type::Class(name) | Type::Interface(name) => Some(name.replace('.', "/")),
            Type::Pointer(_) => Some(POINTER_CLASS.to_string()),
            Type::Reference(inner) => inner.to_jvm_internal_name(), // delegate to inner type
            // For array-valued types, the descriptor is the component class name
            // expected by `anewarray`. Mutable references use one-element arrays.
            Type::Array(_) | Type::MutableReference(_) => Some(self.to_jvm_descriptor()),
            Type::Slice(_) => Some(SLICE_VIEW_CLASS.to_string()),
            // Primitives don't have an internal name for anewarray.
            _ => None,
        }
    }

    /// Returns the 'atype' code used by the `newarray` instruction for primitive types.
    /// See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.newarray
    pub fn to_jvm_primitive_array_type_code(&self) -> Option<u8> {
        match self {
            Type::Boolean => Some(4),          // T_BOOLEAN
            Type::Char => Some(5),             // T_CHAR
            Type::F32 => Some(6),              // T_FLOAT
            Type::F64 => Some(7),              // T_DOUBLE
            Type::I8 | Type::U8 => Some(8),    // T_BYTE
            Type::I16 | Type::F16 => Some(9),  // T_SHORT
            Type::U16 => Some(5),              // T_CHAR
            Type::I32 | Type::U32 => Some(10), // T_INT
            Type::I64 | Type::U64 => Some(11), // T_LONG
            _ => None,                         // Not a primitive type suitable for newarray
        }
    }

    /// Returns the appropriate JVM array element store instruction.
    pub fn get_jvm_array_store_instruction(&self) -> Option<JVMInstruction> {
        match self {
            Type::I8 | Type::U8 => Some(JVMInstruction::Bastore),
            Type::I16 | Type::F16 => Some(JVMInstruction::Sastore),
            Type::U16 => Some(JVMInstruction::Castore),
            Type::Boolean => Some(JVMInstruction::Bastore),
            Type::Char => Some(JVMInstruction::Castore),
            Type::I32 | Type::U32 => Some(JVMInstruction::Iastore),
            Type::I64 | Type::U64 => Some(JVMInstruction::Lastore),
            Type::F32 => Some(JVMInstruction::Fastore),
            Type::F64 => Some(JVMInstruction::Dastore),
            // Reference types:
            Type::Str
            | Type::Class(_)
            | Type::Interface(_)
            | Type::Array(_)
            | Type::Slice(_)
            | Type::Reference(_)
            | Type::MutableReference(_) => Some(JVMInstruction::Aastore),
            Type::Pointer(_) => Some(JVMInstruction::Aastore),
            Type::Void => None,
            Type::Unit => None,
        }
    }

    /// Create a Type from a Constant.
    pub fn from_constant(constant: &Constant) -> Self {
        match constant {
            Constant::Unit => Type::Unit,
            Constant::StaticRef { ty, .. } => ty.clone(),
            Constant::FunctionPointer { interface_name, .. } => {
                Type::Interface(interface_name.clone())
            }
            Constant::FactoryCall { ty, .. } => ty.clone(),
            Constant::StaticCall { ty, .. } => ty.clone(),
            Constant::PointerAddress { pointee, .. } => Type::Pointer(pointee.clone()),
            Constant::RepeatedBytePointer { pointee, .. } => Type::Pointer(pointee.clone()),
            Constant::ByteArrayPointer { pointee, .. } => Type::Pointer(pointee.clone()),
            Constant::InternedPointer { pointee, .. } => Type::Pointer(pointee.clone()),
            Constant::Null(ty) => ty.clone(),
            Constant::I8(_) => Type::I8,
            Constant::U8(_) => Type::U8,
            Constant::I16(_) => Type::I16,
            Constant::U16(_) => Type::U16,
            Constant::I32(_) => Type::I32,
            Constant::U32(_) => Type::U32,
            Constant::I64(_) => Type::I64,
            Constant::U64(_) => Type::U64,
            Constant::F16(_) => Type::F16,
            Constant::F32(_) => Type::F32,
            Constant::F64(_) => Type::F64,
            Constant::Array(inner_ty, _) => Type::Array(inner_ty.clone()),
            Constant::Slice(inner_ty, _) => Type::Slice(inner_ty.clone()),
            Constant::SliceRef { element_type, .. } => Type::Slice(element_type.clone()),
            Constant::Boolean(_) => Type::Boolean,
            Constant::Char(_) => Type::Char,
            Constant::Str(_) => Type::Str,
            Constant::String(_) => Type::java_string(),
            Constant::Instance {
                class_name, params, ..
            } if class_name == POINTER_CLASS => {
                Type::Pointer(Box::new(if params.len() == 3 {
                    params
                        .first()
                        .map(Type::from_constant)
                        .unwrap_or(Type::Unit)
                } else {
                    // Address-only constructors carry no JVM pointee value from
                    // which to infer a more specific OOMIR type.
                    Type::Unit
                }))
            }
            Constant::Instance { class_name, .. } => Type::Class(class_name.to_string()),
        }
    }

    pub fn is_jvm_primitive(&self) -> bool {
        matches!(
            self,
            Type::Boolean
                | Type::Char
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::I64
                | Type::U64
                | Type::F16
                | Type::F32
                | Type::F64
        )
    }

    /// Checks if the type corresponds to a JVM reference type (Object, Array, String, etc.)
    /// as opposed to a primitive (int, float, boolean, etc.) or Void.
    pub fn is_jvm_reference_type(&self) -> bool {
        matches!(
            self,
            Type::Reference(_)
                | Type::Pointer(_)
                | Type::MutableReference(_)
                | Type::Array(_)
                | Type::Slice(_)
                | Type::Str
                | Type::Class(_)
                | Type::Interface(_)
        )
    }

    /// Checks if the type is treated as a primitive on the JVM stack
    /// (byte, short, int, long, float, double, char, boolean).
    pub fn is_jvm_primitive_like(&self) -> bool {
        matches!(
            self,
            Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::I64
                | Type::U64
                | Type::F16
                | Type::F32
                | Type::F64
                | Type::Char
                | Type::Boolean
        )
    }

    /// Provides the JVM internal name or descriptor needed for Checkcast/Anewarray.
    pub fn to_jvm_descriptor_or_internal_name(&self) -> Option<String> {
        match self {
            Type::Class(name) | Type::Interface(name) => Some(name.clone()),
            Type::Pointer(_) => Some(POINTER_CLASS.to_string()),
            Type::Array(_) => Some(self.to_jvm_descriptor()), // Array descriptor works for checkcast/anewarray
            Type::Slice(_) => Some(SLICE_VIEW_CLASS.to_string()),
            Type::Str => Some(UTF8_VIEW_CLASS.to_string()),
            Type::Reference(inner) => inner.to_jvm_descriptor_or_internal_name(),
            Type::MutableReference(inner) => {
                Type::Array(inner.clone()).to_jvm_descriptor_or_internal_name()
            } // MutableReference is treated as an array
            _ => None,
        }
    }

    /// Recursively replaces all occurrences of `Type::Class(old_name)` with `Type::Class(new_name)`.
    pub fn replace_class(&mut self, old_name: &str, new_name: &str) -> bool {
        match self {
            Type::Class(name) | Type::Interface(name) => {
                if name == old_name {
                    *name = new_name.to_string();
                    return true;
                }
                false
            }
            // Handle nested types recursively
            Type::MutableReference(inner)
            | Type::Pointer(inner)
            | Type::Reference(inner)
            | Type::Array(inner)
            | Type::Slice(inner) => inner.replace_class(old_name, new_name),
            // Primitive types and Void are unaffected.
            Type::Void
            | Type::Unit
            | Type::Boolean
            | Type::Char
            | Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::F16
            | Type::F32
            | Type::F64
            | Type::Str => {
                // No class names to replace here
                false
            }
        }
    }

    /// Gets the name of the class to call methods on, if applicable.
    pub fn get_class_name(&self) -> Option<&str> {
        breadcrumbs::log!(
            LogLevel::Info,
            "class_name_fetching",
            format!("Fetching class name for type: {:?}", self)
        );
        match self {
            Type::Class(name) | Type::Interface(name) => Some(name),
            Type::Str => Some(UTF8_VIEW_CLASS),
            Type::Array(inner) | Type::MutableReference(inner) | Type::Reference(inner) => {
                inner.get_class_name()
            }
            // Method dispatch through a Rust reference targets the pointee;
            // pointer-native methods are redirected explicitly during lowering.
            Type::Pointer(inner) => inner.get_class_name(),
            _ => None,
        }
    }
}
