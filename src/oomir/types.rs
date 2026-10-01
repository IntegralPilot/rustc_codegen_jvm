//! Representation types used by semantic lowering and JVM schemas.
use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Pointer(Pointee), // A sized Rust reference or raw pointer.
    Array(Box<Type>), // Representing arrays
    Slice(Box<Type>), // A view over an array with an offset and length.
    TaggedI64,
    Str,               // A borrowed UTF-8 byte view.
    Class(String),     // For structs, enums, and potentially Objects
    Interface(String), // dyn TraitName
}

/// Source-language layout attached to an address, independent of its JVM carrier.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AddressSchema {
    pub size: u32,
    pub codec: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pointee {
    pub value: Box<Type>,
    pub layout: Option<std::sync::Arc<AddressSchema>>,
}
impl AsRef<Type> for Pointee {
    fn as_ref(&self) -> &Type {
        &self.value
    }
}
impl std::ops::Deref for Pointee {
    type Target = Type;
    fn deref(&self) -> &Type {
        &self.value
    }
}
impl std::ops::DerefMut for Pointee {
    fn deref_mut(&mut self) -> &mut Type {
        &mut self.value
    }
}

pub fn is_non_null_class_name(class_name: &str) -> bool {
    class_name
        .rsplit('/')
        .next()
        .is_some_and(|leaf| leaf.starts_with("NonNull_"))
}

impl Type {
    pub fn pointer(value: Type) -> Self {
        Self::Pointer(Pointee {
            value: Box::new(value),
            layout: None,
        })
    }
    pub fn with_address_layout(self, size: u32, codec: Option<String>) -> Self {
        let Self::Pointer(mut pointee) = self else {
            return self;
        };
        pointee.layout = Some(std::sync::Arc::new(AddressSchema { size, codec }));
        Self::Pointer(pointee)
    }

    pub fn materialize_address(
        &self,
        cp: &mut jvm_compiler_core::classfile::constant_pool::InternedConstantPool,
        code: &mut Vec<jvm_compiler_core::classfile::attributes::Instruction>,
    ) -> jvm_compiler_core::classfile::Result<()> {
        if let Self::Pointer(pointee) = self
            && let Some(layout) = &pointee.layout
        {
            jvm_compiler_core::jvm::abi::materialize_typed_address(
                cp,
                code,
                layout.size,
                layout.codec.as_deref(),
            )
        } else {
            jvm_compiler_core::jvm::abi::materialize_address(cp, code, self.address_plan())
        }
    }
    pub fn scalar_address_size(&self) -> Option<u32> {
        let Self::Pointer(inner) = self else {
            return None;
        };
        Some(match inner.as_ref() {
            Self::Boolean | Self::I8 | Self::U8 => 1,
            Self::I16 | Self::U16 | Self::F16 => 2,
            Self::I32 | Self::U32 | Self::F32 => 4,
            Self::I64 | Self::U64 | Self::F64 => 8,
            _ => return None,
        })
    }

    pub fn address_plan(&self) -> u32 {
        use jvm_compiler_core::jvm::abi;
        match self {
            Self::Pointer(inner) => match inner.as_ref() {
                Self::Slice(_) | Self::Str => abi::STORED_VIEW,
                Self::Pointer(_) => abi::STORED_ADDRESS,
                _ => self.scalar_address_size().unwrap_or(0),
            },
            _ => 0,
        }
    }

    pub fn component_shape(&self) -> Option<jvm_compiler_core::ir::ComponentShape> {
        use jvm_compiler_core::ir::ComponentShape;
        if matches!(self, Self::TaggedI64) {
            Some(ComponentShape::TaggedI64)
        } else if matches!(self, Self::Slice(_) | Self::Str) {
            Some(ComponentShape::View)
        } else if self.scalar_address_size().is_some() {
            Some(ComponentShape::Address)
        } else if matches!(self, Self::Pointer(_)) {
            Some(ComponentShape::StorageAddress)
        } else {
            None
        }
    }

    pub fn components(&self) -> Option<impl ExactSizeIterator<Item = Type> + use<>> {
        use jvm_compiler_core::ir::ComponentShape;
        self.component_shape().map(|shape| {
            let object = Type::Class("java/lang/Object".into());
            let (parts, count) = match shape {
                ComponentShape::TaggedI64 => ([Self::I64, Self::I64, Self::Unit], 2),
                ComponentShape::View => ([object, Self::I32, Self::U64], 3),
                ComponentShape::Address | ComponentShape::StorageAddress => {
                    ([object, Self::I64, Self::Unit], 2)
                }
            };
            parts.into_iter().take(count)
        })
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

    /// Compare erased JVM carriers without constructing descriptor strings.
    pub fn same_jvm_type(&self, other: &Self) -> bool {
        let (arrays, primitive, name) = self.jvm_shape();
        let (other_arrays, other_primitive, other_name) = other.jvm_shape();
        arrays == other_arrays
            && primitive == other_primitive
            && (name == other_name
                || name
                    .bytes()
                    .map(normalize_separator)
                    .eq(other_name.bytes().map(normalize_separator)))
    }

    /// Array depth, primitive descriptor (or `L`), and borrowed class name.
    fn jvm_shape(&self) -> (usize, char, &str) {
        let mut ty = self;
        let mut arrays = 0;
        let (primitive, name) = loop {
            match ty {
                Type::Array(inner) if inner.has_jvm_value() => {
                    arrays += 1;
                    ty = inner;
                }
                Type::Array(_) => {
                    arrays += 1;
                    break ('L', "java/lang/Object");
                }
                Type::Pointer(_) => break ('L', POINTER_CLASS),
                Type::TaggedI64 => break ('L', TAGGED_LONG_CLASS),
                Type::Str => break ('L', UTF8_VIEW_CLASS),
                Type::Slice(_) => break ('L', SLICE_VIEW_CLASS),
                Type::Class(name) | Type::Interface(name) => break ('L', name.as_str()),
                Type::Void | Type::Unit => break ('V', ""),
                Type::Boolean => break ('Z', ""),
                Type::Char | Type::U16 => break ('C', ""),
                Type::I8 | Type::U8 => break ('B', ""),
                Type::I16 | Type::F16 => break ('S', ""),
                Type::I32 | Type::U32 => break ('I', ""),
                Type::I64 | Type::U64 => break ('J', ""),
                Type::F32 => break ('F', ""),
                Type::F64 => break ('D', ""),
            }
        };
        (arrays, primitive, name)
    }

    /// Append a descriptor while borrowing class names and nested carriers.
    pub fn write_jvm_descriptor(&self, descriptor: &mut String) {
        let (arrays, primitive, name) = self.jvm_shape();
        descriptor.extend(std::iter::repeat_n('[', arrays));
        descriptor.push(primitive);
        if primitive == 'L' {
            if name.contains('.') {
                descriptor.extend(name.chars().map(|c| if c == '.' { '/' } else { c }));
            } else {
                descriptor.push_str(name);
            }
            descriptor.push(';');
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
            Type::TaggedI64 => Some(TAGGED_LONG_CLASS.to_string()),
            Type::Str => Some(UTF8_VIEW_CLASS.to_string()),
            Type::Class(name) | Type::Interface(name) => Some(name.replace('.', "/")),
            Type::Pointer(_) => Some(POINTER_CLASS.to_string()),
            // For array-valued types, the descriptor is the component class name
            // expected by `anewarray`. Mutable references use one-element arrays.
            Type::Array(_) => Some(self.to_jvm_descriptor()),
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
            Type::TaggedI64
            | Type::Str
            | Type::Class(_)
            | Type::Interface(_)
            | Type::Array(_)
            | Type::Slice(_) => Some(JVMInstruction::Aastore),
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
            Constant::FunctionPointer { interface_name, .. }
            | Constant::FunctionHandle { interface_name, .. } => {
                Type::Interface(interface_name.clone())
            }
            Constant::FactoryCall { ty, .. } => ty.clone(),
            Constant::StaticCall { ty, .. } => ty.clone(),
            Constant::PointerAddress { pointee, .. } => Type::pointer(*pointee.clone()),
            Constant::RepeatedBytePointer { pointee, .. } => Type::pointer(*pointee.clone()),
            Constant::ByteArrayPointer { pointee, .. } => Type::pointer(*pointee.clone()),
            Constant::InternedPointer { pointee, .. } => Type::pointer(*pointee.clone()),
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
            Constant::String(_) | Constant::LiteralString(_) => Type::java_string(),
            Constant::Instance {
                class_name, params, ..
            } if class_name == POINTER_CLASS => {
                Type::pointer(if params.len() == 3 {
                    params
                        .first()
                        .map(Type::from_constant)
                        .unwrap_or(Type::Unit)
                } else {
                    // Address-only constructors carry no JVM pointee value from
                    // which to infer a more specific OOMIR type.
                    Type::Unit
                })
            }
            Constant::Instance { class_name, .. } if class_name == TAGGED_LONG_CLASS => {
                Type::TaggedI64
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
            Type::Pointer(_)
                | Type::Array(_)
                | Type::Slice(_)
                | Type::TaggedI64
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
            Type::TaggedI64 => Some(TAGGED_LONG_CLASS.to_string()),
            Type::Str => Some(UTF8_VIEW_CLASS.to_string()),
            // MutableReference is treated as an array
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
            Type::Array(inner) | Type::Slice(inner) => inner.replace_class(old_name, new_name),
            Type::Pointer(inner) => inner.replace_class(old_name, new_name),
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
            | Type::TaggedI64
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
            Type::TaggedI64 => Some(TAGGED_LONG_CLASS),
            Type::Str => Some(UTF8_VIEW_CLASS),
            Type::Array(inner) => inner.get_class_name(),
            // Method dispatch through a Rust reference targets the pointee;
            // pointer-native methods are redirected explicitly during lowering.
            Type::Pointer(inner) => inner.get_class_name(),
            _ => None,
        }
    }
}

fn normalize_separator(byte: u8) -> u8 {
    if byte == b'.' { b'/' } else { byte }
}

#[cfg(test)]
mod tests {
    use super::Type;

    #[test]
    fn erased_carriers_preserve_arrays_zero_sized_values_and_java_names() {
        use Type::*;
        let cases = [
            (I16, "S"),
            (F16, "S"),
            (Char, "C"),
            (U16, "C"),
            (Class("java.lang.É".into()), "Ljava/lang/É;"),
            (Interface("java/lang/É".into()), "Ljava/lang/É;"),
            (Type::pointer(Unit), "Lorg/rustlang/runtime/Pointer;"),
            (Array(Box::new(Unit)), "[Ljava/lang/Object;"),
            (Array(Box::new(Array(Box::new(I8)))), "[[B"),
        ];
        for (ty, expected) in &cases {
            assert_eq!(ty.to_jvm_descriptor(), *expected);
            for (other, other_expected) in &cases {
                assert_eq!(ty.same_jvm_type(other), expected == other_expected);
            }
        }
    }
}
