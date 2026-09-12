//! Runtime constant descriptions and stable structural identity.
use super::*;

#[derive(Debug, Clone)]
pub enum Constant {
    Unit,
    StaticRef {
        owner_class: String,
        field_name: String,
        ty: Type,
    },
    FunctionPointer {
        adapter_class: String,
        interface_name: String,
    },
    /// A lower2-generated call to a private factory method. Large static object
    /// graphs use these to stay within the JVM's per-method bytecode limit.
    FactoryCall {
        owner_class: String,
        method_name: String,
        ty: Type,
    },
    /// A call to a generated pure helper used to construct a constant whose
    /// JVM representation cannot be expressed as a constructor alone.
    StaticCall {
        owner_class: String,
        method_name: String,
        args: Vec<Constant>,
        /// Declared JVM parameter types when they are wider than the concrete
        /// constant carriers (for example, a SliceView passed as Object).
        param_types: Vec<Type>,
        ty: Type,
    },
    /// A provenance-free Rust pointer represented by its exposed address.
    /// The pointee type keeps address-only constants from degenerating into an
    /// ambiguous `Pointer<Unit>` carrier during representation adaptation.
    PointerAddress {
        address: u64,
        view_size: u64,
        pointee: Box<Type>,
    },
    /// A pointer into an anonymous repeated-byte CTFE allocation. This compact
    /// form preserves the allocation without duplicating a large byte image.
    RepeatedBytePointer {
        identity: String,
        byte: u8,
        length: u64,
        offset: u64,
        view_size: u64,
        alignment: u64,
        view_codec: Option<String>,
        pointee: Box<Type>,
    },
    /// A pointer into an arbitrary provenance-free CTFE byte allocation.
    /// This preserves the complete allocation when the pointee is only one
    /// scalar within it, such as the inline pieces used by `fmt::Arguments`.
    ByteArrayPointer {
        identity: String,
        bytes: Vec<u8>,
        offset: u64,
        view_size: u64,
        alignment: u64,
        view_codec: Option<String>,
        pointee: Box<Type>,
    },
    /// A pointer to a fully materialized anonymous CTFE allocation. Repeated
    /// loads use `identity` to retain Rust allocation identity on the JVM.
    InternedPointer {
        identity: String,
        value: Box<Constant>,
        array_backed: bool,
        allocation_size: u64,
        offset: u64,
        view_size: u64,
        alignment: u64,
        view_codec: Box<Constant>,
        pointee: Box<Type>,
    },
    /// A typed JVM null. The type is needed when null appears in a constructor
    /// argument list, because constructor descriptors are exact.
    Null(Type),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    /// Raw IEEE 754 binary16 bits.
    F16(u16),
    F32(f32),
    F64(f64),
    Boolean(bool),
    Char(char),
    Str(String),
    String(String),
    // 0 = the type of elements, 1 = the elements as a vec of constants
    Array(Box<Type>, Vec<Constant>),
    Slice(Box<Type>, Vec<Constant>),
    SliceRef {
        backing: Box<Constant>,
        element_type: Box<Type>,
        offset: u64,
        length: u64,
    },
    Instance {
        /// The fully qualified JVM class name (e.g., "MyStruct", "MyEnum$VariantA").
        class_name: String,
        /// The constant values of the fields, keyed by field name.
        /// Enum fields use their public JVM ABI names (`value`, `_0`, or the
        /// source name for a struct-like variant).
        fields: HashMap<String, Constant>,
        /// Any parameters to the constructor.
        params: Vec<Constant>,
        /// The declared JVM type of each constructor parameter. This can differ
        /// from the concrete constant type when, for example, an enum variant
        /// is passed through its enum interface.
        param_types: Vec<Type>,
    },
}

impl Eq for Constant {}

impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Constant::Unit => 0.hash(state),
            Constant::StaticRef {
                owner_class,
                field_name,
                ty,
            } => {
                1.hash(state);
                owner_class.hash(state);
                field_name.hash(state);
                ty.hash(state);
            }
            Constant::FunctionPointer {
                adapter_class,
                interface_name,
            } => {
                adapter_class.hash(state);
                interface_name.hash(state);
            }
            Constant::FactoryCall {
                owner_class,
                method_name,
                ty,
            } => {
                owner_class.hash(state);
                method_name.hash(state);
                ty.hash(state);
            }
            Constant::StaticCall {
                owner_class,
                method_name,
                args,
                param_types,
                ty,
            } => {
                owner_class.hash(state);
                method_name.hash(state);
                args.hash(state);
                param_types.hash(state);
                ty.hash(state);
            }
            Constant::PointerAddress {
                address,
                view_size,
                pointee,
            } => {
                2.hash(state);
                address.hash(state);
                view_size.hash(state);
                pointee.hash(state);
            }
            Constant::RepeatedBytePointer {
                identity,
                byte,
                length,
                offset,
                view_size,
                alignment,
                view_codec,
                pointee,
            } => {
                identity.hash(state);
                byte.hash(state);
                length.hash(state);
                offset.hash(state);
                view_size.hash(state);
                alignment.hash(state);
                view_codec.hash(state);
                pointee.hash(state);
            }
            Constant::ByteArrayPointer {
                identity,
                bytes,
                offset,
                view_size,
                alignment,
                view_codec,
                pointee,
            } => {
                identity.hash(state);
                bytes.hash(state);
                offset.hash(state);
                view_size.hash(state);
                alignment.hash(state);
                view_codec.hash(state);
                pointee.hash(state);
            }
            Constant::InternedPointer {
                identity,
                value,
                array_backed,
                allocation_size,
                offset,
                view_size,
                alignment,
                view_codec,
                pointee,
            } => {
                identity.hash(state);
                value.hash(state);
                array_backed.hash(state);
                allocation_size.hash(state);
                offset.hash(state);
                view_size.hash(state);
                alignment.hash(state);
                view_codec.hash(state);
                pointee.hash(state);
            }
            Constant::Null(ty) => {
                3.hash(state);
                ty.hash(state);
            }
            Constant::I8(i) => i.hash(state),
            Constant::U8(i) => i.hash(state),
            Constant::I16(i) => i.hash(state),
            Constant::U16(i) => i.hash(state),
            Constant::I32(i) => i.hash(state),
            Constant::U32(i) => i.hash(state),
            Constant::I64(i) => i.hash(state),
            Constant::U64(i) => i.hash(state),
            Constant::F16(bits) => bits.hash(state),
            Constant::F32(f) => f.to_bits().hash(state),
            Constant::F64(f) => f.to_bits().hash(state),
            Constant::Boolean(b) => b.hash(state),
            Constant::Char(c) => c.hash(state),
            Constant::Str(s) => s.hash(state),
            Constant::String(s) => s.hash(state),
            Constant::Array(ty, elements) => {
                ty.hash(state);
                elements.hash(state);
            }
            Constant::Slice(ty, elements) => {
                ty.hash(state);
                elements.hash(state);
            }
            Constant::SliceRef {
                backing,
                element_type,
                offset,
                length,
            } => {
                backing.hash(state);
                element_type.hash(state);
                offset.hash(state);
                length.hash(state);
            }
            Constant::Instance {
                class_name,
                fields,
                params,
                param_types,
            } => {
                class_name.hash(state);
                let mut fields = fields.iter().collect::<Vec<_>>();
                fields.sort_unstable_by_key(|(key, _)| *key);
                for (key, value) in fields {
                    key.hash(state);
                    value.hash(state);
                }
                params.hash(state);
                param_types.hash(state);
            }
        }
    }
}
