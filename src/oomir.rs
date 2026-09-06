//! Module declarations and ownership at the semantic-to-SSA boundary.
mod forward;
pub use forward::{MethodForwarder, ReceiverPointer};
mod body;
pub(crate) mod construct;
pub(crate) mod outline;
pub use body::SsaBody;
pub type SsaFunction = Function<Arc<SsaBody>>;
use breadcrumbs::LogLevel;
use ristretto_classfile::attributes::Instruction as JVMInstruction;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::{
    fmt,
    hash::{Hash, Hasher},
    sync::Arc,
};

mod constant_cost;
mod constant_eq;
pub(crate) use constant_cost::constant_instruction_cost;
pub mod scalar;
mod visit;

pub use jvm_compiler_core::jvm::abi::{
    POINTER_CLASS, RELATIVE_POINTER_METHOD_SUFFIX, SLICE_VIEW_CLASS, UTF8_VIEW_CLASS,
    relative_pointer_byte_offset_field, relative_pointer_element_offset_field,
};
pub const JAVA_STRING_CLASS: &str = "java/lang/String";
pub const CALLER_LOCATION_PARAM_NAME: &str = "__caller_location";
pub use jvm_compiler_core::debug::SourceLocation;

/// A source-level Rust variable that can be represented by a JVM local slot.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DebugVariable {
    pub name: String,
    pub oomir_name: String,
    pub ty: Type,
}

// OOMIR definitions
#[derive(Debug, Clone)]
pub struct Module<D = HashMap<String, DataType>> {
    pub name: String,
    pub source_file: Option<String>,
    pub functions: HashMap<FunctionKey, Function>,
    pub data_types: D,
    /// Definitions already emitted by an earlier shard in this crate. They
    /// remain available as lowering context, but lower2 must not rebuild them.
    pub suppressed_data_types: HashSet<String>,
    /// Read-only crate-wide type schemas used by canonical data-type emission
    /// shards without copying every definition into every shard.
    pub shared_data_types: Option<Arc<HashMap<String, DataType>>>,
    /// Static methods removed into the canonical data-type contribution table
    /// that still use the component-carrying internal pointer ABI.
    pub relative_static_methods: Arc<HashSet<FunctionKey>>,
    /// JVM interfaces referenced by this shard but defined in another crate.
    pub external_interfaces: HashSet<String>,
    pub statics: HashMap<String, Static>,
}

impl<D> Module<D> {
    /// Consume the producer's definitions and construction state at the worker
    /// boundary, without retaining a second module or cloning function bodies.
    pub fn map_definitions<E>(self, map: impl FnOnce(D) -> E) -> Module<E> {
        Module {
            name: self.name,
            source_file: self.source_file,
            functions: self.functions,
            data_types: map(self.data_types),
            suppressed_data_types: self.suppressed_data_types,
            shared_data_types: self.shared_data_types,
            relative_static_methods: self.relative_static_methods,
            external_interfaces: self.external_interfaces,
            statics: self.statics,
        }
    }

    pub fn insert_function(&mut self, function: Function) {
        let key = FunctionKey::new(
            function.owner_class.as_deref().unwrap_or(&self.name),
            &function.name,
            &function.signature,
        );
        self.functions.entry(key).or_insert(function);
    }

    pub fn owner_class_for_function<'a>(&'a self, function: &'a Function) -> &'a str {
        function.owner_class.as_deref().unwrap_or(&self.name)
    }
}

impl Module {
    pub fn data_type(&self, name: &str) -> Option<&DataType> {
        self.data_types.get(name).or_else(|| {
            self.shared_data_types
                .as_ref()
                .and_then(|data_types| data_types.get(name))
        })
    }
}

impl Function {
    /// Returns the definition file marker placed at the start of a MIR-lowered
    /// function. Synthetic helper functions intentionally have no marker.
    pub fn source_file(&self) -> Option<&str> {
        let entry = self.body.basic_blocks.get(&self.body.entry)?;
        entry
            .instructions
            .iter()
            .find_map(|instruction| match instruction {
                Instruction::SourceLocation(location) => Some(location.file_name.as_str()),
                _ => None,
            })
    }
}

/// The identity of a method in a JVM class file. Return types are included in the
/// descriptor for diagnostics and consistency, even though JVM invocation lookup is
/// principally distinguished by owner, name, and parameter descriptor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionKey {
    owner_class: Arc<str>,
    method_name: Arc<str>,
    descriptor: Arc<str>,
    hash: u64,
}

impl FunctionKey {
    pub fn new(owner_class: &str, method_name: &str, signature: &Signature) -> Self {
        let descriptor = signature.to_string();
        Self {
            owner_class: Arc::from(owner_class),
            method_name: Arc::from(method_name),
            hash: crate::stable_hash::hash_value(&(owner_class, method_name, descriptor.as_str())),
            descriptor: Arc::from(descriptor),
        }
    }
}

impl Hash for FunctionKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl std::fmt::Display for FunctionKey {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "{}::{}{}",
            self.owner_class, self.method_name, self.descriptor
        )
    }
}

#[derive(Debug, Clone)]
pub struct Static {
    pub owner_class: String,
    pub field_name: String,
    pub storage_type: Type,
    pub initializer: Constant,
    pub allocation_size: usize,
    pub allocation_alignment: usize,
    pub allocation_codec_class_name: Option<String>,
    pub is_thread_local: bool,
}

impl Static {
    pub fn key(&self) -> String {
        format!("{}::{}", self.owner_class, self.field_name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DataTypeMethod {
    Abstract(Signature),
    SimpleConstantReturn(Type, Option<Constant>),
    Function(Function),
    Forwarder(MethodForwarder),
    AdtHelperMethod { kind: AdtHelperKind },
}

impl DataTypeMethod {
    pub fn function_signature(&self) -> Option<&Signature> {
        match self {
            Self::Function(function) => Some(&function.signature),
            Self::Forwarder(forwarder) => Some(&forwarder.signature),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantShape {
    /// The concrete class used by an ordinary case, or the nested enum
    /// interface used by a transparent subtype case.
    pub runtime_type: String,
    /// JVM fields on an ordinary case. A transparent case has one logical
    /// payload but no wrapper field.
    pub fields: Vec<(String, Type)>,
    pub transparent: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AdtHelperKind {
    EnumVariantIndex {
        enum_class: String,
        variants: Vec<EnumVariantShape>,
    },
    EnumDiscriminant {
        enum_class: String,
        variants: Vec<EnumVariantShape>,
        values: Vec<i64>,
    },
    EnumIsVariant {
        enum_class: String,
        runtime_type: String,
    },
    StaticPartialEqEnum {
        enum_class: String,
        variants: Vec<EnumVariantShape>,
    },
    PartialEqClass {
        fields: Vec<(String, Type)>,
    },
    Component {
        field_name: String,
        field_ty: Type,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Class {
        is_abstract: bool,
        super_class: Option<String>,
        fields: Vec<(String, Type)>,
        methods: HashMap<String, DataTypeMethod>,
        interfaces: Vec<String>,
    },
    Interface {
        methods: HashMap<String, DataTypeMethod>,
        interfaces: Vec<String>,
        is_enum: bool,
    },
}

impl Hash for DataType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Class {
                is_abstract,
                super_class,
                fields,
                methods,
                interfaces,
            } => {
                is_abstract.hash(state);
                super_class.hash(state);
                fields.hash(state);
                interfaces.hash(state);
                let mut methods = methods.iter().collect::<Vec<_>>();
                methods.sort_unstable_by_key(|(name, _)| *name);
                methods.hash(state);
            }
            Self::Interface {
                methods,
                interfaces,
                is_enum,
            } => {
                interfaces.hash(state);
                is_enum.hash(state);
                let mut methods = methods.iter().collect::<Vec<_>>();
                methods.sort_unstable_by_key(|(name, _)| *name);
                methods.hash(state);
            }
        }
    }
}

impl DataType {
    // Remove duplicate methods and fields
    pub fn clean_duplicates(&mut self) {
        match self {
            DataType::Class {
                is_abstract: _,
                super_class: _,
                fields,
                methods: _,
                interfaces,
            } => {
                // Remove duplicate fields while preserving the original declaration order.
                let mut seen_fields = HashSet::default();
                fields.retain(|(name, _)| seen_fields.insert(name.clone()));

                let mut seen_interfaces = HashSet::default();
                interfaces.retain(|name| seen_interfaces.insert(name.clone()));
            }
            DataType::Interface { interfaces, .. } => {
                let mut seen_interfaces = HashSet::default();
                interfaces.retain(|name| seen_interfaces.insert(name.clone()));
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function<B = CodeBlock> {
    pub name: String,
    pub owner_class: Option<String>,
    pub signature: Signature,
    pub debug_variables: Vec<DebugVariable>,
    pub body: B,
}

mod signature;
pub use signature::*;
mod emission;
pub use emission::*;
mod constant;
pub use constant::*;
mod types;
pub use types::*;
