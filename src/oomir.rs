// src/oomir.rs
//! This is the output of the stage 1 lowering pass of the compiler.
//! It is responsible for converting the MIR into a lower-level IR, called OOMIR (defined in this file).
use breadcrumbs::LogLevel;
use ristretto_classfile::attributes::Instruction as JVMInstruction;
use std::{
    collections::{HashMap, HashSet},
    fmt,
};

pub mod interpret;

pub const SLICE_VIEW_CLASS: &str = "org/rustlang/runtime/SliceView";
pub const UTF8_VIEW_CLASS: &str = "org/rustlang/runtime/Utf8View";
pub const POINTER_CLASS: &str = "org/rustlang/runtime/Pointer";
pub const JAVA_STRING_CLASS: &str = "java/lang/String";
pub const CALLER_LOCATION_PARAM_NAME: &str = "__caller_location";
pub const INSTANCE_RECEIVER_POINTER_LOCAL: &str = "__instance_receiver_pointer";

/// A Rust source position attached to generated code.
///
/// JVM line tables only store line numbers; the corresponding file name is
/// stored once on the containing class through its `SourceFile` attribute.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    pub file_name: String,
    pub line: u32,
}

/// A source-level Rust variable that can be represented by a JVM local slot.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DebugVariable {
    pub name: String,
    pub oomir_name: String,
    pub ty: Type,
}

// OOMIR definitions
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub source_file: Option<String>,
    pub functions: HashMap<FunctionKey, Function>,
    pub data_types: HashMap<String, DataType>,
    pub statics: HashMap<String, Static>,
}

impl Module {
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionKey {
    pub owner_class: String,
    pub method_name: String,
    pub descriptor: String,
}

impl FunctionKey {
    pub fn new(owner_class: &str, method_name: &str, signature: &Signature) -> Self {
        Self {
            owner_class: owner_class.to_string(),
            method_name: method_name.to_string(),
            descriptor: signature.to_string(),
        }
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
    pub is_thread_local: bool,
}

impl Static {
    pub fn key(&self) -> String {
        format!("{}::{}", self.owner_class, self.field_name)
    }
}

#[derive(Debug, Clone)]
pub enum DataTypeMethod {
    SimpleConstantReturn(Type, Option<Constant>),
    Function(Function),
    AdtHelperMethod { kind: AdtHelperKind },
}

#[derive(Debug, Clone)]
pub enum AdtHelperKind {
    IsVariant { variant_idx: u32 },
    PartialEqEnum { variants: Vec<(String, Vec<Type>)> },
    PartialEqClass { fields: Vec<(String, Type)> },
}

#[derive(Debug, Clone)]
pub enum DataType {
    Class {
        is_abstract: bool,
        super_class: Option<String>,
        fields: Vec<(String, Type)>,
        methods: HashMap<String, DataTypeMethod>,
        interfaces: Vec<String>,
    },
    Interface {
        methods: HashMap<String, Signature>,
    },
}

impl DataType {
    // Remove duplicate methods and fields
    pub fn clean_duplicates(&mut self) {
        match self {
            DataType::Class {
                is_abstract: _,
                super_class: _,
                fields,
                methods,
                interfaces: _,
            } => {
                // Remove duplicate fields while preserving the original declaration order.
                let mut seen_fields = HashSet::new();
                fields.retain(|(name, _)| seen_fields.insert(name.clone()));

                // Remove duplicate methods
                let mut unique_methods = HashMap::new();
                for (name, method) in methods.iter() {
                    unique_methods.insert(name.clone(), method.clone());
                }
                *methods = unique_methods;
            }
            DataType::Interface { methods } => {
                // Remove duplicate methods
                let mut unique_methods = HashMap::new();
                for (name, method) in methods.iter() {
                    unique_methods.insert(name.clone(), method.clone());
                }
                *methods = unique_methods;
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub owner_class: Option<String>,
    pub signature: Signature,
    pub debug_variables: Vec<DebugVariable>,
    pub body: CodeBlock,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<(String, Type)>,
    pub ret: Box<Type>,
    pub is_static: bool,
}

impl Signature {
    /// Whether the first OOMIR parameter is represented by the JVM's implicit
    /// receiver slot rather than appearing in the method descriptor.
    pub fn has_implicit_jvm_receiver(&self) -> bool {
        !self.is_static
            && self
                .params
                .first()
                .is_some_and(|(_, ty)| ty.is_jvm_reference_type())
    }

    pub fn explicit_jvm_params(&self) -> &[(String, Type)] {
        if self.has_implicit_jvm_receiver() {
            &self.params[1..]
        } else {
            &self.params
        }
    }

    fn write_jvm_params(&self, result: &mut String, params: &[(String, Type)]) {
        for (_param_name, param_type) in params {
            if param_type.has_jvm_value() {
                result.push_str(&param_type.to_jvm_descriptor());
            }
        }
    }

    pub fn to_jvm_descriptor_with_explicit_params(&self) -> String {
        let mut result = String::new();
        result.push('(');
        self.write_jvm_params(&mut result, &self.params);
        result.push(')');
        result.push_str(&self.ret.to_jvm_return_descriptor());
        result
    }

    pub fn fn_ptr_interface_name(&self) -> String {
        let descriptor = self.to_jvm_descriptor_with_explicit_params();
        let params = self
            .explicit_jvm_params()
            .iter()
            .filter_map(|(_, ty)| ty.has_jvm_value().then(|| ty.jvm_abi_name_token()))
            .collect::<Vec<_>>();
        let params = if params.is_empty() {
            "no_args".to_string()
        } else {
            params.join("_")
        };
        let readable = format!("{params}_to_{}", self.ret.jvm_abi_name_token());
        crate::stable_hash::readable_or_hashed_name("FnPtr", &readable, &descriptor, 180)
    }

    pub fn fn_ptr_interface_method_signature(&self) -> Signature {
        Signature {
            params: self.params.clone(),
            ret: self.ret.clone(),
            is_static: false,
        }
    }

    /// Replaces all occurrences of `Type::Class(old_name)` with `Type::Class(new_name)`
    /// in the signature's parameters and return type.
    /// Returns a tuple (params_changed, return_changed) indicating whether any replacements were made.
    pub fn replace_class_in_signature(
        &mut self,
        old_class_name: &str,
        new_class_name: &str,
    ) -> (bool, bool) {
        let mut params_changed = false;
        let mut return_changed = false;

        // Replace in parameters
        for (_param_name, param_type) in self.params.iter_mut() {
            let result = param_type.replace_class(old_class_name, new_class_name);
            if result {
                params_changed = true;
            }
        }

        // Replace in return type (accessing the Type inside the Box)
        if self.ret.replace_class(old_class_name, new_class_name) {
            return_changed = true;
        }

        (params_changed, return_changed)
    }
}

// impl Display for Signature, to make it so we can get the signature as a string suitable for the JVM bytecode, i.e. (I)V etc.
impl Signature {
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        result.push('(');
        self.write_jvm_params(&mut result, self.explicit_jvm_params());
        result.push(')');
        result.push_str(&self.ret.to_jvm_return_descriptor());
        result
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CodeBlock {
    pub entry: String,
    pub basic_blocks: HashMap<String, BasicBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    SourceLocation(SourceLocation), // metadata. does not emit JVM bytecode.
    LocalVariableScope(Vec<usize>), // same
    UnwindStart {
        target: String,
    }, // metadata for a protected JVM region.
    UnwindEnd,                      // ends the current protected region.
    Rethrow,                        // resumes the current Rust unwind.
    Add {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Sub {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Mul {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Div {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Rem {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Eq {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Ne {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Lt {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Le {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Gt {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Ge {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    BitAnd {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    BitOr {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    BitXor {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Shl {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Shr {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Not {
        // Logical/Bitwise NOT
        dest: String,
        src: Operand,
    },
    Neg {
        // Arithmetic Negation
        dest: String,
        src: Operand,
    },
    Jump {
        target: String, // Label of the target BB
    },
    Branch {
        condition: Operand,
        true_block: String,  // Label of the true BB
        false_block: String, // Label of the false BB
    },
    Return {
        operand: Option<Operand>, // Optional return value
    },
    CallIndirect {
        dest: Option<String>,       // Optional destination variable for the return value
        function_ptr: Box<Operand>, // Operand holding the function pointer object
        args: Vec<Operand>,         // Arguments to the function
        signature: Signature,       // Function pointer signature
    },
    /// Creates a stateless JVM functional-interface instance through
    /// `LambdaMetafactory`. The implementation target must have the same
    /// flattened JVM descriptor as the SAM signature.
    CreateFunctionPointer {
        dest: String,
        interface_name: String,
        signature: Signature,
        target_class_name: String,
        target_method_name: String,
    },
    InvokeInterface {
        class_name: String,   // JVM interface name (e.g., MyTrait)
        method_name: String,  // Name of the method to call
        method_ty: Signature, // Signature of the method (input/output types)
        args: Vec<Operand>,   // Arguments to the function
        dest: Option<String>, // Optional destination variable for the return value
        operand: Operand,     // The object reference (this) for the method call
    },
    Move {
        dest: String,
        src: Operand, // Source operand (could be Variable or Constant, though in this context, it's likely Variable)
    },
    ThrowNewWithMessage {
        exception_class: String, // e.g., "java/lang/RuntimeException"
        message: String,         // The message from the panic/assert
    },
    Switch {
        discr: Operand, // The value being switched on
        // Vec of (Constant Value, Target Label) pairs
        targets: Vec<(Constant, String)>,
        otherwise: String, // Label for the default case
    },
    NewArray {
        dest: String,
        element_type: Type,
        size: Operand,
    },
    ArrayStore {
        array: String,
        index: Operand,
        value: Operand,
        copy_value: bool,
    },
    ArrayFill {
        array: String,
        value: Operand,
        copy_value: bool,
    },
    ArrayGet {
        dest: String,
        array: Operand,
        index: Operand,
    },
    Length {
        dest: String,
        array: Operand,
    },
    ConstructObject {
        dest: String,               // Variable to hold the new object reference
        class_name: String,         // JVM class name (e.g., my_crate/MyStruct)
        args: Vec<(Operand, Type)>, // Constructor arguments in field declaration order.
    },
    SetField {
        object: String,      // Variable holding the object reference
        field_name: String,  // Name of the field in the class
        value: Operand,      // Value to store in the field
        field_ty: Type,      // Type of the field (needed for JVM descriptor)
        owner_class: String, // JVM class name where the field is defined
    },
    GetField {
        dest: String,        // Variable to store the loaded field value
        object: Operand,     // Variable holding the object reference
        field_name: String,  // Name of the field in the class
        field_ty: Type,      // Type of the field (needed for JVM descriptor)
        owner_class: String, // JVM class name where the field is defined
    },
    Label {
        name: String,
    },
    Cast {
        op: Operand,
        ty: Type,
        dest: String, // Destination variable for the casted value
    },
    InvokeVirtual {
        dest: Option<String>, // Optional destination variable for the return value
        class_name: String,   // JVM class name (e.g., MyStruct)
        method_name: String,  // Name of the method to call
        method_ty: Signature, // Signature of the method (input/output types)
        args: Vec<Operand>,   // Arguments to the function
        operand: Operand,     // The object reference (this) for the method call
    },
    InvokeStatic {
        dest: Option<String>, // Optional destination variable for the return value
        class_name: String,   // JVM class name
        method_name: String,  // Name of the static method to call
        method_ty: Signature, // Signature of the method (input/output types)
        args: Vec<Operand>,   // Arguments to the function
    },
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Operand {
    Constant(Constant),
    Variable { name: String, ty: Type },
}

impl Operand {
    pub fn get_name(&self) -> Option<&str> {
        match self {
            Operand::Variable { name, .. } => Some(name),
            _ => None,
        }
    }
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Operand::Variable { ty, .. } => Some(ty.clone()),
            Operand::Constant(c) => Some(Type::from_constant(c)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    /// A pointer to a fully materialized anonymous CTFE allocation. Repeated
    /// loads use `identity` to retain Rust allocation identity on the JVM.
    InternedPointer {
        identity: String,
        value: Box<Constant>,
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
        /// For enum variants using numbered fields, use "field0", "field1", etc.
        fields: std::collections::HashMap<String, Constant>,
        /// Any parameters to the constructor.
        params: Vec<Constant>,
    },
}

impl Eq for Constant {}

impl Constant {
    /// Whether this value can be substituted freely by the OOMIR constant
    /// propagation pass. A static reference is an instruction-sized runtime
    /// field load, not a compile-time value. Containers inherit that property
    /// so folding never discards a required static load hidden inside one.
    pub fn is_propagatable(&self) -> bool {
        match self {
            Constant::StaticRef { .. }
            | Constant::FactoryCall { .. }
            | Constant::StaticCall { .. }
            | Constant::InternedPointer { .. }
            | Constant::Str(..)
            | Constant::Slice(..)
            | Constant::SliceRef { .. } => false,
            Constant::Array(_, elements) => elements.iter().all(Constant::is_propagatable),
            Constant::Instance { fields, params, .. } => {
                fields.values().all(Constant::is_propagatable)
                    && params.iter().all(Constant::is_propagatable)
            }
            _ => true,
        }
    }
}

impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
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
                ty,
            } => {
                owner_class.hash(state);
                method_name.hash(state);
                args.hash(state);
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
            Constant::InternedPointer {
                identity,
                value,
                view_size,
                alignment,
                view_codec,
                pointee,
            } => {
                identity.hash(state);
                value.hash(state);
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
            } => {
                class_name.hash(state);
                // iterate over the fields and hash them
                for (key, value) in fields {
                    key.hash(state);
                    value.hash(state);
                }
                params.hash(state);
            }
        }
    }
}

// Helper to check if a Constant is integer-like (needed for Switch)
impl Constant {
    pub fn is_integer_like(&self) -> bool {
        match self {
            Constant::I8(_)
            | Constant::U8(_)
            | Constant::I16(_)
            | Constant::U16(_)
            | Constant::I32(_)
            | Constant::U32(_)
            | Constant::I64(_)
            | Constant::U64(_)
            | Constant::Char(_) // Chars can be switched on in JVM
            | Constant::Boolean(_) => true,
            Constant::Instance { class_name, .. } => {
                class_name == "org/rustlang/runtime/I128"
                    || class_name == "org/rustlang/runtime/U128"
            }
            _ => false,
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Constant::I8(i) => *i == 0,
            Constant::I16(i) => *i == 0,
            Constant::I32(i) => *i == 0,
            Constant::I64(i) => *i == 0,
            Constant::F32(f) => *f == 0.0,
            Constant::F64(f) => *f == 0.0,
            Constant::Char(c) => *c == '\0',
            Constant::Boolean(b) => !*b,
            _ => false, // Null, String, Array, etc. are not zero
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            Constant::I8(i) => *i == 1,
            Constant::I16(i) => *i == 1,
            Constant::I32(i) => *i == 1,
            Constant::I64(i) => *i == 1,
            Constant::F32(f) => *f == 1.0,
            Constant::F64(f) => *f == 1.0,
            Constant::Char(c) => *c == '1',
            Constant::Boolean(b) => *b,
            _ => false, // Null, String, Array, Instance are not one
        }
    }

    // Helper to get a zero constant of a type compatible with an operand
    // there's NO Integer, Float type use I8, F32, F64 etc. etc.
    pub fn zero_for_operand(op: &Operand) -> Option<Constant> {
        match op {
            Operand::Constant(Constant::I8(_)) => Some(Constant::I8(0)),
            Operand::Constant(Constant::I16(_)) => Some(Constant::I16(0)),
            Operand::Constant(Constant::I32(_)) => Some(Constant::I32(0)),
            Operand::Constant(Constant::I64(_)) => Some(Constant::I64(0)),
            Operand::Constant(Constant::F32(_)) => Some(Constant::F32(0.0)),
            Operand::Constant(Constant::F64(_)) => Some(Constant::F64(0.0)),
            Operand::Constant(Constant::Char(_)) => Some(Constant::Char('\0')),
            Operand::Constant(Constant::Boolean(_)) => Some(Constant::Boolean(false)),
            _ => None,
        }
    }

    // Helper to get a one constant of a type compatible with an operand
    pub fn one_for_operand(op: &Operand) -> Option<Constant> {
        match op {
            Operand::Constant(Constant::I8(_)) => Some(Constant::I8(1)),
            Operand::Constant(Constant::I16(_)) => Some(Constant::I16(1)),
            Operand::Constant(Constant::I32(_)) => Some(Constant::I32(1)),
            Operand::Constant(Constant::I64(_)) => Some(Constant::I64(1)),
            Operand::Constant(Constant::F32(_)) => Some(Constant::F32(1.0)),
            Operand::Constant(Constant::F64(_)) => Some(Constant::F64(1.0)),
            Operand::Constant(Constant::Char(_)) => Some(Constant::Char('1')),
            Operand::Constant(Constant::Boolean(_)) => Some(Constant::Boolean(true)),
            _ => None,
        }
    }
}

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
        match self {
            Type::Void => "V".to_string(),
            // Unit is only descriptor-compatible as a method return. Parameters and fields
            // omit it before descriptors are built.
            Type::Unit => "V".to_string(),
            Type::Boolean => "Z".to_string(),
            Type::Char => "C".to_string(),
            Type::I8 | Type::U8 => "B".to_string(),
            Type::I16 => "S".to_string(),
            Type::U16 => "C".to_string(),
            Type::I32 | Type::U32 => "I".to_string(),
            Type::I64 | Type::U64 => "J".to_string(),
            // Binary16 is stored as its raw 16-bit IEEE representation.
            Type::F16 => "S".to_string(),
            Type::F32 => "F".to_string(),
            Type::F64 => "D".to_string(),
            Type::Pointer(_) => format!("L{};", POINTER_CLASS),
            Type::Str => format!("L{};", UTF8_VIEW_CLASS),
            Type::Class(name) | Type::Interface(name) => format!("L{};", name.replace('.', "/")),
            Type::Reference(inner) => inner.to_jvm_descriptor(),
            Type::MutableReference(inner) => {
                if inner.has_jvm_value() {
                    format!("[{}", inner.to_jvm_descriptor())
                } else {
                    "Ljava/lang/Object;".to_string()
                }
            }
            Type::Array(element_type) => {
                if element_type.has_jvm_value() {
                    format!("[{}", element_type.to_jvm_descriptor())
                } else {
                    "[Ljava/lang/Object;".to_string()
                }
            }
            Type::Slice(_) => format!("L{};", SLICE_VIEW_CLASS),
        }
    }

    pub fn to_jvm_return_descriptor(&self) -> String {
        if matches!(self, Type::Unit | Type::Void) {
            "V".to_string()
        } else {
            self.to_jvm_descriptor()
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

    /// Returns the appropriate JVM array element load instruction.
    pub fn get_jvm_array_load_instruction(&self) -> Option<JVMInstruction> {
        match self {
            Type::I8 | Type::U8 => Some(JVMInstruction::Baload),
            Type::I16 | Type::F16 => Some(JVMInstruction::Saload),
            Type::U16 => Some(JVMInstruction::Caload),
            Type::Boolean => Some(JVMInstruction::Baload),
            Type::Char => Some(JVMInstruction::Caload),
            Type::I32 | Type::U32 => Some(JVMInstruction::Iaload),
            Type::I64 | Type::U64 => Some(JVMInstruction::Laload),
            Type::F32 => Some(JVMInstruction::Faload),
            Type::F64 => Some(JVMInstruction::Daload),
            // Reference types:
            Type::Str
            | Type::Class(_)
            | Type::Array(_)
            | Type::Slice(_)
            | Type::Reference(_)
            | Type::MutableReference(_)
            | Type::Interface(_) => Some(JVMInstruction::Aaload),
            Type::Pointer(_) => Some(JVMInstruction::Aaload),
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
            } if class_name == POINTER_CLASS => Type::Pointer(Box::new(if params.len() == 3 {
                params
                    .first()
                    .map(Type::from_constant)
                    .unwrap_or(Type::Unit)
            } else {
                // Address-only constructors carry no JVM pointee value from
                // which to infer a more specific OOMIR type.
                Type::Unit
            })),
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

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (_param_name, param_ty) in self.explicit_jvm_params() {
            if param_ty.has_jvm_value() {
                write!(f, "{}", param_ty.to_jvm_descriptor())?;
            }
        }
        write!(f, "){}", self.ret.to_jvm_return_descriptor())
    }
}
