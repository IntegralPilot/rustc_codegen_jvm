//! Semantic construction buffers consumed by the SSA builder.
use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeBlock {
    pub entry: String,
    pub basic_blocks: HashMap<String, BasicBlock>,
}

impl Hash for CodeBlock {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.entry.hash(state);
        let mut blocks = self.basic_blocks.iter().collect::<Vec<_>>();
        blocks.sort_unstable_by_key(|(label, _)| *label);
        blocks.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
}

pub use jvm_compiler_core::scalar::BinaryOp;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    SourceLocation(SourceLocation), // metadata. does not emit JVM bytecode.
    LocalVariableScope(Vec<usize>), // same
    UnwindStart {
        target: String,
    }, // metadata for a protected JVM region.
    UnwindEnd,                      // ends the current protected region.
    Rethrow,                        // resumes the current Rust unwind.
    Binary {
        op: BinaryOp,
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
        array: Operand,
        index: Operand,
        value: Operand,
        copy_value: bool,
    },
    ArrayFill {
        array: Operand,
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
    /// An exact JVM instance-field load. Unlike Rust-generated object fields,
    /// pointer values are self-contained and have no companion offset fields.
    GetJvmField {
        dest: String,
        object: Operand,
        class_name: String,
        field_name: String,
        field_ty: Type,
    },
    /// An exact JVM instance-field store; pointer offsets are materialized
    /// into the public `Pointer` carrier before the value crosses the ABI.
    SetJvmField {
        object: Operand,
        class_name: String,
        field_name: String,
        value: Operand,
        field_ty: Type,
    },
    GetStaticField {
        dest: String,
        class_name: String,
        field_name: String,
        field_ty: Type,
    },
    SetStaticField {
        class_name: String,
        field_name: String,
        value: Operand,
        field_ty: Type,
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
    /// A generated Rust-to-Rust static call. Unlike exact JVM imports, lower2
    /// may use the component-carrying internal pointer ABI for this edge.
    InvokeRustStatic {
        dest: Option<String>,
        class_name: String,
        method_name: String,
        method_ty: Signature,
        args: Vec<Operand>,
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
