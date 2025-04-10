// src/oomir.rs
//! This is the output of the stage 1 lowering pass of the compiler.
//! It is responsible for converting the MIR into a lower-level IR, called OOMIR (defined in this file).
use ristretto_classfile::attributes::Instruction as JVMInstruction;
use std::{collections::HashMap, fmt};

// OOMIR definitions
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub functions: HashMap<String, Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub signature: Signature,
    pub body: CodeBlock,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

// impl Display for Signature, to make it so we can get the signature as a string suitable for the JVM bytecode, i.e. (I)V etc.
impl Signature {
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        result.push('(');
        for param in &self.params {
            result.push_str(&param.to_jvm_descriptor());
        }
        result.push(')');
        result.push_str(&self.ret.to_jvm_descriptor());
        result
    }
}

#[derive(Debug, Clone)]
pub struct CodeBlock {
    pub entry: String,
    pub basic_blocks: HashMap<String, BasicBlock>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
}

impl BasicBlock {
    pub fn with_extra(self, extra: BasicBlock) -> (BasicBlock, Option<BasicBlock>) {
        (self, Some(extra))
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Const {
        dest: String,
        value: Constant,
    },
    Add {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    AddWithOverflow {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Sub {
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    SubWithOverflow {
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
    And {
        // Logical AND
        dest: String,
        op1: Operand,
        op2: Operand,
    },
    Or {
        dest: String,
        op1: Operand,
        op2: Operand,
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
    Call {
        dest: Option<String>, // Optional destination variable for the return value
        function: String,     // Name of the function to call
        args: Vec<Operand>,   // Arguments to the function
    },
    Move {
        dest: String,
        src: Operand, // Source operand (could be Variable or Constant, though in this context, it's likely Variable)
    },
    Throw {
        exception: Operand, // Exception to throw
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
        array_var: String,
        index: Operand,
        value: Operand,
    },
}

#[derive(Debug, Clone)]
pub enum Operand {
    Constant(Constant),
    Variable { name: String, ty: Type },
}

#[derive(Debug, Clone)]
pub enum Constant {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Class(String),
    // 0 = the type of elements, 1 = the elements as a vec of constants
    Array(Box<Type>, Vec<Constant>),
    Null,
}

// Helper to check if a Constant is integer-like (needed for Switch)
impl Constant {
    pub fn is_integer_like(&self) -> bool {
        matches!(
            self,
            Constant::I8(_) | Constant::I16(_) | Constant::I32(_) | Constant::I64(_) |
            Constant::Char(_) | // Chars can be switched on in JVM
            Constant::Boolean(_) // Booleans (0 or 1) can be switched on
        )
    }
}

// The unsigned types have been removed from this enum. The mapping is now handled in the conversion,
// e.g. Constant::U8 is treated as a signed I16.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Boolean,
    Char,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Reference(Box<Type>), // Representing references
    Array(Box<Type>),     // Representing arrays, need to handle dimensions
    String,               // String type
    Class(String),        // For structs, enums, and potentially Objects
}

impl Type {
    /// Returns the JVM type descriptor string (e.g., "I", "Ljava/lang/String;", "[I").
    pub fn to_jvm_descriptor(&self) -> String {
        match self {
            Type::Void => "V".to_string(),
            Type::Boolean => "Z".to_string(),
            Type::Char => "C".to_string(),
            Type::I8 => "B".to_string(),
            // I16 holds both native I16 and promoted U8:
            Type::I16 => "S".to_string(),
            // I32 holds both native I32 and promoted U16:
            Type::I32 => "I".to_string(),
            // I64 holds both native I64 and promoted U32:
            Type::I64 => "J".to_string(),
            // U64 is too large for a primitive; it's mapped to a BigInteger:
            Type::F32 => "F".to_string(),
            Type::F64 => "D".to_string(),
            Type::String => "Ljava/lang/String;".to_string(),
            Type::Class(name) => format!("L{};", name.replace('.', "/")),
            Type::Reference(inner) => inner.to_jvm_descriptor(),
            Type::Array(element_type) => format!("[{}", element_type.to_jvm_descriptor()),
        }
    }

    pub fn from_jvm_descriptor(descriptor: &str) -> Self {
        match descriptor.chars().next() {
            Some('V') => Type::Void,
            Some('Z') => Type::Boolean,
            Some('C') => Type::Char,
            Some('B') => Type::I8,
            Some('S') => Type::I16,
            Some('I') => Type::I32,
            Some('J') => Type::I64,
            Some('F') => Type::F32,
            Some('D') => Type::F64,
            Some('L') => {
                let class_name = descriptor[1..descriptor.len() - 1].replace('/', ".");
                Type::Class(class_name)
            }
            Some('[') => {
                let inner_descriptor = &descriptor[1..];
                let inner_type = Self::from_jvm_descriptor(inner_descriptor);
                Type::Array(Box::new(inner_type))
            }
            _ => panic!("Unknown JVM type descriptor: {}", descriptor),
        }
    }

    /// Returns the JVM internal name for class/interface types used by anewarray.
    /// Returns None for primitive types.
    pub fn to_jvm_internal_name(&self) -> Option<String> {
        match self {
            Type::String => Some("java/lang/String".to_string()),
            Type::Class(name) => Some(name.replace('.', "/")),
            Type::Reference(inner) => inner.to_jvm_internal_name(), // delegate to inner type
            // For arrays, the descriptor is the internal name.
            Type::Array(_) => Some(self.to_jvm_descriptor()),
            // Primitives don't have an internal name for anewarray.
            _ => None,
        }
    }

    /// Returns the 'atype' code used by the `newarray` instruction for primitive types.
    /// See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.newarray
    pub fn to_jvm_primitive_array_type_code(&self) -> Option<u8> {
        match self {
            Type::Boolean => Some(4), // T_BOOLEAN
            Type::Char => Some(5),    // T_CHAR
            Type::F32 => Some(6),     // T_FLOAT
            Type::F64 => Some(7),     // T_DOUBLE
            Type::I8 => Some(8),      // T_BYTE
            Type::I16 => Some(9),     // T_SHORT
            Type::I32 => Some(10),    // T_INT
            Type::I64 => Some(11),    // T_LONG
            _ => None,                // Not a primitive type suitable for newarray
        }
    }

    /// Returns the appropriate JVM array element store instruction.
    pub fn get_jvm_array_store_instruction(&self) -> Option<JVMInstruction> {
        match self {
            Type::I8 => Some(JVMInstruction::Bastore),
            // I16 (including promoted U8) is stored with Sastore:
            Type::I16 => Some(JVMInstruction::Sastore),
            Type::Boolean => Some(JVMInstruction::Bastore),
            Type::Char => Some(JVMInstruction::Castore),
            // I32 (including promoted U16) is stored with Iastore:
            Type::I32 => Some(JVMInstruction::Iastore),
            // I64 (including promoted U32) is stored with Lastore:
            Type::I64 => Some(JVMInstruction::Lastore),
            Type::F32 => Some(JVMInstruction::Fastore),
            Type::F64 => Some(JVMInstruction::Dastore),
            // Reference types:
            Type::String | Type::Class(_) | Type::Array(_) | Type::Reference(_) => {
                Some(JVMInstruction::Aastore)
            }
            Type::Void => None,
        }
    }

    /// Create a Type from a Constant.
    /// Unsigned constants are promoted:
    /// - U8 -> I16,
    /// - U16 -> I32,
    /// - U32 -> I64,
    /// - U64 -> Mapped as a Class ("java/lang/BigInteger").
    pub fn from_constant(constant: &Constant) -> Self {
        match constant {
            Constant::I8(_) => Type::I8,
            Constant::I16(_) => Type::I16,
            Constant::I32(_) => Type::I32,
            Constant::I64(_) => Type::I64,
            Constant::F32(_) => Type::F32,
            Constant::F64(_) => Type::F64,
            Constant::Array(inner_ty, _) => Type::Array(inner_ty.clone()),
            Constant::Boolean(_) => Type::Boolean,
            Constant::Char(_) => Type::Char,
            Constant::String(_) => Type::String,
            Constant::Class(name) => Type::Class(name.to_string()),
            // For Null, assume a base Object type if a specific type is needed from the constant itself.
            // Often, the type will come from the destination variable context instead.
            Constant::Null => Type::Class("java/lang/Object".to_string()),
        }
    }

    pub fn from_jvm_descriptor_return_type(descriptor: &str) -> Self {
        // this contains the whole desciptor for the function, extract the return type from it
        // i.e. "(I)V" -> "V"
        let ret_type_start = descriptor.find(')').unwrap() + 1;
        let ret_type = &descriptor[ret_type_start..];
        Self::from_jvm_descriptor(ret_type)
    }

    pub fn is_jvm_primitive(&self) -> bool {
        matches!(
            self,
            Type::Boolean
                | Type::Char
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::F32
                | Type::F64
        )
    }

    /// Checks if the type corresponds to a JVM reference type (Object, Array, String, etc.)
    /// as opposed to a primitive (int, float, boolean, etc.) or Void.
    pub fn is_jvm_reference_type(&self) -> bool {
        matches!(
            self,
            Type::Reference(_) | Type::Array(_) | Type::String | Type::Class(_)
        )
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for param_ty in &self.params {
            write!(f, "{}", param_ty.to_jvm_descriptor())?;
        }
        write!(f, "){}", self.ret.to_jvm_descriptor())
    }
}
