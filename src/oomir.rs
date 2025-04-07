// src/oomir.rs
//! This is the output of the stage 1 lowering pass of the compiler.
//! It is responsible for converting the MIR into a lower-level IR, called OOMIR (defined in this file).
use std::collections::HashMap;

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
        // Logical OR
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
}

#[derive(Debug, Clone)]
pub enum Operand {
    Constant(Constant),
    Variable(String), // Representing variables by name for now
}

#[derive(Debug, Clone)]
pub enum Constant {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Class(String),
}

// Helper to check if a Constant is integer-like (needed for Switch)
impl Constant {
    pub fn is_integer_like(&self) -> bool {
        matches!(
            self,
            Constant::I8(_) | Constant::I16(_) | Constant::I32(_) | Constant::I64(_) |
            Constant::U8(_) | Constant::U16(_) | Constant::U32(_) | Constant::U64(_) |
            Constant::Char(_) | // Chars can be switched on in JVM
            Constant::Boolean(_) // Booleans (0 or 1) can be switched on
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Boolean,
    Char,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Reference(Box<Type>), // Representing references
    Array(Box<Type>),     // Representing arrays, need to handle dimensions
    String,               // String type
    Class(String),        // For structs, enums, and potentially Objects
}

impl Type {
    pub fn to_jvm_descriptor(&self) -> String {
        match self {
            Type::Void => "V".to_string(),
            Type::Boolean => "Z".to_string(),
            Type::Char => "C".to_string(),
            Type::I8 => "B".to_string(),
            Type::I16 => "S".to_string(),
            Type::I32 => "I".to_string(),
            Type::I64 => "J".to_string(),
            Type::U8 => "B".to_string(), // JVM doesn't have unsigned types, so we use signed
            Type::U16 => "S".to_string(), // JVM doesn't have unsigned types, so we use signed
            Type::U32 => "I".to_string(), // JVM doesn't have unsigned types, so we use signed
            Type::U64 => "J".to_string(), // JVM doesn't have unsigned types, so we use signed
            Type::F32 => "F".to_string(),
            Type::F64 => "D".to_string(),
            Type::Reference(inner) => format!("L{};", inner.to_jvm_descriptor()),
            Type::Array(inner) => format!("[{}", inner.to_jvm_descriptor()),
            Type::String => "Ljava/lang/String;".to_string(),
            Type::Class(name) => format!("L{};", name),
        }
    }
}
