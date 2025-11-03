// src/oomir.rs
//! This is the output of the stage 1 lowering pass of the compiler.
//! It is responsible for converting the MIR into a lower-level IR, called OOMIR (defined in this file).
use crate::lower2::BIG_DECIMAL_CLASS;

use super::lower2::BIG_INTEGER_CLASS;
use core::panic;
use ristretto_classfile::attributes::Instruction as JVMInstruction;
use std::{collections::HashMap, fmt};

pub mod interpret;

// OOMIR definitions
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub functions: HashMap<String, Function>,
    pub data_types: HashMap<String, DataType>,
}

impl Module {
    // Helper function to merge data types into the module
    // Ensure no dups (if there are, give the older one precedence)
    pub fn merge_data_types(&mut self, other: &HashMap<String, DataType>) {
        for (name, data_type) in other {
            if !self.data_types.contains_key(name) {
                self.data_types.insert(name.clone(), data_type.clone());
            } else {
                // try and merge them
                let cur_data_type = self.data_types.get(name).unwrap().clone();
                let other_data_type = other.get(name).unwrap();
                match &cur_data_type {
                    DataType::Class {
                        is_abstract,
                        super_class,
                        fields,
                        methods,
                        interfaces,
                    } => match other_data_type {
                        DataType::Class {
                            is_abstract: o_is_abstract,
                            super_class: o_super_class,
                            fields: o_fields,
                            methods: o_methods,
                            interfaces: o_interfaces,
                        } => {
                            let mut new_is_abstract = false;
                            if *is_abstract || *o_is_abstract {
                                new_is_abstract = true;
                            }
                            let new_super_class = match super_class {
                                Some(x) => {
                                    if x == "java/lang/Object" && o_super_class.is_some() {
                                        o_super_class
                                    } else if o_super_class.is_none() {
                                        super_class
                                    } else if o_super_class != super_class {
                                        if o_super_class.clone().unwrap() == "java/lang/Object" {
                                            super_class
                                        } else {
                                            panic!(
                                                "Incompadible DataTypes (super) for {}: {:?} and {:?}",
                                                name, cur_data_type, other_data_type
                                            )
                                        }
                                    } else {
                                        super_class
                                    }
                                }
                                None => o_super_class,
                            };
                            let mut new_fields = fields.clone();
                            new_fields.extend(o_fields.iter().cloned());
                            let mut new_methods = methods.clone();
                            new_methods
                                .extend(o_methods.iter().map(|(k, v)| (k.clone(), v.clone())));
                            let mut new_interfaces = interfaces.clone();
                            new_interfaces.extend(o_interfaces.iter().cloned());

                            self.data_types.insert(
                                name.clone(),
                                DataType::Class {
                                    is_abstract: new_is_abstract,
                                    super_class: new_super_class.clone(),
                                    fields: new_fields,
                                    methods: new_methods,
                                    interfaces: new_interfaces,
                                },
                            );
                        }
                        _ => {
                            panic!(
                                "Incompadible DataTypes (type) for {}: {:?} and {:?}",
                                name, cur_data_type, other_data_type
                            )
                        }
                    },
                    DataType::Interface { methods } => match other_data_type {
                        DataType::Interface { methods: o_methods } => {
                            let mut new_methods = methods.clone();
                            new_methods
                                .extend(o_methods.iter().map(|(k, v)| (k.clone(), v.clone())));

                            self.data_types.remove(name);
                            self.data_types.insert(
                                name.clone(),
                                DataType::Interface {
                                    methods: new_methods,
                                },
                            );
                        }
                        _ => {
                            panic!(
                                "Incompadible DataTypes (type) for {}: {:?} and {:?}",
                                name, cur_data_type, other_data_type
                            )
                        }
                    },
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum DataTypeMethod {
    SimpleConstantReturn(Type, Option<Constant>),
    Function(Function),
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
                // Remove duplicate fields
                let mut unique_fields = HashMap::new();
                for (name, ty) in fields.iter() {
                    unique_fields.insert(name.clone(), ty.clone());
                }
                *fields = unique_fields.into_iter().collect();

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
    pub signature: Signature,
    pub body: CodeBlock,
    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<(String, Type)>,
    pub ret: Box<Type>,
}

impl Signature {
    /// Replaces all occurrences of `Type::Class(old_name)` with `Type::Class(new_name)`
    /// in the signature's parameters and return type.
    pub fn replace_class_in_signature(&mut self, old_class_name: &str, new_class_name: &str) {
        // Replace in parameters
        for (_param_name, param_type) in self.params.iter_mut() {
            param_type.replace_class(old_class_name, new_class_name);
        }

        // Replace in return type (accessing the Type inside the Box)
        self.ret.replace_class(old_class_name, new_class_name);
    }
}

// impl Display for Signature, to make it so we can get the signature as a string suitable for the JVM bytecode, i.e. (I)V etc.
impl Signature {
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        result.push('(');
        for (_param_name, param_type) in &self.params {
            result.push_str(&param_type.to_jvm_descriptor());
        }
        result.push(')');
        result.push_str(&self.ret.to_jvm_descriptor());
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
    Call {
        dest: Option<String>, // Optional destination variable for the return value
        function: String,     // Name of the function to call
        args: Vec<Operand>,   // Arguments to the function
    },
    InvokeInterface {
        class_name: String,   // JVM class name (e.g., MyStruct)
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
        dest: String, // Variable to hold the new object reference
        class_name: String, // JVM class name (e.g., my_crate/MyStruct)
                      // Implicitly calls the default constructor <init>()V
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

impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Constant::I8(i) => i.hash(state),
            Constant::I16(i) => i.hash(state),
            Constant::I32(i) => i.hash(state),
            Constant::I64(i) => i.hash(state),
            Constant::F32(f) => f.to_bits().hash(state),
            Constant::F64(f) => f.to_bits().hash(state),
            Constant::Boolean(b) => b.hash(state),
            Constant::Char(c) => c.hash(state),
            Constant::String(s) => s.hash(state),
            Constant::Class(s) => s.hash(state),
            Constant::Array(ty, elements) => {
                ty.hash(state);
                elements.hash(state);
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
            | Constant::I16(_)
            | Constant::I32(_)
            | Constant::I64(_)
            | Constant::Char(_) // Chars can be switched on in JVM
            | Constant::Boolean(_) => true,
            Constant::Instance { class_name, .. } => class_name == BIG_INTEGER_CLASS,
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
            Constant::Instance {
                class_name, params, ..
            } => {
                if class_name == BIG_INTEGER_CLASS {
                    // Check if the params are all zero
                    let param0 = params[0].clone(); // string of the number to be made
                    if let Constant::String(s) = param0 {
                        // Check if the string is "0" or "-0"
                        return s == "0" || s == "-0";
                    }
                } else if class_name == BIG_DECIMAL_CLASS {
                    // Check if the params are all zero
                    let param0 = params[0].clone();
                    if let Constant::String(s) = param0 {
                        // remove all - and .
                        let s = s.replace("-", "").replace(".", "");
                        // check if every char is 0
                        for c in s.chars() {
                            if c != '0' {
                                return false;
                            }
                        }
                        return true;
                    }
                }
                return false;
            }
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
            Constant::Instance {
                class_name, params, ..
            } => {
                if class_name == BIG_INTEGER_CLASS {
                    // Check if the params are all one
                    let param0 = params[0].clone(); // string of the number to be made
                    if let Constant::String(s) = param0 {
                        // Check if the string is "1"
                        return s == "1";
                    }
                } else if class_name == BIG_DECIMAL_CLASS {
                    // Check if the params are all one
                    let param0 = params[0].clone();
                    if let Constant::String(s) = param0 {
                        let mut after_dp = false;
                        let mut had_before_dp_1 = false;
                        for c in s.chars() {
                            if c == '.' {
                                after_dp = true;
                                continue;
                            }
                            if after_dp {
                                if c != '0' {
                                    return false;
                                }
                            } else {
                                if !had_before_dp_1 {
                                    if c != '0' {
                                        return false;
                                    }
                                    if c == '1' {
                                        had_before_dp_1 = true;
                                    }
                                } else {
                                    return false;
                                }
                            }
                        }
                    }
                }
                return false;
            }
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
            Operand::Constant(Constant::Instance { class_name, .. }) => {
                if class_name == BIG_INTEGER_CLASS {
                    return Some(Constant::Instance {
                        class_name: class_name.clone(),
                        fields: std::collections::HashMap::new(),
                        params: vec![Constant::String("0".to_string())],
                    });
                } else if class_name == BIG_DECIMAL_CLASS {
                    return Some(Constant::Instance {
                        class_name: class_name.clone(),
                        fields: std::collections::HashMap::new(),
                        params: vec![Constant::String("0".to_string())],
                    });
                }
                None
            }
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
            Operand::Constant(Constant::Instance { class_name, .. }) => {
                if class_name == BIG_INTEGER_CLASS {
                    return Some(Constant::Instance {
                        class_name: class_name.clone(),
                        fields: std::collections::HashMap::new(),
                        params: vec![Constant::String("1".to_string())],
                    });
                } else if class_name == BIG_DECIMAL_CLASS {
                    return Some(Constant::Instance {
                        class_name: class_name.clone(),
                        fields: std::collections::HashMap::new(),
                        params: vec![Constant::String("1".to_string())],
                    });
                }
                None
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)] /* Reference variant currently unused */
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
    MutableReference(Box<Type>), // Same as an Array
    Reference(Box<Type>), // Representing references, not currently constructed but might be useful in future for more complex things.
    Array(Box<Type>),     // Representing arrays
    String,               // String type
    Class(String),        // For structs, enums, and potentially Objects
    Interface(String),    // dyn TraitName
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
            Type::Class(name) | Type::Interface(name) => format!("L{};", name.replace('.', "/")),
            Type::Reference(inner) => inner.to_jvm_descriptor(),
            Type::Array(element_type) | Type::MutableReference(element_type) => {
                format!("[{}", element_type.to_jvm_descriptor())
            }
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
            Type::String
            | Type::Class(_)
            | Type::Interface(_)
            | Type::Array(_)
            | Type::Reference(_) => Some(JVMInstruction::Aastore),
            Type::MutableReference(box t) => t.get_jvm_array_store_instruction(),
            Type::Void => None,
        }
    }

    /// Returns the appropriate JVM array element load instruction.
    pub fn get_jvm_array_load_instruction(&self) -> Option<JVMInstruction> {
        match self {
            Type::I8 => Some(JVMInstruction::Baload),
            // I16 (including promoted U8) is loaded with Saload:
            Type::I16 => Some(JVMInstruction::Saload),
            Type::Boolean => Some(JVMInstruction::Baload),
            Type::Char => Some(JVMInstruction::Caload),
            // I32 (including promoted U16) is loaded with Iaload:
            Type::I32 => Some(JVMInstruction::Iaload),
            // I64 (including promoted U32) is loaded with Laload:
            Type::I64 => Some(JVMInstruction::Laload),
            Type::F32 => Some(JVMInstruction::Faload),
            Type::F64 => Some(JVMInstruction::Daload),
            // Reference types:
            Type::String
            | Type::Class(_)
            | Type::Array(_)
            | Type::Reference(_)
            | Type::MutableReference(_)
            | Type::Interface(_) => Some(JVMInstruction::Aaload),
            Type::Void => None,
        }
    }

    /// Create a Type from a Constant.
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
            Constant::Instance { class_name, .. } => Type::Class(class_name.to_string()),
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
            Type::Reference(_)
                | Type::MutableReference(_)
                | Type::Array(_)
                | Type::String
                | Type::Class(_)
                | Type::Interface(_)
        )
    }

    /// If this is one of the primitive types that Java boxes,
    /// returns (wrapper_class_internal_name, "valueOf", valueOf_descriptor).
    /// Otherwise returns None.
    pub fn get_boxing_info(&self) -> Option<(&'static str, &'static str, &'static str)> {
        match self {
            Type::I32 => Some(("java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;")),
            Type::I16 => Some(("java/lang/Short", "valueOf", "(S)Ljava/lang/Short;")),
            Type::I8 => Some(("java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;")),
            Type::Boolean => Some(("java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;")),
            Type::I64 => Some(("java/lang/Long", "valueOf", "(J)Ljava/lang/Long;")),
            Type::F32 => Some(("java/lang/Float", "valueOf", "(F)Ljava/lang/Float;")),
            Type::F64 => Some(("java/lang/Double", "valueOf", "(D)Ljava/lang/Double;")),
            Type::Char => Some(("java/lang/Character", "valueOf", "(C)Ljava/lang/Character;")),
            _ => None,
        }
    }
    /// Checks if the type is treated as a primitive on the JVM stack
    /// (byte, short, int, long, float, double, char, boolean).
    pub fn is_jvm_primitive_like(&self) -> bool {
        matches!(
            self,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
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
            Type::Array(_) => Some(self.to_jvm_descriptor()), // Array descriptor works for checkcast/anewarray
            Type::String => Some("java/lang/String".to_string()),
            Type::Reference(inner) => inner.to_jvm_descriptor_or_internal_name(),
            Type::MutableReference(inner) => {
                Type::Array(inner.clone()).to_jvm_descriptor_or_internal_name()
            } // MutableReference is treated as an array
            _ => None,
        }
    }

    /// Recursively replaces all occurrences of `Type::Class(old_name)` with `Type::Class(new_name)`.
    pub fn replace_class(&mut self, old_name: &str, new_name: &str) {
        match self {
            Type::Class(name) | Type::Interface(name) => {
                if name == old_name {
                    *name = new_name.to_string();
                }
            }
            // Handle nested types recursively
            Type::MutableReference(inner) | Type::Reference(inner) | Type::Array(inner) => {
                inner.replace_class(old_name, new_name);
            }
            // Primitive types, Void, and String are unaffected
            Type::Void
            | Type::Boolean
            | Type::Char
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::F32
            | Type::F64
            | Type::String => {
                // No class names to replace here
            }
        }
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (_param_name, param_ty) in &self.params {
            write!(f, "{}", param_ty.to_jvm_descriptor())?;
        }
        write!(f, "){}", self.ret.to_jvm_descriptor())
    }
}
