pub mod abi;
pub mod casts;
pub mod constants;
pub mod encoding;
pub mod frames;
pub mod select;

use crate::classfile::attributes::{Attribute, ExceptionTableEntry, Instruction};

pub struct MethodCode {
    pub instructions: Vec<Instruction>,
    pub max_stack: u16,
    pub max_locals: u16,
    pub attributes: Vec<Attribute>,
    pub exceptions: Vec<ExceptionTableEntry>,
}

pub mod flow;
pub mod locals;
