//! Constant encoding, bounded materialization, and static initialization.
use super::{
    constant_pool::InternedConstantPool,
    helpers::{get_cast_instructions, return_instruction_for_type},
    jvm, oomir,
};
use jvm::{
    MethodAccessFlags,
    attributes::{ArrayType, Attribute, Instruction, MaxStack},
};
mod encoding;
pub(super) use encoding::*;
mod arrays;
use arrays::append_empty_array;
mod factories;
use factories::{create_constant_factory, create_shared_array_factory};
mod prepare;
use crate::oomir::constant_instruction_cost;
use prepare::MAX_INLINE_CONSTANT_INSTRUCTIONS;
pub(super) use prepare::{function_needs_constant_preparation, prepare_function_constants};
mod statics;
pub(super) use statics::create_static_initializer_method;
