mod analysis;
pub use analysis::analyze;
use analysis::*;
mod locals;
pub use locals::{
    initial_locals_for_descriptor, initialize_locals_loaded_as_top, move_zero_branch_target,
    push_local_value, set_slot_value,
};
mod flow;
use flow::*;
mod transfer;
use transfer::*;
mod encoding;
pub use encoding::{build_stack_map_attributes, build_stack_map_attributes_from_analysis};
mod descriptors;
use crate::classfile::constant_pool::InternedConstantPool;
use crate::classfile::{
    self as jvm, BaseType, Constant, ConstantPool, FieldType,
    attributes::{
        ArrayType, Attribute, ExceptionTableEntry, Instruction, StackFrame, VerificationType,
    },
};
use descriptors::*;
use rustc_hash::FxHashMap as HashMap;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameValue {
    Top,
    Integer,
    Float,
    Long,
    Double,
    Null,
    Object(Arc<str>),
    UninitializedThis,
    Uninitialized(u16),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FrameState {
    locals: Arc<Vec<FrameValue>>,
    stack: Vec<FrameValue>,
    stack_words: usize,
}

pub struct FrameAnalysis {
    pub max_stack: u16,
    block_starts: Vec<usize>,
    entry_states: Vec<Option<FrameState>>,
}

impl FrameAnalysis {
    fn state_at(&self, instruction: usize) -> Option<&FrameState> {
        let block = self.block_starts.binary_search(&instruction).ok()?;
        self.entry_states.get(block)?.as_ref()
    }
}

impl FrameValue {
    fn is_category2(&self) -> bool {
        matches!(self, FrameValue::Long | FrameValue::Double)
    }

    fn is_reference_like(&self) -> bool {
        matches!(
            self,
            FrameValue::Null
                | FrameValue::Object(_)
                | FrameValue::UninitializedThis
                | FrameValue::Uninitialized(_)
        )
    }
}

impl FrameState {
    fn new(initial_locals: Vec<FrameValue>, max_locals: usize) -> Self {
        let mut locals = initial_locals;
        locals.resize(max_locals, FrameValue::Top);
        Self {
            locals: Arc::new(locals),
            stack: Vec::new(),
            stack_words: 0,
        }
    }

    fn push(&mut self, value: FrameValue) {
        self.stack_words += if value.is_category2() { 2 } else { 1 };
        self.stack.push(value);
    }

    fn pop(&mut self, context: &str, instruction_index: usize) -> jvm::Result<FrameValue> {
        let value = self
            .stack
            .pop()
            .ok_or_else(|| jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!("Stack underflow at instruction {instruction_index}"),
            })?;
        self.stack_words -= if value.is_category2() { 2 } else { 1 };
        Ok(value)
    }

    fn pop_category1(
        &mut self,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<FrameValue> {
        let value = self.pop(context, instruction_index)?;
        if value.is_category2() {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Expected category-1 value at instruction {instruction_index}, found {value:?}"
                ),
            });
        }
        Ok(value)
    }

    fn pop_reference(
        &mut self,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<FrameValue> {
        let value = self.pop_category1(context, instruction_index)?;
        if !value.is_reference_like() && value != FrameValue::Top {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Expected reference value at instruction {instruction_index}, found {value:?}"
                ),
            });
        }
        Ok(value)
    }

    fn load_local(
        &mut self,
        index: u16,
        local_hints: &[FrameValue],
        load_hint: FrameValue,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<()> {
        let mut value = self
            .locals
            .get(index as usize)
            .cloned()
            .unwrap_or(FrameValue::Top);
        if value == FrameValue::Top {
            value = local_hints
                .get(index as usize)
                .cloned()
                .unwrap_or(FrameValue::Top);
            if value == FrameValue::Top {
                value = load_hint;
                if value == FrameValue::Top {
                    return Err(jvm::Error::VerificationError {
                        context: context.to_string(),
                        message: format!(
                            "Loaded uninitialized local {index} at instruction {instruction_index}"
                        ),
                    });
                }
            }
            self.store_local(index, value.clone());
        }
        self.push(value);
        Ok(())
    }

    fn store_local(&mut self, index: u16, value: FrameValue) {
        let index = index as usize;
        let width = if value.is_category2() { 2 } else { 1 };
        if self.locals.get(index) == Some(&value)
            && (index == 0 || !self.locals[index - 1].is_category2())
            && (width == 1 || self.locals.get(index + 1) == Some(&FrameValue::Top))
        {
            return;
        }
        let locals = Arc::make_mut(&mut self.locals);
        if locals.len() < index + width {
            locals.resize(index + width, FrameValue::Top);
        }

        if index > 0 && locals[index - 1].is_category2() {
            locals[index - 1] = FrameValue::Top;
        }
        locals[index] = value;
        if width == 2 {
            locals[index + 1] = FrameValue::Top;
        }
    }

    fn initialize_object(&mut self, uninitialized: &FrameValue, class_name: &str) {
        let initialized = FrameValue::Object(normalize_class_name(class_name).into());
        if self.locals.contains(uninitialized) {
            for local in Arc::make_mut(&mut self.locals) {
                if local == uninitialized {
                    *local = initialized.clone();
                }
            }
        }
        for stack_value in &mut self.stack {
            if stack_value == uninitialized {
                *stack_value = initialized.clone();
            }
        }
    }
}

pub fn normalize_class_name(class_name: &str) -> String {
    class_name.replace('.', "/")
}

#[cfg(test)]
mod tests;
