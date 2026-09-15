//! Demand scanning and alias-aware materialization of large constants.
use super::*;

pub(super) const MAX_INLINE_CONSTANT_INSTRUCTIONS: usize = 1_024;

use crate::oomir::constant_instruction_cost;

/// Ordinary scalar bodies never allocate analysis state for constants.
pub(in crate::lower2) fn function_needs_constant_preparation(
    function: &oomir::SsaFunction,
) -> bool {
    let body = &function.body;
    body.constants
        .iter()
        .any(|constant| constant_instruction_cost(constant) > MAX_INLINE_CONSTANT_INSTRUCTIONS)
}

/// Propagate escapes backwards through SSA aliases, block parameters and
/// reference-valued reads. Sharing is allowed only when no observable mutable
/// identity can leave the body, including identities nested inside an array.
fn shared_arrays(body: &oomir::SsaBody) -> Vec<bool> {
    use jvm_compiler_core::ir::{Constant, Op, Type};
    let ir = &body.ir;
    let mut depends = vec![Vec::new(); ir.values.len()];
    let mut pending = Vec::new();
    for inst in &ir.instructions {
        let parent = match inst.op {
            Op::Reinterpret(value) | Op::Cast(value) => Some(value),
            Op::GetField { object, .. } => Some(object),
            Op::ArrayGet { array, index } => {
                pending.push(ir.resolve(index));
                Some(array)
            }
            Op::ArrayLength(_) | Op::Length(_) => None,
            op => {
                op.visit_uses(&ir.args, |value| pending.push(ir.resolve(value)));
                None
            }
        };
        if let Some(parent) = parent {
            let result = inst.result.unwrap();
            if !matches!(
                body.types.get(ir.value_type(result)),
                Some(Type::Scalar(_) | Type::Unit)
            ) {
                depends[ir.resolve(result).index()].push(ir.resolve(parent));
            }
        }
    }
    for block in &ir.blocks {
        if let Some(term) = block.terminator {
            term.visit_uses(|value| pending.push(ir.resolve(value)));
        }
    }
    for edge in &ir.edges {
        for (&param, &arg) in ir.blocks[edge.target.index()].params.iter().zip(&edge.args) {
            depends[ir.resolve(param).index()].push(ir.resolve(arg));
        }
    }
    let mut escapes = vec![false; ir.values.len()];
    while let Some(value) = pending.pop() {
        if !std::mem::replace(&mut escapes[value.index()], true) {
            pending.extend_from_slice(&depends[value.index()]);
        }
    }
    let mut shared = vec![true; body.constants.len()];
    for inst in &ir.instructions {
        if let Op::Constant(id) = inst.op
            && let Constant::External { index, .. } = ir.constants[id.index()]
            && escapes[ir.resolve(inst.result.unwrap()).index()]
        {
            shared[index as usize] = false;
        }
    }
    shared
}

pub(in crate::lower2) fn prepare_function_constants(
    function: &mut oomir::SsaFunction,
    cp: &mut InternedConstantPool,
    owner_class: &str,
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<()> {
    let body = &mut function.body;
    let shared = shared_arrays(body);
    let body = std::sync::Arc::make_mut(body);
    for (index, constant) in body.constants.iter_mut().enumerate() {
        if constant_instruction_cost(constant) <= MAX_INLINE_CONSTANT_INSTRUCTIONS {
            continue;
        }
        *constant = if let oomir::Constant::Array(element, values) = constant
            && shared[index]
        {
            create_shared_array_factory(cp, owner_class, element, values, methods, next_factory)?
        } else {
            create_constant_factory(cp, owner_class, constant, methods, next_factory)?
        };
    }
    Ok(())
}
