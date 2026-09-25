//! Structural and dominance checks for SSA bodies, including throw-point values.
use super::*;
use crate::scalar::{BinaryOp, ScalarType};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyError(pub String);
impl fmt::Display for VerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl std::error::Error for VerifyError {}

macro_rules! check {
    ($condition:expr, $($message:tt)*) => {
        if !$condition { return Err(VerifyError(format!($($message)*))); }
    }
}

mod dominance;
mod types;
use dominance::dominance;
use types::verify_types;

pub fn verify(body: &Body, types: &Types) -> Result<(), VerifyError> {
    verify_with_debug(body, types, None)
}

pub fn verify_with_debug(
    body: &Body,
    types: &Types,
    debug: Option<&DebugInfo>,
) -> Result<(), VerifyError> {
    check!(body.entry.index() < body.blocks.len(), "invalid entry");
    check!(
        types
            .get(body.return_type)
            .is_some_and(|t| !matches!(t, Type::Opaque(_))),
        "invalid return type"
    );
    for value in &body.values {
        check!(
            types
                .get(value.ty)
                .is_some_and(|t| !matches!(t, Type::Opaque(_))),
            "invalid value type"
        );
    }
    for slot in &body.slots {
        check!(
            slot.size <= i32::MAX as u32
                && slot.alignment <= i32::MAX as u32
                && slot.alignment.is_power_of_two(),
            "invalid storage allocation layout"
        );
        check!(
            slot.codec
                .is_none_or(|codec| types.symbol_name(codec).is_some()),
            "invalid storage codec"
        );
        match types.get(slot.ty) {
            Some(Type::Scalar(_)) => check!(
                StorageSlot::scalar(slot.ty, types).as_ref() == Some(slot),
                "invalid scalar storage layout"
            ),
            Some(Type::Class(symbol) | Type::Interface(symbol)) => {
                check!(types.symbol_name(symbol).is_some(), "invalid storage class")
            }
            Some(Type::Pointer(_) | Type::Slice(_) | Type::Str) => {}
            _ => return Err(VerifyError("unsupported storage type".into())),
        }
    }
    for field in &body.fields {
        check!(
            !field.relative_pointer
                || (!field.is_static && matches!(types.get(field.ty), Some(Type::Pointer(_)))),
            "invalid relative pointer field"
        );
        check!(
            matches!(types.get(field.owner), Some(Type::Class(symbol) | Type::Interface(symbol)) if types.symbol_name(symbol).is_some()),
            "invalid field owner"
        );
        check!(
            types
                .get(field.ty)
                .is_some_and(|t| !matches!(t, Type::Unit | Type::Opaque(_))),
            "invalid field type"
        );
    }
    let roots = resolve_aliases(body)?;
    let mut owners = vec![None; body.instructions.len()];
    let mut edge_owners = vec![None; body.edges.len()];
    let mut predecessors = vec![Vec::new(); body.blocks.len()];
    let mut parameters = vec![false; body.values.len()];
    // Validate every target's parameter table before following any edge.
    for (index, block) in body.blocks.iter().enumerate() {
        for &param in &block.params {
            check!(
                body.values
                    .get(param.index())
                    .is_some_and(|v| v.def == ValueDef::Param(BlockId::new(index))),
                "invalid parameter {param:?}"
            );
            check!(
                !std::mem::replace(&mut parameters[param.index()], true),
                "duplicate parameter {param:?}"
            );
        }
    }
    for (index, block) in body.blocks.iter().enumerate() {
        let b = BlockId::new(index);
        let term = block
            .terminator
            .ok_or_else(|| VerifyError(format!("unterminated {b:?}")))?;
        if let Terminator::Switch { cases, .. } = term {
            check!(
                cases.range().end <= body.cases.len(),
                "invalid switch cases"
            );
        }
        let mut edges = Vec::new();
        term.visit_edges(&body.cases, |edge| edges.push(edge));
        for edge in edges {
            let e = body
                .edges
                .get(edge.index())
                .ok_or_else(|| VerifyError("invalid edge".into()))?;
            check!(
                edge_owners[edge.index()].replace(b).is_none(),
                "edge shared between branch sites"
            );
            let target = body
                .blocks
                .get(e.target.index())
                .ok_or_else(|| VerifyError("invalid target".into()))?;
            check!(
                e.args.len() == target.params.len(),
                "edge parameter count mismatch"
            );
            for (&arg, &param) in e.args.iter().zip(&target.params) {
                check!(
                    body.values
                        .get(arg.index())
                        .is_some_and(|v| v.ty == body.values[param.index()].ty),
                    "edge argument type mismatch"
                );
            }
            predecessors[e.target.index()].push(b);
        }
        for (position, &inst) in block.instructions.iter().enumerate() {
            check!(inst.index() < owners.len(), "invalid instruction");
            check!(
                owners[inst.index()].replace((b, position + 1)).is_none(),
                "instruction appears twice"
            );
        }
        if let Terminator::Invoke { inst, normal, .. } = term {
            check!(inst.index() < owners.len(), "invalid invoke instruction");
            check!(
                owners[inst.index()]
                    .replace((body.edges[normal.index()].target, 0))
                    .is_none(),
                "invoke instruction appears twice"
            );
        }
    }
    for (b, block) in body.blocks.iter().enumerate() {
        if let Some(Terminator::Invoke { normal, .. }) = block.terminator {
            let target = body.edges[normal.index()].target;
            check!(
                target.index() != b
                    && target != body.entry
                    && predecessors[target.index()].len() == 1,
                "invoke needs a private normal continuation"
            );
        }
    }
    for (index, inst) in body.instructions.iter().enumerate() {
        check!(owners[index].is_some(), "orphan instruction");
        if let Some(result) = inst.result {
            check!(
                body.values
                    .get(result.index())
                    .is_some_and(|v| v.def == ValueDef::Inst(InstId::new(index))),
                "invalid instruction result"
            );
        }
        match inst.op {
            Op::Call { method, args, .. } => {
                check!(method.index() < body.methods.len(), "invalid call target");
                check!(args.range().end <= body.args.len(), "invalid operand list");
            }
            Op::Overflow { args, .. } => {
                check!(args.range().end <= body.args.len(), "invalid operand list")
            }
            Op::Constant(id) => check!(id.index() < body.constants.len(), "invalid constant"),
            Op::LoadSlot(id) | Op::AddressOfSlot(id) | Op::StoreSlot { slot: id, .. } => {
                check!(id.index() < body.slots.len(), "invalid storage slot")
            }
            _ => {}
        }
        let mut bad = false;
        inst.op.visit_uses(&body.args, |value| {
            bad |= value.index() >= body.values.len()
        });
        check!(!bad, "invalid instruction operand");
        verify_types(inst, body, types)?;
    }
    for (index, value) in body.values.iter().enumerate() {
        match value.def {
            ValueDef::Inst(inst) => check!(
                body.instructions
                    .get(inst.index())
                    .is_some_and(|i| i.result == Some(ValueId::new(index))),
                "unowned result value"
            ),
            ValueDef::Param(_) => check!(parameters[index], "unowned parameter value"),
            ValueDef::Alias(_) | ValueDef::Unreachable => {}
        }
    }

    let reachable = body.reachable();
    let (pre, post) = dominance(body, &predecessors, &reachable);
    let use_value = |value: ValueId,
                     at: BlockId,
                     position: usize,
                     invoke_result: Option<InstId>|
     -> Result<(), VerifyError> {
        check!(value.index() < roots.len(), "invalid value use");
        let root = roots[value.index()];
        if !reachable[at.index()] {
            return Ok(());
        }
        let (defined, order) = match body.values[root.index()].def {
            ValueDef::Inst(inst) if Some(inst) == invoke_result => return Ok(()),
            ValueDef::Inst(inst) => owners[inst.index()].unwrap(),
            ValueDef::Param(block) => (block, 0),
            ValueDef::Unreachable => {
                return Err(VerifyError(
                    "unreachable value used by reachable code".into(),
                ));
            }
            ValueDef::Alias(_) => unreachable!(),
        };
        check!(
            reachable[defined.index()]
                && pre[defined.index()] <= pre[at.index()]
                && pre[at.index()] < post[defined.index()],
            "{root:?} does not dominate its use in {at:?}"
        );
        check!(
            defined != at || order < position,
            "value used before definition"
        );
        Ok(())
    };
    if let Some(debug) = debug {
        for local in &debug.locals {
            match *local {
                DebugLocal::Value(ty) => check!(
                    types.get(ty).is_some_and(|t| !matches!(t, Type::Opaque(_))),
                    "invalid debug local type"
                ),
                DebugLocal::Storage(slot) => {
                    check!(slot.index() < body.slots.len(), "invalid debug cell")
                }
            }
        }
        for variable in &debug.variables {
            check!(
                (variable.local as usize) < debug.locals.len(),
                "invalid debug variable binding"
            );
        }
        for scope in &debug.scopes {
            check!(
                scope.iter().all(|&v| (v as usize) < debug.variables.len()),
                "invalid debug scope"
            );
        }
        for event in &debug.events {
            check!(
                event.block.index() < body.blocks.len(),
                "invalid debug block"
            );
            check!(
                event.position as usize <= body.blocks[event.block.index()].instructions.len(),
                "invalid debug position"
            );
            match event.change {
                DebugChange::Set { local, value } => {
                    use_value(value, event.block, event.position as usize + 1, None)?;
                    check!(
                        debug.locals.get(local as usize)
                            == Some(&DebugLocal::Value(body.value_type(value))),
                        "debug binding type mismatch"
                    );
                }
                DebugChange::Clear(local) => check!(
                    (local as usize) < debug.locals.len(),
                    "invalid cleared debug binding"
                ),
                DebugChange::Scope(scope) => check!(
                    (scope as usize) < debug.scopes.len(),
                    "invalid debug scope event"
                ),
            }
        }
    }
    for (index, block) in body.blocks.iter().enumerate() {
        let b = BlockId::new(index);
        let term = block.terminator.unwrap();
        let end = block.instructions.len() + 1;
        for (position, &inst) in block.instructions.iter().enumerate() {
            let mut result = Ok(());
            body.instructions[inst.index()]
                .op
                .visit_uses(&body.args, |v| {
                    if result.is_ok() {
                        result = use_value(v, b, position + 1, None);
                    }
                });
            result?;
        }
        if let Terminator::Invoke { inst, .. } = term {
            let mut result = Ok(());
            body.instructions[inst.index()]
                .op
                .visit_uses(&body.args, |v| {
                    if result.is_ok() {
                        result = use_value(v, b, end, None);
                    }
                });
            result?;
        }
        let mut result = Ok(());
        term.visit_edges(&body.cases, |edge| {
            let allow = match term {
                Terminator::Invoke { inst, normal, .. } if edge == normal => Some(inst),
                _ => None,
            };
            for &arg in &body.edges[edge.index()].args {
                if result.is_ok() {
                    result = use_value(arg, b, end, allow);
                }
            }
        });
        result?;
        match term {
            Terminator::Branch { condition, .. } => {
                use_value(condition, b, end, None)?;
                check!(
                    types.get(body.value_type(condition)) == Some(Type::Scalar(ScalarType::Bool)),
                    "branch needs boolean"
                );
            }
            Terminator::Switch { value, cases, .. } => {
                use_value(value, b, end, None)?;
                let Some(Type::Scalar(scalar)) = types.get(body.value_type(value)) else {
                    return Err(VerifyError("switch requires scalar".into()));
                };
                check!(
                    scalar.integer().is_some() || scalar == ScalarType::Bool,
                    "switch requires integer or boolean"
                );
                let mut keys = rustc_hash::FxHashSet::default();
                for &(key, _) in &body.cases[cases.range()] {
                    check!(key.ty() == scalar, "switch key type mismatch");
                    check!(keys.insert(key.bits()), "duplicate switch key");
                }
            }
            Terminator::Throw { value, .. } => {
                use_value(value, b, end, None)?;
            }
            Terminator::Return(Some(value)) => {
                use_value(value, b, end, None)?;
                check!(
                    body.value_type(value) == body.return_type,
                    "return type mismatch"
                );
            }
            Terminator::Return(None) => check!(
                types.get(body.return_type) == Some(Type::Unit),
                "missing return value"
            ),
            _ => {}
        }
    }
    Ok(())
}

fn resolve_aliases(body: &Body) -> Result<Vec<ValueId>, VerifyError> {
    let mut roots = vec![ValueId::new(0); body.values.len()];
    let mut state = vec![0; body.values.len()];
    let mut path = Vec::new();
    for index in 0..body.values.len() {
        if state[index] == 2 {
            continue;
        }
        let mut value = ValueId::new(index);
        let root = loop {
            check!(value.index() < body.values.len(), "invalid alias");
            if state[value.index()] == 2 {
                break roots[value.index()];
            }
            check!(state[value.index()] != 1, "alias cycle");
            state[value.index()] = 1;
            path.push(value);
            match body.values[value.index()].def {
                ValueDef::Alias(next) => {
                    check!(
                        body.values
                            .get(next.index())
                            .is_some_and(|v| v.ty == body.value_type(value)),
                        "alias type mismatch"
                    );
                    value = next;
                }
                _ => break value,
            }
        };
        for value in path.drain(..) {
            roots[value.index()] = root;
            state[value.index()] = 2;
        }
    }
    Ok(roots)
}
