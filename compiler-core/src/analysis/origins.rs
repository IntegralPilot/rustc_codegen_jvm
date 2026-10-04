//! Allocation origins through identity annotations and equal-origin CFG joins.
use crate::ir::*;

pub(crate) const NO_ORIGIN: u32 = u32::MAX;
const UNKNOWN: u32 = u32::MAX - 1;

/// Track allocation identity through aliases and joins.
/// Only roots in an entry block without incoming edges can cross joins.
/// Conflicting inputs make joins unknown. Each value changes at most twice.
pub(crate) fn origins(body: &Body, roots: &[u32]) -> Vec<u32> {
    origins_with(
        body,
        roots,
        |op| match op {
            Op::Reinterpret(source) | Op::Refine(source) => Some(source),
            _ => None,
        },
        false,
    )
}

pub(crate) fn origins_with(
    body: &Body,
    roots: &[u32],
    source_of: impl Fn(Op) -> Option<ValueId>,
    ignore_uninit: bool,
) -> Vec<u32> {
    let count = body.values.len();
    let mut stable = vec![
        false;
        roots
            .iter()
            .filter(|&&r| r != NO_ORIGIN)
            .max()
            .map_or(0, |r| *r as usize + 1)
    ];
    if !body.edges.iter().any(|edge| edge.target == body.entry) {
        let entry = &body.blocks[body.entry.index()];
        let invoke = match entry.terminator {
            Some(Terminator::Invoke { inst, .. }) => Some(inst),
            _ => None,
        };
        for &id in entry.instructions.iter().chain(invoke.iter()) {
            if let Some(value) = body.instructions[id.index()].result {
                let root = roots[value.index()];
                if root != NO_ORIGIN {
                    stable[root as usize] = true;
                }
            }
        }
    }
    let mut state = roots.to_vec();
    let mut joins = vec![false; count];
    let mut undefined = vec![false; count];
    let mut users = super::ValueUsers::new(count);
    for (index, value) in body.values.iter().enumerate() {
        if roots[index] != NO_ORIGIN {
            continue;
        }
        let source = match value.def {
            ValueDef::Alias(source) => Some(source),
            ValueDef::Inst(inst) => {
                let op = body.instructions[inst.index()].op;
                // An inactive enum payload can use any allocation. A real null cannot.
                if ignore_uninit
                    && matches!(op, Op::Constant(id) if matches!(body.constants[id.index()], Constant::Uninit(_)))
                {
                    undefined[index] = true;
                    state[index] = UNKNOWN;
                }
                source_of(op)
            }
            ValueDef::Param(block) if block != body.entry => {
                joins[index] = true;
                state[index] = UNKNOWN;
                None
            }
            _ => None,
        };
        if let Some(source) = source {
            state[index] = UNKNOWN;
            users.connect(source, index);
        }
    }
    for edge in &body.edges {
        for (&source, &target) in edge
            .args
            .iter()
            .zip(&body.blocks[edge.target.index()].params)
        {
            if joins[target.index()] {
                users.connect(source, target.index());
            }
        }
    }
    let mut pending = (0..count)
        .filter(|&i| state[i] != UNKNOWN)
        .collect::<Vec<_>>();
    for phase in 0..2 {
        while let Some(source) = pending.pop() {
            for target in users.users(source) {
                let mut incoming = state[source];
                if joins[target] && incoming != NO_ORIGIN && !stable[incoming as usize] {
                    incoming = NO_ORIGIN;
                }
                let previous = state[target];
                let merged = if previous == UNKNOWN || previous == incoming {
                    incoming
                } else {
                    NO_ORIGIN
                };
                if merged != previous {
                    state[target] = merged;
                    pending.push(target);
                }
            }
        }
        if phase == 0 {
            // A cycle without a defining root cannot prove allocation identity,
            // even if another incoming edge has a known root.
            for (index, value) in state.iter_mut().enumerate() {
                if *value == UNKNOWN && !undefined[index] {
                    *value = NO_ORIGIN;
                    pending.push(index);
                }
            }
        }
    }
    for origin in &mut state {
        if *origin == UNKNOWN {
            *origin = NO_ORIGIN;
        }
    }
    state
}
