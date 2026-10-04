//! Promote private generated carriers to SSA fields.
//! Require supplied constructor layouts. Arbitrary JVM constructors can run code.
use crate::ir::*;

const NONE: u32 = u32::MAX;

pub(super) struct Candidate {
    pub(super) fields: Vec<FieldRef>,
    pub(super) initial: List,
}

pub fn promote_aggregates(
    mut body: Body,
    types: &Types,
    mut layout: impl FnMut(&str) -> Option<Vec<FieldRef>>,
) -> Result<Body, VerifyError> {
    super::owned_copies::reuse_readonly_copies(&mut body, types, &mut layout);
    super::value_copies::expand_flat_copies(&mut body, types, &mut layout);
    if !body.instructions.iter().any(|inst| {
        matches!(
            inst.op,
            Op::Call {
                kind: CallKind::Constructor,
                ..
            }
        )
    }) {
        return Ok(body);
    }
    let mut roots = vec![NONE; body.values.len()];
    let mut candidates = Vec::new();
    for inst in &body.instructions {
        let Op::Call {
            method,
            kind: CallKind::Constructor,
            args,
        } = inst.op
        else {
            continue;
        };
        let Some(fields) = layout(&body.methods[method.index()].owner) else {
            continue;
        };
        if fields.len() != args.len as usize
            || fields
                .iter()
                .zip(&body.args[args.range()])
                .any(|(f, &v)| f.ty != body.value_type(v))
        {
            continue;
        }
        roots[inst.result.unwrap().index()] = candidates.len() as u32;
        candidates.push(Candidate {
            fields,
            initial: args,
        });
    }
    if candidates.is_empty() {
        return Ok(body);
    }
    let mut escaped = super::aggregate_joins::promote(&mut body, &mut roots, &candidates);
    let origins = crate::analysis::origins(&body, &roots);
    let position = |index: usize, field: MemberId| {
        candidates[index]
            .fields
            .iter()
            .position(|f| f == &body.fields[field.index()])
    };
    for inst in &body.instructions {
        inst.op.visit_uses(&body.args, |value| {
            let index = origins[value.index()];
            if index == NONE {
                return;
            }
            let index = index as usize;
            let allowed = match inst.op {
                Op::Reinterpret(_) | Op::Refine(_) => true,
                Op::GetField { object, field } => {
                    object == value && position(index, field).is_some()
                }
                Op::SetField {
                    object,
                    field,
                    value: stored,
                } => object == value && stored != value && position(index, field).is_some(),
                _ => false,
            };
            escaped[index] |= !allowed;
        });
    }
    let mut escape = |value: ValueId| {
        let index = origins[value.index()];
        if index != NONE {
            escaped[index as usize] = true;
        }
    };
    for edge in &body.edges {
        for (&value, &param) in edge
            .args
            .iter()
            .zip(&body.blocks[edge.target.index()].params)
        {
            if origins[value.index()] != origins[param.index()] {
                escape(value);
            }
        }
    }
    for block in &body.blocks {
        block.terminator.unwrap().visit_uses(&mut escape);
    }
    if escaped.iter().all(|&e| e) {
        return Ok(body);
    }
    let mut builder = Builder::from_body(body, types);
    let variables: Vec<_> = candidates
        .iter()
        .enumerate()
        .map(|(index, candidate)| {
            (!escaped[index]).then(|| {
                candidate
                    .fields
                    .iter()
                    .map(|field| builder.variable(field.ty))
                    .collect::<Vec<_>>()
            })
        })
        .collect();
    for block_index in 0..builder.body.blocks.len() {
        builder.switch_to(BlockId::new(block_index));
        let instructions = builder.body.blocks[block_index].instructions.clone();
        let invoke = match builder.body.blocks[block_index].terminator.unwrap() {
            Terminator::Invoke { inst, .. } => Some(inst),
            _ => None,
        };
        for id in instructions.into_iter().chain(invoke) {
            let inst = builder.body.instructions[id.index()];
            if let Some(result) = inst.result {
                let index = roots[result.index()];
                if index != NONE
                    && let Some(vars) = &variables[index as usize]
                {
                    let initial = candidates[index as usize].initial;
                    for (&var, pos) in vars.iter().zip(initial.range()) {
                        builder.define(var, builder.body.args[pos]);
                    }
                    let constant = ConstId::new(builder.body.constants.len());
                    builder
                        .body
                        .constants
                        .push(Constant::Null(builder.body.value_type(result)));
                    builder.body.instructions[id.index()].op = Op::Constant(constant);
                    continue;
                }
            }
            let (object, field) = match inst.op {
                Op::GetField { object, field } | Op::SetField { object, field, .. } => {
                    (object, field)
                }
                _ => continue,
            };
            let index = origins[object.index()];
            if index == NONE {
                continue;
            }
            let Some(vars) = &variables[index as usize] else {
                continue;
            };
            let position = candidates[index as usize]
                .fields
                .iter()
                .position(|f| f == &builder.body.fields[field.index()])
                .unwrap();
            let variable = vars[position];
            builder.body.instructions[id.index()].op = match inst.op {
                Op::GetField { .. } => Op::Reinterpret(builder.read(variable)),
                Op::SetField { value, .. } => {
                    builder.define(variable, value);
                    Op::Nop
                }
                _ => unreachable!(),
            };
        }
        if let Some(Terminator::Invoke { inst, normal, .. }) =
            builder.body.blocks[block_index].terminator
            && !builder.body.instructions[inst.index()]
                .op
                .may_throw(&builder.body, types)
        {
            builder.body.blocks[block_index].instructions.push(inst);
            builder.body.blocks[block_index].terminator = Some(Terminator::Jump(normal));
        }
    }
    builder.finish()
}
