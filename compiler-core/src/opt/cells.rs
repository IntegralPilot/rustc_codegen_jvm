//! Escape analysis and SSA promotion for private, explicitly initialized cells.
use crate::ir::*;

use crate::analysis::{NO_ORIGIN as NONE, origins};

fn cell_index(index: u32) -> Option<usize> {
    (index != NONE).then_some(index as usize)
}

struct Location {
    cell: usize,
    fields: Vec<MemberId>,
}

fn location(body: &Body, types: &Types, origins: &[u32], mut value: ValueId) -> Option<Location> {
    types.pointee(body.value_type(value))?;
    let mut fields = Vec::new();
    for _ in 0..64 {
        value = body.resolve(value);
        if let Some(cell) = cell_index(origins[value.index()]) {
            fields.reverse();
            return Some(Location { cell, fields });
        }
        let ValueDef::Inst(id) = body.values[value.index()].def else {
            return None;
        };
        match body.instructions[id.index()].op {
            Op::Project { base, projection } => {
                let field = body.projections[projection.index()].field;
                let member = &body.fields[field.index()];
                if types.pointee(body.value_type(base)) != Some(member.owner)
                    || types.pointee(body.value_type(value)) != Some(member.ty)
                {
                    return None;
                }
                fields.push(field);
                value = base;
            }
            Op::Reinterpret(source) | Op::Refine(source)
                if body.value_type(source) == body.value_type(value) =>
            {
                value = source
            }
            _ => return None,
        }
    }
    None
}

/// Each candidate is a fresh allocation and its typed initial contents. The
/// producer guarantees that creating it has no effects beyond allocation.
/// Keep cells whose address escapes, is retyped, or participates in a join of
/// distinct addresses. Contents of private cells may cross arbitrary CFG joins.
pub fn promote_cells(
    body: Body,
    types: &Types,
    cells: &[(ValueId, ValueId)],
) -> Result<Body, VerifyError> {
    let mut roots = vec![NONE; body.values.len()];
    for (index, &(cell, initial)) in cells.iter().enumerate() {
        assert_eq!(
            types.get(body.value_type(cell)),
            Some(Type::Pointer(body.value_type(initial)))
        );
        roots[cell.index()] = u32::try_from(index).expect("too many private cells");
    }
    let origins = origins(&body, &roots);
    let locations = (0..body.values.len())
        .map(|i| location(&body, types, &origins, ValueId::new(i)))
        .collect::<Vec<_>>();
    let mut invoked = vec![false; body.instructions.len()];
    for block in &body.blocks {
        if let Some(Terminator::Invoke { inst, .. }) = block.terminator {
            invoked[inst.index()] = true;
        }
    }
    // Field promotion can leave unused projections.
    // They do not observe addresses and must not block SSA storage promotion.
    let live = super::live(&body, types);
    let mut escaped = vec![false; cells.len()];
    for (id, inst) in body.instructions.iter().enumerate() {
        if !live.instructions[id] {
            continue;
        }
        inst.op.visit_uses(&body.args, |value| {
            let Some(location) = &locations[value.index()] else {
                return;
            };
            let index = location.cell;
            let pointee = location.fields.last().map_or_else(
                || body.value_type(cells[index].1),
                |field| body.fields[field.index()].ty,
            );
            // Keep handlers when promotion would need extra field reads.
            if invoked[id]
                && (!location.fields.is_empty() || matches!(inst.op, Op::LoadFieldCopy { .. }))
            {
                escaped[index] = true;
                return;
            }
            let allowed = match inst.op {
                Op::Reinterpret(_) | Op::Refine(_) => {
                    inst.result.is_some_and(|v| locations[v.index()].is_some())
                }
                Op::Load(pointer) | Op::LoadCopy(pointer) => {
                    pointer == value && inst.result.is_some_and(|v| body.value_type(v) == pointee)
                }
                Op::Store {
                    pointer,
                    value: stored,
                } => pointer == value && stored != value && body.value_type(stored) == pointee,
                Op::Project { base, projection } => {
                    base == value
                        && body.fields[body.projections[projection.index()].field.index()].owner
                            == pointee
                        && inst.result.is_some_and(|v| locations[v.index()].is_some())
                }
                Op::LoadField { base, projection } | Op::LoadFieldCopy { base, projection } => {
                    base == value
                        && body.fields[body.projections[projection.index()].field.index()].owner
                            == pointee
                }
                Op::StoreField {
                    base,
                    projection,
                    value: stored,
                } => {
                    base == value
                        && stored != value
                        && body.fields[body.projections[projection.index()].field.index()].owner
                            == pointee
                }
                _ => false,
            };
            escaped[index] |= !allowed;
        });
    }
    // Equal-origin joins retain one allocation.
    // Mixed joins must retain each allocation's addressable storage.
    let mut escape = |value: ValueId| {
        if let Some(location) = &locations[value.index()] {
            escaped[location.cell] = true;
        }
    };
    for edge in &body.edges {
        for (&value, &param) in edge
            .args
            .iter()
            .zip(&body.blocks[edge.target.index()].params)
        {
            if live.values[body.resolve(param).index()]
                && (origins[value.index()] != origins[param.index()]
                    || locations[value.index()]
                        .as_ref()
                        .is_some_and(|l| !l.fields.is_empty()))
            {
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
    let variables = cells
        .iter()
        .enumerate()
        .map(|(index, &(_, initial))| {
            (!escaped[index]).then(|| builder.variable(builder.body.value_type(initial)))
        })
        .collect::<Vec<_>>();
    for block_index in 0..builder.body.blocks.len() {
        let block = BlockId::new(block_index);
        builder.switch_to(block);
        let instructions = std::mem::take(&mut builder.body.blocks[block_index].instructions);
        let invoke = match builder.body.blocks[block_index].terminator.unwrap() {
            Terminator::Invoke { inst, .. } => Some(inst),
            _ => None,
        };
        for id in instructions.into_iter().chain(invoke) {
            if Some(id) != invoke {
                builder.body.blocks[block_index].instructions.push(id);
            }
            let inst = builder.body.instructions[id.index()];
            if let Some(index) = inst
                .result
                .and_then(|v| roots.get(v.index()).copied().and_then(cell_index))
                && let Some(variable) = variables[index]
            {
                builder.define(variable, cells[index].1);
                // Alias-only remnants are dead after their loads and stores
                // disappear. A typed null preserves table and debug positions.
                let constant = ConstId::new(builder.body.constants.len());
                builder
                    .body
                    .constants
                    .push(Constant::Null(builder.body.value_type(cells[index].0)));
                builder.body.instructions[id.index()].op = Op::Constant(constant);
                continue;
            }
            let pointer = match inst.op {
                Op::Load(pointer) | Op::LoadCopy(pointer) | Op::Store { pointer, .. } => pointer,
                Op::Project { base, .. }
                | Op::LoadField { base, .. }
                | Op::LoadFieldCopy { base, .. }
                | Op::StoreField { base, .. } => base,
                _ => continue,
            };
            let Some(location) = &locations[pointer.index()] else {
                continue;
            };
            let Some(variable) = variables[location.cell] else {
                continue;
            };
            if matches!(inst.op, Op::Project { .. }) {
                let constant = ConstId::new(builder.body.constants.len());
                builder.body.constants.push(Constant::Null(
                    builder.body.value_type(inst.result.unwrap()),
                ));
                builder.body.instructions[id.index()].op = Op::Constant(constant);
                continue;
            }
            let mut object = builder.read(variable);
            let mut prefix = Vec::new();
            let (stored_field, fields) = if matches!(inst.op, Op::Store { .. }) {
                location
                    .fields
                    .split_last()
                    .map_or((None, &location.fields[..]), |(field, parents)| {
                        (Some(*field), parents)
                    })
            } else {
                (None, &location.fields[..])
            };
            for &field in fields {
                let ty = builder.body.fields[field.index()].ty;
                let (read, value) =
                    super::append_value(&mut builder.body, Op::GetField { object, field }, ty);
                prefix.push(read);
                object = value;
            }
            builder.body.instructions[id.index()].op = match inst.op {
                Op::Load(_) => Op::Reinterpret(object),
                Op::LoadCopy(_) => Op::CopyValue(object),
                Op::Store { value, .. } => {
                    if let Some(field) = stored_field {
                        Op::SetField {
                            object,
                            field,
                            value,
                        }
                    } else {
                        builder.define(variable, value);
                        Op::Nop
                    }
                }
                Op::LoadField { projection, .. } => Op::GetField {
                    object,
                    field: builder.body.projections[projection.index()].field,
                },
                Op::LoadFieldCopy { projection, .. } => {
                    let field = builder.body.projections[projection.index()].field;
                    let ty = builder.body.fields[field.index()].ty;
                    let (read, value) =
                        super::append_value(&mut builder.body, Op::GetField { object, field }, ty);
                    prefix.push(read);
                    Op::CopyValue(value)
                }
                Op::StoreField {
                    projection, value, ..
                } => Op::SetField {
                    object,
                    field: builder.body.projections[projection.index()].field,
                    value,
                },
                _ => unreachable!(),
            };
            if !prefix.is_empty() {
                assert_ne!(Some(id), invoke);
                let block = &mut builder.body.blocks[block_index];
                block.instructions.pop();
                block.instructions.extend(prefix);
                block.instructions.push(id);
            }
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
