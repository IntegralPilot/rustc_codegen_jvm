//! Promote exact typed field projections to direct JVM field accesses.
use crate::ir::*;

fn projection(body: &Body, mut value: ValueId) -> Option<(ValueId, ProjectionId)> {
    // Reinterpretations change only the annotation, never allocation identity.
    // Casts, offsets and nontrivial joins must retain the general pointer path.
    for _ in 0..64 {
        value = body.resolve(value);
        let ValueDef::Inst(id) = body.values[value.index()].def else {
            return None;
        };
        match body.instructions[id.index()].op {
            Op::Project { base, projection } => return Some((base, projection)),
            Op::Reinterpret(source) => value = source,
            _ => return None,
        }
    }
    None
}

/// Recover exact values after construction has resolved source-binding joins.
/// An upcast to the common Object carrier followed by an adaptation back to
/// the original type is an identity, including its pointer provenance.
fn recover_identity(body: &mut Body) {
    for index in 0..body.instructions.len() {
        let inst = body.instructions[index];
        let Op::Adapt(mut source) = inst.op else {
            continue;
        };
        let target = body.value_type(inst.result.unwrap());
        for _ in 0..64 {
            source = body.resolve(source);
            if body.value_type(source) == target {
                body.instructions[index].op = Op::Reinterpret(source);
                break;
            }
            let ValueDef::Inst(def) = body.values[source.index()].def else {
                break;
            };
            let Op::Reinterpret(input) = body.instructions[def.index()].op else {
                break;
            };
            source = input;
        }
    }
}

pub fn promote_fields(body: &mut Body, types: &Types) {
    recover_identity(body);
    for index in 0..body.instructions.len() {
        let inst = body.instructions[index];
        let (pointer, ty) = match inst.op {
            Op::Load(pointer) => (pointer, body.value_type(inst.result.unwrap())),
            Op::Store { pointer, value } => (pointer, body.value_type(value)),
            _ => continue,
        };
        let Some((base, projection)) = projection(body, pointer) else {
            continue;
        };
        let field = &body.fields[body.projections[projection.index()].field.index()];
        if field.ty != ty || !matches!(types.get(ty), Some(Type::Scalar(_))) {
            continue;
        }
        body.instructions[index].op = match inst.op {
            Op::Load(_) => Op::LoadField { base, projection },
            Op::Store { value, .. } => Op::StoreField {
                base,
                projection,
                value,
            },
            _ => unreachable!(),
        };
    }
}
