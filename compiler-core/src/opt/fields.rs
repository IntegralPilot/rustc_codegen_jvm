//! Promote exact typed field projections to direct JVM field accesses.
use crate::ir::*;

fn projection_of(
    body: &Body,
    types: &Types,
    mut value: ValueId,
) -> Option<(ValueId, ProjectionId)> {
    // Annotations and exact-layout retypes preserve field storage identity.
    // Casts, offsets and nontrivial joins require the general pointer path.
    let mut layout: Option<(u32, Option<SymbolId>)> = None;
    for _ in 0..64 {
        value = body.resolve(value);
        let ValueDef::Inst(id) = body.values[value.index()].def else {
            return None;
        };
        match body.instructions[id.index()].op {
            Op::Project { base, projection } => {
                let field = &body.projections[projection.index()];
                return layout
                    .is_none_or(|(size, codec)| {
                        field.size == u64::from(size)
                            && field.codec.as_deref()
                                == codec.and_then(|codec| types.symbol_name(codec))
                    })
                    .then_some((base, projection));
            }
            Op::RetypeAddress {
                pointer,
                size: size @ 1..,
                codec,
            } if layout.is_none_or(|previous| previous == (size, codec)) => {
                layout = Some((size, codec));
                value = pointer;
            }
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
            Op::Load(pointer) | Op::LoadCopy(pointer) => {
                (pointer, body.value_type(inst.result.unwrap()))
            }
            Op::Store { pointer, value } => (pointer, body.value_type(value)),
            _ => continue,
        };
        let Some((base, projection)) = projection_of(body, types, pointer) else {
            continue;
        };
        let field = &body.fields[body.projections[projection.index()].field.index()];
        if matches!(inst.op, Op::LoadCopy(_)) {
            if field.ty == ty && matches!(types.get(ty), Some(Type::Class(_) | Type::Array(_))) {
                body.instructions[index].op = Op::LoadFieldCopy { base, projection };
            }
            continue;
        }
        if let Op::Store { value, .. } = inst.op
            && field.ty == ty
            && matches!(types.get(ty), Some(Type::Class(_) | Type::Array(_)))
        {
            body.instructions[index].op = Op::StoreField {
                base,
                projection,
                value,
            };
            continue;
        }
        // Pointer values are immutable carriers.
        // Read and replace them through the same checked storage path.
        if field.ty != ty
            || !matches!(
                types.get(ty),
                Some(Type::Scalar(_) | Type::Pointer(_) | Type::Slice(_) | Type::Str)
            )
        {
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

/// Follow managed field chains without materializing their intermediate addresses.
pub fn fold_field_paths(body: &mut Body, types: &Types) {
    for index in 0..body.instructions.len() {
        let (mut base, projection) = match body.instructions[index].op {
            Op::LoadField { base, projection }
            | Op::LoadFieldCopy { base, projection }
            | Op::LoadFieldPart {
                base, projection, ..
            } => (base, projection),
            _ => continue,
        };
        let mut path = vec![projection];
        for _ in 0..32 {
            let Some((parent, projection)) = projection_of(body, types, base) else {
                break;
            };
            let member = &body.fields[body.projections[projection.index()].field.index()];
            let child = &body.fields[body.projections[path[path.len() - 1].index()].field.index()];
            if member.ty != child.owner
                || types.pointee(body.value_type(parent)) != Some(member.owner)
            {
                break;
            }
            path.push(projection);
            base = parent;
        }
        if path.len() < 2 {
            continue;
        }
        let mut parent = path.pop();
        for id in path.into_iter().rev() {
            let mut field = body.projections[id.index()].clone();
            field.parent = parent;
            parent = Some(ProjectionId::new(body.projections.len()));
            body.projections.push(field);
        }
        let projection = parent.unwrap();
        body.instructions[index].op = match body.instructions[index].op {
            Op::LoadField { .. } => Op::LoadField { base, projection },
            Op::LoadFieldCopy { .. } => Op::LoadFieldCopy { base, projection },
            Op::LoadFieldPart { index, .. } => Op::LoadFieldPart {
                base,
                projection,
                index,
            },
            _ => unreachable!(),
        };
    }
}
