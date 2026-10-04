//! Expose bounded scalar snapshots before aggregate promotion.
//! Supplied constructor schemas prove non-null values. Nullable boundaries
//! retain the original copy operation.
use crate::ir::*;
use rustc_hash::FxHashMap;

/// Transfer a fresh primitive array at its last local use.
/// Earlier reads and writes are allowed. Escaping aliases and later uses are not.
/// Require allocation and transfer in the same block to prevent snapshot reuse across loop iterations.
fn reuse_array_snapshots(body: &mut Body, types: &Types) {
    let mut candidates = Vec::new();
    for (index, inst) in body.instructions.iter().enumerate() {
        let Op::CopyValue(source) = inst.op else {
            continue;
        };
        let source = body.resolve(source);
        let ty = body.value_type(source);
        let Some(Type::Array(element)) = types.get(ty) else {
            continue;
        };
        if body.value_type(inst.result.unwrap()) != ty
            || !types
                .get(element)
                .is_some_and(|t| matches!(t, Type::Scalar(_)) && t.carrier() != 5)
        {
            continue;
        }
        let ValueDef::Inst(def) = body.values[source.index()].def else {
            continue;
        };
        if !matches!(
            body.instructions[def.index()].op,
            Op::NewArray(_)
                | Op::CopyValue(_)
                | Op::LoadCopy(_)
                | Op::LoadFieldCopy { .. }
                | Op::LoadStorageFieldCopy { .. }
                | Op::LoadAddressCopy(_)
                | Op::LoadTypedCopy { .. }
        ) {
            continue;
        }
        candidates.push((index, source, def));
    }
    if candidates.is_empty() {
        return;
    }
    let mut positions = vec![None; body.instructions.len()];
    for (block, data) in body.blocks.iter().enumerate() {
        for (position, id) in data.instructions.iter().enumerate() {
            positions[id.index()] = Some((block as u32, position as u32));
        }
    }
    let mut transfers = vec![None; body.values.len()];
    let mut invalid = vec![false; body.values.len()];
    for (index, source, def) in candidates {
        let (Some((owner, _)), Some((block, _))) = (positions[def.index()], positions[index])
        else {
            continue;
        };
        if owner != block {
            continue;
        }
        if transfers[source.index()]
            .replace(InstId::new(index))
            .is_some()
        {
            invalid[source.index()] = true;
        }
    }
    if transfers.iter().all(Option::is_none) {
        return;
    }
    for (index, inst) in body.instructions.iter().enumerate() {
        inst.op.visit_uses(&body.args, |value| {
            let value = body.resolve(value);
            let Some(copy) = transfers[value.index()] else {
                return;
            };
            if copy.index() == index {
                return;
            }
            let local_array = match inst.op {
                Op::ArrayGet { array, .. }
                | Op::ArraySet { array, .. }
                | Op::ArrayFill { array, .. }
                | Op::ArrayLength(array) => body.resolve(array) == value,
                _ => false,
            };
            let before = match (positions[index], positions[copy.index()]) {
                (Some((a, i)), Some((b, j))) => a == b && i < j,
                _ => false,
            };
            invalid[value.index()] |= !local_array || !before;
        });
    }
    let mut escape = |value| {
        invalid[body.resolve(value).index()] = true;
    };
    for block in &body.blocks {
        if let Some(term) = block.terminator {
            term.visit_uses(&mut escape);
        }
    }
    for edge in &body.edges {
        for &value in &edge.args {
            escape(value);
        }
    }
    for (source, copy) in transfers.into_iter().enumerate() {
        if let Some(copy) = copy
            && !invalid[source]
        {
            body.instructions[copy.index()].op = Op::Reinterpret(ValueId::new(source));
        }
    }
}

pub(super) fn expand_flat_copies(
    body: &mut Body,
    types: &Types,
    layout: &mut impl FnMut(&MethodRef) -> Option<Vec<FieldRef>>,
) {
    if !body
        .instructions
        .iter()
        .any(|i| matches!(i.op, Op::CopyValue(_)))
    {
        return;
    }
    reuse_array_snapshots(body, types);
    let mut schemas = FxHashMap::<TypeId, (MethodId, Vec<FieldRef>)>::default();
    for inst in &body.instructions {
        let Op::Call {
            method,
            kind: CallKind::Constructor,
            ..
        } = inst.op
        else {
            continue;
        };
        let ty = body.value_type(inst.result.unwrap());
        if schemas.contains_key(&ty) {
            continue;
        }
        let method_ref = &body.methods[method.index()];
        let Some(fields) = layout(method_ref) else {
            continue;
        };
        if fields.len() > 8
            || fields.len() != method_ref.params.len()
            || !fields.iter().zip(&method_ref.params).all(|(field, &ty)| {
                field.ty == ty && !field.is_static && matches!(types.get(ty), Some(Type::Scalar(_)))
            })
        {
            continue;
        }
        schemas.insert(ty, (method, fields));
    }
    if schemas.is_empty() {
        return;
    }
    reuse_scalar_snapshots(body, types, &schemas);
    // Join non-null facts across all incoming values.
    // Each value changes from unseen to non-null to unknown at most once.
    let count = body.values.len();
    let mut facts = vec![2u8; count];
    let mut users = crate::analysis::ValueUsers::new(count);
    let predecessors = body.predecessors();
    for (index, value) in body.values.iter().enumerate() {
        if !schemas.contains_key(&value.ty) {
            continue;
        }
        match value.def {
            ValueDef::Inst(id) => match body.instructions[id.index()].op {
                Op::Call {
                    kind: CallKind::Constructor,
                    ..
                } => facts[index] = 1,
                Op::CopyValue(source) | Op::Reinterpret(source) | Op::Refine(source)
                    if body.value_type(source) == value.ty =>
                {
                    facts[index] = 0;
                    users.connect(source, index);
                }
                _ => {}
            },
            ValueDef::Alias(source) => {
                facts[index] = 0;
                users.connect(source, index);
            }
            ValueDef::Param(block)
                if block != body.entry && !predecessors[block.index()].is_empty() =>
            {
                facts[index] = 0;
                let position = body.blocks[block.index()]
                    .params
                    .iter()
                    .position(|v| v.index() == index)
                    .unwrap();
                for &(_, edge) in &predecessors[block.index()] {
                    users.connect(body.edges[edge.index()].args[position], index);
                }
            }
            _ => {}
        }
    }
    let mut pending = (0..count).filter(|&i| facts[i] != 0).collect::<Vec<_>>();
    for phase in 0..2 {
        while let Some(source) = pending.pop() {
            for target in users.users(source) {
                let next = facts[source].max(facts[target]);
                if next != facts[target] {
                    facts[target] = next;
                    pending.push(target);
                }
            }
        }
        if phase == 0 {
            for (i, fact) in facts.iter_mut().enumerate() {
                if *fact == 0 {
                    *fact = 2;
                    pending.push(i);
                }
            }
        }
    }
    let mut prefixes = FxHashMap::<InstId, Vec<InstId>>::default();
    for index in 0..body.instructions.len() {
        let Op::CopyValue(source) = body.instructions[index].op else {
            continue;
        };
        if facts[source.index()] != 1 {
            continue;
        }
        let Some((method, fields)) = schemas.get(&body.value_type(source)) else {
            continue;
        };
        let mut prefix = Vec::with_capacity(fields.len());
        let args = fields
            .iter()
            .map(|field| {
                let member = body
                    .fields
                    .iter()
                    .position(|f| f == field)
                    .map(MemberId::new)
                    .unwrap_or_else(|| {
                        let id = MemberId::new(body.fields.len());
                        body.fields.push(field.clone());
                        id
                    });
                let inst = InstId::new(body.instructions.len());
                let value = ValueId::new(body.values.len());
                body.values.push(Value {
                    ty: field.ty,
                    def: ValueDef::Inst(inst),
                });
                body.instructions.push(Inst {
                    op: Op::GetField {
                        object: source,
                        field: member,
                    },
                    result: Some(value),
                });
                prefix.push(inst);
                value
            })
            .collect::<Vec<_>>();
        body.instructions[index].op = Op::Call {
            method: *method,
            kind: CallKind::Constructor,
            args: List::append(&mut body.args, args),
        };
        prefixes.insert(InstId::new(index), prefix);
    }
    for block in &mut body.blocks {
        let previous = std::mem::take(&mut block.instructions);
        for id in previous {
            if let Some(prefix) = prefixes.remove(&id) {
                block.instructions.extend(prefix);
            }
            block.instructions.push(id);
        }
        if let Some(Terminator::Invoke { inst, .. }) = block.terminator {
            if let Some(prefix) = prefixes.remove(&inst) {
                block.instructions.extend(prefix);
            }
        }
    }
}

// Scalar field reads can use the source while no intervening operation can change it.
fn reuse_scalar_snapshots(
    body: &mut Body,
    types: &Types,
    schemas: &FxHashMap<TypeId, (MethodId, Vec<FieldRef>)>,
) {
    let mut positions = vec![None; body.instructions.len()];
    for (block, data) in body.blocks.iter().enumerate() {
        let mut epoch = 0;
        for &id in &data.instructions {
            positions[id.index()] = Some((block, epoch));
            let inst = body.instructions[id.index()];
            let read_only = match inst.op {
                Op::CopyValue(source) => schemas.contains_key(&body.value_type(source)),
                Op::GetField { field, .. } => schemas
                    .get(&body.fields[field.index()].owner)
                    .is_some_and(|(_, fields)| fields.contains(&body.fields[field.index()])),
                _ => !inst.op.may_throw(body, types),
            };
            if !read_only {
                epoch += 1;
            }
        }
    }
    let mut candidates = vec![None; body.values.len()];
    for (index, inst) in body.instructions.iter().enumerate() {
        if let Op::CopyValue(source) = inst.op {
            if schemas.contains_key(&body.value_type(source)) && positions[index].is_some() {
                candidates[inst.result.unwrap().index()] = Some(InstId::new(index));
            }
        }
    }
    let mut valid = vec![true; body.values.len()];
    for (index, inst) in body.instructions.iter().enumerate() {
        inst.op.visit_uses(&body.args, |value| {
            let value = body.resolve(value);
            if let Some(copy) = candidates[value.index()] {
                valid[value.index()] &= positions[index] == positions[copy.index()]
                    && matches!(inst.op, Op::GetField { object, field } if body.resolve(object) == value
                        && schemas.get(&body.value_type(value)).is_some_and(|(_, fields)| fields.contains(&body.fields[field.index()])));
            }
        });
    }
    for block in &body.blocks {
        block
            .terminator
            .unwrap()
            .visit_uses(|value| valid[body.resolve(value).index()] = false);
    }
    for edge in &body.edges {
        for &value in &edge.args {
            valid[body.resolve(value).index()] = false;
        }
    }
    for (value, copy) in candidates.into_iter().enumerate() {
        if valid[value] {
            if let Some(copy) = copy {
                let Op::CopyValue(source) = body.instructions[copy.index()].op else {
                    unreachable!()
                };
                body.instructions[copy.index()].op = Op::Reinterpret(source);
            }
        }
    }
}
