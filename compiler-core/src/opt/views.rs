//! Decompose local views through SSA joins.
//! Data and length consumers use scalars. Opaque boundaries allocate carriers only when needed.
use super::append_value as append;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};
use rustc_hash::FxHashMap;

pub fn decompose_views(body: &mut Body, types: &mut Types, debug: Option<&mut DebugInfo>) {
    if !body.instructions.iter().any(|i| {
        matches!(
            i.op,
            Op::View { .. } | Op::ViewPack(_) | Op::ViewPart { .. }
        )
    }) {
        return;
    }
    let count = body.values.len();
    let mut eligible = vec![false; count];
    let mut users = crate::analysis::ValueUsers::new(count);
    let predecessors = body.predecessors();
    for index in 0..count {
        let value = ValueId::new(index);
        if !ComponentShape::View.accepts_annotation(types, body.value_type(value)) {
            continue;
        }
        match body.values[index].def {
            ValueDef::Inst(id) => match body.instructions[id.index()].op {
                Op::View { .. } | Op::ViewPack(_) => eligible[index] = true,
                Op::Constant(id) if matches!(body.constants[id.index()], Constant::Null(_)) => {
                    eligible[index] = true;
                }
                Op::Reinterpret(source) | Op::Adapt(source) => {
                    eligible[index] = true;
                    users.connect(source, index);
                }
                _ => {}
            },
            ValueDef::Alias(source) => {
                eligible[index] = true;
                users.connect(source, index);
            }
            ValueDef::Param(block) if block != body.entry => {
                eligible[index] = !predecessors[block.index()].is_empty();
                let position = body.blocks[block.index()]
                    .params
                    .iter()
                    .position(|&v| v == value)
                    .unwrap();
                for &(_, edge) in &predecessors[block.index()] {
                    users.connect(body.edges[edge.index()].args[position], index);
                }
            }
            _ => {}
        }
    }
    // Reject each node at most once, including cycles with unknown inputs.
    // No repeated whole-body scan is needed.
    users.close(&mut eligible);
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let int = types.scalar(ScalarType::I32);
    let length_type = types.scalar(ScalarType::U64);
    let mut components = vec![None::<[ValueId; 3]>; count];
    let mut prefixes = FxHashMap::<InstId, Vec<InstId>>::default();
    let mut joins = vec![Vec::new(); body.blocks.len()];
    for index in 0..count {
        if !eligible[index] {
            continue;
        }
        match body.values[index].def {
            ValueDef::Param(block) => {
                let mut parts = [ValueId::new(0); 3];
                for (part, ty) in parts.iter_mut().zip([object, int, length_type]) {
                    *part = ValueId::new(body.values.len());
                    body.values.push(Value {
                        ty,
                        def: ValueDef::Param(block),
                    });
                    body.blocks[block.index()].params.push(*part);
                }
                let position = body.blocks[block.index()]
                    .params
                    .iter()
                    .position(|&value| value.index() == index)
                    .unwrap();
                joins[block.index()].push((index, position));
                components[index] = Some(parts);
            }
            ValueDef::Inst(id) => {
                // Partially initialized aggregates use null borrowed fields.
                // Read their default component slots without dereferencing a view carrier.
                if matches!(body.instructions[id.index()].op, Op::Constant(_)) {
                    let mut parts = [ValueId::new(0); 3];
                    let mut prefix = Vec::new();
                    for (index, (ty, constant)) in [
                        (object, Constant::Null(object)),
                        (
                            int,
                            Constant::Scalar(Scalar::integer(ScalarType::I32, 0).unwrap()),
                        ),
                        (
                            length_type,
                            Constant::Scalar(Scalar::integer(ScalarType::U64, 0).unwrap()),
                        ),
                    ]
                    .into_iter()
                    .enumerate()
                    {
                        let constant_id = ConstId::new(body.constants.len());
                        body.constants.push(constant);
                        let (instruction, value) = append(body, Op::Constant(constant_id), ty);
                        prefix.push(instruction);
                        parts[index] = value;
                    }
                    prefixes.insert(id, prefix);
                    components[index] = Some(parts);
                }
                if let Op::ViewPack(parts) = body.instructions[id.index()].op {
                    components[index] = Some(body.args[parts.range()].try_into().unwrap());
                }
                if let Op::View { data, length } = body.instructions[id.index()].op {
                    let (cast, data) = append(body, Op::Reinterpret(data), object);
                    let constant = ConstId::new(body.constants.len());
                    body.constants.push(Constant::Scalar(
                        Scalar::integer(ScalarType::I32, 0).unwrap(),
                    ));
                    let (zero, start) = append(body, Op::Constant(constant), int);
                    prefixes.insert(id, vec![cast, zero]);
                    components[index] = Some([data, start, length]);
                }
            }
            _ => {}
        }
    }
    users.propagate(&eligible, &mut components);
    for block in 0..body.blocks.len() {
        // Visit parameters in the same order used when extending their target.
        for &(_, edge) in &predecessors[block] {
            for &(_, position) in &joins[block] {
                let input = body.edges[edge.index()].args[position];
                body.edges[edge.index()]
                    .args
                    .extend(components[input.index()].unwrap());
            }
        }
    }
    for inst in &mut body.instructions {
        inst.op = match inst.op {
            Op::Adapt(source)
                if inst
                    .result
                    .and_then(|v| components.get(v.index()).copied().flatten())
                    .is_some() =>
            {
                let result = inst.result.unwrap();
                if matches!(
                    types.get(body.values[result.index()].ty),
                    Some(Type::Slice(_) | Type::Str)
                ) {
                    Op::ViewPack(List::append(
                        &mut body.args,
                        components[result.index()].unwrap(),
                    ))
                } else {
                    Op::Reinterpret(source)
                }
            }
            Op::Length(view) if components.get(view.index()).is_some_and(Option::is_some) => {
                Op::Reinterpret(components[view.index()].unwrap()[2])
            }
            Op::ArrayLength(view) if components.get(view.index()).is_some_and(Option::is_some) => {
                // SliceView.length contains the low 32 bits of the Rust length.
                // Extract them without a view carrier.
                Op::Cast(components[view.index()].unwrap()[2])
            }
            Op::ViewPart { view, index }
                if components.get(view.index()).is_some_and(Option::is_some) =>
            {
                Op::Reinterpret(components[view.index()].unwrap()[index as usize])
            }
            Op::ViewData { view, size, codec }
                if components.get(view.index()).is_some_and(Option::is_some) =>
            {
                let parts = List::append(&mut body.args, components[view.index()].unwrap());
                Op::ViewAddress { parts, size, codec }
            }
            Op::ArrayGet {
                array,
                index,
                native: false,
            }
            | Op::ArrayGetCopy { array, index }
                if components.get(array.index()).is_some_and(Option::is_some) =>
            {
                let [root, start, _] = components[array.index()].unwrap();
                let parts = List::append(&mut body.args, [root, start, index]);
                if matches!(inst.op, Op::ArrayGetCopy { .. }) {
                    Op::ViewGetCopy(parts)
                } else {
                    Op::ViewGet(parts)
                }
            }
            Op::ArraySet {
                array,
                index,
                value,
                native: false,
            } if components.get(array.index()).is_some_and(Option::is_some) => {
                let [root, start, _] = components[array.index()].unwrap();
                Op::ViewSet {
                    parts: List::append(&mut body.args, [root, start, index]),
                    value,
                }
            }
            op => op,
        };
    }
    let mut positions = debug
        .as_ref()
        .map(|_| Vec::with_capacity(body.blocks.len()));
    for block in &mut body.blocks {
        let mut mapping = positions
            .as_ref()
            .map(|_| Vec::with_capacity(block.instructions.len() + 1));
        let previous = std::mem::take(&mut block.instructions);
        for id in previous {
            if let Some(mapping) = &mut mapping {
                mapping.push(block.instructions.len() as u32);
            }
            if let Some(prefix) = prefixes.remove(&id) {
                block.instructions.extend(prefix);
            }
            block.instructions.push(id);
        }
        if let Some(mapping) = &mut mapping {
            mapping.push(block.instructions.len() as u32);
        }
        if let (Some(positions), Some(mapping)) = (&mut positions, mapping) {
            positions.push(mapping);
        }
        if let Some(Terminator::Invoke { inst, normal, .. }) = block.terminator {
            if let Some(prefix) = prefixes.remove(&inst) {
                block.instructions.extend(prefix);
            }
            let op = body.instructions[inst.index()].op;
            if matches!(op, Op::View { .. } | Op::ViewPack(_) | Op::Reinterpret(_)) {
                block.instructions.push(inst);
                block.terminator = Some(Terminator::Jump(normal));
            }
        }
    }
    if let (Some(debug), Some(positions)) = (debug, positions) {
        for event in &mut debug.events {
            event.position = positions[event.block.index()][event.position as usize];
        }
    }
}
