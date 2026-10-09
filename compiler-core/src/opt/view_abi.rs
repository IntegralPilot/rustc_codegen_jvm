//! Flatten borrowed-view arguments before instruction selection.
//! Keep boundary values as virtual packs. SSA liveness determines whether a
//! consumer needs a JVM carrier. Retain only one body's physical operands.
use super::append_value as append;
use crate::ir::*;
use crate::scalar::ScalarType;
use rustc_hash::FxHashMap;

pub fn component_argument_slots(types: &Types, params: impl IntoIterator<Item = TypeId>) -> usize {
    params
        .into_iter()
        .map(|ty| match types.get(ty).unwrap() {
            _ if ComponentShape::of(types, ty).is_some() => {
                ComponentShape::of(types, ty).unwrap().slots()
            }
            Type::Scalar(ScalarType::I64 | ScalarType::U64 | ScalarType::F64) => 2,
            Type::Unit | Type::Opaque(_) | Type::Layout(_) => 0,
            _ => 1,
        })
        .sum()
}

pub fn lower_component_arguments(
    body: &mut Body,
    types: &mut Types,
    entry: bool,
    select: impl Fn(&MethodRef) -> bool,
    debug: Option<&mut DebugInfo>,
) {
    let shape = |ty| ComponentShape::of(types, ty);
    let methods = body
        .methods
        .iter()
        .map(|method| {
            (select(method)
                && component_argument_slots(types, method.params.iter().copied())
                    + usize::from(ComponentShape::of(types, method.returns).is_some())
                    <= 254)
                .then(|| {
                    method
                        .params
                        .iter()
                        .map(|&ty| shape(ty))
                        .collect::<Vec<_>>()
                })
                .filter(|p| p.iter().any(Option::is_some))
        })
        .collect::<Vec<_>>();
    let entry = entry
        && body.blocks[body.entry.index()]
            .params
            .iter()
            .any(|&p| shape(body.value_type(p)).is_some());
    if !entry && methods.iter().all(Option::is_none) {
        return;
    }
    let mut prologue = Vec::new();
    if entry {
        let params = std::mem::take(&mut body.blocks[body.entry.index()].params);
        for value in params {
            let Some(shape) = ComponentShape::of(types, body.value_type(value)) else {
                body.blocks[body.entry.index()].params.push(value);
                continue;
            };
            let args = shape
                .parts(types)
                .into_iter()
                .map(|ty| {
                    let value = ValueId::new(body.values.len());
                    body.values.push(Value {
                        ty,
                        def: ValueDef::Param(body.entry),
                    });
                    body.blocks[body.entry.index()].params.push(value);
                    value
                })
                .collect::<Vec<_>>();
            let id = InstId::new(body.instructions.len());
            body.values[value.index()].def = ValueDef::Inst(id);
            let args = List::append(&mut body.args, args);
            body.instructions.push(Inst {
                op: shape.pack(args),
                result: Some(value),
            });
            prologue.push(id);
        }
    }
    let mut prefixes = FxHashMap::<InstId, Vec<InstId>>::default();
    let count = body.instructions.len();
    for index in 0..count {
        let Op::Call { method, kind, args } = body.instructions[index].op else {
            continue;
        };
        let Some(expand) = &methods[method.index()] else {
            continue;
        };
        let receiver = usize::from(matches!(kind, CallKind::Virtual | CallKind::Interface));
        let original = body.args[args.range()].to_vec();
        let mut arguments = original[..receiver].to_vec();
        let mut prefix = Vec::new();
        for (&value, &expand) in original[receiver..].iter().zip(expand) {
            if let Some(shape) = expand {
                for (index, ty) in shape.parts(types).into_iter().enumerate() {
                    let (inst, value) = append(body, shape.part(value, index as u8), ty);
                    prefix.push(inst);
                    arguments.push(value);
                }
            } else {
                arguments.push(value);
            }
        }
        let args = List::append(&mut body.args, arguments);
        body.instructions[index].op = Op::Call { method, kind, args };
        prefixes.insert(InstId::new(index), prefix);
    }
    for (method, expand) in body.methods.iter_mut().zip(methods) {
        if let Some(expand) = expand {
            let original = std::mem::take(&mut method.params);
            for (ty, expand) in original.into_iter().zip(expand) {
                if let Some(shape) = expand {
                    method.params.extend(shape.parts(types));
                } else {
                    method.params.push(ty);
                }
            }
        }
    }
    let mut positions = debug
        .as_ref()
        .map(|_| Vec::with_capacity(body.blocks.len()));
    for (index, block) in body.blocks.iter_mut().enumerate() {
        let previous = std::mem::take(&mut block.instructions);
        if index == body.entry.index() {
            block.instructions.append(&mut prologue);
        }
        let mut mapping = positions
            .as_ref()
            .map(|_| Vec::with_capacity(previous.len() + 1));
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
        if let Some(Terminator::Invoke { inst, .. }) = block.terminator {
            if let Some(prefix) = prefixes.remove(&inst) {
                block.instructions.extend(prefix);
            }
        }
        if let (Some(positions), Some(mapping)) = (&mut positions, mapping) {
            positions.push(mapping);
        }
    }
    if let (Some(debug), Some(positions)) = (debug, positions) {
        for event in &mut debug.events {
            event.position = positions[event.block.index()][event.position as usize];
        }
    }
}
