//! Return borrowed roots directly. Write scalar metadata into caller-owned scratch.
//! Each frame reuses one scratch array and captures successful results into SSA.
//! Other frames and Rust code cannot access this scratch.
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};
use rustc_hash::FxHashMap;

fn emit(body: &mut Body, op: Op, ty: Option<TypeId>, into: &mut Vec<InstId>) -> Option<ValueId> {
    let inst = InstId::new(body.instructions.len());
    let result = ty.map(|ty| {
        let value = ValueId::new(body.values.len());
        body.values.push(Value {
            ty,
            def: ValueDef::Inst(inst),
        });
        value
    });
    body.instructions.push(Inst { op, result });
    into.push(inst);
    result
}
fn integer(body: &mut Body, ty: TypeId, bits: u128, into: &mut Vec<InstId>) -> ValueId {
    let constant = ConstId::new(body.constants.len());
    body.constants.push(Constant::Scalar(
        Scalar::integer(ScalarType::I32, bits).unwrap(),
    ));
    emit(body, Op::Constant(constant), Some(ty), into).unwrap()
}

/// Bypass scratch copies only for the last call that writes metadata.
/// Aliases do not prove this condition because a later call can overwrite scratch.
fn forwards_last_call(
    body: &Body,
    predecessors: &[Vec<(BlockId, EdgeId)>],
    mut block: usize,
    wanted: InstId,
    first: &[InstId],
    calls: &FxHashMap<InstId, ()>,
) -> bool {
    let mut budget = 64usize;
    for step in 0..16 {
        let current = &body.blocks[block];
        let invoke = match current.terminator {
            Some(Terminator::Invoke { inst, .. }) => Some(inst),
            _ => None,
        };
        let instructions = if step == 0 {
            first
        } else {
            &current.instructions
        };
        for inst in invoke.into_iter().chain(instructions.iter().rev().copied()) {
            if inst == wanted {
                return true;
            }
            if calls.contains_key(&inst) || budget == 0 {
                return false;
            }
            budget -= 1;
        }
        let [incoming] = predecessors[block].as_slice() else {
            return false;
        };
        block = incoming.0.index();
    }
    false
}

pub fn lower_component_returns(
    body: &mut Body,
    types: &mut Types,
    entry: bool,
    select: impl Fn(&MethodRef) -> bool,
    debug: Option<&mut DebugInfo>,
) {
    let entry = entry
        .then(|| ComponentShape::of(types, body.return_type))
        .flatten();
    let methods = body
        .methods
        .iter()
        .map(|method| {
            (select(method)
                && super::component_argument_slots(types, method.params.iter().copied()) < 254)
                .then(|| ComponentShape::of(types, method.returns))
                .flatten()
        })
        .collect::<Vec<_>>();
    let has_calls = body.instructions.iter().any(|inst| {
        matches!(inst.op,
        Op::Call { method, .. } if methods[method.index()].is_some())
    });
    if entry.is_none() && methods.iter().all(Option::is_none) {
        return;
    }

    let long = types.scalar(ScalarType::I64);
    let int = types.scalar(ScalarType::I32);
    let metadata = types.intern(Type::Array(long));
    if entry.is_none() && !has_calls {
        // Function addresses and handles use the same physical descriptor,
        // including targets that this body never calls directly.
        for (method, shape) in body.methods.iter_mut().zip(methods) {
            if let Some(shape) = shape {
                method.params.push(metadata);
                method.returns = shape.parts(types).next().unwrap();
            }
        }
        return;
    }

    let mut prologue = Vec::new();
    let scratch = if entry.is_some() {
        let value = ValueId::new(body.values.len());
        body.values.push(Value {
            ty: metadata,
            def: ValueDef::Param(body.entry),
        });
        body.blocks[body.entry.index()].params.push(value);
        body.return_type = entry.unwrap().parts(types).next().unwrap();
        value
    } else {
        let length = integer(body, int, 2, &mut prologue);
        emit(body, Op::NewArray(length), Some(metadata), &mut prologue).unwrap()
    };
    let indices = [0, 1].map(|index| integer(body, int, index, &mut prologue));
    let count = body.instructions.len();
    let predecessors = body.predecessors();
    let mut call_results = FxHashMap::default();
    let mut calls = FxHashMap::default();
    let mut suffixes = FxHashMap::default();
    for index in 0..count {
        let Inst {
            op: Op::Call { method, kind, args },
            result,
        } = body.instructions[index]
        else {
            continue;
        };
        let Some(shape) = methods[method.index()] else {
            continue;
        };
        calls.insert(InstId::new(index), ());
        let mut arguments = body.args[args.range()].to_vec();
        arguments.push(scratch);
        body.instructions[index].op = Op::Call {
            method,
            kind,
            args: List::append(&mut body.args, arguments),
        };
        if let Some(value) = result {
            let root = ValueId::new(body.values.len());
            body.values.push(Value {
                ty: shape.parts(types).next().unwrap(),
                def: ValueDef::Inst(InstId::new(index)),
            });
            body.instructions[index].result = Some(root);
            call_results.insert(value, (InstId::new(index), root, shape));
            let mut suffix = Vec::new();
            let mut parts = vec![root];
            for (position, ty) in shape.parts(types).skip(1).enumerate() {
                let value = emit(
                    body,
                    Op::ArrayGet {
                        native: true,
                        array: scratch,
                        index: indices[position],
                    },
                    Some(long),
                    &mut suffix,
                )
                .unwrap();
                let value = if ty == long {
                    value
                } else {
                    emit(body, Op::Cast(value), Some(ty), &mut suffix).unwrap()
                };
                parts.push(value);
            }
            let inst = InstId::new(body.instructions.len());
            body.values[value.index()].def = ValueDef::Inst(inst);
            body.instructions.push(Inst {
                op: shape.pack(List::append(&mut body.args, parts)),
                result: Some(value),
            });
            suffix.push(inst);
            suffixes.insert(InstId::new(index), suffix);
        }
    }
    for (method, shape) in body.methods.iter_mut().zip(methods) {
        if let Some(shape) = shape {
            method.params.push(metadata);
            method.returns = shape.parts(types).next().unwrap();
        }
    }
    let original_blocks = body.blocks.len();
    let mut positions = debug.as_ref().map(|_| Vec::new());
    for index in 0..original_blocks {
        let previous = std::mem::take(&mut body.blocks[index].instructions);
        let mut instructions = Vec::new();
        if index == body.entry.index() {
            instructions.append(&mut prologue);
        }
        let mut mapping = Vec::new();
        for id in previous {
            mapping.push(instructions.len() as u32);
            instructions.push(id);
            if let Some(suffix) = suffixes.remove(&id) {
                instructions.extend(suffix);
            }
        }
        mapping.push(instructions.len() as u32);
        if let Some(positions) = &mut positions {
            positions.push(mapping);
        }
        match body.blocks[index].terminator {
            Some(Terminator::Return(Some(value))) if entry.is_some() => {
                let shape = entry.unwrap();
                let mut source = body.resolve(value);
                for _ in 0..16 {
                    let ValueDef::Inst(inst) = body.values[source.index()].def else {
                        break;
                    };
                    match body.instructions[inst.index()].op {
                        Op::Reinterpret(next) | Op::Refine(next) => source = body.resolve(next),
                        _ => break,
                    }
                }
                if let Some(&(call, root, returned)) = call_results.get(&source)
                    && shape == returned
                    && forwards_last_call(body, &predecessors, index, call, &instructions, &calls)
                {
                    body.blocks[index].terminator = Some(Terminator::Return(Some(root)));
                    body.blocks[index].instructions = instructions;
                    continue;
                }
                let mut root = None;
                for (position, ty) in shape.parts(types).enumerate() {
                    let part = emit(
                        body,
                        shape.part(value, position as u8),
                        Some(ty),
                        &mut instructions,
                    )
                    .unwrap();
                    if position == 0 {
                        root = Some(part);
                        continue;
                    }
                    let part = if ty == long {
                        part
                    } else {
                        emit(body, Op::Cast(part), Some(long), &mut instructions).unwrap()
                    };
                    emit(
                        body,
                        Op::ArraySet {
                            native: true,
                            array: scratch,
                            index: indices[position - 1],
                            value: part,
                        },
                        None,
                        &mut instructions,
                    );
                }
                body.blocks[index].terminator = Some(Terminator::Return(root));
            }
            Some(Terminator::Invoke {
                inst,
                normal,
                unwind,
            }) => {
                if let Some(suffix) = suffixes.remove(&inst) {
                    // A throw leaves the caller's previous values intact.
                    // Only the successful edge can consume returned metadata.
                    let continuation = BlockId::new(body.blocks.len());
                    body.blocks.push(Block {
                        params: Vec::new(),
                        instructions: suffix,
                        terminator: Some(Terminator::Jump(normal)),
                    });
                    let edge = EdgeId::new(body.edges.len());
                    body.edges.push(Edge {
                        target: continuation,
                        args: Vec::new(),
                    });
                    body.blocks[index].terminator = Some(Terminator::Invoke {
                        inst,
                        normal: edge,
                        unwind,
                    });
                }
            }
            _ => {}
        }
        body.blocks[index].instructions = instructions;
    }
    if let (Some(debug), Some(positions)) = (debug, positions) {
        for event in &mut debug.events {
            event.position = positions[event.block.index()][event.position as usize];
        }
    }
}
