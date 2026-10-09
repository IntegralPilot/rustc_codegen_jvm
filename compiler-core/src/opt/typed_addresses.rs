//! Use an exact source-language view layout without allocating a cast address.
//! Facts are attached to operations, never inferred from shared JVM classes.
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};
use rustc_hash::FxHashMap;

fn append(body: &mut Body, prefix: &mut Vec<InstId>, op: Op, ty: TypeId) -> ValueId {
    let (id, value) = super::append_value(body, op, ty);
    prefix.push(id);
    value
}
fn literal(body: &mut Body, prefix: &mut Vec<InstId>, ty: TypeId, n: u32) -> ValueId {
    let id = ConstId::new(body.constants.len());
    body.constants.push(Constant::Scalar(
        Scalar::integer(ScalarType::I64, n.into()).unwrap(),
    ));
    append(body, prefix, Op::Constant(id), ty)
}

pub fn lower_typed_addresses(body: &mut Body, types: &mut Types, debug: Option<&mut DebugInfo>) {
    if !body.instructions.iter().any(|i| {
        matches!(
            i.op,
            Op::RetypeAddress { size: 1.., .. } | Op::ViewAddress { size: 1.., .. }
        )
    }) && !body
        .values
        .iter()
        .any(|v| types.address_layout(v.ty).is_some())
    {
        return;
    }
    // Join exact layouts across aliases and CFG edges. Each value starts unseen,
    // gains one layout, can lose its root guarantee, then becomes unknown.
    // Process backedges without whole-body scans or origin sets.
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Fact {
        Unseen,
        // A rooted value retains the layout in its root.
        // The component ABI can pass it without an exact type annotation.
        Known {
            size: u32,
            codec: Option<SymbolId>,
            rooted: bool,
        },
        Unknown,
    }
    let count = body.values.len();
    let predecessors = body.predecessors();
    let mut facts = vec![Fact::Unknown; count];
    let mut users = crate::analysis::ValueUsers::new(count);
    for (index, value) in body.values.iter().enumerate() {
        if !matches!(types.get(value.ty), Some(Type::Pointer(_))) {
            continue;
        }
        match value.def {
            ValueDef::Inst(id) => match body.instructions[id.index()].op {
                Op::AddressPack(_) if types.address_layout(value.ty).is_some() => {
                    let (size, codec) = types.address_layout(value.ty).unwrap();
                    facts[index] = Fact::Known {
                        size,
                        codec,
                        rooted: false,
                    };
                }
                Op::RetypeAddress {
                    pointer,
                    size: size @ 1..,
                    codec,
                } => {
                    facts[index] = Fact::Known {
                        size,
                        codec,
                        rooted: true,
                    };
                    users.connect(pointer, index);
                }
                Op::ViewAddress {
                    size: size @ 1..,
                    codec,
                    ..
                } if codec.is_some()
                    || types
                        .pointee(value.ty)
                        .and_then(|ty| StorageSlot::scalar(ty, types))
                        .is_some_and(|slot| slot.size == size) =>
                {
                    facts[index] = Fact::Known {
                        size,
                        codec,
                        rooted: true,
                    };
                }
                Op::Offset {
                    pointer: source, ..
                }
                | Op::Refine(source)
                | Op::Reinterpret(source) => {
                    facts[index] = Fact::Unseen;
                    users.connect(source, index);
                }
                _ => {}
            },
            ValueDef::Alias(source) => {
                facts[index] = Fact::Unseen;
                users.connect(source, index);
            }
            ValueDef::Param(block)
                if block != body.entry && !predecessors[block.index()].is_empty() =>
            {
                facts[index] = Fact::Unseen;
                let position = body.blocks[block.index()]
                    .params
                    .iter()
                    .position(|p| p.index() == index)
                    .unwrap();
                for &(_, edge) in &predecessors[block.index()] {
                    users.connect(body.edges[edge.index()].args[position], index);
                }
            }
            _ => {}
        }
    }
    let mut pending = (0..count)
        .filter(|&i| facts[i] != Fact::Unseen)
        .collect::<Vec<_>>();
    for phase in 0..2 {
        while let Some(source) = pending.pop() {
            for target in users.users(source) {
                let previous = facts[target];
                let incoming = facts[source];
                let retype = match body.values[target].def {
                    ValueDef::Inst(id) => {
                        matches!(body.instructions[id.index()].op, Op::RetypeAddress { .. })
                    }
                    _ => false,
                };
                let merged = match (previous, incoming) {
                    (
                        Fact::Known {
                            size,
                            codec,
                            rooted,
                        },
                        fact,
                    ) if retype => Fact::Known {
                        size,
                        codec,
                        rooted: rooted
                            && matches!(fact, Fact::Known { size: s, codec: c, rooted: true } if s == size && c == codec),
                    },
                    (Fact::Unseen, fact) => fact,
                    (
                        Fact::Known {
                            size: a,
                            codec: ac,
                            rooted: ar,
                        },
                        Fact::Known {
                            size: b,
                            codec: bc,
                            rooted: br,
                        },
                    ) if a == b && ac == bc => Fact::Known {
                        size: a,
                        codec: ac,
                        rooted: ar && br,
                    },
                    _ => Fact::Unknown,
                };
                if previous != merged {
                    facts[target] = merged;
                    pending.push(target);
                }
            }
        }
        if phase == 0 {
            for (index, fact) in facts.iter_mut().enumerate() {
                if *fact == Fact::Unseen {
                    *fact = Fact::Unknown;
                    pending.push(index);
                }
            }
        }
    }
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let long = types.scalar(ScalarType::I64);
    let mut components = vec![None::<[ValueId; 2]>; count];
    let mut roots = Vec::new();
    let mut joins = vec![Vec::new(); body.blocks.len()];
    for index in 0..count {
        if !matches!(facts[index], Fact::Known { .. }) {
            continue;
        }
        match body.values[index].def {
            ValueDef::Param(block) if block != body.entry => {
                let position = body.blocks[block.index()]
                    .params
                    .iter()
                    .position(|p| p.index() == index)
                    .unwrap();
                components[index] = Some([object, long].map(|ty| {
                    let value = ValueId::new(body.values.len());
                    body.values.push(Value {
                        ty,
                        def: ValueDef::Param(block),
                    });
                    body.blocks[block.index()].params.push(value);
                    value
                }));
                joins[block.index()].push((index, position));
            }
            ValueDef::Inst(id)
                if matches!(body.instructions[id.index()].op, Op::AddressPack(_)) =>
            {
                let Op::AddressPack(parts) = body.instructions[id.index()].op else {
                    unreachable!()
                };
                components[index] = Some(body.args[parts.range()].try_into().unwrap());
            }
            ValueDef::Inst(id)
                if matches!(
                    body.instructions[id.index()].op,
                    Op::RetypeAddress { .. } | Op::Offset { .. } | Op::ViewAddress { .. }
                ) =>
            {
                let mut pending = Vec::new();
                components[index] =
                    Some([object, long].map(|ty| append(body, &mut pending, Op::Nop, ty)));
                roots.push((id, pending));
            }
            _ => {}
        }
    }
    let eligible = facts
        .iter()
        .map(|f| matches!(f, Fact::Known { .. }))
        .collect::<Vec<_>>();
    users.propagate(&eligible, &mut components);
    let mut prefixes = FxHashMap::default();
    for (id, created) in roots {
        let original = body.instructions[id.index()];
        let result = original.result.unwrap();
        let Fact::Known {
            size,
            codec,
            rooted,
        } = facts[result.index()]
        else {
            unreachable!();
        };
        let mut prefix = Vec::new();
        let source_parts = if let Op::ViewAddress { parts, .. } = original.op {
            let values = &body.args[parts.range()];
            let (backing, start) = (values[0], values[1]);
            let root = append(
                body,
                &mut prefix,
                Op::ViewRoot {
                    backing,
                    size,
                    codec,
                },
                object,
            );
            let start = append(body, &mut prefix, Op::Cast(start), long);
            let stride = literal(body, &mut prefix, long, size);
            let offset = append(
                body,
                &mut prefix,
                Op::Binary {
                    op: BinaryOp::Mul,
                    left: start,
                    right: stride,
                },
                long,
            );
            [root, offset]
        } else {
            let source = match original.op {
                Op::RetypeAddress { pointer, .. } | Op::Offset { pointer, .. } => pointer,
                Op::Refine(pointer) | Op::Reinterpret(pointer) => pointer,
                _ => unreachable!(),
            };
            // Repeated retypes can carry ZST or source-layout metadata.
            // Keep the boundary unless the exact layouts match.
            let source = body.resolve(source);
            let source_parts = if matches!(facts.get(source.index()),
            Some(&Fact::Known { size: s, codec: c, .. }) if s == size && c == codec)
            {
                components[source.index()]
            } else {
                None
            };
            source_parts
                .or_else(|| {
                    let parts = super::address_parts::address_parts(body, types, source)?;
                    let ValueDef::Inst(inst) = body.values[parts.value.index()].def else {
                        unreachable!()
                    };
                    matches!(body.instructions[inst.index()].op, Op::AddressPack(_))
                        .then(|| body.args[parts.parts.range()].try_into().unwrap())
                })
                .unwrap_or_else(|| {
                    [
                        append(body, &mut prefix, Op::Reinterpret(source), object),
                        literal(body, &mut prefix, long, 0),
                    ]
                })
        };
        let [root, mut offset] = source_parts;
        if let Op::Offset {
            offset: delta,
            bytes,
            ..
        } = original.op
        {
            let mut delta = append(body, &mut prefix, Op::Cast(delta), long);
            if !bytes {
                let stride = literal(body, &mut prefix, long, size);
                delta = append(
                    body,
                    &mut prefix,
                    Op::Binary {
                        op: BinaryOp::Mul,
                        left: delta,
                        right: stride,
                    },
                    long,
                );
            }
            offset = append(
                body,
                &mut prefix,
                Op::Binary {
                    op: BinaryOp::Add,
                    left: offset,
                    right: delta,
                },
                long,
            );
        }
        body.instructions[created[0].index()].op = Op::Reinterpret(root);
        body.instructions[created[1].index()].op = Op::Reinterpret(offset);
        prefix.extend(created);
        let parts = List::append(&mut body.args, components[result.index()].unwrap());
        body.instructions[id.index()].op =
            if rooted || types.address_layout(body.value_type(result)) == Some((size, codec)) {
                Op::AddressPack(parts)
            } else {
                Op::TypedAddressPack { parts, size, codec }
            };
        prefixes.insert(id, prefix);
    }
    for (block, incoming) in predecessors.iter().enumerate() {
        for &(_, edge) in incoming {
            for &(_, position) in &joins[block] {
                let input = body.edges[edge.index()].args[position];
                body.edges[edge.index()]
                    .args
                    .extend(components[input.index()].unwrap());
            }
        }
    }
    for index in 0..body.instructions.len() {
        let op = body.instructions[index].op;
        if let Op::AddressViewPart {
            address,
            index: part,
        } = op
            && let Some(&Fact::Known { size, codec, .. }) = facts.get(address.index())
            && let Some(source) = components.get(address.index()).copied().flatten()
        {
            // Use ordinary scalar normalization for scalar components.
            // It can retain a primitive backing array without a carrier.
            if codec.is_none()
                && ComponentShape::of(types, body.value_type(address))
                    == Some(ComponentShape::Address)
            {
                continue;
            }
            body.instructions[index].op = Op::TypedAddressViewPart {
                parts: List::append(&mut body.args, source),
                size,
                codec,
                index: part,
            };
            continue;
        }
        if let Op::AddressPart {
            address,
            index: part,
        } = op
            && (types.address_layout(body.value_type(address)).is_some()
                || matches!(
                    facts.get(address.index()),
                    Some(Fact::Known { rooted: true, .. })
                ))
            && let Some(parts) = components.get(address.index()).copied().flatten()
        {
            body.instructions[index].op = Op::Reinterpret(parts[part as usize]);
            continue;
        }
        let pointer = match op {
            Op::LoadCopy(pointer) => pointer,
            Op::Store { pointer, value }
                if types
                    .get(body.value_type(value))
                    .is_some_and(|t| t.carrier() == 5) =>
            {
                pointer
            }
            _ => continue,
        };
        if let Some(&Fact::Known { size, codec, .. }) = facts.get(pointer.index())
            && let Some(parts) = components[pointer.index()]
        {
            body.instructions[index].op = match op {
                Op::LoadCopy(_) => Op::LoadTypedCopy {
                    parts: List::append(&mut body.args, parts),
                    size,
                    codec,
                },
                Op::Store { value, .. } => Op::StoreTyped {
                    parts: List::append(&mut body.args, [parts[0], parts[1], value]),
                    size,
                    codec,
                },
                _ => unreachable!(),
            };
        }
    }

    let mut positions = debug.as_ref().map(|_| Vec::new());
    for block in &mut body.blocks {
        let previous = std::mem::take(&mut block.instructions);
        let mut mapping = Vec::new();
        for id in previous {
            if positions.is_some() {
                mapping.push(block.instructions.len() as u32);
            }
            if let Some(prefix) = prefixes.remove(&id) {
                block.instructions.extend(prefix);
            }
            block.instructions.push(id);
        }
        if let Some(positions) = &mut positions {
            mapping.push(block.instructions.len() as u32);
            positions.push(mapping);
        }
        if let Some(Terminator::Invoke { inst, normal, .. }) = block.terminator {
            if let Some(prefix) = prefixes.remove(&inst) {
                block.instructions.extend(prefix);
            }
            if matches!(
                body.instructions[inst.index()].op,
                Op::TypedAddressPack { .. } | Op::AddressPack(_)
            ) {
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
