//! Keep scalar addresses as a storage root and byte displacement through loops
//! and calls. Unknown roots retain their full runtime provenance as one object.
use super::append_value as append;
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};
use rustc_hash::{FxHashMap, FxHashSet};

fn literal(body: &mut Body, ty: TypeId, bits: i64) -> (InstId, ValueId) {
    let id = ConstId::new(body.constants.len());
    body.constants.push(Constant::Scalar(
        Scalar::integer(ScalarType::I64, bits as u128).unwrap(),
    ));
    append(body, Op::Constant(id), ty)
}
fn parts(
    body: &mut Body,
    types: &Types,
    value: ValueId,
    known: &[Option<[ValueId; 2]>],
    object: TypeId,
    long: TypeId,
    prefix: &mut Vec<InstId>,
) -> [ValueId; 2] {
    if let Some(parts) = known.get(value.index()).copied().flatten() {
        return parts;
    }
    // Scalar projections can use an aggregate location's components.
    // Their borrowed shapes do not need to match.
    let mut source = value;
    for _ in 0..64 {
        source = body.resolve(source);
        let ValueDef::Inst(id) = body.values[source.index()].def else {
            break;
        };
        match body.instructions[id.index()].op {
            Op::AddressPack(parts) => return body.args[parts.range()].try_into().unwrap(),
            Op::Reinterpret(value) | Op::Refine(value)
                if types.address_layout(body.value_type(source))
                    == types.address_layout(body.value_type(value)) =>
            {
                source = value
            }
            _ => break,
        }
    }
    let (cast, root) = append(body, Op::Reinterpret(value), object);
    let (zero, offset) = literal(body, long, 0);
    prefix.extend([cast, zero]);
    [root, offset]
}

pub fn decompose_addresses(body: &mut Body, types: &mut Types, mut debug: Option<&mut DebugInfo>) {
    for shape in [ComponentShape::Address, ComponentShape::StorageAddress] {
        if body
            .values
            .iter()
            .any(|v| ComponentShape::of(types, v.ty) == Some(shape))
        {
            decompose(body, types, debug.as_deref_mut(), shape);
        }
    }
}

fn decompose(
    body: &mut Body,
    types: &mut Types,
    debug: Option<&mut DebugInfo>,
    shape: ComponentShape,
) {
    if !body.instructions.iter().any(|i| {
        matches!(
            i.op,
            Op::AddressPack(_)
                | Op::AddressOfSlot(_)
                | Op::Offset { .. }
                | Op::AddressPart { .. }
                | Op::AddressTag(_)
                | Op::AddressEqual { .. }
                | Op::AddressCompare { .. }
                | Op::ViewAddress { .. }
                | Op::AddressViewPart { .. }
                | Op::Project { .. }
        )
    }) {
        return;
    }
    let count = body.values.len();
    let predecessors = body.predecessors();
    let mut eligible = vec![false; count];
    let mut users = crate::analysis::ValueUsers::new(count);
    for index in 0..count {
        let ty = body.values[index].ty;
        let address = ComponentShape::of(types, ty) == Some(shape);
        if !shape.accepts_annotation(types, ty) {
            continue;
        }
        match body.values[index].def {
            ValueDef::Inst(id) => match body.instructions[id.index()].op {
                Op::AddressPack(_) | Op::AddressOfSlot(_) | Op::Offset { .. } if address => {
                    eligible[index] = true
                }
                Op::Project { projection, .. } if address => {
                    let projection = &body.projections[projection.index()];
                    let Type::Pointer(inner) = types.get(ty).unwrap() else {
                        unreachable!()
                    };
                    eligible[index] = (projection.codec.is_none()
                        && StorageSlot::scalar(inner, types)
                            .is_some_and(|slot| u64::from(slot.size) == projection.size))
                        || (ComponentShape::of(types, inner)
                            .is_some_and(ComponentShape::is_borrowed)
                            && matches!(projection.size, 8 | 16));
                }
                Op::Constant(constant)
                    if matches!(body.constants[constant.index()], Constant::Uninit(_))
                        || (address
                            && matches!(body.constants[constant.index()], Constant::Null(_))) =>
                {
                    eligible[index] = true
                }
                Op::ViewAddress {
                    size, codec: None, ..
                } if address && shape == ComponentShape::Address => {
                    let Some(Type::Pointer(inner)) = types.get(ty) else {
                        unreachable!()
                    };
                    eligible[index] =
                        StorageSlot::scalar(inner, types).is_some_and(|slot| slot.size == size);
                }
                Op::Cast(source)
                    if address
                        && shape == ComponentShape::Address
                        && matches!(types.get(body.value_type(source)), Some(Type::Pointer(_))) =>
                {
                    eligible[index] = true
                }
                Op::Reinterpret(source) | Op::Adapt(source) | Op::Refine(source)
                    if types.address_layout(ty)
                        == types.address_layout(body.value_type(source)) =>
                {
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
                    .position(|&p| p.index() == index)
                    .unwrap();
                for &(_, edge) in &predecessors[block.index()] {
                    users.connect(body.edges[edge.index()].args[position], index);
                }
            }
            _ => {}
        }
    }
    users.close(&mut eligible);
    let mut component_types = shape.parts(types);
    let (object, long) = (
        component_types.next().unwrap(),
        component_types.next().unwrap(),
    );
    let mut known = vec![None::<[ValueId; 2]>; count];
    let mut roots = Vec::new();
    let mut joins = vec![Vec::new(); body.blocks.len()];
    for index in 0..count {
        if !eligible[index] {
            continue;
        }
        match body.values[index].def {
            ValueDef::Param(block) => {
                let parts = [object, long].map(|ty| {
                    let value = ValueId::new(body.values.len());
                    body.values.push(Value {
                        ty,
                        def: ValueDef::Param(block),
                    });
                    body.blocks[block.index()].params.push(value);
                    value
                });
                known[index] = Some(parts);
                let position = body.blocks[block.index()]
                    .params
                    .iter()
                    .position(|&value| value.index() == index)
                    .unwrap();
                joins[block.index()].push((index, position));
            }
            ValueDef::Inst(id) => match body.instructions[id.index()].op {
                Op::AddressPack(args) => {
                    known[index] = Some(body.args[args.range()].try_into().unwrap())
                }
                Op::Constant(_)
                | Op::AddressOfSlot(_)
                | Op::Offset { .. }
                | Op::Cast(_)
                | Op::Project { .. }
                | Op::ViewAddress { .. } => {
                    let a = append(body, Op::Nop, object);
                    let b = append(body, Op::Nop, long);
                    known[index] = Some([a.1, b.1]);
                    roots.push((id, a.0, b.0));
                }
                _ => {}
            },
            _ => {}
        }
    }
    users.propagate(&eligible, &mut known);
    let mut prefixes = FxHashMap::<InstId, Vec<InstId>>::default();
    for (id, root, offset) in roots {
        let original = body.instructions[id.index()];
        let mut prefix = Vec::new();
        let (root_op, offset_op) = match original.op {
            Op::Project { base, projection } => {
                let displacement = body.projections[projection.index()].offset;
                let (base, projection, displacement) = if shape == ComponentShape::Address {
                    super::fields::fold_projection(body, types, base, projection).unwrap_or((
                        base,
                        projection,
                        displacement,
                    ))
                } else {
                    (base, projection, displacement)
                };
                let source = parts(body, types, base, &known, object, long, &mut prefix);
                let (constant, displacement) = literal(body, long, displacement as i64);
                let (sum_inst, sum) = append(
                    body,
                    Op::Binary {
                        op: BinaryOp::Add,
                        left: source[1],
                        right: displacement,
                    },
                    long,
                );
                prefix.extend([constant, sum_inst]);
                (
                    Op::ProjectRoot {
                        address: List::append(&mut body.args, source),
                        projection,
                    },
                    Op::ProjectOffset {
                        root: body.instructions[root.index()].result.unwrap(),
                        base: source[0],
                        offset: sum,
                    },
                )
            }
            Op::Constant(constant)
                if matches!(body.constants[constant.index()], Constant::Uninit(_)) =>
            {
                let root = ConstId::new(body.constants.len());
                body.constants.push(Constant::Uninit(object));
                let offset = ConstId::new(body.constants.len());
                body.constants.push(Constant::Uninit(long));
                (Op::Constant(root), Op::Constant(offset))
            }
            Op::Constant(_) => {
                let null = ConstId::new(body.constants.len());
                body.constants.push(Constant::Null(object));
                let (zero, value) = literal(body, long, 0);
                prefix.push(zero);
                (Op::Constant(null), Op::Reinterpret(value))
            }
            Op::ViewAddress { parts, size, .. } => {
                let values = body.args[parts.range()].to_vec();
                let int = types.scalar(ScalarType::I32);
                let constant = ConstId::new(body.constants.len());
                body.constants.push(Constant::Scalar(
                    Scalar::integer(ScalarType::I32, size.into()).unwrap(),
                ));
                let (size_inst, size_arg) = append(body, Op::Constant(constant), int);
                let (cast, start) = append(body, Op::Cast(values[1]), long);
                let (stride_inst, stride) = literal(body, long, size as i64);
                prefix.extend([size_inst, cast, stride_inst]);
                let method = MethodId::new(body.methods.len());
                body.methods.push(MethodRef {
                    owner: "org/rustlang/runtime/Pointer".into(),
                    name: "scalarSliceRoot".into(),
                    params: vec![object, int],
                    returns: object,
                    interface: false,
                });
                (
                    Op::Call {
                        method,
                        kind: CallKind::JvmStatic,
                        args: List::append(&mut body.args, [values[0], size_arg]),
                    },
                    Op::Binary {
                        op: BinaryOp::Mul,
                        left: start,
                        right: stride,
                    },
                )
            }
            Op::AddressOfSlot(slot) => {
                let (zero, value) = literal(body, long, 0);
                prefix.push(zero);
                (Op::SlotRoot(slot), Op::Reinterpret(value))
            }
            Op::Cast(value) => {
                let source = super::address_parts::scalar_cast_parts(body, types, value)
                    .map(|address| body.args[address.parts.range()].try_into().unwrap())
                    .unwrap_or_else(|| {
                        parts(body, types, value, &known, object, long, &mut prefix)
                    });
                (Op::Reinterpret(source[0]), Op::Reinterpret(source[1]))
            }
            Op::Offset {
                pointer,
                offset: delta,
                bytes,
                ..
            } => {
                let mut source = parts(body, types, pointer, &known, object, long, &mut prefix);
                if let Some(Type::Pointer(inner)) = types.get(body.value_type(pointer))
                    && ComponentShape::of(types, inner).is_some_and(ComponentShape::is_borrowed)
                {
                    // Reference-to-borrow arithmetic needs the exact Rust stride.
                    // A fixed-array borrow uses one word. A slice borrow uses two.
                    // The enclosing storage size does not determine this stride.
                    let int = types.scalar(ScalarType::I32);
                    let plan = crate::jvm::abi::address_plan(types, inner);
                    let constant = ConstId::new(body.constants.len());
                    body.constants.push(Constant::Scalar(
                        Scalar::integer(ScalarType::I32, plan.into()).unwrap(),
                    ));
                    let (id, plan) = append(body, Op::Constant(constant), int);
                    prefix.push(id);
                    let method = MethodId::new(body.methods.len());
                    let carrier = types.symbol("org/rustlang/runtime/Pointer");
                    let carrier = types.intern(Type::Class(carrier));
                    body.methods.push(MethodRef {
                        owner: "org/rustlang/runtime/Pointer".into(),
                        name: "addressFromParts".into(),
                        params: vec![object, long, int],
                        returns: carrier,
                        interface: false,
                    });
                    let args = List::append(&mut body.args, [source[0], source[1], plan]);
                    let (id, root) = append(
                        body,
                        Op::Call {
                            method,
                            kind: CallKind::JvmStatic,
                            args,
                        },
                        carrier,
                    );
                    let (erase, root) = append(body, Op::Reinterpret(root), object);
                    let (zero, displacement) = literal(body, long, 0);
                    prefix.extend([id, erase, zero]);
                    source = [root, displacement];
                }
                let (cast, mut delta) = append(body, Op::Cast(delta), long);
                prefix.push(cast);
                if !bytes {
                    let Some(Type::Pointer(inner)) = types.get(body.value_type(pointer)) else {
                        unreachable!()
                    };
                    let (constant, size) =
                        if let Some((size, _)) = types.address_layout(body.value_type(pointer)) {
                            literal(body, long, size as i64)
                        } else if let Some(slot) = StorageSlot::scalar(inner, types) {
                            literal(body, long, slot.size as i64)
                        } else if let ValueDef::Inst(id) =
                            body.values[body.resolve(pointer).index()].def
                            && let Op::RetypeAddress { size, .. } = body.instructions[id.index()].op
                        {
                            literal(body, long, size as i64)
                        } else {
                            let method = MethodId::new(body.methods.len());
                            body.methods.push(MethodRef {
                                owner: "org/rustlang/runtime/Pointer".into(),
                                name: "locationStride".into(),
                                params: vec![object],
                                returns: long,
                                interface: false,
                            });
                            let args = List::append(&mut body.args, [source[0]]);
                            append(
                                body,
                                Op::Call {
                                    method,
                                    kind: CallKind::JvmStatic,
                                    args,
                                },
                                long,
                            )
                        };
                    let (mul, scaled) = append(
                        body,
                        Op::Binary {
                            op: BinaryOp::Mul,
                            left: delta,
                            right: size,
                        },
                        long,
                    );
                    prefix.extend([constant, mul]);
                    delta = scaled;
                }
                (
                    Op::Reinterpret(source[0]),
                    Op::Binary {
                        op: BinaryOp::Add,
                        left: source[1],
                        right: delta,
                    },
                )
            }
            _ => unreachable!(),
        };
        body.instructions[root.index()].op = root_op;
        body.instructions[offset.index()].op = offset_op;
        prefix.extend([root, offset]);
        let values = known[original.result.unwrap().index()].unwrap();
        if ComponentShape::of(types, body.value_type(original.result.unwrap())) == Some(shape) {
            body.instructions[id.index()].op =
                Op::AddressPack(List::append(&mut body.args, values));
        }
        prefixes.insert(id, prefix);
    }
    for (block, incoming) in predecessors.iter().enumerate() {
        for &(_, edge) in incoming {
            for &(_, position) in &joins[block] {
                let input = body.edges[edge.index()].args[position];
                body.edges[edge.index()]
                    .args
                    .extend(known[input.index()].unwrap());
            }
        }
    }
    // A decoded load and its later commit must use the same carrier.
    // Separate carriers would lose the mutable view binding.
    let bound_views = if shape == ComponentShape::StorageAddress {
        body.instructions
            .iter()
            .filter_map(|inst| {
                if let Op::Commit(pointer) = inst.op {
                    return known.get(pointer.index()).copied().flatten();
                }
                let Op::Call {
                    method,
                    kind: CallKind::Virtual,
                    args,
                } = inst.op
                else {
                    return None;
                };
                let method = &body.methods[method.index()];
                if method.owner != "org/rustlang/runtime/Pointer"
                    || method.name != "commitMemoryView"
                {
                    return None;
                }
                known
                    .get(body.args[args.start as usize].index())
                    .copied()
                    .flatten()
            })
            .collect::<FxHashSet<_>>()
    } else {
        FxHashSet::default()
    };
    for index in 0..body.instructions.len() {
        let original = body.instructions[index];
        if shape == ComponentShape::StorageAddress {
            if let Op::LoadFieldCopy { base, projection } = original.op
                && let Some(parts) = known.get(base.index()).copied().flatten()
            {
                body.instructions[index].op = Op::LoadStorageFieldCopy {
                    address: List::append(&mut body.args, parts),
                    projection,
                };
                continue;
            }
            let stored = match original.op {
                Op::StoreField {
                    base,
                    projection,
                    value,
                } => Some((base, projection, vec![value], false)),
                Op::StoreFieldParts {
                    base,
                    projection,
                    parts,
                } => Some((base, projection, body.args[parts.range()].to_vec(), true)),
                _ => None,
            };
            if let Some((base, projection, values, split)) = stored
                && let Some(parts) = known.get(base.index()).copied().flatten()
            {
                body.instructions[index].op = Op::StoreStorageField {
                    args: List::append(&mut body.args, parts.into_iter().chain(values)),
                    projection,
                    split,
                };
                continue;
            }
            let field = match original.op {
                Op::LoadField { base, projection } => Some((base, projection, None)),
                Op::LoadFieldPart {
                    base,
                    projection,
                    index,
                } => Some((base, projection, Some(index))),
                _ => None,
            };
            if let Some((base, projection, index_part)) = field
                && let Some(parts) = known.get(base.index()).copied().flatten()
            {
                body.instructions[index].op = Op::LoadStorageField {
                    address: List::append(&mut body.args, parts),
                    projection,
                    index: index_part,
                };
                continue;
            }
        }
        if let Op::AddressViewPart {
            address,
            index: part,
        } = original.op
        {
            // Opaque helpers can retain a layout that differs from the pointer type,
            // such as a DST tail or fixed-array cast. Normalize only proven scalar locations.
            if shape != ComponentShape::Address
                || known.get(address.index()).copied().flatten().is_none()
            {
                continue;
            }
            let mut prefix = Vec::new();
            let source = parts(body, types, address, &known, object, long, &mut prefix);
            let Some(Type::Pointer(inner)) = types.get(body.value_type(address)) else {
                unreachable!()
            };
            let size = StorageSlot::scalar(inner, types).unwrap().size;
            let int = types.scalar(ScalarType::I32);
            let constant = ConstId::new(body.constants.len());
            body.constants.push(Constant::Scalar(
                Scalar::integer(ScalarType::I32, size.into()).unwrap(),
            ));
            let (instruction, size) = append(body, Op::Constant(constant), int);
            prefix.push(instruction);
            let method = MethodId::new(body.methods.len());
            body.methods.push(MethodRef {
                owner: "org/rustlang/runtime/Pointer".into(),
                name: if part == 0 {
                    "locationSliceBacking"
                } else {
                    "locationSliceOffset"
                }
                .into(),
                params: vec![object, long, int],
                returns: body.value_type(original.result.unwrap()),
                interface: false,
            });
            body.instructions[index].op = Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args: List::append(&mut body.args, [source[0], source[1], size]),
            };
            prefixes.insert(InstId::new(index), prefix);
            continue;
        }
        if let Op::AddressTag(value) = original.op {
            if ComponentShape::of(types, body.value_type(value)) != Some(shape) {
                continue;
            }
            let mut prefix = Vec::new();
            let source = parts(body, types, value, &known, object, long, &mut prefix);
            body.instructions[index].op = Op::LocationTag(List::append(&mut body.args, source));
            prefixes.insert(InstId::new(index), prefix);
            continue;
        }
        if let Op::AddressEqual { left, right } | Op::AddressCompare { left, right } = original.op {
            // Leave storage comparisons for the later storage-address pass.
            // Boxing components here would hide their storage layout.
            if [left, right]
                .into_iter()
                .any(|value| ComponentShape::of(types, body.value_type(value)) != Some(shape))
            {
                continue;
            }
            let mut prefix = Vec::new();
            let left = parts(body, types, left, &known, object, long, &mut prefix);
            let right = parts(body, types, right, &known, object, long, &mut prefix);
            let parts = List::append(&mut body.args, left.into_iter().chain(right));
            body.instructions[index].op = if matches!(original.op, Op::AddressCompare { .. }) {
                Op::LocationCompare(parts)
            } else {
                Op::LocationEqual(parts)
            };
            prefixes.insert(InstId::new(index), prefix);
            continue;
        }
        if let Op::Adapt(source) = original.op {
            if let Some(result) = original.result
                && let Some(parts) = known.get(result.index()).copied().flatten()
            {
                body.instructions[index].op = if shape == ComponentShape::StorageAddress {
                    // Keep the JVM type refinement even though the storage identity
                    // and layout are already known.
                    Op::Refine(source)
                } else if ComponentShape::of(types, body.value_type(result)) == Some(shape) {
                    Op::AddressPack(List::append(&mut body.args, parts))
                } else {
                    Op::Reinterpret(source)
                };
                continue;
            }
        }
        let pointer = match original.op {
            Op::Load(p)
            | Op::LoadCopy(p)
            | Op::Store { pointer: p, .. }
            | Op::AddressPart { address: p, .. } => p,
            _ => continue,
        };
        let Some(parts) = known.get(pointer.index()).copied().flatten() else {
            continue;
        };
        body.instructions[index].op = match original.op {
            Op::AddressPart { index, .. } => Op::Reinterpret(parts[index as usize]),
            Op::LoadCopy(_) if shape == ComponentShape::StorageAddress => {
                Op::LoadAddressCopy(List::append(&mut body.args, parts))
            }
            Op::Load(_)
                if (shape == ComponentShape::StorageAddress && !bound_views.contains(&parts))
                    || StorageSlot::scalar(body.value_type(original.result.unwrap()), types)
                        .is_some() =>
            {
                Op::LoadAddress(List::append(&mut body.args, parts))
            }
            Op::Store { value, .. }
                if shape == ComponentShape::StorageAddress
                    || StorageSlot::scalar(body.value_type(value), types).is_some() =>
            {
                Op::StoreAddress {
                    parts: List::append(&mut body.args, parts),
                    value,
                }
            }
            op => op,
        };
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
            if matches!(body.instructions[inst.index()].op, Op::AddressPack(_)) {
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
