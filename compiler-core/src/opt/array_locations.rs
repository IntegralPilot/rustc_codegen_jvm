//! Use JVM arrays for scalar locations whose storage never escapes.
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};

pub fn lower_array_locations(body: &mut Body, types: &mut Types) -> bool {
    let roots = crate::analysis::native_array_roots(body, types, &super::live(body, types));
    if roots.is_empty() {
        return false;
    }
    let mut changed = false;
    let long = types.scalar(ScalarType::I64);
    let int = types.scalar(ScalarType::I32);
    let mut prefixes = rustc_hash::FxHashMap::default();
    for index in 0..body.instructions.len() {
        let inst = body.instructions[index];
        let observers = match inst.op {
            Op::LocationEqual(args) | Op::LocationTag(args) => Some((
                args,
                if matches!(inst.op, Op::LocationEqual(_)) {
                    "sameLocation"
                } else {
                    "nullableLocationTag"
                },
            )),
            Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            } if body.methods[method.index()].owner == "org/rustlang/runtime/Pointer" => {
                Some((args, body.methods[method.index()].name.as_str()))
            }
            _ => None,
        };
        if let Some((args, name)) = observers {
            let values = body.args[args.range()].to_vec();
            let root = values
                .first()
                .and_then(|v| roots.get(v.index()))
                .copied()
                .flatten();
            if let Some((allocation, element)) = root {
                if matches!(name, "nullableLocationTag" | "nullableViewLocationTag") {
                    let constant = ConstId::new(body.constants.len());
                    body.constants.push(Constant::Scalar(
                        Scalar::integer(ScalarType::I64, 1).unwrap(),
                    ));
                    body.instructions[index].op = Op::Constant(constant);
                    changed = true;
                    continue;
                }
                if matches!(
                    name,
                    "sameLocation"
                        | "offsetLocations"
                        | "offsetLocationsUnsigned"
                        | "byteOffsetLocations"
                        | "byteOffsetLocationsUnsigned"
                ) && values.len() >= 4
                    && roots
                        .get(values[2].index())
                        .copied()
                        .flatten()
                        .is_some_and(|(other, _)| other == allocation)
                {
                    let op = if name == "sameLocation" {
                        BinaryOp::Eq
                    } else {
                        BinaryOp::Sub
                    };
                    let difference = Op::Binary {
                        op,
                        left: values[1],
                        right: values[3],
                    };
                    if name.starts_with("offsetLocations") {
                        let size = StorageSlot::scalar(element, types).unwrap().size;
                        if values.len() != 5
                            || body.scalar_value(values[4]).map(|s| s.bits())
                                != Some(u128::from(size))
                        {
                            continue;
                        }
                        let (prefix, delta) = super::append_value(body, difference, long);
                        prefixes.insert(InstId::new(index), vec![prefix]);
                        body.instructions[index].op = Op::Binary {
                            op: BinaryOp::Div,
                            left: delta,
                            right: values[4],
                        };
                    } else {
                        body.instructions[index].op = difference;
                    }
                    changed = true;
                    continue;
                }
            }
        }
        let (root, offset, view_index, stored) = match inst.op {
            Op::ViewRoot {
                backing,
                codec: None,
                ..
            } if roots.get(backing.index()).copied().flatten().is_some() => {
                body.instructions[index].op = Op::Reinterpret(backing);
                changed = true;
                continue;
            }
            Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            } if body.methods[method.index()].owner == "org/rustlang/runtime/Pointer" => {
                let values = &body.args[args.range()];
                let Some(&root) = values.first() else {
                    continue;
                };
                if roots[root.index()].is_none() {
                    continue;
                }
                match body.methods[method.index()].name.as_str() {
                    "scalarSliceRoot" | "locationSliceBacking" => {
                        body.instructions[index].op = Op::Reinterpret(root);
                        changed = true;
                        continue;
                    }
                    "locationSliceOffset" => (root, values[1], None, None),
                    _ => continue,
                }
            }
            Op::LoadAddress(parts) => (
                body.args[parts.start as usize],
                body.args[parts.start as usize + 1],
                None,
                None,
            ),
            Op::StoreAddress { parts, value } => (
                body.args[parts.start as usize],
                body.args[parts.start as usize + 1],
                None,
                Some(value),
            ),
            Op::ViewGet(parts) | Op::ViewSet { parts, .. } => {
                let values = &body.args[parts.range()];
                (
                    values[0],
                    values[1],
                    Some(values[2]),
                    if let Op::ViewSet { value, .. } = inst.op {
                        Some(value)
                    } else {
                        None
                    },
                )
            }
            _ => continue,
        };
        let Some((_, element)) = roots.get(root.index()).copied().flatten() else {
            continue;
        };
        let mut prefix = Vec::new();
        let position = if let Some(view_index) = view_index {
            let (instruction, position) = super::append_value(
                body,
                Op::Binary {
                    op: BinaryOp::Add,
                    left: offset,
                    right: view_index,
                },
                int,
            );
            prefix.push(instruction);
            position
        } else {
            let size = StorageSlot::scalar(element, types).unwrap().size;
            if !aligned_offset(body, offset, size, 0) {
                continue;
            }
            let constant = ConstId::new(body.constants.len());
            body.constants.push(Constant::Scalar(
                Scalar::integer(ScalarType::I64, u128::from(size)).unwrap(),
            ));
            let (size_inst, stride) = super::append_value(body, Op::Constant(constant), long);
            let (quotient_inst, quotient) = super::append_value(
                body,
                Op::Binary {
                    op: BinaryOp::Div,
                    left: offset,
                    right: stride,
                },
                long,
            );
            let (position_inst, position) = super::append_value(body, Op::Cast(quotient), int);
            prefix.extend([size_inst, quotient_inst, position_inst]);
            position
        };
        let array = if matches!(inst.op, Op::Call { .. }) {
            root
        } else {
            let ty = types.find(Type::Array(element)).unwrap();
            let (cast, array) = super::append_value(body, Op::Refine(root), ty);
            prefix.push(cast);
            array
        };
        body.instructions[index].op = if matches!(inst.op, Op::Call { .. }) {
            Op::Reinterpret(position)
        } else if let Some(value) = stored {
            Op::ArraySet {
                array,
                index: position,
                value,
                native: true,
            }
        } else {
            Op::ArrayGet {
                array,
                index: position,
                native: true,
            }
        };
        prefixes.insert(InstId::new(index), prefix);
        changed = true;
    }
    for block in &mut body.blocks {
        let previous = std::mem::take(&mut block.instructions);
        for id in previous {
            if let Some(prefix) = prefixes.remove(&id) {
                block.instructions.extend(prefix);
            }
            block.instructions.push(id);
        }
        if let Some(Terminator::Invoke { inst, .. }) = block.terminator
            && let Some(prefix) = prefixes.remove(&inst)
        {
            block.instructions.extend(prefix);
        }
    }
    changed
}

fn aligned_offset(body: &Body, value: ValueId, size: u32, depth: usize) -> bool {
    if size == 1 {
        return true;
    }
    if let Some(value) = body.scalar_value(value) {
        return value.bits() % u128::from(size) == 0;
    }
    if depth == 16 {
        return false;
    }
    let ValueDef::Inst(id) = body.values[body.resolve(value).index()].def else {
        return false;
    };
    let aligned = |value| aligned_offset(body, value, size, depth + 1);
    match body.instructions[id.index()].op {
        Op::Reinterpret(value) | Op::Refine(value) => aligned(value),
        Op::Binary {
            op: BinaryOp::Mul,
            left,
            right,
        } => aligned(left) || aligned(right),
        Op::Binary {
            op: BinaryOp::Add | BinaryOp::Sub,
            left,
            right,
        } => aligned(left) && aligned(right),
        _ => false,
    }
}
