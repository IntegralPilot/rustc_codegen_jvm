//! Prove that fresh primitive arrays have no decoded or encoded aliases.
//! Escape checks cover calls, storage, addresses and mixed joins. Local checks
//! also permit native writes before the first escape in a block.
use super::{NO_ORIGIN, origins::origins_with};
use crate::ir::*;
use crate::opt::Live;

pub(crate) fn native_array_accesses(
    body: &Body,
    types: &Types,
    live: &Live,
) -> Vec<Option<TypeId>> {
    analyze(body, types, live).0
}

pub(crate) fn native_array_roots(
    body: &Body,
    types: &Types,
    live: &Live,
) -> Vec<Option<(ValueId, TypeId)>> {
    analyze(body, types, live).1
}

fn analyze(
    body: &Body,
    types: &Types,
    live: &Live,
) -> (Vec<Option<TypeId>>, Vec<Option<(ValueId, TypeId)>>) {
    let candidates = body
        .instructions
        .iter()
        .enumerate()
        .filter_map(|(id, inst)| {
            if !live.instructions[id] || !matches!(inst.op, Op::NewArray(_)) {
                return None;
            }
            let value = inst.result?;
            let Type::Array(element) = types.get(body.value_type(value))? else {
                return None;
            };
            StorageSlot::scalar(element, types).map(|_| (value, element))
        })
        .collect::<Vec<_>>();
    if candidates.is_empty() {
        return (Vec::new(), Vec::new());
    }
    let mut roots = vec![NO_ORIGIN; body.values.len()];
    for (index, &(value, _)) in candidates.iter().enumerate() {
        roots[value.index()] = index as u32;
    }
    let origins = origins_with(
        body,
        &roots,
        |op| match op {
            Op::Reinterpret(value) | Op::Refine(value) => Some(value),
            Op::ViewRoot {
                backing,
                codec: None,
                ..
            } => Some(backing),
            Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            } if body.methods[method.index()].owner == "org/rustlang/runtime/Pointer"
                && matches!(
                    body.methods[method.index()].name.as_str(),
                    "scalarSliceRoot" | "locationSliceBacking"
                ) =>
            {
                Some(body.args[args.start as usize])
            }
            _ => None,
        },
        true,
    );
    let mut escaped = vec![false; candidates.len()];
    for (id, inst) in body.instructions.iter().enumerate() {
        if !live.instructions[id] {
            continue;
        }
        inst.op.visit_uses(&body.args, |value| {
            let origin = origins[value.index()];
            if origin == NO_ORIGIN {
                return;
            }
            let element = candidates[origin as usize].1;
            let safe = safe_use(body, types, *inst, value, element);
            escaped[origin as usize] |= !safe;
        });
    }
    let mut escape = |value: ValueId| {
        let origin = origins[value.index()];
        if origin != NO_ORIGIN {
            escaped[origin as usize] = true;
        }
    };
    for edge in &body.edges {
        for (&value, &param) in edge
            .args
            .iter()
            .zip(&body.blocks[edge.target.index()].params)
        {
            if live.values[body.resolve(param).index()]
                && origins[value.index()] != origins[param.index()]
            {
                escape(value);
            }
        }
    }
    for (index, block) in body.blocks.iter().enumerate() {
        if live.blocks[index] {
            block.terminator.unwrap().visit_uses(&mut escape);
        }
    }
    // Permit native initialization before the first escape, including a later return.
    // Restrict this proof to one block because other paths can expose the array.
    let mut fresh = vec![None; body.blocks.len()];
    for block in &body.blocks {
        if let Some(Terminator::Invoke { inst, normal, .. }) = block.terminator {
            if let Some(value) = body.instructions[inst.index()].result {
                let root = roots[value.index()];
                if root != NO_ORIGIN {
                    fresh[body.edges[normal.index()].target.index()] = Some(root);
                }
            }
        }
    }
    let mut available = vec![None; candidates.len()];
    let mut native = vec![None; body.instructions.len()];
    for (index, block) in body.blocks.iter().enumerate() {
        if !live.blocks[index] {
            continue;
        }
        let current = Some(BlockId::new(index));
        if let Some(root) = fresh[index] {
            available[root as usize] = current;
        }
        let invoke = match block.terminator {
            Some(Terminator::Invoke { inst, .. }) => Some(inst),
            _ => None,
        };
        for &id in block.instructions.iter().chain(invoke.iter()) {
            if !live.instructions[id.index()] {
                continue;
            }
            let inst = body.instructions[id.index()];
            if let Some((array, element)) = access(body, inst) {
                let origin = origins[array.index()];
                if origin != NO_ORIGIN
                    && candidates[origin as usize].1 == element
                    && (!escaped[origin as usize] || available[origin as usize] == current)
                {
                    native[id.index()] = Some(element);
                }
            }
            inst.op.visit_uses(&body.args, |value| {
                let origin = origins[value.index()];
                if origin != NO_ORIGIN
                    && !safe_use(body, types, inst, value, candidates[origin as usize].1)
                {
                    available[origin as usize] = None;
                }
            });
            if let Some(value) = inst.result {
                let root = roots[value.index()];
                if root != NO_ORIGIN {
                    available[root as usize] = current;
                }
            }
        }
    }
    let values = origins
        .iter()
        .map(|&origin| {
            (origin != NO_ORIGIN && !escaped[origin as usize]).then(|| candidates[origin as usize])
        })
        .collect();
    (native, values)
}

fn access(body: &Body, inst: Inst) -> Option<(ValueId, TypeId)> {
    Some(match inst.op {
        Op::ArrayGet { array, .. } => (array, body.value_type(inst.result?)),
        Op::ArraySet { array, value, .. } | Op::ArrayFill { array, value } => {
            (array, body.value_type(value))
        }
        Op::ViewGet(parts) => (
            body.args[parts.start as usize],
            body.value_type(inst.result?),
        ),
        Op::ViewSet { parts, value } => (body.args[parts.start as usize], body.value_type(value)),
        _ => return None,
    })
}

fn safe_use(body: &Body, types: &Types, inst: Inst, value: ValueId, element: TypeId) -> bool {
    match inst.op {
        Op::ViewRoot {
            backing,
            size,
            codec: None,
        } => {
            backing == value && StorageSlot::scalar(element, types).is_some_and(|s| s.size == size)
        }
        Op::LoadAddress(parts) => {
            body.args[parts.start as usize] == value
                && inst.result.is_some_and(|v| body.value_type(v) == element)
        }
        Op::StoreAddress {
            parts,
            value: stored,
        } => body.args[parts.start as usize] == value && body.value_type(stored) == element,
        Op::LocationEqual(parts) | Op::LocationCompare(parts) => {
            body.args[parts.start as usize] == value || body.args[parts.start as usize + 2] == value
        }
        Op::LocationTag(parts) => body.args[parts.start as usize] == value,
        Op::Call {
            method,
            kind: CallKind::JvmStatic,
            args,
        } => {
            let method = &body.methods[method.index()];
            if method.owner == "org/rustlang/runtime/Pointer" {
                match method.name.as_str() {
                    "nullableViewLocationTag" | "nullableLocationTag" => {
                        return body.args[args.start as usize] == value;
                    }
                    "offsetLocations"
                    | "offsetLocationsUnsigned"
                    | "byteOffsetLocations"
                    | "byteOffsetLocationsUnsigned"
                    | "sameLocation" => {
                        return body.args[args.start as usize] == value
                            || body.args[args.start as usize + 2] == value;
                    }
                    _ => {}
                }
            }
            let size = match method.name.as_str() {
                "scalarSliceRoot" if args.len == 2 => 1,
                "locationSliceBacking" | "locationSliceOffset" if args.len == 3 => 2,
                _ => return false,
            };
            method.owner == "org/rustlang/runtime/Pointer"
                && body.args[args.start as usize] == value
                && body
                    .scalar_value(body.args[args.start as usize + size])
                    .map(|s| s.bits())
                    == StorageSlot::scalar(element, types).map(|s| u128::from(s.size))
        }
        Op::Reinterpret(_) | Op::Refine(_) | Op::ArrayLength(_) => true,
        Op::ArrayGet { array, .. } => {
            array == value && inst.result.is_some_and(|r| body.value_type(r) == element)
        }
        Op::ArraySet {
            array,
            value: stored,
            ..
        }
        | Op::ArrayFill {
            array,
            value: stored,
        } => array == value && body.value_type(stored) == element,
        Op::ViewGet(parts) => {
            body.args[parts.start as usize] == value
                && inst.result.is_some_and(|r| body.value_type(r) == element)
        }
        Op::ViewSet {
            parts,
            value: stored,
        } => body.args[parts.start as usize] == value && body.value_type(stored) == element,
        _ => false,
    }
}
