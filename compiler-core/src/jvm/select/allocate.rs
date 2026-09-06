//! Conservative SSA intervals without instruction-by-value liveness matrices.
//! Uses extend backwards through predecessor blocks until their definition;
//! intervals also cover edge-copy destinations, including pre-throw copies.
use super::*;
use std::{cmp::Reverse, collections::BinaryHeap};

pub(super) struct Allocation {
    pub slots: Vec<Option<u16>>,
    pub count: u16,
}

fn value_kind(body: &Body, types: &Types, value: ValueId) -> jvm::Result<Kind> {
    representation::value_kind(types, body.value_type(value))
}

pub(super) fn allocate(
    body: &Body,
    types: &Types,
    live: &crate::opt::Live,
    relative_pointer_abi: bool,
    debug: Option<&DebugInfo>,
    order: &[BlockId],
) -> jvm::Result<Allocation> {
    let intervals = intervals(body, live, debug, order)?;
    let mut result = Allocation {
        slots: vec![None; body.values.len()],
        count: 0,
    };
    let mut active = BinaryHeap::new();
    let mut free: [Vec<u16>; 5] = Default::default();
    for &param in &body.blocks[body.entry.index()].params {
        result.slots[param.index()] = Some(result.count);
        result.count = result
            .count
            .checked_add(value_kind(body, types, param)?.width())
            .ok_or_else(|| error("JVM local limit"))?;
        if relative_pointer_abi
            && matches!(types.get(body.value_type(param)), Some(Type::Pointer(_)))
        {
            result.count = result
                .count
                .checked_add(4)
                .ok_or_else(|| error("JVM parameter limit"))?;
        }
        active.push(Reverse((intervals[param.index()].1, param)));
    }
    let mut order = (0..body.values.len())
        .filter(|&i| {
            live.values[i]
                && literal(body, ValueId::new(i)).is_none()
                && result.slots[i].is_none()
                && matches!(body.values[i].def, ValueDef::Inst(_) | ValueDef::Param(_))
        })
        .collect::<Vec<_>>();
    order.sort_unstable_by_key(|&i| (intervals[i].0, i));
    for index in order {
        let value = ValueId::new(index);
        let (first, last) = intervals[index];
        while active.peek().is_some_and(|Reverse((end, _))| *end < first) {
            let Reverse((_, expired)) = active.pop().unwrap();
            free[value_kind(body, types, expired)? as usize]
                .push(result.slots[expired.index()].unwrap());
        }
        let kind = value_kind(body, types, value)?;
        let slot = if let Some(slot) = free[kind as usize].pop() {
            slot
        } else {
            let slot = result.count;
            result.count = result
                .count
                .checked_add(kind.width())
                .ok_or_else(|| error("JVM local limit"))?;
            slot
        };
        result.slots[index] = Some(slot);
        active.push(Reverse((last, value)));
    }
    Ok(result)
}

fn intervals(
    body: &Body,
    live: &crate::opt::Live,
    debug: Option<&DebugInfo>,
    order: &[BlockId],
) -> jvm::Result<Vec<(u32, u32)>> {
    let mut ranges = vec![(u32::MAX, 0); body.values.len()];
    let mut definitions = vec![None; body.values.len()];
    let mut starts = vec![0; body.blocks.len()];
    let mut ends = starts.clone();
    let mut uses = Vec::new();
    let mut debug_uses = vec![
        Vec::new();
        if debug.is_some() {
            body.blocks.len()
        } else {
            0
        }
    ];
    if let Some(debug) = debug {
        for event in &debug.events {
            if let DebugChange::Set { value, .. } = event.change {
                debug_uses[event.block.index()].push((event.position as usize, value));
            }
        }
        for uses in &mut debug_uses {
            uses.sort_by_key(|&(position, _)| position);
        }
    }
    let mut position = 0u32;
    for &block in order {
        starts[block.index()] = position;
        let data = &body.blocks[block.index()];
        for &param in &data.params {
            definitions[param.index()] = Some(block);
            extend(&mut ranges[param.index()], position);
        }
        let mut events = debug_uses
            .get(block.index())
            .into_iter()
            .flatten()
            .peekable();
        for index in 0..=data.instructions.len() {
            while events.peek().is_some_and(|&&(point, _)| point == index) {
                let &(_, value) = events.next().unwrap();
                use_at(body, value, block, position, &mut ranges, &mut uses);
            }
            let Some(&inst) = data.instructions.get(index) else {
                break;
            };
            if !live.instructions[inst.index()]
                || body.instructions[inst.index()]
                    .result
                    .is_some_and(|v| literal(body, v).is_some())
            {
                continue;
            }
            position = position
                .checked_add(2)
                .ok_or_else(|| error("SSA position limit"))?;
            let inst = body.instructions[inst.index()];
            inst.op.visit_uses(&body.args, |value| {
                use_at(body, value, block, position - 1, &mut ranges, &mut uses)
            });
            if let Some(result) = inst.result {
                definitions[result.index()] = Some(block);
                extend(&mut ranges[result.index()], position);
            }
        }
        position = position
            .checked_add(3)
            .ok_or_else(|| error("SSA position limit"))?;
        let term = data.terminator.unwrap();
        term.visit_uses(|value| use_at(body, value, block, position - 2, &mut ranges, &mut uses));
        if let Terminator::Invoke { inst, normal, .. } = term {
            let inst = body.instructions[inst.index()];
            inst.op.visit_uses(&body.args, |value| {
                use_at(body, value, block, position - 2, &mut ranges, &mut uses)
            });
            if let Some(result) = inst.result {
                definitions[result.index()] = Some(body.edges[normal.index()].target);
                extend(&mut ranges[result.index()], position - 1);
            }
        }
        term.visit_edges(&body.cases, |edge| {
            let copy_position = if matches!(term, Terminator::Invoke { normal, .. } if normal == edge) { position } else { position - 2 };
            let edge = &body.edges[edge.index()];
            for (&param, &arg) in body.blocks[edge.target.index()].params.iter().zip(&edge.args) {
                if live.values[param.index()] {
                    use_at(body, arg, block, copy_position, &mut ranges, &mut uses);
                    extend(&mut ranges[param.index()], copy_position);
                }
            }
        });
        ends[block.index()] = position;
    }

    // Group use blocks by value in one flat table. No per-value heap vectors.
    let mut offsets = vec![0; body.values.len() + 1];
    for &(value, _) in &uses {
        offsets[value.index() + 1] += 1;
    }
    for i in 1..offsets.len() {
        offsets[i] += offsets[i - 1];
    }
    let mut cursor = offsets.clone();
    let mut use_blocks = vec![body.entry; uses.len()];
    for (value, block) in uses {
        use_blocks[cursor[value.index()]] = block;
        cursor[value.index()] += 1;
    }
    drop(cursor);
    let predecessors = body.predecessors();
    let mut visited = vec![None; body.blocks.len()];
    let mut pending = Vec::new();
    for index in 0..body.values.len() {
        if !live.values[index] {
            continue;
        }
        let value = ValueId::new(index);
        let definition = definitions[index];
        pending.extend_from_slice(&use_blocks[offsets[index]..offsets[index + 1]]);
        while let Some(block) = pending.pop() {
            if Some(block) == definition || visited[block.index()] == Some(value) {
                continue;
            }
            visited[block.index()] = Some(value);
            extend(&mut ranges[index], starts[block.index()]);
            for &(source, _) in &predecessors[block.index()] {
                if live.blocks[source.index()] {
                    extend(&mut ranges[index], ends[source.index()]);
                    pending.push(source);
                }
            }
        }
    }
    Ok(ranges)
}

fn extend(range: &mut (u32, u32), position: u32) {
    range.0 = range.0.min(position);
    range.1 = range.1.max(position);
}

fn use_at(
    body: &Body,
    value: ValueId,
    block: BlockId,
    position: u32,
    ranges: &mut [(u32, u32)],
    uses: &mut Vec<(ValueId, BlockId)>,
) {
    let value = body.resolve(value);
    if literal(body, value).is_some() {
        return;
    }
    extend(&mut ranges[value.index()], position);
    uses.push((value, block));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_hundred_thousand_live_definitions_need_two_slots() {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let mut b = Builder::new(&types, int);
        let parameter = b.parameter(b.current(), int);
        let mut result = parameter;
        for _ in 0..100_000 {
            result = b
                .emit(
                    Op::Binary {
                        op: BinaryOp::Add,
                        left: result,
                        right: parameter,
                    },
                    Some(int),
                )
                .unwrap();
        }
        b.terminate(Terminator::Return(Some(result)));
        let body = b.finish().unwrap();
        let allocation = allocate(
            &body,
            &types,
            &crate::opt::live(&body, &types),
            false,
            None,
            &body.layout(),
        )
        .unwrap();
        assert_eq!(allocation.count, 2);
    }
}
