//! Fuse an element read and its sole copy without crossing effects or handlers.
use crate::ir::*;

pub fn lower_owned_reads(body: &mut Body, types: &Types) {
    if !body
        .instructions
        .iter()
        .any(|inst| matches!(inst.op, Op::CopyValue(_)))
    {
        return;
    }
    let mut uses = vec![0_u8; body.values.len()];
    let mut count = |value| {
        let count = &mut uses[body.resolve(value).index()];
        *count = count.saturating_add(1);
    };
    for inst in &body.instructions {
        inst.op.visit_uses(&body.args, &mut count);
    }
    for block in &body.blocks {
        block.terminator.unwrap().visit_uses(&mut count);
    }
    for edge in &body.edges {
        for &value in &edge.args {
            count(value);
        }
    }
    let mut positions = vec![None; body.instructions.len()];
    for (block, data) in body.blocks.iter().enumerate() {
        for (position, id) in data.instructions.iter().enumerate() {
            positions[id.index()] = Some((block, position, None));
        }
        if let Some(Terminator::Invoke { inst, unwind, .. }) = data.terminator {
            positions[inst.index()] = Some((block, data.instructions.len(), Some(unwind)));
        }
    }
    for copy in (0..body.instructions.len()).rev() {
        let Op::CopyValue(source) = body.instructions[copy].op else {
            continue;
        };
        let source = body.resolve(source);
        if uses[source.index()] != 1 {
            continue;
        }
        let ty = body.value_type(source);
        if body.value_type(body.instructions[copy].result.unwrap()) != ty
            || !matches!(types.get(ty), Some(Type::Class(_) | Type::Array(_)))
        {
            continue;
        }
        let ValueDef::Inst(read) = body.values[source.index()].def else {
            continue;
        };
        let op = match body.instructions[read.index()].op {
            Op::ArrayGet {
                array,
                index,
                native: false,
            } => Op::ArrayGetCopy { array, index },
            Op::ViewGet(parts) => Op::ViewGetCopy(parts),
            owned @ (Op::CopyValue(_)
            | Op::LoadCopy(_)
            | Op::LoadFieldCopy { .. }
            | Op::LoadStorageFieldCopy { .. }
            | Op::LoadAddressCopy(_)
            | Op::LoadTypedCopy { .. }
            | Op::ArrayGetCopy { .. }
            | Op::ViewGetCopy(_)) => owned,
            _ => continue,
        };
        let (Some((from, start, read_handler)), Some((to, end, copy_handler))) =
            (positions[read.index()], positions[copy])
        else {
            continue;
        };
        if read_handler.map(|e| &body.edges[e.index()])
            != copy_handler.map(|e| &body.edges[e.index()])
        {
            continue;
        }
        let between = if from == to && start < end {
            &body.blocks[to].instructions[start + 1..end]
        } else if matches!(body.blocks[from].terminator,
            Some(Terminator::Invoke { inst, normal, .. })
                if inst == read && body.edges[normal.index()].target.index() == to)
        {
            &body.blocks[to].instructions[..end]
        } else {
            continue;
        };
        if between
            .iter()
            .any(|id| body.instructions[id.index()].op.may_throw(body, types))
        {
            continue;
        }
        body.instructions[read.index()].op = op;
        body.instructions[copy].op = Op::Reinterpret(source);
    }
}
