//! Keep a single-use result on the operand stack when the next operation loads
//! it first. Nothing moves across an effect, a block edge or a debug event.
use super::*;

pub(super) fn values(body: &Body, live: &crate::opt::Live, debug: Option<&DebugInfo>) -> Vec<bool> {
    let mut forwarded = vec![false; body.values.len()];
    // Debug mirrors can insert loads between any two SSA instructions.
    if debug.is_some() {
        return forwarded;
    }
    let mut connect = |previous: Option<ValueId>, operand: Option<ValueId>| {
        if let (Some(previous), Some(operand)) = (previous, operand) {
            let operand = body.resolve(operand);
            if previous == operand && live.uses[operand.index()] == 1 {
                forwarded[operand.index()] = true;
            }
        }
    };
    for (index, block) in body.blocks.iter().enumerate() {
        if !live.blocks[index] {
            continue;
        }
        let mut previous = None;
        for &id in &block.instructions {
            let inst = body.instructions[id.index()];
            if !live.instructions[id.index()]
                || inst
                    .result
                    .is_some_and(|value| literal(body, value).is_some())
            {
                continue;
            }
            connect(previous, first_operand(body, inst.op));
            previous = inst.result;
        }
        connect(
            previous,
            match block.terminator.unwrap() {
                Terminator::Return(value) => value,
                Terminator::Branch { condition, .. } => Some(condition),
                Terminator::Throw {
                    value,
                    unwind: None,
                } => Some(value),
                _ => None,
            },
        );
    }
    forwarded
}

/// These selectors load the operand exactly once, before emitting any other
/// bytecode. Relative fields and switches may load one SSA operand repeatedly.
fn first_operand(body: &Body, op: Op) -> Option<ValueId> {
    match op {
        Op::Binary { left, .. } => Some(left),
        Op::Neg(value)
        | Op::Not(value)
        | Op::Cast(value)
        | Op::Reinterpret(value)
        | Op::Adapt(value)
        | Op::NewArray(value)
        | Op::ArrayLength(value)
        | Op::Opaque(value)
        | Op::Load(value)
        | Op::Length(value)
        | Op::SetStatic { value, .. } => Some(value),
        Op::Project { base, .. } => Some(base),
        Op::Offset { pointer, .. } | Op::Store { pointer, .. } => Some(pointer),
        Op::ViewData { view, .. } => Some(view),
        Op::GetField { object, field } | Op::SetField { object, field, .. }
            if !body.fields[field.index()].relative_pointer =>
        {
            Some(object)
        }
        Op::Call { kind, args, .. } if kind != CallKind::Constructor => {
            body.args[args.range()].first().copied()
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn forwards_chains_without_locals_but_preserves_multiple_uses() {
        for scalar in [
            ScalarType::I32,
            ScalarType::I64,
            ScalarType::F32,
            ScalarType::F64,
        ] {
            let mut types = Types::default();
            let ty = types.scalar(scalar);
            for duplicate in [false, true] {
                let mut b = Builder::new(&types, ty);
                let arg = b.parameter(b.current(), ty);
                let negated = b.emit(Op::Neg(arg), Some(ty)).unwrap();
                let sum = b
                    .emit(
                        Op::Binary {
                            op: BinaryOp::Add,
                            left: negated,
                            right: if duplicate { negated } else { arg },
                        },
                        Some(ty),
                    )
                    .unwrap();
                b.terminate(Terminator::Return(Some(sum)));
                let body = b.finish().unwrap();
                let code = compile(&body, &types, &mut Default::default()).unwrap();
                let width = kind(scalar).unwrap().width();
                assert_eq!(code.max_locals, width);
                assert_eq!(code.instructions.len(), if duplicate { 7 } else { 5 });
            }
        }
    }
}
