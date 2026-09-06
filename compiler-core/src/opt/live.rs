use crate::ir::*;

pub struct Live {
    pub values: Vec<bool>,
    /// Saturating live use counts, including ABI/debug roots and edge copies.
    pub uses: Vec<u8>,
    pub instructions: Vec<bool>,
    pub blocks: Vec<bool>,
}

/// Mark backwards from control flow and ordered effects. SSA definitions make a
/// separate per-instruction use table unnecessary for this analysis. Unused
/// parameter cycles die together; live parameters follow their incoming edges.
pub fn live(body: &Body, types: &Types) -> Live {
    live_with_roots(body, types, std::iter::empty())
}

pub fn live_with_roots(
    body: &Body,
    types: &Types,
    roots: impl IntoIterator<Item = ValueId>,
) -> Live {
    let mut live = Live {
        values: vec![false; body.values.len()],
        uses: vec![0; body.values.len()],
        instructions: vec![false; body.instructions.len()],
        blocks: body.reachable(),
    };
    let mut pending = roots.into_iter().collect::<Vec<_>>();
    let mut positions = vec![0; body.values.len()];
    let predecessors = body.predecessors();
    for (index, block) in body.blocks.iter().enumerate() {
        for (position, &param) in block.params.iter().enumerate() {
            positions[param.index()] = position;
        }
        if !live.blocks[index] {
            continue;
        }
        for &inst in &block.instructions {
            if has_effects(body.instructions[inst.index()].op, body, types) {
                live.instructions[inst.index()] = true;
                body.instructions[inst.index()]
                    .op
                    .visit_uses(&body.args, |v| pending.push(v));
            }
        }
        let term = block.terminator.unwrap();
        term.visit_uses(|v| pending.push(v));
        if let Terminator::Invoke { inst, .. } = term {
            live.instructions[inst.index()] = true;
            body.instructions[inst.index()]
                .op
                .visit_uses(&body.args, |v| pending.push(v));
        }
    }
    // Signature parameters remain available at their ABI slots even if unused.
    pending.extend(&body.blocks[body.entry.index()].params);
    while let Some(value) = pending.pop() {
        let value = body.resolve(value);
        live.uses[value.index()] = live.uses[value.index()].saturating_add(1);
        if std::mem::replace(&mut live.values[value.index()], true) {
            continue;
        }
        match body.values[value.index()].def {
            ValueDef::Inst(inst) => {
                if !std::mem::replace(&mut live.instructions[inst.index()], true) {
                    body.instructions[inst.index()]
                        .op
                        .visit_uses(&body.args, |v| pending.push(v));
                }
            }
            ValueDef::Param(block) => {
                for &(source, edge) in &predecessors[block.index()] {
                    if live.blocks[source.index()] {
                        pending.push(body.edges[edge.index()].args[positions[value.index()]]);
                    }
                }
            }
            ValueDef::Unreachable => {}
            ValueDef::Alias(_) => unreachable!(),
        }
    }
    live
}

fn has_effects(op: Op, body: &Body, types: &Types) -> bool {
    // An unused fat-pointer carrier has no observable identity.
    !matches!(op, Op::View { .. }) && op.may_throw(body, types)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scalar::{BinaryOp, Scalar, ScalarType};
    #[test]
    fn discards_dead_scalar_chains_but_keeps_potentially_throwing_division() {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let mut b = Builder::new(&types, int);
        let divisor = b.parameter(b.current(), int);
        let mut previous = b.constant(int, Scalar::integer(ScalarType::I32, 7).unwrap());
        for _ in 0..100_000 {
            previous = b
                .emit(
                    Op::Binary {
                        op: BinaryOp::Add,
                        left: previous,
                        right: divisor,
                    },
                    Some(int),
                )
                .unwrap();
        }
        let seven = b.constant(int, Scalar::integer(ScalarType::I32, 7).unwrap());
        let divided = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Div,
                    left: seven,
                    right: divisor,
                },
                Some(int),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(divisor)));
        let body = b.finish().unwrap();
        let live = live(&body, &types);
        assert!(!live.values[previous.index()]);
        assert!(!live.values[divided.index()]);
        assert_eq!(live.instructions.iter().filter(|&&x| x).count(), 2);
        let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        // The literal numerator is rematerialized; only the argument needs a local.
        assert_eq!(code.max_locals, 1);
        assert!(
            code.instructions
                .contains(&crate::classfile::attributes::Instruction::Idiv)
        );
    }
}
