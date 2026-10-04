//! Fold arithmetic and aliases after representation lowering.
//! Each definition becomes an identity or constant at most twice.
//! Revisit only its consumers. Preserve source positions and instruction IDs.
use crate::ir::*;

pub fn simplify_components(body: &mut Body, types: &Types) {
    let mut users = crate::analysis::ValueUsers::new(body.values.len());
    let mut pending = Vec::new();
    let mut queued = vec![false; body.instructions.len()];
    for (index, inst) in body.instructions.iter().enumerate() {
        if inst.result.is_some()
            && matches!(
                inst.op,
                Op::Binary { .. }
                    | Op::Cast(_)
                    | Op::Reinterpret(_)
                    | Op::Neg(_)
                    | Op::Not(_)
                    | Op::Bit { .. }
                    | Op::Overflow { .. }
                    | Op::Length(_)
            )
        {
            inst.op
                .visit_uses(&body.args, |v| users.connect(body.resolve(v), index));
            pending.push(index);
            queued[index] = true;
        }
    }
    // Visit definitions in order to avoid repeated work for straight-line code.
    // The worklist also handles earlier users of appended components.
    pending.reverse();
    while let Some(index) = pending.pop() {
        queued[index] = false;
        let inst = body.instructions[index];
        let Some(result) = inst.result else {
            continue;
        };
        let folded = match inst.op {
            Op::Reinterpret(value) if body.value_type(value) == body.value_type(result) => Some(
                body.scalar_value(value)
                    .map_or(Folded::Value(value), Folded::Constant),
            ),
            _ => body.fold(types, inst.op, body.value_type(result)),
        };
        let Some(folded) = folded else {
            continue;
        };
        match folded {
            Folded::Value(source) => {
                let source = body.resolve(source);
                if source == result {
                    continue;
                }
                let replacement = if let Some(value) = body.scalar_value(source) {
                    let id = ConstId::new(body.constants.len());
                    body.constants.push(Constant::Scalar(value));
                    Op::Constant(id)
                } else {
                    Op::Reinterpret(source)
                };
                if inst.op == replacement {
                    continue;
                }
                // Retain the result until constant propagation ends.
                // An appended source can become constant later and must reach these consumers.
                body.instructions[index].op = replacement;
            }
            Folded::Constant(value) => {
                let id = ConstId::new(body.constants.len());
                body.constants.push(Constant::Scalar(value));
                body.instructions[index].op = Op::Constant(id);
            }
        }
        for user in users.users(result.index()) {
            if !std::mem::replace(&mut queued[user], true) {
                pending.push(user);
            }
        }
    }
    simplify_control_flow(body);
}

/// Builder joins can expose constants without adding representation instructions.
pub fn simplify_control_flow(body: &mut Body) {
    // Remove exact identities only after the constant worklist has settled.
    for index in 0..body.instructions.len() {
        let inst = body.instructions[index];
        if let (Some(result), Op::Reinterpret(source)) = (inst.result, inst.op)
            && body.value_type(source) == body.value_type(result)
        {
            let source = body.resolve_mut(source);
            body.values[result.index()].def = ValueDef::Alias(source);
            body.instructions[index] = Inst {
                op: Op::Nop,
                result: None,
            };
        }
    }
    for index in 0..body.values.len() {
        body.resolve_mut(ValueId::new(index));
    }
    for index in 0..body.blocks.len() {
        let terminator = body.blocks[index].terminator.unwrap();
        let replacement = match terminator {
            Terminator::Invoke { inst, normal, .. }
                if matches!(body.instructions[inst.index()].op, Op::Nop)
                    || matches!(body.instructions[inst.index()].op, Op::Constant(id) if matches!(body.constants[id.index()], Constant::Scalar(_))) =>
            {
                body.blocks[index].instructions.push(inst);
                Some(Terminator::Jump(normal))
            }
            Terminator::Branch { condition, yes, no } => body
                .scalar_value(condition)
                .map(|value| Terminator::Jump(if value.bits() != 0 { yes } else { no })),
            Terminator::Switch {
                value,
                cases,
                otherwise,
            } => body.scalar_value(value).map(|value| {
                let edge = body.cases[cases.range()]
                    .iter()
                    .find_map(|&(key, edge)| (key == value).then_some(edge))
                    .unwrap_or(otherwise);
                Terminator::Jump(edge)
            }),
            _ => None,
        };
        let replacement = replacement.or_else(|| super::unreachable::simplify(body, terminator));
        if let Some(replacement) = replacement {
            body.blocks[index].terminator = Some(replacement);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scalar::{BinaryOp, Scalar, ScalarType};

    #[test]
    fn constants_reach_earlier_users_through_late_signedness_annotations() {
        let mut types = Types::default();
        let long = types.scalar(ScalarType::I64);
        let unsigned = types.scalar(ScalarType::U64);
        let mut b = Builder::new(&types, long);
        let x = b.parameter(b.current(), long);
        let first = b.emit(Op::Reinterpret(x), Some(long)).unwrap();
        let result = b.emit(Op::Neg(first), Some(long)).unwrap();
        let late = b.emit(Op::Reinterpret(x), Some(unsigned)).unwrap();
        let a = b.constant(unsigned, Scalar::integer(ScalarType::U64, 8).unwrap());
        b.terminate(Terminator::Return(Some(result)));
        let mut body = b.finish().unwrap();
        let ValueDef::Inst(first_inst) = body.values[first.index()].def else {
            panic!()
        };
        let ValueDef::Inst(late_inst) = body.values[late.index()].def else {
            panic!()
        };
        body.instructions[first_inst.index()].op = Op::Reinterpret(late);
        body.instructions[late_inst.index()].op = Op::Binary {
            op: BinaryOp::Mul,
            left: a,
            right: a,
        };
        // Definitions have newer IDs but occur before their users in the block.
        body.blocks[body.entry.index()]
            .instructions
            .sort_by_key(|id| {
                if *id == late_inst {
                    0
                } else if *id == first_inst {
                    1
                } else {
                    2
                }
            });
        simplify_components(&mut body, &types);
        assert_eq!(body.scalar_value(result).unwrap().signed(), Some(-64));
        verify(&body, &types).unwrap();
    }

    // Representation passes append instructions after the builder's fold point.
    // Propagate facts from these newer IDs to older users.
    #[test]
    fn folds_appended_components_without_dropping_traps_or_float_work() {
        let mut types = Types::default();
        let long = types.scalar(ScalarType::I64);
        let float = types.scalar(ScalarType::F64);
        let mut b = Builder::new(&types, long);
        let x = b.parameter(b.current(), long);
        let f = b.parameter(b.current(), float);
        let first = b.emit(Op::Reinterpret(x), Some(long)).unwrap();
        let second = b.emit(Op::Reinterpret(first), Some(long)).unwrap();
        let zero = b.constant(long, Scalar::integer(ScalarType::I64, 0).unwrap());
        let eight = b.constant(long, Scalar::integer(ScalarType::I64, 8).unwrap());
        let trap = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Div,
                    left: zero,
                    right: x,
                },
                Some(long),
            )
            .unwrap();
        let float_zero = b.constant(float, Scalar::f64(0.0));
        let float_sum = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Add,
                    left: f,
                    right: float_zero,
                },
                Some(float),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(second)));
        let mut body = b.finish().unwrap();
        let ValueDef::Inst(id) = body.values[first.index()].def else {
            panic!()
        };
        body.instructions[id.index()].op = Op::Binary {
            op: BinaryOp::Mul,
            left: eight,
            right: eight,
        };
        simplify_components(&mut body, &types);
        assert_eq!(body.scalar_value(second).unwrap().bits(), 64);
        assert!(body.scalar_value(trap).is_none());
        assert!(body.scalar_value(float_sum).is_none());
        verify(&body, &types).unwrap();
        let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        assert!(
            code.instructions
                .contains(&crate::classfile::attributes::Instruction::Ldiv)
        );
        assert!(
            !code
                .instructions
                .contains(&crate::classfile::attributes::Instruction::Lmul)
        );
    }
}
