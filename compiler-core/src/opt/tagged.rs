//! Keep tagged scalar products decomposed through joins and loop backedges.
use crate::ir::*;

pub fn decompose_tagged(body: &mut Body, types: &Types, debug: Option<&mut DebugInfo>) {
    if !body
        .instructions
        .iter()
        .any(|i| matches!(i.op, Op::TaggedPack(_) | Op::TaggedPart { .. }))
    {
        return;
    }
    let count = body.values.len();
    let mut eligible = vec![false; count];
    let mut users = crate::analysis::ValueUsers::new(count);
    let predecessors = body.predecessors();
    for (index, value) in body.values.iter().enumerate() {
        if types.get(value.ty) != Some(Type::TaggedI64) {
            continue;
        }
        match value.def {
            ValueDef::Inst(id) => match body.instructions[id.index()].op {
                Op::TaggedPack(_) => eligible[index] = true,
                Op::Reinterpret(source) | Op::Adapt(source) | Op::Refine(source) => {
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
                    .position(|v| v.index() == index)
                    .unwrap();
                for &(_, edge) in &predecessors[block.index()] {
                    users.connect(body.edges[edge.index()].args[position], index);
                }
            }
            _ => {}
        }
    }
    users.close(&mut eligible);
    let long = types
        .find(Type::Scalar(crate::scalar::ScalarType::I64))
        .unwrap();
    let mut components = vec![None::<[ValueId; 2]>; count];
    let mut joins = vec![Vec::new(); body.blocks.len()];
    for index in 0..count {
        if !eligible[index] {
            continue;
        }
        match body.values[index].def {
            ValueDef::Inst(id) => {
                if let Op::TaggedPack(parts) = body.instructions[id.index()].op {
                    components[index] = Some(body.args[parts.range()].try_into().unwrap());
                }
            }
            ValueDef::Param(block) => {
                let parts = std::array::from_fn(|_| {
                    let value = ValueId::new(body.values.len());
                    body.values.push(Value {
                        ty: long,
                        def: ValueDef::Param(block),
                    });
                    value
                });
                let position = body.blocks[block.index()]
                    .params
                    .iter()
                    .position(|v| v.index() == index)
                    .unwrap();
                joins[block.index()].push((index, position));
                components[index] = Some(parts);
            }
            _ => {}
        }
    }
    users.propagate(&eligible, &mut components);
    for (block, joins) in joins.iter_mut().enumerate() {
        joins.sort_unstable_by_key(|&(_, position)| position);
        let mut prologue = Vec::new();
        for &(index, position) in joins.iter().rev() {
            let parts = components[index].unwrap();
            body.blocks[block]
                .params
                .splice(position..position + 1, parts);
            for &(_, edge) in &predecessors[block] {
                let input = body.edges[edge.index()].args[position];
                body.edges[edge.index()]
                    .args
                    .splice(position..position + 1, components[input.index()].unwrap());
            }
            let inst = InstId::new(body.instructions.len());
            body.values[index].def = ValueDef::Inst(inst);
            body.instructions.push(Inst {
                op: Op::TaggedPack(List::append(&mut body.args, parts)),
                result: Some(ValueId::new(index)),
            });
            prologue.push(inst);
        }
        prologue.append(&mut body.blocks[block].instructions);
        body.blocks[block].instructions = prologue;
    }
    for inst in &mut body.instructions {
        if let Op::TaggedPart { value, index } = inst.op {
            if let Some(Some(parts)) = components.get(value.index()) {
                inst.op = Op::Reinterpret(parts[index as usize]);
            }
        }
    }
    if let Some(debug) = debug {
        for event in &mut debug.events {
            event.position += joins[event.block.index()].len() as u32;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scalar::{BinaryOp, Scalar, ScalarType};

    #[test]
    fn loop_joins_retain_scalars_without_a_boundary_carrier() {
        let mut types = Types::default();
        let long = types.scalar(ScalarType::I64);
        let boolean = types.scalar(ScalarType::Bool);
        let tagged = types.intern(Type::TaggedI64);
        let mut b = Builder::new(&types, long);
        let limit = b.parameter(b.current(), long);
        let zero = b.constant(long, Scalar::integer(ScalarType::I64, 0).unwrap());
        let one = b.constant(long, Scalar::integer(ScalarType::I64, 1).unwrap());
        let parts = b.args([zero, one]);
        let first = b.emit(Op::TaggedPack(parts), Some(tagged)).unwrap();
        let value = b.variable(tagged);
        b.define(value, first);
        let header = b.create_block();
        let step = b.create_block();
        let done = b.create_block();
        b.jump(header, vec![]);
        b.switch_to(header);
        let pair = b.read(value);
        let payload = b
            .emit(
                Op::TaggedPart {
                    value: pair,
                    index: 0,
                },
                Some(long),
            )
            .unwrap();
        let more = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Lt,
                    left: payload,
                    right: limit,
                },
                Some(boolean),
            )
            .unwrap();
        b.branch(more, step, done);
        b.switch_to(step);
        let next = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Add,
                    left: payload,
                    right: one,
                },
                Some(long),
            )
            .unwrap();
        let parts = b.args([next, one]);
        let pair = b.emit(Op::TaggedPack(parts), Some(tagged)).unwrap();
        b.define(value, pair);
        b.jump(header, vec![]);
        b.switch_to(done);
        b.terminate(Terminator::Return(Some(payload)));
        let mut body = b.finish().unwrap();
        decompose_tagged(&mut body, &types, None);
        verify(&body, &types).unwrap();
        let live = crate::opt::live(&body, &types);
        assert!(
            !body
                .instructions
                .iter()
                .enumerate()
                .any(|(index, inst)| live.instructions[index]
                    && matches!(inst.op, Op::TaggedPack(_) | Op::TaggedPart { .. }))
        );
        let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        assert!(
            !code
                .instructions
                .iter()
                .any(|i| matches!(i, crate::classfile::attributes::Instruction::New(_)))
        );
    }
}
