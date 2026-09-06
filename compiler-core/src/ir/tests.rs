use super::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};

#[test]
fn construction_prunes_constant_control_flow_without_reading_dead_bindings() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let boolean = types.scalar(ScalarType::Bool);
    let mut b = Builder::new(&types, int);
    let dead = b.create_block();
    let live = b.create_block();
    let condition = b.constant(boolean, Scalar::boolean(false));
    b.branch(condition, dead, live);
    b.switch_to(dead);
    let missing = b.variable(int);
    let value = b.read(missing);
    b.terminate(Terminator::Return(Some(value)));
    b.switch_to(live);
    let value = integer(&mut b, int, 17);
    let result = b.create_block();
    b.switch(
        value,
        [(Scalar::integer(ScalarType::I32, 17).unwrap(), result)],
        dead,
    );
    b.switch_to(result);
    b.terminate(Terminator::Return(Some(value)));
    let body = b.finish().unwrap();
    assert!(!body.reachable()[dead.index()]);
    assert_eq!(execute(&body, &[]).bits(), 17);
}

fn integer(builder: &mut Builder<'_>, ty: TypeId, n: i32) -> ValueId {
    builder.constant(ty, Scalar::integer(ScalarType::I32, n as u128).unwrap())
}

#[test]
fn merges_mutable_source_bindings_with_block_parameters() {
    let mut types = Types::default();
    let i32 = types.scalar(ScalarType::I32);
    let boolean = types.scalar(ScalarType::Bool);
    let mut b = Builder::new(&types, i32);
    let condition = b.parameter(b.current(), boolean);
    let var = b.variable(i32);
    let yes = b.create_block();
    let no = b.create_block();
    let join = b.create_block();
    b.branch(condition, yes, no);
    b.switch_to(yes);
    let one = integer(&mut b, i32, 1);
    b.define(var, one);
    b.jump(join, vec![]);
    b.switch_to(no);
    let two = integer(&mut b, i32, 2);
    b.define(var, two);
    b.jump(join, vec![]);
    b.switch_to(join);
    let value = b.read(var);
    b.terminate(Terminator::Return(Some(value)));
    let body = b.finish().unwrap();
    assert_eq!(body.blocks[join.index()].params, vec![value]);
    assert_eq!(execute(&body, &[Scalar::boolean(true)]).bits(), 1);
    assert_eq!(execute(&body, &[Scalar::boolean(false)]).bits(), 2);
}

#[test]
fn loop_backedges_keep_the_induction_value_in_ssa() {
    let mut types = Types::default();
    let i32 = types.scalar(ScalarType::I32);
    let boolean = types.scalar(ScalarType::Bool);
    let mut b = Builder::new(&types, i32);
    let count = b.parameter(b.current(), i32);
    let var = b.variable(i32);
    let zero = integer(&mut b, i32, 0);
    b.define(var, zero);
    let one = integer(&mut b, i32, 1);
    let header = b.create_block();
    let update = b.create_block();
    let exit = b.create_block();
    b.jump(header, vec![]);
    b.switch_to(header);
    let n = b.read(var);
    let cond = b
        .emit(
            Op::Binary {
                op: BinaryOp::Lt,
                left: n,
                right: count,
            },
            Some(boolean),
        )
        .unwrap();
    b.branch(cond, update, exit);
    b.switch_to(update);
    let previous = b.read(var);
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: previous,
                right: one,
            },
            Some(i32),
        )
        .unwrap();
    b.define(var, next);
    b.jump(header, vec![]);
    b.switch_to(exit);
    let value = b.read(var);
    b.terminate(Terminator::Return(Some(value)));
    let body = b.finish().unwrap();
    assert_eq!(body.blocks[header.index()].params.len(), 1);
    assert!(body.blocks[update.index()].params.is_empty());
    assert!(body.blocks[exit.index()].params.is_empty());
    for n in [0, 1, 20] {
        assert_eq!(
            execute(&body, &[Scalar::integer(ScalarType::I32, n).unwrap()]).bits(),
            n
        );
    }
}

fn throwing_body() -> (Types, Body, ValueId, BlockId) {
    let mut types = Types::default();
    let i32 = types.scalar(ScalarType::I32);
    let mut b = Builder::new(&types, i32);
    let divisor = b.parameter(b.current(), i32);
    let source = b.variable(i32);
    let seven = integer(&mut b, i32, 7);
    b.define(source, seven);
    let handler = b.create_block();
    let result = b
        .invoke(
            Op::Binary {
                op: BinaryOp::Div,
                left: seven,
                right: divisor,
            },
            Some(i32),
            handler,
        )
        .unwrap();
    b.define(source, result);
    b.terminate(Terminator::Return(Some(result)));
    b.switch_to(handler);
    let old = b.read(source);
    b.terminate(Terminator::Return(Some(old)));
    let body = b.finish().unwrap();
    (types, body, result, handler)
}

#[test]
fn exceptional_edge_keeps_old_binding_and_normal_edge_gets_result() {
    let (_, body, _, _) = throwing_body();
    assert_eq!(
        execute(&body, &[Scalar::integer(ScalarType::I32, 0).unwrap()]).bits(),
        7
    );
    assert_eq!(
        execute(&body, &[Scalar::integer(ScalarType::I32, 7).unwrap()]).bits(),
        1
    );
}

#[test]
fn verifier_rejects_a_call_result_used_on_the_exception_path() {
    let (types, mut body, result, handler) = throwing_body();
    body.blocks[handler.index()].terminator = Some(Terminator::Return(Some(result)));
    assert!(verify(&body, &types).unwrap_err().0.contains("dominate"));
}

#[test]
fn undefined_reads_and_alias_cycles_are_errors() {
    let mut types = Types::default();
    let i32 = types.scalar(ScalarType::I32);
    let mut b = Builder::new(&types, i32);
    let variable = b.variable(i32);
    let value = b.read(variable);
    b.terminate(Terminator::Return(Some(value)));
    assert!(b.finish().unwrap_err().0.contains("undefined"));
    let mut body = Body::new(i32);
    body.values.push(Value {
        ty: i32,
        def: ValueDef::Alias(ValueId::new(0)),
    });
    assert!(verify(&body, &types).unwrap_err().0.contains("cycle"));
}

#[test]
fn deep_graph_construction_and_verification_do_not_recurse_on_the_host_stack() {
    let mut types = Types::default();
    let i32 = types.scalar(ScalarType::I32);
    let mut b = Builder::new(&types, i32);
    let variable = b.variable(i32);
    let one = integer(&mut b, i32, 1);
    b.define(variable, one);
    for _ in 0..10_000 {
        let next = b.create_block();
        b.jump(next, vec![]);
        b.switch_to(next);
    }
    let value = b.read(variable);
    b.terminate(Terminator::Return(Some(value)));
    let body = b.finish().unwrap();
    assert_eq!(body.resolve(value), one);
    assert!(body.blocks.iter().all(|block| block.params.is_empty()));
    assert_eq!(
        body.values.len(),
        2,
        "empty blocks do not allocate SSA values"
    );
}

#[test]
fn identifiers_and_instruction_records_have_bounded_inline_sizes() {
    assert_eq!(std::mem::size_of::<ValueId>(), 4);
    assert_eq!(std::mem::size_of::<Option<ValueId>>(), 4);
    assert_eq!(std::mem::size_of::<Scalar>(), 24);
    assert!(std::mem::size_of::<Inst>() <= 32);
    assert!(std::mem::size_of::<Value>() <= 16);
}

/// Small semantic oracle for IR tests. JVM execution separately checks emission.
fn execute(body: &Body, arguments: &[Scalar]) -> Scalar {
    let mut values = vec![None; body.values.len()];
    let get = |values: &[Option<Scalar>], v| values[body.resolve(v).index()].unwrap();
    for (&param, &value) in body.blocks[body.entry.index()].params.iter().zip(arguments) {
        values[param.index()] = Some(value);
    }
    let evaluate =
        |inst: InstId, values: &[Option<Scalar>]| match body.instructions[inst.index()].op {
            Op::Constant(id) => match body.constants[id.index()] {
                Constant::Scalar(v) => Some(v),
                _ => panic!("unsupported constant"),
            },
            Op::Binary { op, left, right } => get(values, left).binary(op, get(values, right)),
            _ => panic!("unsupported test instruction"),
        };
    let mut block = body.entry;
    for _ in 0..100_000 {
        for &inst in &body.blocks[block.index()].instructions {
            let value = evaluate(inst, &values).expect("unexpected trap");
            values[body.instructions[inst.index()].result.unwrap().index()] = Some(value);
        }
        let edge = match body.blocks[block.index()].terminator.unwrap() {
            Terminator::Return(Some(v)) => return get(&values, v),
            Terminator::Jump(e) => e,
            Terminator::Branch { condition, yes, no } => {
                if get(&values, condition).bits() != 0 {
                    yes
                } else {
                    no
                }
            }
            Terminator::Invoke {
                inst,
                normal,
                unwind,
            } => match evaluate(inst, &values) {
                Some(value) => {
                    values[body.instructions[inst.index()].result.unwrap().index()] = Some(value);
                    normal
                }
                None => unwind,
            },
            _ => panic!("unsupported test terminator"),
        };
        let edge = &body.edges[edge.index()];
        let args = edge
            .args
            .iter()
            .map(|&v| get(&values, v))
            .collect::<Vec<_>>();
        block = edge.target;
        for (&param, value) in body.blocks[block.index()].params.iter().zip(args) {
            values[param.index()] = Some(value);
        }
    }
    panic!("test instruction budget exhausted")
}

#[test]
fn source_local_initial_state_is_explicit_and_temporaries_remain_strict() {
    let mut types = Types::default();
    let int = types.scalar(crate::scalar::ScalarType::I32);
    let flag = types.scalar(crate::scalar::ScalarType::Bool);
    for source_local in [false, true] {
        let mut b = Builder::new(&types, int);
        let condition = b.parameter(b.current(), flag);
        let local = if source_local {
            b.local(int)
        } else {
            b.variable(int)
        };
        let assigned = b.create_block();
        let joined = b.create_block();
        let observed = b.create_block();
        let empty = b.create_block();
        b.branch(condition, assigned, joined);
        b.switch_to(assigned);
        let value = b.constant(
            int,
            crate::scalar::Scalar::integer(crate::scalar::ScalarType::I32, 7).unwrap(),
        );
        b.define(local, value);
        b.jump(joined, vec![]);
        b.switch_to(joined);
        b.branch(condition, observed, empty);
        b.switch_to(observed);
        let value = b.read(local);
        b.terminate(Terminator::Return(Some(value)));
        b.switch_to(empty);
        let zero = b.constant(
            int,
            crate::scalar::Scalar::integer(crate::scalar::ScalarType::I32, 0).unwrap(),
        );
        b.terminate(Terminator::Return(Some(zero)));
        let result = b.finish();
        if source_local {
            let body = result.unwrap();
            assert_eq!(
                body.constants
                    .iter()
                    .filter(|c| matches!(c, Constant::Uninit(_)))
                    .count(),
                1
            );
        } else {
            assert!(result.unwrap_err().0.contains("undefined variable"));
        }
    }
}
