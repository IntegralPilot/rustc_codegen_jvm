use super::promote_cells;
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};

fn cell(b: &mut Builder<'_>, ty: TypeId, pointer: TypeId, initial: ValueId) -> ValueId {
    let method = b.method(MethodRef {
        owner: "TestCells".into(),
        name: "allocate".into(),
        params: vec![ty],
        returns: pointer,
        interface: false,
    });
    let args = b.args([initial]);
    b.emit(
        Op::Call {
            method,
            kind: CallKind::JvmStatic,
            args,
        },
        Some(pointer),
    )
    .unwrap()
}

#[test]
fn promotes_loop_carried_contents_to_ssa_parameters() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let pointer = types.intern(Type::Pointer(int));
    let mut b = Builder::new(&types, int);
    let limit = b.parameter(b.current(), int);
    let zero = b.constant(int, Scalar::integer(ScalarType::I64, 0).unwrap());
    let storage = cell(&mut b, int, pointer, zero);
    let header = b.create_block();
    let step = b.create_block();
    let done = b.create_block();
    b.jump(header, vec![]);
    b.switch_to(header);
    let value = b.emit(Op::Load(storage), Some(int)).unwrap();
    let less = b
        .emit(
            Op::Binary {
                op: BinaryOp::Lt,
                left: value,
                right: limit,
            },
            Some(boolean),
        )
        .unwrap();
    b.branch(less, step, done);
    b.switch_to(step);
    let one = b.constant(int, Scalar::integer(ScalarType::I64, 1).unwrap());
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: value,
                right: one,
            },
            Some(int),
        )
        .unwrap();
    b.emit(
        Op::Store {
            pointer: storage,
            value: next,
        },
        None,
    );
    b.jump(header, vec![]);
    b.switch_to(done);
    let result = b.emit(Op::Load(storage), Some(int)).unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let body = promote_cells(b.finish().unwrap(), &types, &[(storage, zero)]).unwrap();
    verify(&body, &types).unwrap();
    assert_eq!(body.blocks[header.index()].params.len(), 1);
    assert!(
        !body
            .instructions
            .iter()
            .any(|i| matches!(i.op, Op::Load(_) | Op::Store { .. } | Op::Call { .. }))
    );
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    assert!(code.max_locals <= 8);
}

#[test]
fn keeps_storage_when_its_address_escapes() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I64);
    let pointer = types.intern(Type::Pointer(int));
    let mut b = Builder::new(&types, pointer);
    let zero = b.constant(int, Scalar::integer(ScalarType::I64, 0).unwrap());
    let storage = cell(&mut b, int, pointer, zero);
    let alias = b.emit(Op::Reinterpret(storage), Some(pointer)).unwrap();
    b.terminate(Terminator::Return(Some(alias)));
    let before = b.finish().unwrap();
    let body = promote_cells(before.clone(), &types, &[(storage, zero)]).unwrap();
    assert_eq!(body, before);
}

#[test]
fn promoted_contents_reach_the_unwind_edge_at_the_throwing_call() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I64);
    let pointer = types.intern(Type::Pointer(int));
    let unit = types.intern(Type::Unit);
    let throwable = types.symbol("java/lang/Throwable");
    let throwable = types.intern(Type::Class(throwable));
    let mut b = Builder::new(&types, int);
    let zero = b.constant(int, Scalar::integer(ScalarType::I64, 0).unwrap());
    let allocation_failure = b.create_block();
    let handler = b.create_block();
    let allocation = b.method(MethodRef {
        owner: "TestCells".into(),
        name: "allocate".into(),
        params: vec![int],
        returns: pointer,
        interface: false,
    });
    let args = b.args([zero]);
    let storage = b
        .invoke(
            Op::Call {
                method: allocation,
                kind: CallKind::JvmStatic,
                args,
            },
            Some(pointer),
            allocation_failure,
        )
        .unwrap();
    let one = b.constant(int, Scalar::integer(ScalarType::I64, 1).unwrap());
    b.emit(
        Op::Store {
            pointer: storage,
            value: one,
        },
        None,
    );
    let throwing = b.method(MethodRef {
        owner: "TestCells".into(),
        name: "mayThrow".into(),
        params: vec![],
        returns: unit,
        interface: false,
    });
    let args = b.args([]);
    b.invoke(
        Op::Call {
            method: throwing,
            kind: CallKind::JvmStatic,
            args,
        },
        None,
        handler,
    );
    b.terminate(Terminator::Return(Some(one)));
    b.switch_to(allocation_failure);
    b.emit(Op::Exception, Some(throwable));
    b.terminate(Terminator::Return(Some(zero)));
    b.switch_to(handler);
    b.emit(Op::Exception, Some(throwable));
    let loaded = b.emit(Op::Load(storage), Some(int)).unwrap();
    b.terminate(Terminator::Return(Some(loaded)));
    let body = promote_cells(b.finish().unwrap(), &types, &[(storage, zero)]).unwrap();
    verify(&body, &types).unwrap();
    let ValueDef::Inst(id) = body.values[loaded.index()].def else {
        unreachable!()
    };
    let Op::Reinterpret(value) = body.instructions[id.index()].op else {
        panic!("cell load was not promoted")
    };
    assert_eq!(body.resolve(value), one);
    assert_eq!(
        body.blocks
            .iter()
            .filter(|b| matches!(b.terminator, Some(Terminator::Invoke { .. })))
            .count(),
        1
    );
}

#[test]
fn follows_long_alias_chains_for_both_loads_and_escapes() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I64);
    let pointer = types.intern(Type::Pointer(int));
    for escapes in [false, true] {
        let mut b = Builder::new(&types, if escapes { pointer } else { int });
        let initial = b.constant(int, Scalar::integer(ScalarType::I64, 42).unwrap());
        let storage = cell(&mut b, int, pointer, initial);
        let mut alias = storage;
        for _ in 0..1024 {
            alias = b.emit(Op::Reinterpret(alias), Some(pointer)).unwrap();
        }
        let result = if escapes {
            alias
        } else {
            b.emit(Op::Load(alias), Some(int)).unwrap()
        };
        b.terminate(Terminator::Return(Some(result)));
        let before = b.finish().unwrap();
        let body = promote_cells(before.clone(), &types, &[(storage, initial)]).unwrap();
        verify(&body, &types).unwrap();
        if escapes {
            assert_eq!(body, before);
        } else {
            let ValueDef::Inst(inst) = body.values[result.index()].def else {
                unreachable!()
            };
            assert_eq!(body.instructions[inst.index()].op, Op::Reinterpret(initial));
        }
    }
}
