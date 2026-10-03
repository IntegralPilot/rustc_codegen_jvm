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
fn private_array_cells_disappear_but_owned_loads_still_copy() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let array = types.intern(Type::Array(byte));
    let pointer = types.intern(Type::Pointer(array));
    let mut b = Builder::new(&types, array);
    let initial = b.parameter(b.current(), array);
    let storage = cell(&mut b, array, pointer, initial);
    let copied = b.emit(Op::LoadCopy(storage), Some(array)).unwrap();
    b.terminate(Terminator::Return(Some(copied)));
    let body = promote_cells(b.finish().unwrap(), &types, &[(storage, initial)]).unwrap();
    verify(&body, &types).unwrap();
    assert!(
        body.instructions
            .iter()
            .any(|i| i.op == Op::CopyValue(initial))
    );
    assert!(
        !body
            .instructions
            .iter()
            .any(|i| matches!(i.op, Op::Call { .. } | Op::LoadCopy(_)))
    );
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

#[test]
fn projected_borrow_follows_replacement_of_private_aggregate_storage() {
    let mut types = Types::default();
    types.intern(Type::Unit);
    let int = types.scalar(ScalarType::I64);
    let name = types.symbol("Counter");
    let object = types.intern(Type::Class(name));
    let pointer = types.intern(Type::Pointer(object));
    let field_pointer = types.intern(Type::Pointer(int));
    for escapes in [false, true] {
        let mut b = Builder::new(&types, if escapes { field_pointer } else { int });
        let first = b.parameter(b.current(), object);
        let second = b.parameter(b.current(), object);
        let storage = cell(&mut b, object, pointer, first);
        let field = b.field(FieldRef {
            owner: object,
            name: "count".into(),
            ty: int,
            is_static: false,
        });
        let projection = b.projection(PointerProjection {
            parent: None,
            field,
            offset: 0,
            size: 8,
            codec: None,
        });
        let borrow = b
            .emit(
                Op::Project {
                    base: storage,
                    projection,
                },
                Some(field_pointer),
            )
            .unwrap();
        let before = b.emit(Op::Load(borrow), Some(int)).unwrap();
        b.emit(
            Op::Store {
                pointer: storage,
                value: second,
            },
            None,
        );
        b.emit(
            Op::Store {
                pointer: borrow,
                value: before,
            },
            None,
        );
        let after = b.emit(Op::Load(borrow), Some(int)).unwrap();
        b.terminate(Terminator::Return(Some(if escapes {
            borrow
        } else {
            after
        })));
        let mut body = b.finish().unwrap();
        super::promote_fields(&mut body, &types);
        let original = body.clone();
        let body = promote_cells(body, &types, &[(storage, first)]).unwrap();
        verify(&body, &types).unwrap();
        if escapes {
            assert_eq!(body, original);
            continue;
        }
        let read = |value: ValueId| {
            let ValueDef::Inst(inst) = body.values[value.index()].def else {
                panic!()
            };
            body.instructions[inst.index()].op
        };
        assert_eq!(
            read(before),
            Op::GetField {
                object: first,
                field
            }
        );
        assert_eq!(
            read(after),
            Op::GetField {
                object: second,
                field
            }
        );
        assert!(body.instructions.iter().any(|i| i.op
            == Op::SetField {
                object: second,
                field,
                value: before,
            }));
        let live = super::live(&body, &types);
        assert!(!live.values[storage.index()] && !live.values[borrow.index()]);
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn equal_allocation_origins_cross_control_flow_joins() {
    for distinct in [false, true] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I64);
        let boolean = types.scalar(ScalarType::Bool);
        let pointer = types.intern(Type::Pointer(int));
        let mut b = Builder::new(&types, int);
        let condition = b.parameter(b.current(), boolean);
        let initial = b.constant(int, Scalar::integer(ScalarType::I64, 7).unwrap());
        let first = cell(&mut b, int, pointer, initial);
        let second = if distinct {
            cell(&mut b, int, pointer, initial)
        } else {
            first
        };
        let left = b.create_block();
        let right = b.create_block();
        let join = b.create_block();
        let selected = b.parameter(join, pointer);
        b.branch(condition, left, right);
        b.switch_to(left);
        let alias = b.emit(Op::Reinterpret(first), Some(pointer)).unwrap();
        b.jump(join, vec![alias]);
        b.switch_to(right);
        let alias = b.emit(Op::Refine(second), Some(pointer)).unwrap();
        b.jump(join, vec![alias]);
        b.switch_to(join);
        let next = b.constant(int, Scalar::integer(ScalarType::I64, 11).unwrap());
        b.emit(
            Op::Store {
                pointer: selected,
                value: next,
            },
            None,
        );
        let result = b.emit(Op::Load(first), Some(int)).unwrap();
        b.terminate(Terminator::Return(Some(result)));
        let mut cells = vec![(first, initial)];
        if distinct {
            cells.push((second, initial));
        }
        let body = promote_cells(b.finish().unwrap(), &types, &cells).unwrap();
        verify(&body, &types).unwrap();
        let retains_storage = body
            .instructions
            .iter()
            .any(|i| matches!(i.op, Op::Call { .. }));
        assert_eq!(retains_storage, distinct);
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn nonentry_allocations_remain_conservative_at_joins() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let pointer = types.intern(Type::Pointer(int));
    let mut b = Builder::new(&types, int);
    let again = b.parameter(b.current(), boolean);
    let initial = b.constant(int, Scalar::integer(ScalarType::I64, 7).unwrap());
    let allocate = b.create_block();
    let join = b.create_block();
    let repeat = b.create_block();
    let done = b.create_block();
    let selected = b.parameter(join, pointer);
    b.jump(allocate, vec![]);
    b.switch_to(allocate);
    let storage = cell(&mut b, int, pointer, initial);
    b.jump(join, vec![storage]);
    b.switch_to(join);
    let value = b.emit(Op::Load(selected), Some(int)).unwrap();
    b.branch(again, repeat, done);
    b.switch_to(repeat);
    let alias = b.emit(Op::Reinterpret(storage), Some(pointer)).unwrap();
    b.jump(join, vec![alias]);
    b.switch_to(done);
    b.terminate(Terminator::Return(Some(value)));
    let body = promote_cells(b.finish().unwrap(), &types, &[(storage, initial)]).unwrap();
    // Only the entry region has a single-execution proof.
    // This pass must retain allocations elsewhere, even if another proof could remove them.
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::Call { .. }))
    );
    verify(&body, &types).unwrap();
}
