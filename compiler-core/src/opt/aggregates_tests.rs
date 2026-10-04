use super::promote_aggregates;
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};

#[test]
fn fresh_arrays_transfer_after_initialization_but_not_with_later_or_escaping_uses() {
    for mode in ["last", "later", "escape", "loop", "invoke"] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let boolean = types.scalar(ScalarType::Bool);
        let array = types.intern(Type::Array(int));
        let mut b = Builder::new(&types, array);
        let repeat = b.parameter(b.current(), boolean);
        let length = b.constant(int, Scalar::integer(ScalarType::I32, 2).unwrap());
        let zero = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
        let source = b.emit(Op::NewArray(length), Some(array)).unwrap();
        b.emit(
            Op::ArraySet {
                array: source,
                index: zero,
                value: length,
                native: true,
            },
            None,
        );
        let value = b
            .emit(
                Op::ArrayGet {
                    array: source,
                    index: zero,
                    native: true,
                },
                Some(int),
            )
            .unwrap();
        if mode == "escape" {
            let method = b.method(MethodRef {
                owner: "Consumer".into(),
                name: "retain".into(),
                params: vec![array],
                returns: int,
                interface: false,
            });
            let args = b.args([source]);
            b.emit(
                Op::Call {
                    method,
                    kind: CallKind::JvmStatic,
                    args,
                },
                Some(int),
            );
        }
        if mode == "loop" {
            let header = b.create_block();
            b.jump(header, vec![]);
            b.switch_to(header);
        }
        let handler = (mode == "invoke").then(|| b.create_block());
        let copy = if let Some(handler) = handler {
            b.invoke(Op::CopyValue(source), Some(array), handler)
                .unwrap()
        } else {
            b.emit(Op::CopyValue(source), Some(array)).unwrap()
        };
        if mode == "later" {
            // Mutating the transferred result must not change the old source.
            b.emit(
                Op::ArraySet {
                    array: copy,
                    index: zero,
                    value: zero,
                    native: true,
                },
                None,
            );
            let later = b
                .emit(
                    Op::ArrayGet {
                        array: source,
                        index: zero,
                        native: true,
                    },
                    Some(int),
                )
                .unwrap();
            b.emit(
                Op::ArraySet {
                    array: copy,
                    index: zero,
                    value: later,
                    native: true,
                },
                None,
            );
        } else {
            b.emit(
                Op::ArraySet {
                    array: copy,
                    index: zero,
                    value,
                    native: true,
                },
                None,
            );
        }
        if mode == "loop" {
            let header = b.current();
            let done = b.create_block();
            b.branch(repeat, header, done);
            b.switch_to(done);
        }
        b.terminate(Terminator::Return(Some(copy)));
        if let Some(handler) = handler {
            b.switch_to(handler);
            b.terminate(Terminator::Rethrow);
        }
        let body = promote_aggregates(b.finish().unwrap(), &types, |_| None).unwrap();
        verify(&body, &types).unwrap();
        let copies = body
            .instructions
            .iter()
            .filter(|i| matches!(i.op, Op::CopyValue(_)))
            .count();
        assert_eq!(copies, usize::from(mode != "last"), "{mode}");
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn redundant_array_copies_keep_escaping_and_loop_snapshots_distinct() {
    for mode in ["local", "escape", "loop", "object"] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let boolean = types.scalar(ScalarType::Bool);
        let object = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(object));
        let element = if mode == "object" { object } else { int };
        let array = types.intern(Type::Array(element));
        let mut b = Builder::new(&types, array);
        let input = b.parameter(b.current(), array);
        let repeat = b.parameter(b.current(), boolean);
        let first = b.emit(Op::CopyValue(input), Some(array)).unwrap();
        if mode == "escape" {
            let method = b.method(MethodRef {
                owner: "Consumer".into(),
                name: "retain".into(),
                params: vec![array],
                returns: int,
                interface: false,
            });
            let args = b.args([first]);
            b.emit(
                Op::Call {
                    method,
                    kind: CallKind::JvmStatic,
                    args,
                },
                Some(int),
            );
        }
        if mode == "loop" {
            let header = b.create_block();
            b.jump(header, vec![]);
            b.switch_to(header);
        }
        let second = b.emit(Op::CopyValue(first), Some(array)).unwrap();
        if mode == "loop" {
            let header = b.current();
            let done = b.create_block();
            b.branch(repeat, header, done);
            b.switch_to(done);
        }
        b.terminate(Terminator::Return(Some(second)));
        let body = promote_aggregates(b.finish().unwrap(), &types, |_| None).unwrap();
        verify(&body, &types).unwrap();
        let copies = body
            .instructions
            .iter()
            .filter(|i| matches!(i.op, Op::CopyValue(_)))
            .count();
        assert_eq!(copies, if mode == "local" { 1 } else { 2 }, "{mode}");
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

fn construct(
    b: &mut Builder<'_>,
    owner: TypeId,
    fields: &[FieldRef],
    values: &[ValueId],
) -> ValueId {
    let method = b.method(MethodRef {
        owner: "Pair".into(),
        name: "<init>".into(),
        params: fields.iter().map(|f| f.ty).collect(),
        returns: TypeId::new(0),
        interface: false,
    });
    let args = b.args(values.iter().copied());
    b.emit(
        Op::Call {
            method,
            kind: CallKind::Constructor,
            args,
        },
        Some(owner),
    )
    .unwrap()
}

#[test]
fn private_mutable_fields_become_loop_parameters() {
    let mut types = Types::default();
    types.intern(Type::Unit);
    let int = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let symbol = types.symbol("Pair");
    let owner = types.intern(Type::Class(symbol));
    let fields = ["count", "sum"].map(|name| FieldRef {
        owner,
        name: name.into(),
        ty: int,
        is_static: false,
    });
    let mut b = Builder::new(&types, int);
    let limit = b.parameter(b.current(), int);
    let zero = b.constant(int, Scalar::integer(ScalarType::I64, 0).unwrap());
    let pair = construct(&mut b, owner, &fields, &[zero, zero]);
    let count = b.field(fields[0].clone());
    let sum = b.field(fields[1].clone());
    let header = b.create_block();
    let step = b.create_block();
    let done = b.create_block();
    b.jump(header, vec![]);
    b.switch_to(header);
    let n = b
        .emit(
            Op::GetField {
                object: pair,
                field: count,
            },
            Some(int),
        )
        .unwrap();
    let test = b
        .emit(
            Op::Binary {
                op: BinaryOp::Lt,
                left: n,
                right: limit,
            },
            Some(boolean),
        )
        .unwrap();
    b.branch(test, step, done);
    b.switch_to(step);
    let previous = b
        .emit(
            Op::GetField {
                object: pair,
                field: sum,
            },
            Some(int),
        )
        .unwrap();
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: n,
                right: previous,
            },
            Some(int),
        )
        .unwrap();
    b.emit(
        Op::SetField {
            object: pair,
            field: sum,
            value: next,
        },
        None,
    );
    let one = b.constant(int, Scalar::integer(ScalarType::I64, 1).unwrap());
    let n = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: n,
                right: one,
            },
            Some(int),
        )
        .unwrap();
    b.emit(
        Op::SetField {
            object: pair,
            field: count,
            value: n,
        },
        None,
    );
    b.jump(header, vec![]);
    b.switch_to(done);
    let result = b
        .emit(
            Op::GetField {
                object: pair,
                field: sum,
            },
            Some(int),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let body = promote_aggregates(b.finish().unwrap(), &types, |_| Some(fields.to_vec())).unwrap();
    verify(&body, &types).unwrap();
    assert_eq!(body.blocks[header.index()].params.len(), 2);
    assert!(!body.instructions.iter().any(|inst| matches!(
        inst.op,
        Op::Call { .. } | Op::GetField { .. } | Op::SetField { .. }
    )));
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    assert!(
        !code
            .instructions
            .iter()
            .any(|op| matches!(op, ristretto_classfile::attributes::Instruction::New(_)))
    );
}

#[test]
fn keeps_returned_carriers_and_unknown_constructors() {
    let mut types = Types::default();
    types.intern(Type::Unit);
    let int = types.scalar(ScalarType::I64);
    let symbol = types.symbol("Pair");
    let owner = types.intern(Type::Class(symbol));
    let field = FieldRef {
        owner,
        name: "value".into(),
        ty: int,
        is_static: false,
    };
    let mut b = Builder::new(&types, owner);
    let value = b.parameter(b.current(), int);
    let pair = construct(&mut b, owner, &[field.clone()], &[value]);
    b.terminate(Terminator::Return(Some(pair)));
    let body = b.finish().unwrap();
    assert_eq!(
        body,
        promote_aggregates(body.clone(), &types, |_| Some(vec![field.clone()])).unwrap()
    );
    assert_eq!(
        body,
        promote_aggregates(body.clone(), &types, |_| None).unwrap()
    );
}

#[test]
fn distinct_readonly_values_join_as_fields() {
    let mut types = Types::default();
    types.intern(Type::Unit);
    let int = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let name = types.symbol("Pair");
    let pair = types.intern(Type::Class(name));
    let fields = ["a", "b"].map(|name| FieldRef {
        owner: pair,
        name: name.into(),
        ty: int,
        is_static: false,
    });
    for escaping in [false, true] {
        let mut b = Builder::new(&types, int);
        let condition = b.parameter(b.current(), boolean);
        let unknown = escaping.then(|| b.parameter(b.current(), pair));
        let left = b.create_block();
        let right = b.create_block();
        let join = b.create_block();
        let merged = b.parameter(join, pair);
        b.branch(condition, left, right);
        b.switch_to(left);
        let a = b.constant(int, Scalar::integer(ScalarType::I64, 13).unwrap());
        let c = b.constant(int, Scalar::integer(ScalarType::I64, 17).unwrap());
        let value = construct(&mut b, pair, &fields, &[a, c]);
        b.jump(join, vec![value]);
        b.switch_to(right);
        let a = b.constant(int, Scalar::integer(ScalarType::I64, 19).unwrap());
        let c = b.constant(int, Scalar::integer(ScalarType::I64, 23).unwrap());
        let value = unknown.unwrap_or_else(|| construct(&mut b, pair, &fields, &[a, c]));
        b.jump(join, vec![value]);
        b.switch_to(join);
        let field = b.field(fields[1].clone());
        let result = b
            .emit(
                Op::GetField {
                    object: merged,
                    field,
                },
                Some(int),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(result)));
        let body =
            promote_aggregates(b.finish().unwrap(), &types, |_| Some(fields.to_vec())).unwrap();
        verify(&body, &types).unwrap();
        let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        let constructed = code
            .instructions
            .iter()
            .any(|i| matches!(i, ristretto_classfile::attributes::Instruction::New(_)));
        assert_eq!(
            constructed, escaping,
            "an unknown input must keep its identity"
        );
        if !escaping {
            assert_eq!(body.blocks[join.index()].params.len(), 3);
            assert!(
                !body
                    .instructions
                    .iter()
                    .any(|i| matches!(i.op, Op::GetField { .. } | Op::Call { .. }))
            );
        }
    }
}

#[test]
fn new_loop_values_are_not_confused_with_previous_iterations() {
    let mut types = Types::default();
    types.intern(Type::Unit);
    let int = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let name = types.symbol("Pair");
    let pair = types.intern(Type::Class(name));
    let fields = ["a", "b"].map(|name| FieldRef {
        owner: pair,
        name: name.into(),
        ty: int,
        is_static: false,
    });
    for mutate in [false, true] {
        let mut b = Builder::new(&types, int);
        let again = b.parameter(b.current(), boolean);
        let zero = b.constant(int, Scalar::integer(ScalarType::I64, 0).unwrap());
        let initial = construct(&mut b, pair, &fields, &[zero, zero]);
        let cursor = b.variable(pair);
        b.define(cursor, initial);
        let header = b.create_block();
        let step = b.create_block();
        let done = b.create_block();
        b.jump(header, vec![]);
        b.switch_to(header);
        let previous = b.read(cursor);
        b.branch(again, step, done);
        b.switch_to(step);
        let field = b.field(fields[1].clone());
        let value = b
            .emit(
                Op::GetField {
                    object: previous,
                    field,
                },
                Some(int),
            )
            .unwrap();
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
        let new = construct(&mut b, pair, &fields, &[value, next]);
        if mutate {
            b.emit(
                Op::SetField {
                    object: previous,
                    field,
                    value: one,
                },
                None,
            );
        }
        b.define(cursor, new);
        b.jump(header, vec![]);
        b.switch_to(done);
        let result = b
            .emit(
                Op::GetField {
                    object: previous,
                    field,
                },
                Some(int),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(result)));
        let body =
            promote_aggregates(b.finish().unwrap(), &types, |_| Some(fields.to_vec())).unwrap();
        verify(&body, &types).unwrap();
        let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        let constructed = code
            .instructions
            .iter()
            .any(|i| matches!(i, ristretto_classfile::attributes::Instruction::New(_)));
        assert_eq!(
            constructed, mutate,
            "aliased mutable identities must not become snapshots"
        );
    }
}

#[test]
fn flat_copy_snapshots_survive_source_mutation_without_carriers() {
    let mut types = Types::default();
    types.intern(Type::Unit);
    let int = types.scalar(ScalarType::I64);
    let symbol = types.symbol("Pair");
    let pair = types.intern(Type::Class(symbol));
    let fields = ["a", "b"].map(|name| FieldRef {
        owner: pair,
        name: name.into(),
        ty: int,
        is_static: false,
    });
    let mut b = Builder::new(&types, int);
    let seven = b.constant(int, Scalar::integer(ScalarType::I64, 7).unwrap());
    let nine = b.constant(int, Scalar::integer(ScalarType::I64, 9).unwrap());
    let original = construct(&mut b, pair, &fields, &[seven, nine]);
    let copy = b.emit(Op::CopyValue(original), Some(pair)).unwrap();
    let second = b.emit(Op::CopyValue(copy), Some(pair)).unwrap();
    let field = b.field(fields[0].clone());
    b.emit(
        Op::SetField {
            object: original,
            field,
            value: nine,
        },
        None,
    );
    b.emit(
        Op::SetField {
            object: copy,
            field,
            value: nine,
        },
        None,
    );
    let result = b
        .emit(
            Op::GetField {
                object: second,
                field,
            },
            Some(int),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let body = promote_aggregates(b.finish().unwrap(), &types, |_| Some(fields.to_vec())).unwrap();
    verify(&body, &types).unwrap();
    assert!(!body.instructions.iter().any(|i| matches!(
        i.op,
        Op::CopyValue(_) | Op::Call { .. } | Op::GetField { .. } | Op::SetField { .. }
    )));
    let Some(Terminator::Return(Some(result))) = body.blocks[body.entry.index()].terminator else {
        panic!("return");
    };
    let mut snapshot = body.resolve(result);
    while let ValueDef::Inst(id) = body.values[snapshot.index()].def {
        let Op::Reinterpret(source) = body.instructions[id.index()].op else {
            break;
        };
        snapshot = body.resolve(source);
    }
    assert_eq!(snapshot, body.resolve(seven));
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    assert!(
        !code
            .instructions
            .iter()
            .any(|i| matches!(i, ristretto_classfile::attributes::Instruction::New(_)))
    );
}

#[test]
fn nullable_or_nested_owned_copies_keep_the_general_operation() {
    let mut types = Types::default();
    types.intern(Type::Unit);
    let int = types.scalar(ScalarType::I64);
    let symbol = types.symbol("Pair");
    let pair = types.intern(Type::Class(symbol));
    for nested in [false, true] {
        let field = FieldRef {
            owner: pair,
            name: "a".into(),
            ty: if nested { pair } else { int },
            is_static: false,
        };
        let mut b = Builder::new(&types, pair);
        let input = b.parameter(b.current(), pair);
        let source = if nested {
            construct(&mut b, pair, &[field.clone()], &[input])
        } else {
            input
        };
        let copy = b.emit(Op::CopyValue(source), Some(pair)).unwrap();
        b.terminate(Terminator::Return(Some(copy)));
        let body =
            promote_aggregates(b.finish().unwrap(), &types, |_| Some(vec![field.clone()])).unwrap();
        verify(&body, &types).unwrap();
        assert!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::CopyValue(_)))
        );
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn scalar_copy_reads_share_only_without_intervening_mutation_or_escape() {
    for mode in ["plain", "store", "call", "escape", "branch"] {
        let mut types = Types::default();
        types.intern(Type::Unit);
        let int = types.scalar(ScalarType::I64);
        let name = types.symbol("Pair");
        let pair = types.intern(Type::Class(name));
        let fields = vec![FieldRef {
            owner: pair,
            name: "a".into(),
            ty: int,
            is_static: false,
        }];
        let mut b = Builder::new(&types, int);
        let input = b.parameter(b.current(), pair);
        let zero = b.constant(int, Scalar::integer(ScalarType::I64, 0).unwrap());
        construct(&mut b, pair, &fields, &[zero]);
        let copy = b.emit(Op::CopyValue(input), Some(pair)).unwrap();
        let field = b.field(fields[0].clone());
        if mode == "store" {
            b.emit(
                Op::SetField {
                    object: input,
                    field,
                    value: zero,
                },
                None,
            );
        }
        if mode == "call" || mode == "escape" {
            let method = b.method(MethodRef {
                owner: "Opaque".into(),
                name: "write".into(),
                params: vec![pair],
                returns: int,
                interface: false,
            });
            let args = b.args([if mode == "escape" { copy } else { input }]);
            b.emit(
                Op::Call {
                    method,
                    kind: CallKind::JvmStatic,
                    args,
                },
                Some(int),
            );
        }
        if mode == "branch" {
            let next = b.create_block();
            b.jump(next, vec![]);
            b.switch_to(next);
        }
        let value = b
            .emit(
                Op::GetField {
                    object: copy,
                    field,
                },
                Some(int),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(value)));
        let body =
            promote_aggregates(b.finish().unwrap(), &types, |_| Some(fields.clone())).unwrap();
        verify(&body, &types).unwrap();
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::CopyValue(_))),
            mode != "plain",
            "{mode}"
        );
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}
