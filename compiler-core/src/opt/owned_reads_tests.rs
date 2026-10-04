use super::lower_owned_reads;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

#[test]
fn owned_elements_fuse_only_without_other_observers_effects_or_handlers() {
    for mode in ["plain", "invoke", "effect", "observer", "handler"] {
        let mut types = Types::default();
        let name = types.symbol("Pair");
        let pair = types.intern(Type::Class(name));
        let slice = types.intern(Type::Slice(pair));
        let int = types.scalar(ScalarType::I32);
        let mut b = Builder::new(&types, pair);
        let array = b.parameter(b.current(), slice);
        let index = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
        let handler = b.create_block();
        let different = b.create_block();
        let op = Op::ArrayGet {
            array,
            index,
            native: false,
        };
        let invoked = matches!(mode, "invoke" | "handler");
        let value = if invoked {
            b.invoke(op, Some(pair), handler).unwrap()
        } else {
            b.emit(op, Some(pair)).unwrap()
        };
        if mode == "effect" {
            b.emit(
                Op::ArraySet {
                    array,
                    index,
                    value,
                    native: false,
                },
                None,
            );
        }
        let copy = if invoked {
            b.invoke(
                Op::CopyValue(value),
                Some(pair),
                if mode == "handler" {
                    different
                } else {
                    handler
                },
            )
            .unwrap()
        } else {
            b.emit(Op::CopyValue(value), Some(pair)).unwrap()
        };
        if mode == "observer" {
            b.emit(
                Op::ArraySet {
                    array,
                    index,
                    value,
                    native: false,
                },
                None,
            );
        }
        b.terminate(Terminator::Return(Some(copy)));
        for block in [handler, different] {
            b.switch_to(block);
            b.terminate(Terminator::Rethrow);
        }
        let mut body = b.finish().unwrap();
        lower_owned_reads(&mut body, &types);
        verify(&body, &types).unwrap();
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::ArrayGetCopy { .. })),
            matches!(mode, "plain" | "invoke"),
            "{mode}"
        );
    }
}

#[test]
fn transfer_a_sole_owned_snapshot_but_keep_observed_copies() {
    for observed in [false, true] {
        let mut types = Types::default();
        let name = types.symbol("Pair");
        let pair = types.intern(Type::Class(name));
        let mut b = Builder::new(&types, pair);
        let input = b.parameter(b.current(), pair);
        let snapshot = b.emit(Op::CopyValue(input), Some(pair)).unwrap();
        let copy = b.emit(Op::CopyValue(snapshot), Some(pair)).unwrap();
        if observed {
            let field = b.field(FieldRef {
                owner: pair,
                name: "next".into(),
                ty: pair,
                is_static: false,
            });
            b.emit(
                Op::SetField {
                    object: input,
                    field,
                    value: snapshot,
                },
                None,
            );
        }
        b.terminate(Terminator::Return(Some(copy)));
        let mut body = b.finish().unwrap();
        lower_owned_reads(&mut body, &types);
        verify(&body, &types).unwrap();
        let ValueDef::Inst(id) = body.values[copy.index()].def else {
            panic!()
        };
        assert_eq!(
            body.instructions[id.index()].op,
            if observed {
                Op::CopyValue(snapshot)
            } else {
                Op::Reinterpret(snapshot)
            }
        );
    }
}

#[test]
fn keep_each_copy_when_the_consumer_is_in_a_loop() {
    let mut types = Types::default();
    let name = types.symbol("Pair");
    let pair = types.intern(Type::Class(name));
    let boolean = types.scalar(ScalarType::Bool);
    let mut b = Builder::new(&types, pair);
    let input = b.parameter(b.current(), pair);
    let again = b.parameter(b.current(), boolean);
    let value = b.emit(Op::CopyValue(input), Some(pair)).unwrap();
    let repeat = b.create_block();
    b.jump(repeat, vec![]);
    b.switch_to(repeat);
    let copy = b.emit(Op::CopyValue(value), Some(pair)).unwrap();
    let done = b.create_block();
    b.branch(again, repeat, done);
    b.switch_to(done);
    b.terminate(Terminator::Return(Some(copy)));
    let mut body = b.finish().unwrap();
    let original = body.clone();
    lower_owned_reads(&mut body, &types);
    assert_eq!(body, original);
}
