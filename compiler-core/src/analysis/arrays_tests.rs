use super::native_array_accesses;
use crate::classfile::attributes::Instruction;
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};

#[test]
fn initialization_is_native_only_until_the_first_escape() {
    for unwind in [false, true] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let array_ty = types.intern(Type::Array(int));
        let unit = types.intern(Type::Unit);
        let mut b = Builder::new(&types, int);
        let one = b.constant(int, Scalar::integer(ScalarType::I32, 1).unwrap());
        let zero = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
        let array = if unwind {
            let handler = b.create_block();
            let value = b
                .invoke(Op::NewArray(one), Some(array_ty), handler)
                .unwrap();
            let normal = b.current();
            b.switch_to(handler);
            b.terminate(Terminator::Rethrow);
            b.switch_to(normal);
            value
        } else {
            b.emit(Op::NewArray(one), Some(array_ty)).unwrap()
        };
        let before = b.body.instructions.len();
        b.emit(
            Op::ArraySet {
                array,
                index: zero,
                value: one,
                native: false,
            },
            None,
        );
        let method = b.method(MethodRef {
            owner: "Escape".into(),
            name: "registerAlias".into(),
            params: vec![array_ty],
            returns: unit,
            interface: false,
        });
        let args = b.args([array]);
        b.emit(
            Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            },
            None,
        );
        let after = b.body.instructions.len();
        let value = b
            .emit(
                Op::ArrayGet {
                    array,
                    index: zero,
                    native: false,
                },
                Some(int),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(value)));
        let body = b.finish().unwrap();
        let native = native_array_accesses(&body, &types, &crate::opt::live(&body, &types));
        assert_eq!(native[before], Some(int));
        assert_eq!(native[after], None);
        let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        assert!(code.instructions.contains(&Instruction::Iastore));
        assert!(!code.instructions.contains(&Instruction::Iaload));
    }
}

#[test]
fn private_slice_loop_uses_native_array_instructions() {
    for escape in [false, true] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let boolean = types.scalar(ScalarType::Bool);
        let long = types.scalar(ScalarType::U64);
        let array_ty = types.intern(Type::Array(int));
        let slice_ty = types.intern(Type::Slice(int));
        let object = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(object));
        let unit = types.intern(Type::Unit);
        let mut b = Builder::new(&types, int);
        let limit = b.parameter(b.current(), int);
        let one = b.constant(int, Scalar::integer(ScalarType::I32, 1).unwrap());
        let three = b.constant(int, Scalar::integer(ScalarType::I32, 3).unwrap());
        let array = b.emit(Op::NewArray(three), Some(array_ty)).unwrap();
        b.emit(Op::ArrayFill { array, value: one }, None);
        let root = b.emit(Op::Reinterpret(array), Some(object)).unwrap();
        let length = b.constant(long, Scalar::integer(ScalarType::U64, 2).unwrap());
        let parts = b.args([root, one, length]);
        let view = b.emit(Op::ViewPack(parts), Some(slice_ty)).unwrap();
        let index = b.variable(int);
        b.define(index, one);
        let header = b.create_block();
        let step = b.create_block();
        let done = b.create_block();
        b.jump(header, vec![]);
        b.switch_to(header);
        let iteration = b.read(index);
        let condition = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Lt,
                    left: iteration,
                    right: limit,
                },
                Some(boolean),
            )
            .unwrap();
        b.branch(condition, step, done);
        b.switch_to(step);
        b.emit(
            Op::ArraySet {
                native: false,
                array: view,
                index: one,
                value: iteration,
            },
            None,
        );
        if escape {
            let method = b.method(MethodRef {
                owner: "Escape".into(),
                name: "registerAlias".into(),
                params: vec![object],
                returns: unit,
                interface: false,
            });
            let args = b.args([root]);
            b.emit(
                Op::Call {
                    method,
                    kind: CallKind::JvmStatic,
                    args,
                },
                None,
            );
        }
        let loaded = b
            .emit(
                Op::ArrayGet {
                    native: false,
                    array: view,
                    index: one,
                },
                Some(int),
            )
            .unwrap();
        let next = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Add,
                    left: loaded,
                    right: one,
                },
                Some(int),
            )
            .unwrap();
        b.define(index, next);
        b.jump(header, vec![]);
        b.switch_to(done);
        b.terminate(Terminator::Return(Some(iteration)));
        let mut body = b.finish().unwrap();
        crate::opt::decompose_views(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        let live = crate::opt::live(&body, &types);
        let native = native_array_accesses(&body, &types, &live);
        for (id, inst) in body.instructions.iter().enumerate() {
            if matches!(inst.op, Op::ViewGet(_) | Op::ViewSet { .. }) {
                assert_eq!(native[id], (!escape).then_some(int));
            }
        }
        let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        assert!(
            !code
                .instructions
                .iter()
                .any(|i| matches!(i, Instruction::New(_)))
        );
        assert_eq!(code.instructions.contains(&Instruction::Iaload), !escape);
        assert_eq!(code.instructions.contains(&Instruction::Iastore), !escape);
        assert!(
            !code
                .instructions
                .iter()
                .any(|i| matches!(i, Instruction::Getfield(_)))
        );
    }
}

#[test]
fn mixed_array_origins_escape_even_when_the_join_only_reads() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let boolean = types.scalar(ScalarType::Bool);
    let array_ty = types.intern(Type::Array(int));
    let mut b = Builder::new(&types, int);
    let condition = b.parameter(b.current(), boolean);
    let input = b.parameter(b.current(), array_ty);
    let one = b.constant(int, Scalar::integer(ScalarType::I32, 1).unwrap());
    let array = b.emit(Op::NewArray(one), Some(array_ty)).unwrap();
    let variable = b.variable(array_ty);
    b.define(variable, array);
    let other = b.create_block();
    let joined = b.create_block();
    b.branch(condition, other, joined);
    b.switch_to(other);
    b.define(variable, input);
    b.jump(joined, vec![]);
    b.switch_to(joined);
    let alias = b.read(variable);
    let zero = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
    let loaded = b
        .emit(
            Op::ArrayGet {
                native: false,
                array: alias,
                index: zero,
            },
            Some(int),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(loaded)));
    let body = b.finish().unwrap();
    let live = crate::opt::live(&body, &types);
    assert!(
        native_array_accesses(&body, &types, &live)
            .iter()
            .all(Option::is_none)
    );
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    assert!(!code.instructions.contains(&Instruction::Iaload));
}
