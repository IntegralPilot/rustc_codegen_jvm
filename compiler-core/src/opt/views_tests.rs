use super::decompose_views;
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};

#[test]
fn jvm_view_length_truncates_without_constructing_the_view() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let long = types.scalar(ScalarType::U64);
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let slice = types.intern(Type::Slice(int));
    let mut b = Builder::new(&types, int);
    let root = b.parameter(b.current(), object);
    let start = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
    let length = b.constant(
        long,
        Scalar::integer(ScalarType::U64, 0x1_0000_0007).unwrap(),
    );
    let parts = b.args([root, start, length]);
    let view = b.emit(Op::ViewPack(parts), Some(slice)).unwrap();
    let result = b.emit(Op::ArrayLength(view), Some(int)).unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let mut body = b.finish().unwrap();
    decompose_views(&mut body, &mut types, None);
    super::simplify_components(&mut body, &types);
    verify(&body, &types).unwrap();
    assert_eq!(body.scalar_value(result).unwrap().bits(), 7);
    let live = super::live(&body, &types);
    assert!(
        !body
            .instructions
            .iter()
            .enumerate()
            .any(|(i, inst)| live.instructions[i]
                && matches!(inst.op, Op::ViewPack(_) | Op::ArrayLength(_)))
    );
}

#[test]
fn view_components_cross_loops_without_a_carrier() {
    let mut types = Types::default();
    let long = types.scalar(ScalarType::U64);
    let boolean = types.scalar(ScalarType::Bool);
    let pointer = types.intern(Type::Pointer(long));
    let slice = types.intern(Type::Slice(long));
    let runtime = types.symbol("org/rustlang/runtime/SliceView");
    let runtime = types.intern(Type::Class(runtime));
    let mut b = Builder::new(&types, pointer);
    let data = b.parameter(b.current(), pointer);
    let length = b.parameter(b.current(), long);
    let initial = b.emit(Op::View { data, length }, Some(slice)).unwrap();
    let initial = b.emit(Op::Reinterpret(initial), Some(runtime)).unwrap();
    let initial = b.emit(Op::Adapt(initial), Some(slice)).unwrap();
    let value = b.variable(slice);
    b.define(value, initial);
    let header = b.create_block();
    let step = b.create_block();
    let done = b.create_block();
    b.jump(header, vec![]);
    b.switch_to(header);
    let view = b.read(value);
    let length = b.emit(Op::Length(view), Some(long)).unwrap();
    let zero = b.constant(long, Scalar::integer(ScalarType::U64, 0).unwrap());
    let more = b
        .emit(
            Op::Binary {
                op: BinaryOp::Gt,
                left: length,
                right: zero,
            },
            Some(boolean),
        )
        .unwrap();
    b.branch(more, step, done);
    b.switch_to(step);
    let one = b.constant(long, Scalar::integer(ScalarType::U64, 1).unwrap());
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Sub,
                left: length,
                right: one,
            },
            Some(long),
        )
        .unwrap();
    let view = b
        .emit(Op::View { data, length: next }, Some(slice))
        .unwrap();
    b.define(value, view);
    b.jump(header, vec![]);
    b.switch_to(done);
    let view = b.read(value);
    let pointer_value = b
        .emit(
            Op::ViewData {
                view,
                size: 8,
                codec: None,
            },
            Some(pointer),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(pointer_value)));
    let mut body = b.finish().unwrap();
    decompose_views(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let live = super::live(&body, &types);
    assert!(
        body.instructions
            .iter()
            .enumerate()
            .all(|(i, inst)| !live.instructions[i]
                || !matches!(
                    inst.op,
                    Op::View { .. }
                        | Op::ViewPack(_)
                        | Op::Adapt(_)
                        | Op::Length(_)
                        | Op::ViewData { .. }
                ))
    );
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    assert!(
        !code
            .instructions
            .iter()
            .any(|i| matches!(i, crate::classfile::attributes::Instruction::New(_)))
    );
}

#[test]
fn unknown_input_keeps_a_mixed_join_conservative() {
    let mut types = Types::default();
    let long = types.scalar(ScalarType::U64);
    let boolean = types.scalar(ScalarType::Bool);
    let pointer = types.intern(Type::Pointer(long));
    let slice = types.intern(Type::Slice(long));
    let mut b = Builder::new(&types, long);
    let input = b.parameter(b.current(), slice);
    let condition = b.parameter(b.current(), boolean);
    let data = b.parameter(b.current(), pointer);
    let value = b.variable(slice);
    b.define(value, input);
    let create = b.create_block();
    let join = b.create_block();
    b.branch(condition, create, join);
    b.switch_to(create);
    let length = b.constant(long, Scalar::integer(ScalarType::U64, 17).unwrap());
    let constructed = b.emit(Op::View { data, length }, Some(slice)).unwrap();
    b.define(value, constructed);
    b.jump(join, vec![]);
    b.switch_to(join);
    let joined = b.read(value);
    let length = b.emit(Op::Length(joined), Some(long)).unwrap();
    b.terminate(Terminator::Return(Some(length)));
    let mut body = b.finish().unwrap();
    decompose_views(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::Length(_)))
    );
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn component_calls_and_entries_need_no_view_objects() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let length = types.scalar(ScalarType::U64);
    let slice = types.intern(Type::Slice(byte));
    let mut b = Builder::new(&types, length);
    let input = b.parameter(b.current(), slice);
    let target = b.method(MethodRef {
        owner: "Leaf".into(),
        name: "len".into(),
        params: vec![slice],
        returns: length,
        interface: false,
    });
    let args = b.args([input]);
    let result = b
        .emit(
            Op::Call {
                method: target,
                kind: CallKind::RustStatic,
                args,
            },
            Some(length),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let mut body = b.finish().unwrap();
    super::lower_component_arguments(&mut body, &mut types, true, |_| true, None);
    decompose_views(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    assert_eq!(body.blocks[body.entry.index()].params.len(), 3);
    assert_eq!(body.methods[target.index()].params.len(), 3);
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    use crate::classfile::attributes::Instruction;
    assert!(!code.instructions.iter().any(|i| matches!(
        i,
        Instruction::New(_) | Instruction::Getfield(_) | Instruction::Checkcast(_)
    )));
    assert_eq!(
        code.instructions
            .iter()
            .filter(|i| matches!(i, Instruction::Invokestatic(_)))
            .count(),
        1
    );
}
