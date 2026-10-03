use super::*;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

#[test]
fn aggregate_slice_addresses_cross_calls_returns_and_fields_as_components() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let long = types.scalar(ScalarType::I64);
    let pair_name = types.symbol("Pair");
    let pair = types.intern(Type::Class(pair_name));
    let pointer = types.intern(Type::Pointer(pair));
    let slice = types.intern(Type::Slice(pair));
    let codec = types.symbol("PairCodec#pair#LPair;");
    let object_name = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object_name));
    let component_types = ComponentShape::View.parts(&mut types);
    let mut b = Builder::new(&types, pointer);
    let input = b.parameter(b.current(), slice);
    let parts = component_types
        .enumerate()
        .map(|(index, ty)| {
            b.emit(
                Op::ViewPart {
                    view: input,
                    index: index as u8,
                },
                Some(ty),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    let args = b.args(parts);
    let first = b
        .emit(
            Op::ViewAddress {
                parts: args,
                size: 8,
                codec: Some(codec),
            },
            Some(pointer),
        )
        .unwrap();
    let one = b.constant(long, Scalar::integer(ScalarType::I64, 1).unwrap());
    let next = b
        .emit(
            Op::Offset {
                pointer: first,
                offset: one,
                bytes: false,
                wrapping: false,
            },
            Some(pointer),
        )
        .unwrap();
    let next = b
        .emit(
            Op::RetypeAddress {
                pointer: next,
                size: 8,
                codec: Some(codec),
            },
            Some(pointer),
        )
        .unwrap();
    let view_root = b
        .emit(
            Op::AddressViewPart {
                address: next,
                index: 0,
            },
            Some(object),
        )
        .unwrap();
    let view_start = b
        .emit(
            Op::AddressViewPart {
                address: next,
                index: 1,
            },
            Some(int),
        )
        .unwrap();
    let field = b.field(FieldRef {
        owner: pair,
        name: "second".into(),
        ty: int,
        is_static: false,
    });
    let projection = b.projection(PointerProjection {
        parent: None,
        field,
        offset: 4,
        size: 4,
        codec: None,
    });
    let value = b
        .emit(
            Op::LoadField {
                base: next,
                projection,
            },
            Some(int),
        )
        .unwrap();
    let method = b.method(MethodRef {
        owner: "Kernel".into(),
        name: "consume".into(),
        params: vec![pointer, int, object, int],
        returns: pointer,
        interface: false,
    });
    let args = b.args([next, value, view_root, view_start]);
    let result = b
        .emit(
            Op::Call {
                method,
                kind: CallKind::RustStatic,
                args,
            },
            Some(pointer),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| true, None);
    lower_component_returns(&mut body, &mut types, true, |_| true, None);
    decompose_views(&mut body, &mut types, None);
    lower_typed_addresses(&mut body, &mut types, None);
    decompose_addresses(&mut body, &mut types, None);
    simplify_components(&mut body, &types);
    verify(&body, &types).unwrap();
    let live = live(&body, &types);
    assert!(
        !body
            .instructions
            .iter()
            .enumerate()
            .any(|(i, inst)| live.instructions[i]
                && matches!(
                    inst.op,
                    Op::ViewAddress { .. }
                        | Op::Offset { .. }
                        | Op::AddressPack(_)
                        | Op::TypedAddressPack { .. }
                        | Op::LoadField { .. }
                ))
    );
    assert_eq!(
        body.instructions
            .iter()
            .filter(|i| matches!(i.op, Op::ViewRoot { .. }))
            .count(),
        1
    );
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::LoadStorageField { .. }))
    );
    assert!(!body.methods.iter().any(|m| m.name == "locationStride"));
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn aggregate_copy_uses_static_layout_without_cast_or_offset_carriers() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let bytes = types.intern(Type::Pointer(byte));
    let name = types.symbol("Pair");
    let pair = types.intern(Type::Class(name));
    let pointer = types.intern(Type::Pointer(pair));
    let long = types.scalar(ScalarType::I64);
    let codec = types.symbol("PairCodec#pair#LPair;");
    let mut b = Builder::new(&types, pair);
    let source = b.parameter(b.current(), bytes);
    let cast = b
        .emit(
            Op::RetypeAddress {
                pointer: source,
                size: 8,
                codec: Some(codec),
            },
            Some(pointer),
        )
        .unwrap();
    let one = b.constant(long, Scalar::integer(ScalarType::I64, 1).unwrap());
    let element = b
        .emit(
            Op::Offset {
                pointer: cast,
                offset: one,
                bytes: false,
                wrapping: false,
            },
            Some(pointer),
        )
        .unwrap();
    let replacement = b.parameter(b.current(), pair);
    b.emit(
        Op::Store {
            pointer: element,
            value: replacement,
        },
        None,
    );
    let copy = b.emit(Op::LoadCopy(element), Some(pair)).unwrap();
    b.terminate(Terminator::Return(Some(copy)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| false, None);
    lower_typed_addresses(&mut body, &mut types, None);
    decompose_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let live = live(&body, &types);
    assert!(
        !body
            .instructions
            .iter()
            .enumerate()
            .any(|(i, inst)| live.instructions[i]
                && matches!(
                    inst.op,
                    Op::RetypeAddress { .. } | Op::Offset { .. } | Op::AddressPack(_)
                ))
    );
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::LoadTypedCopy { size: 8, .. }))
    );
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::StoreTyped { size: 8, .. }))
    );
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn aggregate_cursor_keeps_components_through_backedges() {
    use crate::scalar::BinaryOp;
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let bytes = types.intern(Type::Pointer(byte));
    let pair_name = types.symbol("Pair");
    let pair = types.intern(Type::Class(pair_name));
    let pointer = types.intern(Type::Pointer(pair));
    let long = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let codec = types.symbol("PairCodec#pair#LPair;");
    let mut b = Builder::new(&types, pair);
    let source = b.parameter(b.current(), bytes);
    let end = b.parameter(b.current(), long);
    let cast = b
        .emit(
            Op::RetypeAddress {
                pointer: source,
                size: 8,
                codec: Some(codec),
            },
            Some(pointer),
        )
        .unwrap();
    let cursor = b.variable(pointer);
    let position = b.variable(long);
    b.define(cursor, cast);
    let zero = b.constant(long, Scalar::integer(ScalarType::I64, 0).unwrap());
    b.define(position, zero);
    let header = b.create_block();
    let step = b.create_block();
    let done = b.create_block();
    b.jump(header, vec![]);
    b.switch_to(header);
    let current = b.read(cursor);
    let index = b.read(position);
    let test = b
        .emit(
            Op::Binary {
                op: BinaryOp::Lt,
                left: index,
                right: end,
            },
            Some(boolean),
        )
        .unwrap();
    b.branch(test, step, done);
    b.switch_to(step);
    b.emit(Op::LoadCopy(current), Some(pair));
    let one = b.constant(long, Scalar::integer(ScalarType::I64, 1).unwrap());
    let next = b
        .emit(
            Op::Offset {
                pointer: current,
                offset: one,
                bytes: false,
                wrapping: true,
            },
            Some(pointer),
        )
        .unwrap();
    b.define(cursor, next);
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: index,
                right: one,
            },
            Some(long),
        )
        .unwrap();
    b.define(position, next);
    b.jump(header, vec![]);
    b.switch_to(done);
    let result = b.emit(Op::LoadCopy(current), Some(pair)).unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| false, None);
    lower_typed_addresses(&mut body, &mut types, None);
    decompose_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let live = live(&body, &types);
    assert!(
        !body
            .instructions
            .iter()
            .enumerate()
            .any(|(i, inst)| live.instructions[i]
                && matches!(
                    inst.op,
                    Op::RetypeAddress { .. }
                        | Op::Offset { .. }
                        | Op::AddressPack(_)
                        | Op::TypedAddressPack { .. }
                        | Op::LoadCopy(_)
                ))
    );
    assert_eq!(
        body.instructions
            .iter()
            .filter(|i| matches!(i.op, Op::LoadTypedCopy { .. }))
            .count(),
        2
    );
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn conflicting_pointee_layouts_keep_a_boundary_at_the_join() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let bytes = types.intern(Type::Pointer(byte));
    let name = types.symbol("Pair");
    let pair = types.intern(Type::Class(name));
    let pointer = types.intern(Type::Pointer(pair));
    let boolean = types.scalar(ScalarType::Bool);
    let mut b = Builder::new(&types, pair);
    let source = b.parameter(b.current(), bytes);
    let condition = b.parameter(b.current(), boolean);
    let result = b.variable(pointer);
    let left = b.create_block();
    let right = b.create_block();
    let join = b.create_block();
    b.branch(condition, left, right);
    for (block, size) in [(left, 8), (right, 16)] {
        b.switch_to(block);
        let value = b
            .emit(
                Op::RetypeAddress {
                    pointer: source,
                    size,
                    codec: None,
                },
                Some(pointer),
            )
            .unwrap();
        b.define(result, value);
        b.jump(join, vec![]);
    }
    b.switch_to(join);
    let pointer = b.read(result);
    let value = b.emit(Op::LoadCopy(pointer), Some(pair)).unwrap();
    b.terminate(Terminator::Return(Some(value)));
    let mut body = b.finish().unwrap();
    lower_typed_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    assert!(
        !body
            .instructions
            .iter()
            .any(|i| matches!(i.op, Op::LoadTypedCopy { .. }))
    );
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn exact_array_layout_survives_call_parameters_and_returns() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let array = types.intern(Type::Array(byte));
    let codec = types.symbol("ArrayCodec#eight#[B#8");
    let layout = types.layout(AddressLayout {
        value: array,
        size: 8,
        codec: Some(codec),
    });
    let pointer = types.intern(Type::Pointer(layout));
    let long = types.scalar(ScalarType::I64);
    let mut b = Builder::new(&types, pointer);
    let input = b.parameter(b.current(), pointer);
    let one = b.constant(long, Scalar::integer(ScalarType::I64, 1).unwrap());
    let next = b
        .emit(
            Op::Offset {
                pointer: input,
                offset: one,
                bytes: false,
                wrapping: false,
            },
            Some(pointer),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(next)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| true, None);
    lower_component_returns(&mut body, &mut types, true, |_| true, None);
    lower_typed_addresses(&mut body, &mut types, None);
    decompose_addresses(&mut body, &mut types, None);
    super::simplify_components(&mut body, &types);
    verify(&body, &types).unwrap();
    let live = live(&body, &types);
    assert!(
        !body
            .instructions
            .iter()
            .enumerate()
            .any(|(i, instruction)| live.instructions[i]
                && matches!(
                    instruction.op,
                    Op::AddressPack(_)
                        | Op::TypedAddressPack { .. }
                        | Op::RetypeAddress { .. }
                        | Op::Offset { .. }
                ))
    );
    assert!(!body.methods.iter().any(|m| m.name == "locationStride"));
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    assert!(
        !code
            .instructions
            .contains(&crate::classfile::attributes::Instruction::Lmul)
    );
}
