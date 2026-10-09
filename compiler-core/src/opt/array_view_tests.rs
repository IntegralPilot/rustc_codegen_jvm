use super::*;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

#[test]
fn zero_sized_and_unsized_views_keep_their_metadata_carrier() {
    for zero_sized in [false, true] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let length = types.scalar(ScalarType::U64);
        let object = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(object));
        let pointee = if zero_sized {
            types.intern(Type::Opaque(0))
        } else {
            let name = types.symbol("test/Dst");
            types.intern(Type::Interface(name))
        };
        let pointer = types.intern(Type::Pointer(pointee));
        let mut b = Builder::new(&types, pointer);
        let backing = b.parameter(b.current(), object);
        let start = b.parameter(b.current(), int);
        let count = b.parameter(b.current(), length);
        let parts = b.args([backing, start, count]);
        let address = b
            .emit(
                Op::ViewAddress {
                    parts,
                    size: if zero_sized { 0 } else { 1 },
                    codec: None,
                },
                Some(pointer),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(address)));
        let mut body = b.finish().unwrap();
        lower_typed_addresses(&mut body, &mut types, None);
        decompose_addresses(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        assert!(
            body.instructions
                .iter()
                .any(|inst| matches!(inst.op, Op::ViewAddress { .. }))
        );
        assert!(live(&body, &types).values[count.index()]);
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn slice_to_fixed_array_to_scalar_slice_keeps_the_source_byte_displacement() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let int = types.scalar(ScalarType::I32);
    let length = types.scalar(ScalarType::U64);
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let byte_pointer = types.intern(Type::Pointer(byte));
    let array = types.intern(Type::Array(byte));
    let codec = types.symbol("org/rustlang/runtime/ArrayMemoryCodec#array#[B#8");
    let layout = types.layout(AddressLayout {
        value: array,
        size: 8,
        codec: Some(codec),
    });
    let fixed_pointer = types.intern(Type::Pointer(layout));
    let erased_fixed = types.intern(Type::Pointer(array));
    let mut b = Builder::new(&types, byte);
    let backing = b.parameter(b.current(), object);
    let start = b.parameter(b.current(), int);
    let count = b.constant(length, Scalar::integer(ScalarType::U64, 8).unwrap());
    let parts = b.args([backing, start, count]);
    let data = b
        .emit(
            Op::ViewAddress {
                parts,
                size: 1,
                codec: None,
            },
            Some(byte_pointer),
        )
        .unwrap();
    let fixed = b
        .emit(
            Op::RetypeAddress {
                pointer: data,
                size: 8,
                codec: Some(codec),
            },
            Some(fixed_pointer),
        )
        .unwrap();
    let erased = b.emit(Op::Reinterpret(fixed), Some(erased_fixed)).unwrap();
    let bytes = b.emit(Op::Cast(erased), Some(byte_pointer)).unwrap();
    let root = b
        .emit(
            Op::AddressViewPart {
                address: bytes,
                index: 0,
            },
            Some(object),
        )
        .unwrap();
    let offset = b
        .emit(
            Op::AddressViewPart {
                address: bytes,
                index: 1,
            },
            Some(int),
        )
        .unwrap();
    let zero = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
    let parts = b.args([root, offset, zero]);
    let first = b.emit(Op::ViewGet(parts), Some(byte)).unwrap();
    b.terminate(Terminator::Return(Some(first)));
    let mut body = b.finish().unwrap();
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
                    Op::AddressPack(_)
                        | Op::TypedAddressPack { .. }
                        | Op::RetypeAddress { .. }
                        | Op::ViewAddress { .. }
                        | Op::TypedAddressViewPart { .. }
                ))
    );
    assert!(body.instructions.iter().any(|inst| matches!(
        inst.op,
        Op::ViewRoot {
            size: 1,
            codec: None,
            ..
        }
    )));
    // The eight-byte array layout must not scale the slice start by eight.
    // Its folded byte displacement must equal i64(start).
    let Op::Call { args, .. } = body
        .instructions
        .iter()
        .find(|inst| {
            matches!(inst.op,
        Op::Call { method, .. } if body.methods[method.index()].name == "locationSliceOffset")
        })
        .unwrap()
        .op
    else {
        unreachable!()
    };
    let displacement = body.resolve(body.args[args.start as usize + 1]);
    let ValueDef::Inst(id) = body.values[displacement.index()].def else {
        panic!()
    };
    assert!(matches!(body.instructions[id.index()].op, Op::Cast(value) if value == start));
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}
