use super::*;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

#[test]
fn address_observers_keep_components_and_exception_handlers() {
    for name in [
        "addr",
        "offset_from",
        "offset_from_unsigned",
        "byte_offset_from",
        "byte_offset_from_unsigned",
        "align_offset",
    ] {
        for typed in [false, true] {
            for protected in [false, true] {
                let mut types = Types::default();
                let long = types.scalar(ScalarType::I64);
                let int = types.scalar(ScalarType::I32);
                let object = types.symbol("java/lang/Object");
                let object = types.intern(Type::Class(object));
                let pointer = types.intern(Type::Pointer(int));
                let codec = types.symbol("test/Codec#pair#Ltest/Pair;");
                let mut b = Builder::new(&types, long);
                let root = b.parameter(b.current(), object);
                let offset = b.parameter(b.current(), long);
                let parts = b.args([root, offset]);
                let packed = b
                    .emit(
                        if typed {
                            Op::TypedAddressPack {
                                parts,
                                size: 16,
                                codec: Some(codec),
                            }
                        } else {
                            Op::AddressPack(parts)
                        },
                        Some(pointer),
                    )
                    .unwrap();
                let alignment = b.constant(long, Scalar::integer(ScalarType::I64, 8).unwrap());
                let (params, args) = match name {
                    "addr" => (vec![], vec![packed]),
                    "align_offset" => (vec![long], vec![packed, alignment]),
                    _ => (vec![pointer], vec![packed, packed]),
                };
                let method = b.method(MethodRef {
                    owner: "org/rustlang/runtime/Pointer".into(),
                    name: name.into(),
                    params,
                    returns: long,
                    interface: false,
                });
                let args = b.args(args);
                let call = Op::Call {
                    method,
                    kind: CallKind::Virtual,
                    args,
                };
                let (result, handler) = if protected {
                    let handler = b.create_block();
                    (b.invoke(call, Some(long), handler).unwrap(), Some(handler))
                } else {
                    (b.emit(call, Some(long)).unwrap(), None)
                };
                b.terminate(Terminator::Return(Some(result)));
                if let Some(handler) = handler {
                    b.switch_to(handler);
                    b.terminate(Terminator::Rethrow);
                }
                let mut body = b.finish().unwrap();
                lower_address_observers(&mut body, &mut types, None);
                verify(&body, &types).unwrap();
                assert!(!live(&body, &types).values[packed.index()], "{name}");
                let (id, inst) = body.instructions.iter().enumerate().find(|(_, inst)| {
                    matches!(inst.op, Op::Call { method, .. } if body.methods[method.index()].name != name)
                }).unwrap();
                let Op::Call { method, kind, args } = inst.op else {
                    unreachable!()
                };
                assert_eq!(kind, CallKind::JvmStatic);
                let selected = &body.methods[method.index()].name;
                assert!(
                    selected.ends_with("Locations")
                        || selected.ends_with("LocationsUnsigned")
                        || selected == "alignLocation"
                        || selected == "locationAddr"
                );
                if name.starts_with("offset") || name == "align_offset" {
                    let size =
                        body.args[args.start as usize + if name == "align_offset" { 2 } else { 4 }];
                    assert_eq!(
                        body.scalar_value(size).unwrap().bits(),
                        if typed { 16 } else { 4 }
                    );
                }
                if protected {
                    assert!(body.blocks.iter().any(|block| matches!(block.terminator,
                        Some(Terminator::Invoke { inst, .. }) if inst.index() == id)));
                }
                crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
            }
        }
    }
}

#[test]
fn dynamic_stride_comes_from_the_root() {
    let mut types = Types::default();
    let long = types.scalar(ScalarType::I64);
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let record = types.symbol("test/Record");
    let record = types.intern(Type::Class(record));
    let pointer = types.intern(Type::Pointer(record));
    let mut b = Builder::new(&types, long);
    let root = b.parameter(b.current(), object);
    let offset = b.parameter(b.current(), long);
    let origin = b.parameter(b.current(), object);
    let parts = b.args([root, offset]);
    let packed = b.emit(Op::AddressPack(parts), Some(pointer)).unwrap();
    let origin_parts = b.args([origin, offset]);
    let origin = b
        .emit(Op::AddressPack(origin_parts), Some(pointer))
        .unwrap();
    let method = b.method(MethodRef {
        owner: "org/rustlang/runtime/Pointer".into(),
        name: "offsetFrom".into(),
        params: vec![pointer, pointer],
        returns: long,
        interface: false,
    });
    let args = b.args([packed, origin]);
    let result = b
        .emit(
            Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            },
            Some(long),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let mut body = b.finish().unwrap();
    lower_address_observers(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    assert!(!live(&body, &types).values[packed.index()]);
    let Op::Call { args, .. } = body
        .instructions
        .iter()
        .find(|inst| matches!(inst.op, Op::Call { .. }))
        .unwrap()
        .op
    else {
        unreachable!()
    };
    let args = &body.args[args.range()];
    assert!(!live(&body, &types).values[origin.index()]);
    assert_eq!(args[3], offset);
    assert_eq!(body.scalar_value(args[4]).unwrap().bits() as i64, -1);
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}
