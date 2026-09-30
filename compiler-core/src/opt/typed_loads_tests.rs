use super::*;
use crate::ir::*;
use crate::scalar::ScalarType;

#[test]
fn typed_borrow_loads_preserve_unwind_edges_and_shared_bindings() {
    assert!(std::mem::size_of::<Op>() <= 20);
    for (named, commit) in [(false, false), (true, false), (true, true)] {
        for protected in [false, true] {
            let (mut body, types, packed) = borrowed_load(named, protected, commit);
            lower_typed_loads(&mut body, &types);
            verify(&body, &types).unwrap();
            assert_eq!(live(&body, &types).values[packed.index()], commit);
            let lowered = body
                .instructions
                .iter()
                .enumerate()
                .find(|(_, inst)| matches!(inst.op, Op::LoadTyped { .. }));
            if commit {
                assert!(lowered.is_none());
            } else {
                let (id, inst) = lowered.unwrap();
                let Op::LoadTyped { parts, size, codec } = inst.op else {
                    unreachable!()
                };
                assert_eq!(parts.len, if named { 3 } else { 2 });
                assert_eq!(size, 16);
                assert_eq!(
                    types.symbol_name(codec.unwrap()),
                    Some("test/EnumCodec#enum#Ltest/Enum;")
                );
                if protected {
                    assert!(body.blocks.iter().any(|block| matches!(block.terminator,
                        Some(Terminator::Invoke { inst, .. }) if inst.index() == id)));
                }
            }
            crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        }
    }
}

#[test]
fn semantic_object_borrows_fuse_but_copies_and_shared_views_do_not() {
    use crate::classfile::attributes::Instruction;

    for kind in 0..6 {
        for (copied, shared) in [(false, false), (false, true), (true, false)] {
            if copied && kind == 5 {
                continue;
            }
            for protected in [false, true] {
                let mut types = Types::default();
                let long = types.scalar(ScalarType::I64);
                let object = types.symbol("java/lang/Object");
                let object = types.intern(Type::Class(object));
                let name = types.symbol("test/Variant");
                let payload = match kind {
                    0 => types.intern(Type::Class(name)),
                    1 => types.intern(Type::Interface(name)),
                    2 => types.intern(Type::Array(long)),
                    3 => object,
                    4 => types.intern(Type::Pointer(long)),
                    _ => long,
                };
                let pointer = types.intern(Type::Pointer(payload));
                let codec = types.symbol("test/EnumCodec#enum#Ltest/Enum;");
                let mut b = Builder::new(&types, payload);
                let root = b.parameter(b.current(), object);
                let offset = b.parameter(b.current(), long);
                let parts = b.args([root, offset]);
                let packed = b
                    .emit(
                        Op::TypedAddressPack {
                            parts,
                            size: 16,
                            codec: Some(codec),
                        },
                        Some(pointer),
                    )
                    .unwrap();
                let op = if copied {
                    Op::LoadCopy(packed)
                } else {
                    Op::Load(packed)
                };
                let handler = protected.then(|| b.create_block());
                let value = match handler {
                    Some(handler) => b.invoke(op, Some(payload), handler),
                    None => b.emit(op, Some(payload)),
                }
                .unwrap();
                if shared {
                    b.emit(Op::Commit(packed), None);
                }
                b.terminate(Terminator::Return(Some(value)));
                if let Some(handler) = handler {
                    b.switch_to(handler);
                    b.terminate(Terminator::Rethrow);
                }
                let mut body = b.finish().unwrap();
                lower_typed_loads(&mut body, &types);
                verify(&body, &types).unwrap();
                let expected = kind < 3 && !copied && !shared;
                assert_eq!(!live(&body, &types).values[packed.index()], expected);
                let ValueDef::Inst(id) = body.values[value.index()].def else {
                    panic!()
                };
                assert_eq!(
                    matches!(body.instructions[id.index()].op, Op::LoadTyped { .. }),
                    expected
                );
                if protected {
                    assert!(body.blocks.iter().any(|block| matches!(block.terminator,
                        Some(Terminator::Invoke { inst, .. }) if inst == id)));
                }
                let mut pool = Default::default();
                let code = crate::jvm::select::compile(&body, &types, &mut pool).unwrap();
                let owner = pool.add_class("org/rustlang/runtime/Pointer").unwrap();
                let load = pool.add_method_ref(owner, "loadTypedStorage",
                    "(Ljava/lang/Object;JILjava/lang/String;Ljava/lang/String;)Ljava/lang/Object;").unwrap();
                assert_eq!(
                    code.instructions.contains(&Instruction::Invokestatic(load)),
                    expected
                );
                if expected && kind < 2 {
                    let name = pool.add_name_string("test/Variant").unwrap();
                    assert!(code.instructions.contains(&Instruction::Ldc_w(name)));
                }
            }
        }
    }
}

#[test]
fn enum_downcasts_keep_the_entry_components_through_erased_annotations() {
    let mut types = Types::default();
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let string = types.symbol("java/lang/String");
    let string = types.intern(Type::Class(string));
    let variant = types.symbol("test/Enum");
    let variant = types.intern(Type::Interface(variant));
    let pointer = types.intern(Type::Pointer(variant));
    let runtime = types.symbol("org/rustlang/runtime/Pointer");
    let runtime = types.intern(Type::Class(runtime));
    let codec = types.symbol("test/EnumCodec#enum#Ltest/Enum;");
    let mut b = Builder::new(&types, object);
    let source = b.parameter(b.current(), pointer);
    let target = b.parameter(b.current(), string);
    let erased = b.emit(Op::Reinterpret(source), Some(runtime)).unwrap();
    let recovered = b.emit(Op::Reinterpret(erased), Some(pointer)).unwrap();
    let retyped = b
        .emit(
            Op::RetypeAddress {
                pointer: recovered,
                size: 16,
                codec: Some(codec),
            },
            Some(pointer),
        )
        .unwrap();
    let method = b.method(MethodRef {
        owner: "org/rustlang/runtime/Pointer".into(),
        name: "getObjectAs".into(),
        params: vec![string],
        returns: object,
        interface: false,
    });
    let args = b.args([retyped, target]);
    let result = b
        .emit(
            Op::Call {
                method,
                kind: CallKind::Virtual,
                args,
            },
            Some(object),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| false, None);
    lower_typed_addresses(&mut body, &mut types, None);
    decompose_addresses(&mut body, &mut types, None);
    lower_typed_loads(&mut body, &types);
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
                    Op::AddressPack(_) | Op::TypedAddressPack { .. } | Op::RetypeAddress { .. }
                ))
    );
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

fn borrowed_load(named: bool, protected: bool, commit: bool) -> (Body, Types, ValueId) {
    let mut types = Types::default();
    let long = types.scalar(ScalarType::I64);
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let string = types.symbol("java/lang/String");
    let string = types.intern(Type::Class(string));
    let variant = types.symbol("test/Enum");
    let variant = types.intern(Type::Interface(variant));
    let pointer = types.intern(Type::Pointer(variant));
    let runtime = types.symbol("org/rustlang/runtime/Pointer");
    let runtime = types.intern(Type::Class(runtime));
    let codec = types.symbol("test/EnumCodec#enum#Ltest/Enum;");
    let mut b = Builder::new(&types, object);
    let root = b.parameter(b.current(), object);
    let offset = b.parameter(b.current(), long);
    let target = b.parameter(b.current(), string);
    let parts = b.args([root, offset]);
    let packed = b
        .emit(
            Op::TypedAddressPack {
                parts,
                size: 16,
                codec: Some(codec),
            },
            Some(pointer),
        )
        .unwrap();
    let erased = b.emit(Op::Reinterpret(packed), Some(runtime)).unwrap();
    let method = b.method(MethodRef {
        owner: "org/rustlang/runtime/Pointer".into(),
        name: if named { "getObjectAs" } else { "getObject" }.into(),
        params: if named { vec![string] } else { vec![] },
        returns: object,
        interface: false,
    });
    let args = b.args(if named {
        vec![erased, target]
    } else {
        vec![erased]
    });
    let call = Op::Call {
        method,
        kind: CallKind::Virtual,
        args,
    };
    let (result, handler) = if protected {
        let handler = b.create_block();
        (
            b.invoke(call, Some(object), handler).unwrap(),
            Some(handler),
        )
    } else {
        (b.emit(call, Some(object)).unwrap(), None)
    };
    if commit {
        b.emit(Op::Commit(packed), None);
    }
    b.terminate(Terminator::Return(Some(result)));
    if let Some(handler) = handler {
        b.switch_to(handler);
        b.terminate(Terminator::Rethrow);
    }
    (b.finish().unwrap(), types, packed)
}
