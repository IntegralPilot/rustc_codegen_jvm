use super::*;
use crate::{ir::*, scalar::ScalarType};

#[test]
fn intrinsic_calls_keep_address_components_and_unwind_edges() {
    for name in [
        "atomicLoad",
        "atomicStore",
        "atomicExchange",
        "atomicAdd",
        "atomicSubtract",
        "atomicAnd",
        "atomicNand",
        "atomicOr",
        "atomicXor",
        "atomicMax",
        "atomicMin",
        "atomicUnsignedMax",
        "atomicUnsignedMin",
        "atomicCompareExchange",
        "writeBytes",
    ] {
        for decomposed in [false, true] {
            for typed in [false, true] {
                let mut types = Types::default();
                let long = types.scalar(ScalarType::I64);
                let int = types.scalar(ScalarType::I32);
                let void = types.intern(Type::Unit);
                let object = types.symbol("java/lang/Object");
                let object = types.intern(Type::Class(object));
                let pointer = types.intern(Type::Pointer(long));
                let result = if matches!(name, "atomicStore" | "writeBytes") {
                    void
                } else {
                    long
                };
                let mut b = Builder::new(&types, result);
                let address = if decomposed {
                    let root = b.parameter(b.current(), object);
                    let offset = b.parameter(b.current(), long);
                    let parts = b.args([root, offset]);
                    b.emit(
                        if typed {
                            Op::TypedAddressPack {
                                parts,
                                size: 8,
                                codec: None,
                            }
                        } else {
                            Op::AddressPack(parts)
                        },
                        Some(pointer),
                    )
                    .unwrap()
                } else {
                    b.parameter(b.current(), pointer)
                };
                let mut params = vec![pointer];
                let mut values = vec![address];
                if name == "writeBytes" {
                    params.extend([int, long]);
                    values.extend([
                        b.parameter(b.current(), int),
                        b.parameter(b.current(), long),
                    ]);
                } else {
                    if name != "atomicLoad" {
                        params.push(long);
                        values.push(b.parameter(b.current(), long));
                    }
                    if name == "atomicCompareExchange" {
                        params.push(long);
                        values.push(b.parameter(b.current(), long));
                    }
                    for _ in 0..if name == "atomicCompareExchange" {
                        3
                    } else {
                        2
                    } {
                        params.push(int);
                        values.push(b.parameter(b.current(), int));
                    }
                }
                let original_arity = params.len();
                let method = b.method(MethodRef {
                    owner: "org/rustlang/runtime/Pointer".into(),
                    name: name.into(),
                    params,
                    returns: result,
                    interface: false,
                });
                let args = b.args(values);
                let handler = b.create_block();
                let result = b.invoke(
                    Op::Call {
                        method,
                        kind: CallKind::JvmStatic,
                        args,
                    },
                    (result != void).then_some(result),
                    handler,
                );
                b.terminate(Terminator::Return(result));
                b.switch_to(handler);
                b.terminate(Terminator::Rethrow);
                let mut body = b.finish().unwrap();
                lower_address_intrinsics(&mut body, &mut types);
                verify(&body, &types).unwrap();
                let (id, method, args) = body
                    .instructions
                    .iter()
                    .enumerate()
                    .find_map(|(id, inst)| {
                        let Op::Call { method, args, .. } = inst.op else {
                            return None;
                        };
                        Some((id, &body.methods[method.index()], args))
                    })
                    .unwrap();
                assert_eq!(method.name, name);
                assert_eq!(args.len as usize, original_arity + usize::from(decomposed));
                if decomposed {
                    assert!(!live(&body, &types).values[address.index()]);
                    assert_eq!(&method.params[..2], &[object, long]);
                }
                assert!(body.blocks.iter().any(|block| matches!(block.terminator,
                    Some(Terminator::Invoke { inst, .. }) if inst.index() == id)));
                crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
            }
        }
    }
}
