use super::lower_memory_copies;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

#[test]
fn exact_copy_components_keep_handlers_and_do_not_materialize_pointers() {
    assert!(
        std::mem::size_of::<Op>() <= 20,
        "copy metadata must not inflate ordinary IR instructions"
    );
    for typed in [false, true] {
        for protected in [false, true] {
            let mut types = Types::default();
            let unit = types.intern(Type::Unit);
            let long = types.scalar(ScalarType::I64);
            let int = types.scalar(ScalarType::I32);
            let object = types.symbol("java/lang/Object");
            let object = types.intern(Type::Class(object));
            let pointer = types.intern(Type::Pointer(int));
            let codec = types.symbol("test/Codec#value#[I");
            let mut b = Builder::new(&types, unit);
            let root = b.parameter(b.current(), object);
            let offset = b.constant(long, Scalar::integer(ScalarType::I64, 4).unwrap());
            let count = b.constant(long, Scalar::integer(ScalarType::I64, 16).unwrap());
            let parts = b.args([root, offset]);
            let op = if typed {
                Op::TypedAddressPack {
                    parts,
                    size: 8,
                    codec: Some(codec),
                }
            } else {
                Op::AddressPack(parts)
            };
            let source = b.emit(op, Some(pointer)).unwrap();
            let method = b.method(MethodRef {
                owner: "org/rustlang/runtime/Pointer".into(),
                name: "copyNonOverlapping".into(),
                params: vec![pointer, pointer, long],
                returns: unit,
                interface: false,
            });
            let args = b.args([source, source, count]);
            let call = Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            };
            if protected {
                let handler = b.create_block();
                b.invoke(call, None, handler);
                b.terminate(Terminator::Return(None));
                b.switch_to(handler);
                b.terminate(Terminator::Rethrow);
            } else {
                b.emit(call, None);
                b.terminate(Terminator::Return(None));
            }
            let mut body = b.finish().unwrap();
            lower_memory_copies(&mut body, &mut types);
            verify(&body, &types).unwrap();
            let (index, op) = body
                .instructions
                .iter()
                .enumerate()
                .find(|(_, i)| matches!(i.op, Op::CopyStorage { .. }))
                .unwrap();
            let Op::CopyStorage {
                layouts,
                nonoverlapping,
                ..
            } = op.op
            else {
                unreachable!()
            };
            assert!(nonoverlapping);
            let layouts = layouts.map(|ty| {
                let Type::Layout(id) = types.get(ty).unwrap() else {
                    panic!()
                };
                let layout = types.get_layout(id);
                (layout.size, layout.codec)
            });
            assert_eq!(
                layouts,
                if typed {
                    [(8, Some(codec)); 2]
                } else {
                    [(4, None); 2]
                }
            );
            assert!(!super::live(&body, &types).values[source.index()]);
            if protected {
                assert!(body.blocks.iter().any(|b| matches!(b.terminator,
                    Some(Terminator::Invoke { inst, .. }) if inst.index() == index)));
            }
            crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        }
    }
}
