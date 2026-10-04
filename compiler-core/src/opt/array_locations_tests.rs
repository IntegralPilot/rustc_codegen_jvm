use super::lower_array_locations;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

#[test]
fn only_private_arrays_use_native_locations() {
    for (escape, byte_offset) in [(false, 4), (true, 4), (false, 1)] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let long = types.scalar(ScalarType::I64);
        let array_ty = types.intern(Type::Array(int));
        let symbol = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(symbol));
        let unit = types.intern(Type::Unit);
        let mut b = Builder::new(&types, int);
        let size = b.constant(int, Scalar::integer(ScalarType::I32, 4).unwrap());
        let offset = b.constant(long, Scalar::integer(ScalarType::I64, byte_offset).unwrap());
        let array = b.emit(Op::NewArray(size), Some(array_ty)).unwrap();
        let root = b
            .emit(
                Op::ViewRoot {
                    backing: array,
                    size: 4,
                    codec: None,
                },
                Some(object),
            )
            .unwrap();
        if escape {
            let method = b.method(MethodRef {
                owner: "Unknown".into(),
                name: "borrow".into(),
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
        let parts = b.args([root, offset]);
        b.emit(Op::StoreAddress { parts, value: size }, None);
        let value = b.emit(Op::LoadAddress(parts), Some(int)).unwrap();
        b.terminate(Terminator::Return(Some(value)));
        let mut body = b.finish().unwrap();
        assert_eq!(lower_array_locations(&mut body, &mut types), !escape);
        verify(&body, &types).unwrap();
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::ArrayGet { native: true, .. })),
            !escape && byte_offset == 4
        );
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::ArraySet { native: true, .. })),
            !escape && byte_offset == 4
        );
    }
}

#[test]
fn pointer_differences_require_the_same_allocation() {
    for shared in [false, true] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let long = types.scalar(ScalarType::I64);
        let array_ty = types.intern(Type::Array(int));
        let symbol = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(symbol));
        let mut b = Builder::new(&types, long);
        let size = b.constant(int, Scalar::integer(ScalarType::I32, 4).unwrap());
        let stride = b.constant(long, Scalar::integer(ScalarType::I64, 4).unwrap());
        let zero = b.constant(long, Scalar::integer(ScalarType::I64, 0).unwrap());
        let left = b.emit(Op::NewArray(size), Some(array_ty)).unwrap();
        let right = if shared {
            left
        } else {
            b.emit(Op::NewArray(size), Some(array_ty)).unwrap()
        };
        let left = b.emit(Op::Reinterpret(left), Some(object)).unwrap();
        let right = b.emit(Op::Reinterpret(right), Some(object)).unwrap();
        let method = b.method(MethodRef {
            owner: "org/rustlang/runtime/Pointer".into(),
            name: "offsetLocations".into(),
            params: vec![object, long, object, long, long],
            returns: long,
            interface: false,
        });
        let args = b.args([left, stride, right, zero, stride]);
        let distance = b
            .emit(
                Op::Call {
                    method,
                    kind: CallKind::JvmStatic,
                    args,
                },
                Some(long),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(distance)));
        let mut body = b.finish().unwrap();
        assert_eq!(lower_array_locations(&mut body, &mut types), shared);
        verify(&body, &types).unwrap();
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::Call { .. })),
            !shared
        );
    }
}

#[test]
fn inactive_payloads_do_not_hide_array_origins_but_null_does() {
    for uninit in [false, true] {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let boolean = types.scalar(ScalarType::Bool);
        let long = types.scalar(ScalarType::I64);
        let array_ty = types.intern(Type::Array(int));
        let symbol = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(symbol));
        let mut b = Builder::new(&types, int);
        let condition = b.parameter(b.current(), boolean);
        let one = b.constant(int, Scalar::integer(ScalarType::I32, 1).unwrap());
        let offset = b.constant(long, Scalar::integer(ScalarType::I64, 0).unwrap());
        let array = b.emit(Op::NewArray(one), Some(array_ty)).unwrap();
        let root = b.emit(Op::Reinterpret(array), Some(object)).unwrap();
        let constant = ConstId::new(b.body.constants.len());
        b.body.constants.push(if uninit {
            Constant::Uninit(object)
        } else {
            Constant::Null(object)
        });
        let empty = b.emit(Op::Constant(constant), Some(object)).unwrap();
        let some = b.create_block();
        let none = b.create_block();
        let join = b.create_block();
        let payload = b.parameter(join, object);
        b.branch(condition, some, none);
        b.switch_to(some);
        b.jump(join, vec![root]);
        b.switch_to(none);
        b.jump(join, vec![empty]);
        b.switch_to(join);
        let parts = b.args([payload, offset]);
        let loaded = b.emit(Op::LoadAddress(parts), Some(int)).unwrap();
        b.terminate(Terminator::Return(Some(loaded)));
        let mut body = b.finish().unwrap();
        assert_eq!(lower_array_locations(&mut body, &mut types), uninit);
        verify(&body, &types).unwrap();
    }
}
