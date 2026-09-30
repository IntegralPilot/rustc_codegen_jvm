use super::{decompose_addresses, live, lower_typed_storage};
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

#[test]
fn scalar_storage_uses_one_primitive_array_without_boxing() {
    for scalar in [
        ScalarType::Bool,
        ScalarType::U8,
        ScalarType::I16,
        ScalarType::U16,
        ScalarType::F16,
        ScalarType::I32,
        ScalarType::U32,
        ScalarType::I64,
        ScalarType::U64,
        ScalarType::F32,
        ScalarType::F64,
    ] {
        for unwind in [false, true] {
            let mut types = Types::default();
            let scalar_ty = types.scalar(scalar);
            let pointer = types.intern(Type::Pointer(scalar_ty));
            let mut b = Builder::new(&types, scalar_ty);
            let initial = b.parameter(b.current(), scalar_ty);
            let cell = if unwind {
                let handler = b.create_block();
                let cell = b
                    .invoke(Op::ScalarCell(initial), Some(pointer), handler)
                    .unwrap();
                let continuation = b.current();
                b.switch_to(handler);
                b.terminate(Terminator::Rethrow);
                b.switch_to(continuation);
                cell
            } else {
                b.emit(Op::ScalarCell(initial), Some(pointer)).unwrap()
            };
            let result = b.emit(Op::Load(cell), Some(scalar_ty)).unwrap();
            b.terminate(Terminator::Return(Some(result)));
            let mut body = b.finish().unwrap();
            let promoted = super::promote_cells(body.clone(), &types, &[(cell, initial)]).unwrap();
            let private = live(&promoted, &types);
            assert!(
                !promoted
                    .instructions
                    .iter()
                    .enumerate()
                    .any(|(id, i)| private.instructions[id]
                        && matches!(i.op, Op::ScalarCell(_) | Op::NewArray(_)))
            );
            lower_typed_storage(&mut body, &mut types, &[(cell, initial)], None);
            decompose_addresses(&mut body, &mut types, None);
            verify(&body, &types).unwrap();
            let retained = live(&body, &types);
            assert!(
                !body
                    .instructions
                    .iter()
                    .enumerate()
                    .any(|(id, i)| retained.instructions[id]
                        && matches!(
                            i.op,
                            Op::ScalarCell(_) | Op::AddressPack(_) | Op::Adapt(_) | Op::Call { .. }
                        ))
            );
            let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
            assert_eq!(
                code.instructions
                    .iter()
                    .filter(|i| matches!(i, crate::classfile::attributes::Instruction::Newarray(_)))
                    .count(),
                1
            );
        }
    }
}

#[test]
fn typed_storage_keeps_its_owner_and_allocation_unwind_edge() {
    for unwind in [false, true] {
        let mut types = Types::default();
        let object = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(object));
        let string = types.symbol("java/lang/String");
        let string = types.intern(Type::Class(string));
        let pointer = types.intern(Type::Pointer(object));
        let int = types.scalar(ScalarType::I32);
        let mut b = Builder::new(&types, object);
        let initial = b.parameter(b.current(), object);
        let size = b.constant(int, Scalar::integer(ScalarType::I32, 8).unwrap());
        let codec = ConstId::new(b.body.constants.len());
        b.body.constants.push(Constant::Null(string));
        let codec = b.emit(Op::Constant(codec), Some(string)).unwrap();
        let method = b.method(MethodRef {
            owner: "org/rustlang/runtime/Pointer".into(),
            name: "cellAligned".into(),
            params: vec![object, int, string, int],
            returns: pointer,
            interface: false,
        });
        let args = b.args([initial, size, codec, size]);
        let op = Op::Call {
            method,
            kind: CallKind::JvmStatic,
            args,
        };
        let cell = if unwind {
            let handler = b.create_block();
            let cell = b.invoke(op, Some(pointer), handler).unwrap();
            let continuation = b.current();
            b.switch_to(handler);
            b.terminate(Terminator::Rethrow);
            b.switch_to(continuation);
            cell
        } else {
            b.emit(op, Some(pointer)).unwrap()
        };
        let result = b.emit(Op::Load(cell), Some(object)).unwrap();
        b.terminate(Terminator::Return(Some(result)));
        let mut body = b.finish().unwrap();
        let mut debug = DebugInfo::default();
        debug.events.push(DebugEvent {
            block: body.entry,
            position: 2,
            change: DebugChange::Scope(0),
            line: Some(1),
        });
        lower_typed_storage(&mut body, &mut types, &[(cell, initial)], Some(&mut debug));
        verify(&body, &types).unwrap();
        decompose_addresses(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        let live = live(&body, &types);
        assert!(
            !body
                .instructions
                .iter()
                .enumerate()
                .any(|(id, inst)| live.instructions[id]
                    && matches!(inst.op, Op::AddressPack(_) | Op::Load(_)))
        );
        assert_eq!(
            body.blocks
                .iter()
                .filter(|b| matches!(b.terminator, Some(Terminator::Invoke { .. })))
                .count(),
            usize::from(unwind)
        );
        assert!(body.instructions.iter().any(|i| matches!(i.op,
            Op::Call { method, .. } if body.methods[method.index()].name == "storageAligned")));
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}
