use super::*;
use crate::{ir::*, scalar::ScalarType};

#[test]
fn stored_borrows_cross_load_store_and_return_without_carriers() {
    for view in 0..3 {
        let mut types = Types::default();
        let byte = types.scalar(ScalarType::U8);
        let borrowed = types.intern(match view {
            0 => Type::Pointer(byte),
            1 => Type::Slice(byte),
            _ => Type::Str,
        });
        let location = types.intern(Type::Pointer(borrowed));
        let mut builder = Builder::new(&types, borrowed);
        let destination = builder.parameter(builder.current(), location);
        let replacement = builder.parameter(builder.current(), borrowed);
        let original = builder.emit(Op::Load(destination), Some(borrowed)).unwrap();
        builder.emit(
            Op::Store {
                pointer: destination,
                value: replacement,
            },
            None,
        );
        builder.terminate(Terminator::Return(Some(original)));
        let mut body = builder.finish().unwrap();
        lower_borrowed_memory(&mut body, &mut types, None);
        lower_component_arguments(&mut body, &mut types, true, borrowed_memory_method, None);
        lower_component_returns(&mut body, &mut types, true, borrowed_memory_method, None);
        decompose_views(&mut body, &mut types, None);
        decompose_addresses(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        let live = live(&body, &types);
        let mut calls = 0;
        for (index, instruction) in body.instructions.iter().enumerate() {
            if !live.instructions[index] {
                continue;
            }
            assert!(
                !matches!(
                    instruction.op,
                    Op::AddressPack(_)
                        | Op::ViewPack(_)
                        | Op::Load(_)
                        | Op::Store { .. }
                        | Op::NewArray(_)
                ),
                "{instruction:?}"
            );
            if let Op::Call { method, .. } = instruction.op {
                let method = &body.methods[method.index()];
                assert!(
                    method
                        .params
                        .iter()
                        .all(|&p| ComponentShape::of(&types, p).is_none())
                );
                assert!(ComponentShape::of(&types, method.returns).is_none());
                calls += 1;
            }
        }
        assert_eq!(calls, 2);
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn references_to_stored_borrows_keep_their_enclosing_owner() {
    for view in [false, true] {
        let mut types = Types::default();
        let byte = types.scalar(ScalarType::U8);
        let borrowed = types.intern(if view {
            Type::Slice(byte)
        } else {
            Type::Pointer(byte)
        });
        let location = types.intern(Type::Pointer(borrowed));
        let name = types.symbol("Slot");
        let owner = types.intern(Type::Class(name));
        let aggregate = types.intern(Type::Pointer(owner));
        let mut builder = Builder::new(&types, borrowed);
        let root = builder.parameter(builder.current(), aggregate);
        let field = builder.field(FieldRef {
            owner,
            name: "borrow".into(),
            ty: borrowed,
            is_static: false,
        });
        let projection = builder.projection(PointerProjection {
            parent: None,
            field,
            offset: 8,
            size: if view { 16 } else { 8 },
            codec: Some("layout".into()),
        });
        let address = builder
            .emit(
                Op::Project {
                    base: root,
                    projection,
                },
                Some(location),
            )
            .unwrap();
        let value = builder.emit(Op::Load(address), Some(borrowed)).unwrap();
        builder.terminate(Terminator::Return(Some(value)));
        let mut body = builder.finish().unwrap();
        lower_borrowed_memory(&mut body, &mut types, None);
        lower_component_arguments(&mut body, &mut types, true, borrowed_memory_method, None);
        lower_component_returns(&mut body, &mut types, true, borrowed_memory_method, None);
        decompose_views(&mut body, &mut types, None);
        decompose_addresses(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        let live = live(&body, &types);
        assert!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::ProjectRoot { .. }))
        );
        assert!(
            !body
                .instructions
                .iter()
                .enumerate()
                .any(|(i, instruction)| live.instructions[i]
                    && matches!(
                        instruction.op,
                        Op::Project { .. } | Op::AddressPack(_) | Op::ViewPack(_)
                    ))
        );
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}
