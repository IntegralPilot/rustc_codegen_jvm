use crate::ir::*;
use crate::scalar::ScalarType;

#[test]
fn uninitialized_view_fields_use_default_components() {
    for utf8 in [false, true] {
        let mut types = Types::default();
        let byte = types.scalar(ScalarType::U8);
        let view = types.intern(if utf8 { Type::Str } else { Type::Slice(byte) });
        let name = types.symbol("test/Partial");
        let owner = types.intern(Type::Class(name));
        let void = types.intern(Type::Unit);
        let mut b = Builder::new(&types, owner);
        let null = ConstId::new(b.body.constants.len());
        b.body.constants.push(Constant::Null(view));
        let initial = b.emit(Op::Constant(null), Some(view)).unwrap();
        let constructor = b.method(MethodRef {
            owner: "test/Partial".into(),
            name: "<init>".into(),
            params: vec![view],
            returns: void,
            interface: false,
        });
        let args = b.args([initial]);
        let result = b
            .emit(
                Op::Call {
                    method: constructor,
                    kind: CallKind::Constructor,
                    args,
                },
                Some(owner),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(result)));
        let mut body = b.finish().unwrap();
        super::lower_borrowed_fields(&mut body, &mut types, |_| true, None);
        super::decompose_views(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        assert!(
            !code
                .instructions
                .iter()
                .any(|i| matches!(i, crate::classfile::attributes::Instruction::Getfield(_)))
        );
    }
}

#[test]
fn stored_scalar_borrows_load_and_store_without_pointer_carriers() {
    check_stored_borrow(false);
    check_stored_borrow(true);
}

#[test]
fn stored_views_have_no_live_view_carriers() {
    for utf8 in [false, true] {
        for indirect in [false, true] {
            let mut types = Types::default();
            let byte = types.scalar(ScalarType::U8);
            let len = types.scalar(ScalarType::U64);
            let view = types.intern(if utf8 { Type::Str } else { Type::Slice(byte) });
            let name = types.symbol("test/Views");
            let owner = types.intern(Type::Class(name));
            let borrowed = types.intern(Type::Pointer(owner));
            let mut b = Builder::new(&types, len);
            let object = b.parameter(b.current(), if indirect { borrowed } else { owner });
            let value = b.parameter(b.current(), view);
            let field = b.field(FieldRef {
                owner,
                name: "value".into(),
                ty: view,
                is_static: false,
            });
            let loaded = if indirect {
                let projection = b.projection(PointerProjection {
                    parent: None,
                    field,
                    offset: 0,
                    size: 16,
                    codec: None,
                });
                b.emit(
                    Op::StoreField {
                        base: object,
                        projection,
                        value,
                    },
                    None,
                );
                b.emit(
                    Op::LoadField {
                        base: object,
                        projection,
                    },
                    Some(view),
                )
                .unwrap()
            } else {
                b.emit(
                    Op::SetField {
                        object,
                        field,
                        value,
                    },
                    None,
                );
                b.emit(Op::GetField { object, field }, Some(view)).unwrap()
            };
            let result = b.emit(Op::Length(loaded), Some(len)).unwrap();
            b.terminate(Terminator::Return(Some(result)));
            let mut body = b.finish().unwrap();
            super::lower_component_arguments(&mut body, &mut types, true, |_| false, None);
            super::lower_borrowed_fields(&mut body, &mut types, |_| true, None);
            super::decompose_views(&mut body, &mut types, None);
            verify(&body, &types).unwrap();
            let live = super::live(&body, &types);
            assert!(
                !body
                    .instructions
                    .iter()
                    .enumerate()
                    .any(|(index, i)| live.instructions[index]
                        && matches!(i.op, Op::ViewPack(_) | Op::ViewPart { .. } | Op::Length(_)))
            );
            crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        }
    }
}

fn check_stored_borrow(indirect: bool) {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let pointer = types.intern(Type::Pointer(int));
    let name = types.symbol("test/References");
    let owner = types.intern(Type::Class(name));
    let borrowed = types.intern(Type::Pointer(owner));
    let mut b = Builder::new(&types, int);
    let object = b.parameter(b.current(), if indirect { borrowed } else { owner });
    let incoming = b.parameter(b.current(), pointer);
    let field = b.field(FieldRef {
        owner,
        name: "value".into(),
        ty: pointer,
        is_static: false,
    });
    let loaded = if indirect {
        let projection = b.projection(PointerProjection {
            parent: None,
            field,
            offset: 0,
            size: 8,
            codec: None,
        });
        b.emit(
            Op::StoreField {
                base: object,
                projection,
                value: incoming,
            },
            None,
        );
        b.emit(
            Op::LoadField {
                base: object,
                projection,
            },
            Some(pointer),
        )
        .unwrap()
    } else {
        b.emit(
            Op::SetField {
                object,
                field,
                value: incoming,
            },
            None,
        );
        b.emit(Op::GetField { object, field }, Some(pointer))
            .unwrap()
    };
    let read = b.emit(Op::Load(loaded), Some(int)).unwrap();
    b.terminate(Terminator::Return(Some(read)));
    let mut body = b.finish().unwrap();
    super::lower_component_arguments(&mut body, &mut types, true, |_| false, None);
    super::lower_borrowed_fields(
        &mut body,
        &mut types,
        |owner| owner == "test/References",
        None,
    );
    super::decompose_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let live = super::live(&body, &types);
    for (index, inst) in body.instructions.iter().enumerate() {
        if live.instructions[index] {
            assert!(!matches!(
                inst.op,
                Op::AddressPack(_) | Op::AddressPart { .. } | Op::Load(_)
            ));
            if let Op::GetField { field, .. } | Op::SetField { field, .. } = inst.op {
                assert_ne!(body.fields[field.index()].ty, pointer);
            }
        }
    }
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn split_field_components_share_resolution_only_before_the_next_effect() {
    use crate::classfile::{
        Constant, attributes::Instruction, constant_pool::InternedConstantPool,
    };
    for intervening_call in [false, true] {
        let mut types = Types::default();
        let unit = types.intern(Type::Unit);
        let byte = types.scalar(ScalarType::U8);
        let view = types.intern(Type::Slice(byte));
        let owner_name = types.symbol("test/Views");
        let owner = types.intern(Type::Class(owner_name));
        let pointer = types.intern(Type::Pointer(owner));
        let parts = ComponentShape::View.parts(&mut types).collect::<Vec<_>>();
        let mut b = Builder::new(&types, unit);
        let base = b.parameter(b.current(), pointer);
        let field = b.field(FieldRef {
            owner,
            name: "view".into(),
            ty: view,
            is_static: false,
        });
        let projection = b.projection(PointerProjection {
            parent: None,
            field,
            offset: 0,
            size: 16,
            codec: None,
        });
        for (index, &ty) in parts.iter().enumerate() {
            if intervening_call && index == 1 {
                let method = b.method(MethodRef {
                    owner: "test/External".into(),
                    name: "replaceStorage".into(),
                    params: vec![pointer],
                    returns: unit,
                    interface: false,
                });
                let args = b.args([base]);
                b.emit(
                    Op::Call {
                        method,
                        kind: CallKind::JvmStatic,
                        args,
                    },
                    None,
                );
            }
            b.emit(
                Op::LoadFieldPart {
                    base,
                    projection,
                    index: index as u8,
                },
                Some(ty),
            );
        }
        b.terminate(Terminator::Return(None));
        let body = b.finish().unwrap();
        let mut cp = InternedConstantPool::default();
        let code = crate::jvm::select::compile(&body, &types, &mut cp).unwrap();
        let pool = cp.into_inner();
        let resolutions = code
            .instructions
            .iter()
            .filter(|instruction| {
                let Instruction::Invokevirtual(index) = instruction else {
                    return false;
                };
                let Some(Constant::MethodRef {
                    name_and_type_index,
                    ..
                }) = pool.get(*index)
                else {
                    return false;
                };
                let (name, _) = pool.try_get_name_and_type(*name_and_type_index).unwrap();
                pool.try_get_utf8(*name).unwrap() == "directAggregate"
            })
            .count();
        assert_eq!(resolutions, if intervening_call { 2 } else { 1 });
    }
}
