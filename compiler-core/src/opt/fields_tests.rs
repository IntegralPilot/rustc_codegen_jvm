use super::fields::promote_fields;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

#[test]
fn owned_field_copies_keep_exception_edges_and_eliminate_address_carriers() {
    for components in [false, true] {
        for protected in [false, true] {
            let mut types = Types::default();
            let byte = types.scalar(ScalarType::U8);
            let array = types.intern(Type::Array(byte));
            let array_pointer = types.intern(Type::Pointer(array));
            let owner = types.symbol("Pixel");
            let owner = types.intern(Type::Class(owner));
            let pointer = types.intern(Type::Pointer(owner));
            let mut b = Builder::new(&types, array);
            let base = b.parameter(b.current(), pointer);
            let field = b.field(FieldRef {
                owner,
                name: "rgba".into(),
                ty: array,
                is_static: false,
            });
            let projection = b.projection(PointerProjection {
                parent: None,
                field,
                offset: 4,
                size: 4,
                codec: Some("org/rustlang/runtime/ArrayMemoryCodec#array#[B#4".into()),
            });
            let address = b
                .emit(Op::Project { base, projection }, Some(array_pointer))
                .unwrap();
            let value = if protected {
                let handler = b.create_block();
                let value = b
                    .invoke(Op::LoadCopy(address), Some(array), handler)
                    .unwrap();
                b.terminate(Terminator::Return(Some(value)));
                b.switch_to(handler);
                b.terminate(Terminator::Rethrow);
                value
            } else {
                let value = b.emit(Op::LoadCopy(address), Some(array)).unwrap();
                b.terminate(Terminator::Return(Some(value)));
                value
            };
            let mut body = b.finish().unwrap();
            promote_fields(&mut body, &types);
            if components {
                super::lower_component_arguments(&mut body, &mut types, true, |_| false, None);
                super::decompose_addresses(&mut body, &mut types, None);
            }
            verify(&body, &types).unwrap();
            let ValueDef::Inst(copy) = body.values[value.index()].def else {
                panic!()
            };
            assert!(if components {
                matches!(
                    body.instructions[copy.index()].op,
                    Op::LoadStorageFieldCopy { .. }
                )
            } else {
                matches!(body.instructions[copy.index()].op, Op::LoadFieldCopy { .. })
            });
            if protected {
                assert!(body.blocks.iter().any(|b| matches!(b.terminator,
                    Some(Terminator::Invoke { inst, .. }) if inst == copy)));
            }
            let live = super::live(&body, &types);
            assert!(!live.values[address.index()]);
            assert!(
                !body
                    .instructions
                    .iter()
                    .enumerate()
                    .any(|(i, inst)| live.instructions[i]
                        && matches!(inst.op, Op::Project { .. } | Op::AddressPack(_)))
            );
            let mut pool = Default::default();
            let code = crate::jvm::select::compile(&body, &types, &mut pool).unwrap();
            let owner = pool.add_class("org/rustlang/runtime/Pointer").unwrap();
            let load = pool.add_method_ref(owner, "loadStorageFieldCopy",
                "(Ljava/lang/Object;JLjava/lang/String;Ljava/lang/String;JJLjava/lang/String;Ljava/lang/String;)Ljava/lang/Object;").unwrap();
            assert!(code.instructions.contains(
                &crate::classfile::attributes::Instruction::Invokestatic(load)
            ));
        }
    }
}

fn layout(types: &mut Types) -> (TypeId, TypeId, TypeId) {
    let scalar = types.scalar(ScalarType::I64);
    let owner = types.symbol("Counter");
    let owner = types.intern(Type::Class(owner));
    let base = types.intern(Type::Pointer(owner));
    let field = types.intern(Type::Pointer(scalar));
    (base, field, scalar)
}

fn project(
    b: &mut Builder<'_>,
    base: ValueId,
    pointer: TypeId,
    scalar: TypeId,
    owner: TypeId,
) -> ValueId {
    let field = b.field(FieldRef {
        owner,
        name: "value".into(),
        ty: scalar,
        is_static: false,
    });
    let projection = b.projection(PointerProjection {
        parent: None,
        field,
        offset: 0,
        size: 8,
        codec: None,
    });
    b.emit(Op::Project { base, projection }, Some(pointer))
        .unwrap()
}

#[test]
fn promotes_reads_and_writes_and_discards_unobserved_field_addresses() {
    let mut types = Types::default();
    let (base_ty, pointer, scalar) = layout(&mut types);
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let Type::Pointer(owner) = types.get(base_ty).unwrap() else {
        unreachable!()
    };
    let mut b = Builder::new(&types, scalar);
    let base = b.parameter(b.current(), base_ty);
    let address = project(&mut b, base, pointer, scalar, owner);
    let erased = b.emit(Op::Reinterpret(address), Some(object)).unwrap();
    let alias = b.emit(Op::Adapt(erased), Some(pointer)).unwrap();
    let value = b.emit(Op::Load(alias), Some(scalar)).unwrap();
    b.emit(
        Op::Store {
            pointer: alias,
            value,
        },
        None,
    );
    b.terminate(Terminator::Return(Some(value)));
    let mut body = b.finish().unwrap();
    promote_fields(&mut body, &types);
    verify(&body, &types).unwrap();
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::LoadField { base: p, .. } if p == base))
    );
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::StoreField { base: p, .. } if p == base))
    );
    assert!(!super::live(&body, &types).values[address.index()]);
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn retains_escaping_projections_and_does_not_promote_pointer_arithmetic() {
    let mut types = Types::default();
    let (base_ty, pointer, scalar) = layout(&mut types);
    let Type::Pointer(owner) = types.get(base_ty).unwrap() else {
        unreachable!()
    };
    let mut b = Builder::new(&types, pointer);
    let base = b.parameter(b.current(), base_ty);
    let address = project(&mut b, base, pointer, scalar, owner);
    let one = b.constant(scalar, Scalar::integer(ScalarType::I64, 1).unwrap());
    let shifted = b
        .emit(
            Op::Offset {
                pointer: address,
                offset: one,
                bytes: true,
                wrapping: false,
            },
            Some(pointer),
        )
        .unwrap();
    b.emit(
        Op::Store {
            pointer: shifted,
            value: one,
        },
        None,
    );
    b.terminate(Terminator::Return(Some(address)));
    let mut body = b.finish().unwrap();
    promote_fields(&mut body, &types);
    verify(&body, &types).unwrap();
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::Store { pointer: p, .. } if p == shifted))
    );
    assert!(super::live(&body, &types).values[address.index()]);
}

#[test]
fn rejects_field_loads_with_a_different_view_type() {
    let mut types = Types::default();
    let (base_ty, pointer, scalar) = layout(&mut types);
    let narrow = types.scalar(ScalarType::I32);
    let narrow_pointer = types.intern(Type::Pointer(narrow));
    let Type::Pointer(owner) = types.get(base_ty).unwrap() else {
        unreachable!()
    };
    let mut b = Builder::new(&types, narrow);
    let base = b.parameter(b.current(), base_ty);
    let address = project(&mut b, base, pointer, scalar, owner);
    let cast = b.emit(Op::Cast(address), Some(narrow_pointer)).unwrap();
    let value = b.emit(Op::Load(cast), Some(narrow)).unwrap();
    b.terminate(Terminator::Return(Some(value)));
    let mut body = b.finish().unwrap();
    promote_fields(&mut body, &types);
    verify(&body, &types).unwrap();
    assert!(
        !body
            .instructions
            .iter()
            .any(|i| matches!(i.op, Op::LoadField { .. }))
    );
}

#[test]
fn retyped_borrowed_fields_promote_only_with_the_original_exact_layout() {
    for mismatch in [0, 1, 2, 3] {
        for protected in [false, true] {
            let mut types = Types::default();
            let (_, address, _) = layout(&mut types);
            let slot = types.intern(Type::Pointer(address));
            let owner = types.symbol("Iterator");
            let owner = types.intern(Type::Class(owner));
            let base_ty = types.intern(Type::Pointer(owner));
            let codec = types.symbol("@raw-pointer\n8\n\n");
            let other_codec = types.symbol("@raw-pointer\n4\n\n");
            let mut b = Builder::new(&types, address);
            let base = b.parameter(b.current(), base_ty);
            let field = b.field(FieldRef {
                owner,
                name: "end".into(),
                ty: address,
                is_static: false,
            });
            let projection = b.projection(PointerProjection {
                parent: None,
                field,
                offset: 8,
                size: 8,
                codec: Some("@raw-pointer\n8\n\n".into()),
            });
            let original = b
                .emit(Op::Project { base, projection }, Some(slot))
                .unwrap();
            let retyped = b
                .emit(
                    Op::RetypeAddress {
                        pointer: original,
                        size: if mismatch == 1 {
                            4
                        } else if mismatch == 3 {
                            0
                        } else {
                            8
                        },
                        codec: Some(if mismatch == 2 { other_codec } else { codec }),
                    },
                    Some(slot),
                )
                .unwrap();
            let value = if protected {
                let handler = b.create_block();
                let value = b.invoke(Op::Load(retyped), Some(address), handler).unwrap();
                let current = b.current();
                b.switch_to(handler);
                b.terminate(Terminator::Rethrow);
                b.switch_to(current);
                value
            } else {
                b.emit(Op::Load(retyped), Some(address)).unwrap()
            };
            b.emit(
                Op::Store {
                    pointer: retyped,
                    value,
                },
                None,
            );
            b.terminate(Terminator::Return(Some(value)));
            let mut body = b.finish().unwrap();
            promote_fields(&mut body, &types);
            verify(&body, &types).unwrap();
            let ValueDef::Inst(load) = body.values[value.index()].def else {
                panic!()
            };
            assert_eq!(
                matches!(body.instructions[load.index()].op, Op::LoadField { .. }),
                mismatch == 0
            );
            assert_eq!(
                body.instructions
                    .iter()
                    .any(|inst| matches!(inst.op, Op::StoreField { .. })),
                mismatch == 0
            );
            if protected {
                assert!(body.blocks.iter().any(|block| matches!(block.terminator,
                    Some(Terminator::Invoke { inst, .. }) if inst == load)));
            }
            if mismatch == 0 {
                assert!(!super::live(&body, &types).values[retyped.index()]);
            }
            crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        }
    }
}

#[test]
fn field_storage_dispatch_preserves_forwarded_roots() {
    for store in [false, true] {
        let mut types = Types::default();
        let (base_ty, _, scalar) = layout(&mut types);
        let Type::Pointer(owner) = types.get(base_ty).unwrap() else {
            unreachable!()
        };
        let mut b = Builder::new(&types, scalar);
        let input = b.parameter(b.current(), base_ty);
        let field = b.field(FieldRef {
            owner,
            name: "value".into(),
            ty: scalar,
            is_static: false,
        });
        let projection = b.projection(PointerProjection {
            parent: None,
            field,
            offset: 8,
            size: 8,
            codec: None,
        });
        let base = b.emit(Op::Opaque(input), Some(base_ty)).unwrap();
        let value = if store {
            let value = b.constant(scalar, Scalar::integer(ScalarType::I64, 17).unwrap());
            b.emit(
                Op::StoreField {
                    base,
                    projection,
                    value,
                },
                None,
            );
            value
        } else {
            b.emit(Op::LoadField { base, projection }, Some(scalar))
                .unwrap()
        };
        b.terminate(Terminator::Return(Some(value)));
        let body = b.finish().unwrap();
        let mut pool = crate::classfile::constant_pool::InternedConstantPool::default();
        let code = crate::jvm::select::compile(&body, &types, &mut pool).unwrap();
        let owner = pool.add_class("org/rustlang/runtime/Pointer").unwrap();
        let direct = pool
            .add_method_ref(
                owner,
                if store {
                    "storeScalarField"
                } else {
                    "loadScalarField"
                },
                if store {
                    "(Ljava/lang/Object;JLjava/lang/String;Ljava/lang/String;JJI)V"
                } else {
                    "(Ljava/lang/Object;JLjava/lang/String;Ljava/lang/String;JI)J"
                },
            )
            .unwrap();
        let project = pool.add_method_ref(owner, "projectStructField",
            "(Ljava/lang/String;Ljava/lang/String;JJLjava/lang/String;)Lorg/rustlang/runtime/Pointer;",
        ).unwrap();
        use crate::classfile::attributes::Instruction;
        let access = code
            .instructions
            .iter()
            .position(|i| *i == Instruction::Invokestatic(direct))
            .expect("byte storage must have a direct scalar access");
        assert!(
            !code.instructions[..access].contains(&Instruction::Invokevirtual(project)),
            "byte storage must not first construct a projected field pointer"
        );
    }
}

#[test]
fn aggregate_field_stores_reuse_storage_components_and_keep_handlers() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let array = types.intern(Type::Array(byte));
    let array_pointer = types.intern(Type::Pointer(array));
    let owner = types.symbol("Pixel");
    let owner = types.intern(Type::Class(owner));
    let pointer = types.intern(Type::Pointer(owner));
    let unit = types.intern(Type::Unit);
    let mut b = Builder::new(&types, unit);
    let base = b.parameter(b.current(), pointer);
    let value = b.parameter(b.current(), array);
    let field = b.field(FieldRef {
        owner,
        name: "rgba".into(),
        ty: array,
        is_static: false,
    });
    let projection = b.projection(PointerProjection {
        parent: None,
        field,
        offset: 4,
        size: 4,
        codec: Some("org/rustlang/runtime/ArrayMemoryCodec#array#[B#4".into()),
    });
    let address = b
        .emit(Op::Project { base, projection }, Some(array_pointer))
        .unwrap();
    let handler = b.create_block();
    b.invoke(
        Op::Store {
            pointer: address,
            value,
        },
        None,
        handler,
    );
    b.terminate(Terminator::Return(None));
    b.switch_to(handler);
    b.terminate(Terminator::Rethrow);
    let mut body = b.finish().unwrap();
    promote_fields(&mut body, &types);
    super::lower_component_arguments(&mut body, &mut types, true, |_| false, None);
    super::decompose_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    assert!(body.blocks.iter().any(|b| matches!(b.terminator,
        Some(Terminator::Invoke { inst, .. }) if matches!(body.instructions[inst.index()].op, Op::StoreStorageField { .. }))));
    assert!(!super::live(&body, &types).values[address.index()]);
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn field_lookup_reuse_stops_at_mutations_and_calls() {
    for barrier in ["none", "store", "call"] {
        let mut types = Types::default();
        let (pointer, _, scalar) = layout(&mut types);
        let Type::Pointer(owner) = types.get(pointer).unwrap() else {
            unreachable!()
        };
        let unit = types.intern(Type::Unit);
        let mut b = Builder::new(&types, scalar);
        let base = b.parameter(b.current(), pointer);
        let field = b.field(FieldRef {
            owner,
            name: "value".into(),
            ty: scalar,
            is_static: false,
        });
        let projection = b.projection(PointerProjection {
            parent: None,
            field,
            offset: 8,
            size: 8,
            codec: None,
        });
        let first = b
            .emit(Op::LoadField { base, projection }, Some(scalar))
            .unwrap();
        if barrier == "store" {
            b.emit(
                Op::StoreField {
                    base,
                    projection,
                    value: first,
                },
                None,
            );
        } else if barrier == "call" {
            let method = b.method(MethodRef {
                owner: "Consumer".into(),
                name: "mutate".into(),
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
        let second = b
            .emit(Op::LoadField { base, projection }, Some(scalar))
            .unwrap();
        b.terminate(Terminator::Return(Some(second)));
        let body = b.finish().unwrap();
        let mut pool = crate::classfile::constant_pool::InternedConstantPool::default();
        let code = crate::jvm::select::compile(&body, &types, &mut pool).unwrap();
        let owner = pool.add_class("org/rustlang/runtime/Pointer").unwrap();
        let lookup = pool
            .add_method_ref(
                owner,
                "directAggregate",
                "(Ljava/lang/Class;)Ljava/lang/Object;",
            )
            .unwrap();
        use crate::classfile::attributes::Instruction;
        let lookups = code
            .instructions
            .iter()
            .filter(|i| **i == Instruction::Invokevirtual(lookup))
            .count();
        assert_eq!(
            lookups,
            match barrier {
                "store" => 3,
                "call" => 2,
                _ => 1,
            },
            "{barrier}"
        );
    }
}

#[test]
fn nested_field_reads_keep_nullable_fallback_and_exception_handler() {
    let mut types = Types::default();
    let scalar = types.scalar(ScalarType::I64);
    let outer = types.symbol("Outer");
    let outer = types.intern(Type::Class(outer));
    let inner = types.symbol("Inner");
    let inner = types.intern(Type::Class(inner));
    let outer_ptr = types.intern(Type::Pointer(outer));
    let inner_ptr = types.intern(Type::Pointer(inner));
    let scalar_ptr = types.intern(Type::Pointer(scalar));
    let mut b = Builder::new(&types, scalar);
    let root = b.parameter(b.current(), outer_ptr);
    let field = b.field(FieldRef {
        owner: outer,
        name: "inner".into(),
        ty: inner,
        is_static: false,
    });
    let parent = b.projection(PointerProjection {
        parent: None,
        field,
        offset: 8,
        size: 8,
        codec: Some("InnerCodec".into()),
    });
    let base = b
        .emit(
            Op::Project {
                base: root,
                projection: parent,
            },
            Some(inner_ptr),
        )
        .unwrap();
    let field = b.field(FieldRef {
        owner: inner,
        name: "value".into(),
        ty: scalar,
        is_static: false,
    });
    let child = b.projection(PointerProjection {
        parent: None,
        field,
        offset: 0,
        size: 8,
        codec: None,
    });
    let address = b
        .emit(
            Op::Project {
                base,
                projection: child,
            },
            Some(scalar_ptr),
        )
        .unwrap();
    let handler = b.create_block();
    let value = b.invoke(Op::Load(address), Some(scalar), handler).unwrap();
    b.terminate(Terminator::Return(Some(value)));
    b.switch_to(handler);
    b.terminate(Terminator::Rethrow);
    let mut body = b.finish().unwrap();
    promote_fields(&mut body, &types);
    super::fold_field_paths(&mut body, &types);
    verify(&body, &types).unwrap();
    assert!(!super::live(&body, &types).values[base.index()]);
    let ValueDef::Inst(load) = body.values[value.index()].def else {
        panic!()
    };
    assert!(body.blocks.iter().any(|block| matches!(block.terminator,
        Some(Terminator::Invoke { inst, .. }) if inst == load)));
    let mut pool = Default::default();
    let code = crate::jvm::select::compile(&body, &types, &mut pool).unwrap();
    use crate::classfile::attributes::Instruction;
    assert_eq!(
        code.instructions
            .iter()
            .filter(|i| matches!(i, Instruction::Ifnull(_)))
            .count(),
        2
    );
    let owner = pool.add_class("Outer").unwrap();
    let field = pool.add_field_ref(owner, "inner", "LInner;").unwrap();
    assert!(code.instructions.contains(&Instruction::Getfield(field)));
    let runtime = pool.add_class("org/rustlang/runtime/Pointer").unwrap();
    let load = pool
        .add_method_ref(runtime, "loadLocationBits", "(Ljava/lang/Object;JI)J")
        .unwrap();
    assert!(code.instructions.contains(&Instruction::Invokestatic(load)));
}
