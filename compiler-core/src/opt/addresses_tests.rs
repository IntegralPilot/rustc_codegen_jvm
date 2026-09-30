use super::{decompose_addresses, lower_component_arguments};
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};

#[test]
fn borrowed_scalar_fields_keep_their_aggregate_root_across_calls() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let owner = types.symbol("Pair");
    let owner = types.intern(Type::Class(owner));
    let aggregate = types.intern(Type::Pointer(owner));
    let scalar = types.intern(Type::Pointer(int));
    let mut b = Builder::new(&types, int);
    let root = b.parameter(b.current(), aggregate);
    let field = b.field(FieldRef {
        owner,
        name: "second".into(),
        ty: int,
        is_static: false,
    });
    let projection = b.projection(PointerProjection {
        field,
        offset: 4,
        size: 4,
        codec: None,
    });
    let pointer = b
        .emit(
            Op::Project {
                base: root,
                projection,
            },
            Some(scalar),
        )
        .unwrap();
    let method = b.method(MethodRef {
        owner: "Kernel".into(),
        name: "consume".into(),
        params: vec![scalar],
        returns: int,
        interface: false,
    });
    let args = b.args([pointer]);
    let result = b
        .emit(
            Op::Call {
                method,
                kind: CallKind::RustStatic,
                args,
            },
            Some(int),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| true, None);
    decompose_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let live = super::live(&body, &types);
    assert!(
        !body
            .instructions
            .iter()
            .enumerate()
            .any(|(id, inst)| live.instructions[id]
                && matches!(inst.op, Op::AddressPack(_) | Op::Project { .. }))
    );
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::ProjectRoot { .. }))
    );
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn thin_pointer_comparisons_use_components() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let boolean = types.scalar(ScalarType::Bool);
    let owner = types.symbol("Item");
    let owner = types.intern(Type::Class(owner));
    for (pointee, ordering) in [int, owner]
        .into_iter()
        .flat_map(|p| [(p, false), (p, true)])
    {
        let result_type = if ordering { int } else { boolean };
        let pointer = types.intern(Type::Pointer(pointee));
        let mut b = Builder::new(&types, result_type);
        let left = b.parameter(b.current(), pointer);
        let right = b.parameter(b.current(), pointer);
        let equal = b
            .emit(
                if ordering {
                    Op::AddressCompare { left, right }
                } else {
                    Op::AddressEqual { left, right }
                },
                Some(result_type),
            )
            .unwrap();
        b.terminate(Terminator::Return(Some(equal)));
        let mut body = b.finish().unwrap();
        lower_component_arguments(&mut body, &mut types, true, |_| false, None);
        decompose_addresses(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        let live = super::live(&body, &types);
        assert!(body.instructions.iter().any(|i| if ordering {
            matches!(i.op, Op::LocationCompare(_))
        } else {
            matches!(i.op, Op::LocationEqual(_))
        }));
        assert!(
            !body
                .instructions
                .iter()
                .enumerate()
                .any(|(index, i)| live.instructions[index] && matches!(i.op, Op::AddressPack(_)))
        );
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn runtime_pointer_annotations_do_not_force_materialization() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let boolean = types.scalar(ScalarType::Bool);
    let pointer = types.intern(Type::Pointer(int));
    let runtime = types.symbol("org/rustlang/runtime/Pointer");
    let runtime = types.intern(Type::Class(runtime));
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let mut b = Builder::new(&types, boolean);
    let left = b.parameter(b.current(), pointer);
    let right = b.parameter(b.current(), pointer);
    // Runtime helper signatures and source bindings both erase annotations.
    let erased = b.emit(Op::Reinterpret(left), Some(runtime)).unwrap();
    let erased = b.emit(Op::Reinterpret(erased), Some(object)).unwrap();
    let recovered = b.emit(Op::Adapt(erased), Some(pointer)).unwrap();
    let equal = b
        .emit(
            Op::AddressEqual {
                left: recovered,
                right,
            },
            Some(boolean),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(equal)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| false, None);
    decompose_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let live = super::live(&body, &types);
    assert!(
        !body
            .instructions
            .iter()
            .enumerate()
            .any(|(i, inst)| live.instructions[i]
                && matches!(inst.op, Op::AddressPack(_) | Op::Adapt(_)))
    );
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn scalar_reference_loop_has_no_materialized_addresses() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let long = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let pointer = types.intern(Type::Pointer(int));
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let mut b = Builder::new(&types, int);
    let start = b.parameter(b.current(), pointer);
    let end = b.parameter(b.current(), long);
    let address = b.variable(object);
    let erased = b.emit(Op::Reinterpret(start), Some(object)).unwrap();
    b.define(address, erased);
    let index = b.variable(long);
    let zero = b.constant(long, Scalar::integer(ScalarType::I64, 0).unwrap());
    b.define(index, zero);
    let header = b.create_block();
    let step = b.create_block();
    let done = b.create_block();
    b.jump(header, vec![]);
    b.switch_to(header);
    let i = b.read(index);
    let current = b.read(address);
    let current = b.emit(Op::Adapt(current), Some(pointer)).unwrap();
    let condition = b
        .emit(
            Op::Binary {
                op: BinaryOp::Lt,
                left: i,
                right: end,
            },
            Some(boolean),
        )
        .unwrap();
    b.branch(condition, step, done);
    b.switch_to(step);
    let value = b.emit(Op::Load(current), Some(int)).unwrap();
    b.emit(
        Op::Store {
            pointer: current,
            value,
        },
        None,
    );
    let one = b.constant(long, Scalar::integer(ScalarType::I64, 1).unwrap());
    let next = b
        .emit(
            Op::Offset {
                pointer: current,
                offset: one,
                bytes: false,
                wrapping: false,
            },
            Some(pointer),
        )
        .unwrap();
    let erased = b.emit(Op::Reinterpret(next), Some(object)).unwrap();
    b.define(address, erased);
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: i,
                right: one,
            },
            Some(long),
        )
        .unwrap();
    b.define(index, next);
    b.jump(header, vec![]);
    b.switch_to(done);
    let value = b.emit(Op::Load(current), Some(int)).unwrap();
    b.terminate(Terminator::Return(Some(value)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| true, None);
    decompose_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let live = super::live(&body, &types);
    for (i, inst) in body.instructions.iter().enumerate() {
        assert!(
            !live.instructions[i]
                || !matches!(
                    inst.op,
                    Op::AddressPack(_) | Op::Offset { .. } | Op::Load(_) | Op::Store { .. }
                )
        );
    }
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    assert!(
        !code
            .instructions
            .iter()
            .any(|i| matches!(i, crate::classfile::attributes::Instruction::New(_)))
    );
}

#[test]
fn aggregate_addresses_keep_layout_roots_through_calls_and_field_access() {
    let mut types = Types::default();
    let long = types.scalar(ScalarType::I64);
    let owner = types.symbol("Pair");
    let owner = types.intern(Type::Class(owner));
    let pointer = types.intern(Type::Pointer(owner));
    let unit = types.intern(Type::Unit);
    let mut b = Builder::new(&types, long);
    let base = b.parameter(b.current(), pointer);
    let offset = b.parameter(b.current(), long);
    let field = b.field(FieldRef {
        owner,
        name: "value".into(),
        ty: long,
        is_static: false,
    });
    let projection = b.projection(PointerProjection {
        field,
        offset: 0,
        size: 8,
        codec: None,
    });
    let address = b
        .emit(
            Op::Offset {
                pointer: base,
                offset,
                bytes: false,
                wrapping: false,
            },
            Some(pointer),
        )
        .unwrap();
    let method = b.method(MethodRef {
        owner: "test/Calls".into(),
        name: "inspect".into(),
        params: vec![pointer],
        returns: unit,
        interface: false,
    });
    let args = b.args([address]);
    b.emit(
        Op::Call {
            method,
            kind: CallKind::RustStatic,
            args,
        },
        None,
    );
    let value = b
        .emit(
            Op::LoadField {
                base: address,
                projection,
            },
            Some(long),
        )
        .unwrap();
    b.emit(
        Op::StoreField {
            base: address,
            projection,
            value,
        },
        None,
    );
    b.terminate(Terminator::Return(Some(value)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| true, None);
    decompose_addresses(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let live = super::live(&body, &types);
    assert!(
        !body
            .instructions
            .iter()
            .enumerate()
            .any(|(i, inst)| live.instructions[i]
                && matches!(
                    inst.op,
                    Op::AddressPack(_)
                        | Op::AddressPart { .. }
                        | Op::Offset { .. }
                        | Op::LoadField { .. }
                        | Op::StoreField { .. }
                ))
    );
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::LoadStorageField { .. }))
    );
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, Op::StoreStorageField { .. }))
    );
    assert!(body.methods.iter().any(|m| m.name == "locationStride"));
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
}

#[test]
fn decoded_aggregate_writeback_retains_its_binding_owner() {
    for (commit, owned) in [(false, false), (true, false), (false, true), (true, true)] {
        let mut types = Types::default();
        let unit = types.intern(Type::Unit);
        let long = types.scalar(ScalarType::I64);
        let name = types.symbol("Pair");
        let object = types.intern(Type::Class(name));
        let pointer = types.intern(Type::Pointer(object));
        let name = types.symbol("java/lang/Object");
        let erased = types.intern(Type::Class(name));
        let mut b = Builder::new(&types, object);
        let root = b.parameter(b.current(), pointer);
        let displacement = b.parameter(b.current(), long);
        let address = b
            .emit(
                Op::Offset {
                    pointer: root,
                    offset: displacement,
                    bytes: true,
                    wrapping: false,
                },
                Some(pointer),
            )
            .unwrap();
        let erased_address = b.emit(Op::Reinterpret(address), Some(erased)).unwrap();
        let load_address = b.emit(Op::Adapt(erased_address), Some(pointer)).unwrap();
        let value = b
            .emit(
                if owned {
                    Op::LoadCopy(load_address)
                } else {
                    Op::Load(load_address)
                },
                Some(object),
            )
            .unwrap();
        if commit {
            let method = b.method(MethodRef {
                owner: "org/rustlang/runtime/Pointer".into(),
                name: "commitMemoryView".into(),
                params: vec![],
                returns: unit,
                interface: false,
            });
            let commit_address = b.emit(Op::Adapt(erased_address), Some(pointer)).unwrap();
            let args = b.args([commit_address]);
            b.emit(
                Op::Call {
                    method,
                    kind: CallKind::Virtual,
                    args,
                },
                None,
            );
        }
        b.terminate(Terminator::Return(Some(value)));
        let mut body = b.finish().unwrap();
        lower_component_arguments(&mut body, &mut types, true, |_| false, None);
        decompose_addresses(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::Load(_))),
            commit && !owned
        );
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::LoadAddress(_))),
            !commit && !owned
        );
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::LoadAddressCopy(_))),
            owned
        );
        if commit && !owned {
            let live = super::live(&body, &types);
            assert_eq!(
                body.instructions
                    .iter()
                    .enumerate()
                    .filter(|(i, inst)| {
                        live.instructions[*i] && matches!(inst.op, Op::AddressPack(_))
                    })
                    .count(),
                1,
                "load and writeback must share one materialized owner"
            );
        }
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn nullable_discriminants_do_not_materialize_borrowed_addresses() {
    let mut types = Types::default();
    let scalar = types.scalar(ScalarType::I32);
    let owner = types.symbol("Item");
    let owner = types.intern(Type::Class(owner));
    let long = types.scalar(ScalarType::I64);
    for pointee in [scalar, owner] {
        let pointer = types.intern(Type::Pointer(pointee));
        let mut b = Builder::new(&types, long);
        let value = b.parameter(b.current(), pointer);
        let tag = b.emit(Op::AddressTag(value), Some(long)).unwrap();
        b.terminate(Terminator::Return(Some(tag)));
        let mut body = b.finish().unwrap();
        lower_component_arguments(&mut body, &mut types, true, |_| false, None);
        decompose_addresses(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        let live = super::live(&body, &types);
        assert!(
            body.instructions
                .iter()
                .any(|i| matches!(i.op, Op::LocationTag(_)))
        );
        assert!(
            !body
                .instructions
                .iter()
                .enumerate()
                .any(|(i, inst)| live.instructions[i]
                    && matches!(inst.op, Op::AddressPack(_) | Op::AddressTag(_)))
        );
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}

#[test]
fn nullable_return_joins_keep_address_components() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let boolean = types.scalar(ScalarType::Bool);
    let owner = types.symbol("Item");
    let owner = types.intern(Type::Class(owner));
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    for pointee in [int, owner] {
        let pointer = types.intern(Type::Pointer(pointee));
        let mut b = Builder::new(&types, pointer);
        let condition = b.parameter(b.current(), boolean);
        let value = b.parameter(b.current(), pointer);
        let result = b.variable(object);
        let some = b.create_block();
        let none = b.create_block();
        let done = b.create_block();
        b.branch(condition, some, none);
        b.switch_to(some);
        let erased = b.emit(Op::Reinterpret(value), Some(object)).unwrap();
        b.define(result, erased);
        b.jump(done, vec![]);
        b.switch_to(none);
        let constant = ConstId::new(b.body.constants.len());
        b.body.constants.push(Constant::Null(pointer));
        let null = b.emit(Op::Constant(constant), Some(pointer)).unwrap();
        let erased = b.emit(Op::Reinterpret(null), Some(object)).unwrap();
        b.define(result, erased);
        b.jump(done, vec![]);
        b.switch_to(done);
        let result = b.read(result);
        let result = b.emit(Op::Adapt(result), Some(pointer)).unwrap();
        b.terminate(Terminator::Return(Some(result)));
        let mut body = b.finish().unwrap();
        lower_component_arguments(&mut body, &mut types, true, |_| false, None);
        super::lower_component_returns(&mut body, &mut types, true, |_| false, None);
        decompose_addresses(&mut body, &mut types, None);
        verify(&body, &types).unwrap();
        let live = super::live(&body, &types);
        assert!(!body.instructions.iter().enumerate().any(|(i, inst)| {
            live.instructions[i] && matches!(inst.op, Op::AddressPack(_) | Op::AddressPart { .. })
        }));
        crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    }
}
