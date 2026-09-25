use super::fields::promote_fields;
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};

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
        relative_pointer: false,
    });
    let projection = b.projection(PointerProjection {
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
