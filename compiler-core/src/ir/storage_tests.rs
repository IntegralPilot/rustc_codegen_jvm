use super::*;
use crate::scalar::ScalarType;

#[test]
fn storage_validates_layout_codec_and_load_store_types() {
    let mut types = Types::default();
    let i32 = types.scalar(ScalarType::I32);
    let pointer = types.intern(Type::Pointer(i32));
    let address = types.intern(Type::Pointer(pointer));
    let codec = types.symbol("pointer:i32");
    let mut b = Builder::new(&types, pointer);
    let value = b.parameter(b.current(), pointer);
    let slot = SlotId::new(0);
    b.body.slots.push(StorageSlot {
        ty: pointer,
        size: 8,
        alignment: 8,
        codec: Some(codec),
    });
    b.emit(Op::StoreSlot { slot, value }, None);
    let address = b.emit(Op::AddressOfSlot(slot), Some(address)).unwrap();
    let loaded = b.emit(Op::Load(address), Some(pointer)).unwrap();
    b.emit(
        Op::StoreSlot {
            slot,
            value: loaded,
        },
        None,
    );
    let loaded = b.emit(Op::LoadSlot(slot), Some(pointer)).unwrap();
    b.terminate(Terminator::Return(Some(loaded)));
    let body = b.finish().unwrap();
    verify(&body, &types).unwrap();
    for (size, alignment) in [(8, 0), (8, 3), (u32::MAX, 8), (8, 1 << 31)] {
        let mut invalid = body.clone();
        invalid.slots[0].size = size;
        invalid.slots[0].alignment = alignment;
        assert!(verify(&invalid, &types).is_err());
    }
    let mut invalid = body.clone();
    invalid.slots[0].codec = Some(SymbolId::new(123));
    assert!(verify(&invalid, &types).is_err());
    let mut invalid = body.clone();
    invalid.slots[0] = StorageSlot::scalar(i32, &types).unwrap();
    assert!(verify(&invalid, &types).is_err());
    let mut invalid = body;
    invalid.slots[0].ty = TypeId::new(123);
    assert!(verify(&invalid, &types).is_err());
}

#[test]
fn symbol_only_extensions_are_retained() {
    let base = std::sync::Arc::new(Types::default());
    let mut extended = Types::with_base(base);
    assert!(!extended.has_additions());
    let codec = extended.symbol("exact-layout-codec");
    assert!(extended.has_additions());
    assert!(extended.is_empty());
    assert_eq!(extended.symbol_name(codec), Some("exact-layout-codec"));
}

#[test]
fn opaque_pointees_pass_through_without_becoming_values() {
    let mut types = Types::default();
    let opaque = types.intern(Type::Opaque(0));
    let pointer = types.intern(Type::Pointer(opaque));
    let mut b = Builder::new(&types, pointer);
    let value = b.parameter(b.current(), pointer);
    b.terminate(Terminator::Return(Some(value)));
    let mut body = b.finish().unwrap();
    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    body.values[value.index()].ty = opaque;
    body.return_type = opaque;
    assert!(verify(&body, &types).is_err());
}
