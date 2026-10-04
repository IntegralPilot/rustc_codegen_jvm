//! Recover address components from representation lowering.
//! Read the defining pack's exact layout, even after JVM type erasure.
use crate::ir::*;

pub(super) struct AddressParts {
    pub value: ValueId,
    pub parts: List,
    pub pointee: Option<TypeId>,
    pub layout: Option<(u32, Option<SymbolId>)>,
}

pub(super) fn address_parts(body: &Body, types: &Types, value: ValueId) -> Option<AddressParts> {
    find_parts(body, types, value, true)
}

/// Scalar pointer casts replace the view width and codec.
/// They can ignore erased layout annotations, unlike ordinary ABI uses.
pub(super) fn scalar_cast_parts(
    body: &Body,
    types: &Types,
    value: ValueId,
) -> Option<AddressParts> {
    find_parts(body, types, value, false)
}

fn find_parts(
    body: &Body,
    types: &Types,
    mut value: ValueId,
    preserve_layout: bool,
) -> Option<AddressParts> {
    for _ in 0..32 {
        value = body.resolve(value);
        let ValueDef::Inst(id) = body.values[value.index()].def else {
            return None;
        };
        let ty = body.value_type(value);
        let pointee = types.pointee(ty);
        match body.instructions[id.index()].op {
            Op::TypedAddressPack { parts, size, codec } => {
                return Some(AddressParts {
                    value,
                    parts,
                    pointee,
                    layout: Some((size, codec)),
                });
            }
            Op::AddressPack(parts) => {
                let layout = types
                    .address_layout(ty)
                    .or_else(|| StorageSlot::scalar(pointee?, types).map(|slot| (slot.size, None)));
                return Some(AddressParts {
                    value,
                    parts,
                    pointee,
                    layout,
                });
            }
            Op::Refine(source) | Op::Reinterpret(source)
                if !preserve_layout
                    || types.address_layout(ty)
                        == types.address_layout(body.value_type(source)) =>
            {
                value = source
            }
            _ => return None,
        }
    }
    None
}
