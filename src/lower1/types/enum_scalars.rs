//! Private fieldless enums use Rust integer tags. Rust identity still controls methods, validity,
//! and destruction.
use super::*;

pub(crate) fn enum_scalar_ty<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Option<Ty<'tcx>> {
    let TyKind::Adt(def, args) = ty.kind() else {
        return None;
    };
    if !def.is_enum()
        || def
            .variants()
            .iter()
            .any(|variant| !variant.fields.is_empty())
        || adt_class_kind(tcx, def, args) != oomir::ClassKind::Value
    {
        return None;
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .ok()?;
    let Variants::Multiple {
        tag,
        tag_encoding: TagEncoding::Direct,
        tag_field,
        ..
    } = &layout.variants
    else {
        return None;
    };
    if layout.fields.offset((*tag_field).into()).bytes() != 0
        || layout.size != tag.size(&tcx.data_layout)
    {
        return None;
    }
    let rustc_abi::Primitive::Int(_, signed) = tag.primitive() else {
        return None;
    };
    Some(match (layout.size.bytes(), signed) {
        (1, true) => tcx.types.i8,
        (1, false) => tcx.types.u8,
        (2, true) => tcx.types.i16,
        (2, false) => tcx.types.u16,
        (4, true) => tcx.types.i32,
        (4, false) => tcx.types.u32,
        (8, true) => tcx.types.i64,
        (8, false) => tcx.types.u64,
        _ => return None,
    })
}

pub(crate) fn enum_scalar_variant<'tcx>(
    ty: Ty<'tcx>,
    variant: VariantIdx,
    tcx: TyCtxt<'tcx>,
) -> Option<oomir::Constant> {
    let scalar = enum_scalar_ty(ty, tcx)?;
    let TyKind::Adt(def, _) = ty.kind() else {
        unreachable!()
    };
    Some(mir_int_to_oomir_const(
        def.discriminant_for_variant(tcx, variant).val,
        scalar,
        tcx,
    ))
}
