//! Enum carriers use proven Rust layouts. Rust identity and discriminants remain separate from Java
//! storage.
use super::*;
use rustc_middle::ty::TypeVisitableExt;

fn nullable_pointer_payload<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Option<Ty<'tcx>> {
    let TyKind::Adt(def, args) = ty.kind() else {
        return None;
    };
    if !tcx.is_lang_item(def.did(), rustc_attr_ir::lang_items::LangItem::Option) {
        return None;
    }
    let payload = args.type_at(0);
    let mut physical = payload;
    while let Some(owner) = transparent_payload(physical, tcx) {
        physical = owner.ty;
    }
    let pointee = match physical.kind() {
        TyKind::Ref(_, pointee, _) if !matches!(pointee.kind(), TyKind::Foreign(_)) => *pointee,
        TyKind::RawPtr(pointee, _)
            if physical != payload && !matches!(pointee.kind(), TyKind::Foreign(_)) =>
        {
            *pointee
        }
        // Sized NonNull values use direct addresses. Unsized NonNull values with nominal wrappers
        // cannot use null niches.
        TyKind::Adt(def, args)
            if crate::lower1::is_non_null_lang_item(tcx, def.did())
                && is_codegen_sized(args.type_at(0), tcx) =>
        {
            args.type_at(0)
        }
        _ => return None,
    };
    // Trait objects and struct tails require separate metadata. Only slices and strings use these
    // view components.
    if !is_codegen_sized(pointee, tcx) && !matches!(pointee.kind(), TyKind::Slice(_) | TyKind::Str)
    {
        return None;
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .ok()?;
    let Variants::Multiple {
        tag_encoding:
            TagEncoding::Niche {
                untagged_variant,
                niche_variants,
                niche_start,
            },
        tag_field,
        tag,
        ..
    } = &layout.variants
    else {
        return None;
    };
    (untagged_variant.as_u32() == 1
        && niche_variants.start.as_u32() == 0
        && niche_variants.last.as_u32() == 0
        && *niche_start == 0
        && layout.fields.offset((*tag_field).into()).bytes() == 0
        && tag.size(&tcx.data_layout) == tcx.data_layout.pointer_size()
        && tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(payload))
            .ok()?
            .size
            == layout.size)
        .then_some(payload)
}

#[derive(Clone, Copy)]
pub(crate) struct EnumCarrier<'tcx> {
    pub payload: Ty<'tcx>,
    pub variant: VariantIdx,
    pub nullable: bool,
}

pub(crate) fn enum_carrier<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Option<EnumCarrier<'tcx>> {
    if ty.has_param() || ty.has_escaping_bound_vars() {
        return None;
    }
    let ty = normalize_union_ty(tcx, ty).ok()?;
    let TyKind::Adt(def, args) = ty.kind() else {
        return None;
    };
    if !def.is_enum() {
        return None;
    }
    if let Some(payload) = nullable_pointer_payload(ty, tcx) {
        if !super::transparent::finite_representation(ty, tcx) {
            return None;
        }
        return Some(EnumCarrier {
            payload,
            variant: VariantIdx::from_usize(1),
            nullable: true,
        });
    }
    // Java subtype enums have an explicit nominal interoperability contract.
    if def
        .variants()
        .iter()
        .any(|variant| is_jvm_subtype_variant(tcx, variant))
    {
        return None;
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .ok()?;
    let Variants::Single { index } = layout.variants else {
        return None;
    };
    let variant = def.variant(index);
    if variant.fields.len() != 1 || layout.fields.offset(0).bytes() != 0 {
        return None;
    }
    let payload = normalize_union_ty(
        tcx,
        variant.fields[FieldIdx::from_usize(0)]
            .ty(tcx, args)
            .skip_norm_wip(),
    )
    .ok()?;
    let payload_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(payload))
        .ok()?;
    if payload_layout.size != layout.size || payload_layout.align != layout.align {
        return None;
    }
    if !super::transparent::finite_representation(ty, tcx) {
        return None;
    }
    Some(EnumCarrier {
        payload,
        variant: index,
        nullable: false,
    })
}

pub(crate) fn direct_enum_payload<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Option<Ty<'tcx>> {
    enum_carrier(ty, tcx).map(|carrier| carrier.payload)
}
