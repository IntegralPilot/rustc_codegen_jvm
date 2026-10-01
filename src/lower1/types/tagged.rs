//! Small tagged values retain their Rust layout separately from JVM components.
use super::*;

#[derive(Clone, Copy)]
pub(crate) struct TaggedScalar<'tcx> {
    pub payload: Ty<'tcx>,
    pub tag_offset: usize,
    pub payload_offset: usize,
}

pub(crate) fn tagged_scalar<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Option<TaggedScalar<'tcx>> {
    if ty.has_param() || ty.has_escaping_bound_vars() {
        return None;
    }
    let ty = normalize_union_ty(tcx, ty).ok()?;
    let TyKind::Adt(def, args) = ty.kind() else {
        return None;
    };
    if !tcx.is_lang_item(def.did(), rustc_attr_ir::lang_items::LangItem::Option) {
        return None;
    }
    let payload = args.type_at(0);
    if !matches!(
        payload.kind(),
        TyKind::Int(IntTy::I64 | IntTy::Isize) | TyKind::Uint(UintTy::U64 | UintTy::Usize)
    ) {
        return None;
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .ok()?;
    let Variants::Multiple {
        tag,
        tag_encoding: TagEncoding::Direct,
        tag_field,
        ref variants,
        ..
    } = layout.variants
    else {
        return None;
    };
    if tag.size(&tcx.data_layout).bytes() != 8 || layout.size.bytes() != 16 {
        return None;
    }
    Some(TaggedScalar {
        payload,
        tag_offset: layout.fields.offset(tag_field.into()).bytes_usize(),
        payload_offset: variants[VariantIdx::from_usize(1)].field_offsets[FieldIdx::from_usize(0)]
            .bytes_usize(),
    })
}

pub(crate) fn tagged_value(dest: String, value: oomir::Operand, tag: u64) -> oomir::Instruction {
    oomir::Instruction::TaggedPack {
        dest,
        value,
        tag: oomir::Operand::Constant(oomir::Constant::U64(tag)),
    }
}
