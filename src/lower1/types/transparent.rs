//! Private wrappers share payload storage when Rust layout and destruction permit it.
use super::*;

#[derive(Clone, Copy)]
pub(crate) struct TransparentPayload<'tcx> {
    pub ty: Ty<'tcx>,
    pub field: usize,
}

fn candidate<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    budget: usize,
) -> Option<TransparentPayload<'tcx>> {
    if budget == 0 || ty.has_param() || ty.has_escaping_bound_vars() {
        return None;
    }
    let TyKind::Adt(def, args) = ty.kind() else {
        return None;
    };
    if !def.is_struct()
        || def.non_enum_variant().fields.len() > 4
        || tcx.is_lang_item(def.did(), rustc_attr_ir::lang_items::LangItem::UnsafeCell)
        || adt_class_kind(tcx, def, args) != oomir::ClassKind::Value
    {
        return None;
    }
    let mut payload = None;
    for (index, field) in def.non_enum_variant().fields.iter().enumerate() {
        let mut ty = normalize_union_ty(tcx, field.ty(tcx, args).skip_norm_wip()).ok()?;
        // NonNull slice and string metadata already uses view components. Other DSTs keep their
        // existing carriers.
        if crate::lower1::is_non_null_lang_item(tcx, def.did())
            && let TyKind::Pat(inner, _) = ty.kind()
            && let TyKind::RawPtr(pointee, _) = inner.kind()
            && matches!(pointee.kind(), TyKind::Slice(_) | TyKind::Str)
        {
            ty = *inner;
        }
        if layout_size_bytes(tcx, ty).ok()? == 0 {
            // Omitted fields must not require destruction. Stateful allocators keep their complete
            // schema.
            if ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                return None;
            }
        } else if payload
            .replace(TransparentPayload { ty, field: index })
            .is_some()
        {
            return None;
        }
    }
    let payload = payload?;
    match payload.ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)
            if !matches!(pointee.kind(), TyKind::Foreign(_)) => {}
        TyKind::Adt(..) if candidate(payload.ty, tcx, budget - 1).is_some() => {}
        // Types that require destruction keep their owner until drop adapters can distinguish types
        // with the same Java carrier.
        TyKind::Adt(inner, args)
            if inner.is_struct()
                && adt_class_kind(tcx, inner, args) == oomir::ClassKind::Value
                && !ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) => {}
        TyKind::Array(..) | TyKind::Tuple(..)
            if !ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) => {}
        TyKind::Bool | TyKind::Char | TyKind::Float(FloatTy::F32 | FloatTy::F64) => {}
        _ => return None,
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .ok()?;
    let inner = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(payload.ty))
        .ok()?;
    (layout.size == inner.size
        && layout.align == inner.align
        && layout.fields.offset(payload.field).bytes() == 0)
        .then_some(payload)
}

pub(crate) fn transparent_payload<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> Option<TransparentPayload<'tcx>> {
    if !matches!(ty.kind(), TyKind::Adt(..) | TyKind::Alias(..)) {
        return None;
    }
    let ty = normalize_union_ty(tcx, ty).ok()?;
    let payload = candidate(ty, tcx, 16)?;
    finite_representation(ty, tcx).then_some(payload)
}

pub(super) fn finite_representation<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> bool {
    // Every recursive type path requires a nominal boundary. The depth and work limits keep large
    // graphs nominal.
    fn finite<'tcx>(
        ty: Ty<'tcx>,
        tcx: TyCtxt<'tcx>,
        path: &mut [Ty<'tcx>; 32],
        depth: usize,
        remaining: &mut usize,
    ) -> bool {
        if depth == path.len() || *remaining == 0 || path[..depth].contains(&ty) {
            return false;
        }
        *remaining -= 1;
        path[depth] = ty;
        let mut follow = |next| finite(next, tcx, path, depth + 1, remaining);
        match ty.kind() {
            TyKind::Ref(_, inner, _)
            | TyKind::RawPtr(inner, _)
            | TyKind::Array(inner, _)
            | TyKind::Slice(inner)
            | TyKind::Pat(inner, _) => follow(*inner),
            TyKind::Tuple(elements) => elements.iter().all(follow),
            TyKind::Alias(..) => normalize_union_ty(tcx, ty).is_ok_and(follow),
            TyKind::FnPtr(..) => {
                let signature = tcx.instantiate_bound_regions_with_erased(ty.fn_sig(tcx));
                signature.inputs_and_output.iter().all(follow)
            }
            TyKind::Adt(def, args)
                if crate::lower1::is_non_null_lang_item(tcx, def.did())
                    && is_codegen_sized(args.type_at(0), tcx) =>
            {
                follow(args.type_at(0))
            }
            TyKind::Adt(def, args) if def.is_enum() => {
                // enum_carrier queries transparent_payload recursively. This check must inspect the
                // layout directly.
                if tcx.is_lang_item(def.did(), rustc_attr_ir::lang_items::LangItem::Option) {
                    return follow(args.type_at(0));
                }
                let Ok(layout) = tcx.layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
                else {
                    return false;
                };
                if let Variants::Single { index } = layout.variants
                    && let [field] = def.variant(index).fields.raw.as_slice()
                {
                    follow(field.ty(tcx, args).skip_norm_wip())
                } else {
                    true
                }
            }
            _ => candidate(ty, tcx, 16).is_none_or(|payload| follow(payload.ty)),
        }
    }
    finite(ty, tcx, &mut [ty; 32], 0, &mut 128)
}
