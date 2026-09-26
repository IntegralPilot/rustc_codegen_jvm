//! Resolve rustc's synthetic statics without querying their nonexistent type.
use super::*;

pub(super) fn resolve_allocation<'tcx>(
    tcx: TyCtxt<'tcx>,
    alloc_id: AllocId,
) -> Result<GlobalAlloc<'tcx>, String> {
    match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Static(def_id) if crate::lower1::statics::is_nested(tcx, def_id) => tcx
            .eval_static_initializer(def_id)
            .map(GlobalAlloc::Memory)
            .map_err(|error| format!("could not evaluate nested static {def_id:?}: {error:?}")),
        allocation => Ok(allocation),
    }
}

pub(super) fn allocation_identity(
    tcx: TyCtxt<'_>,
    data_types: &Definitions<'_>,
    alloc_id: AllocId,
    candidate: String,
) -> String {
    // The same nested static can be decoded through multiple types and in
    // different compilation units. Its identity must not depend on the view.
    if let GlobalAlloc::Static(def_id) = tcx.global_alloc(alloc_id) {
        return format!(
            "static::{}",
            crate::lower1::types::stable_def_identity(tcx, def_id)
        );
    }
    data_types.allocation_identity(alloc_id, candidate)
}
