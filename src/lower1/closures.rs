//! Closure naming helpers for MIR lowering.

use super::jvm_names;
use rustc_hir::def_id::DefId;
use rustc_middle::ty::TyCtxt;

/// Generate a consistent JVM-safe name for a closure function
///
/// Format: closure_<crate>_<item_path>_<unique_suffix>
pub fn generate_closure_function_name(tcx: TyCtxt<'_>, closure_def_id: DefId) -> String {
    let def_path = tcx.def_path_str(closure_def_id);
    let safe_name = jvm_names::path_segment(&def_path);

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "closure-naming",
        format!(
            "Generated closure name: {} from path: {}",
            safe_name, def_path
        )
    );

    safe_name
}
