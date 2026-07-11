//! Closure naming helpers for MIR lowering.

use super::jvm_names;
use rustc_middle::ty::{Instance, TyCtxt};

/// Generate a consistent JVM-safe name for a closure function
///
/// Format: closure_<crate>_<item_path>_<unique_suffix>
pub fn generate_closure_function_name<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> String {
    let def_path = tcx.def_path_str(instance.def_id());
    let base = jvm_names::path_segment(&def_path);
    let hash =
        super::types::short_hash(&format!("{:?}:{:?}", instance.def_id(), instance.args), 10);
    let safe_name = format!("{base}_{hash}");

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
