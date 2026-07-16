//! Closure naming helpers for MIR lowering.

use super::jvm_names;
use rustc_middle::ty::{Instance, TyCtxt};

/// Generate a consistent JVM-safe name for a closure function
///
/// Format: closure_<crate>_<item_path>_<unique_suffix>
pub fn generate_closure_function_name<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> String {
    let def_path = super::types::stable_def_path(tcx, instance.def_id());
    let safe_name =
        jvm_names::closure_class_for_args(tcx, instance.def_id(), instance.args, instance)
            .rsplit('/')
            .next()
            .expect("closure class names have a final path segment")
            .to_string();

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
