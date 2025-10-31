//! Closure handling for MIR lowering
//!
//! This module provides utilities for detecting, extracting, and lowering Rust closures
//! from MIR to OOMIR.

use super::place::make_jvm_safe;
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::Operand as MirOperand,
    ty::{GenericArgsRef, TyCtxt, TyKind},
};

/// Information about a closure call detected in MIR
#[derive(Debug, Clone)]
pub struct ClosureCallInfo {
    /// The DefId of the closure itself (not the trait method)
    pub closure_def_id: DefId,
    /// The closure's generic arguments (captures, etc.)
    pub closure_args: GenericArgsRef<'static>,
    /// The kind of closure call (Fn, FnMut, FnOnce)
    pub call_kind: ClosureCallKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureCallKind {
    Fn,
    FnMut,
    FnOnce,
}

/// Try to extract closure information from a function operand in a Call terminator
///
/// When Rust lowers closure calls, they become trait method calls:
/// - `closure(args)` becomes `<Closure as Fn<Args>>::call(&closure, args)`
/// - `closure(args)` becomes `<Closure as FnMut<Args>>::call_mut(&mut closure, args)`
/// - `closure(args)` becomes `<Closure as FnOnce<Args>>::call_once(closure, args)`
///
/// This function detects these patterns and extracts the actual closure DefId.
pub fn extract_closure_info<'tcx>(
    func: &MirOperand<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> Option<ClosureCallInfo> {
    // The func operand should be a constant zero-sized function pointer
    let MirOperand::Constant(box constant) = func else {
        return None;
    };

    // Get the type of the function being called
    let fn_ty = constant.const_.ty();

    // Check if this is a function definition (FnDef)
    let TyKind::FnDef(def_id, fn_args) = fn_ty.kind() else {
        return None;
    };

    // Get the trait information
    // For trait methods, we need to check the DefId
    let fn_name = tcx.item_name(*def_id);

    // Use the debug format of DefId which includes the full path
    // Format is like "DefId(2:3991 ~ core[7e6a]::ops::function::Fn::call)"
    let def_id_debug = format!("{:?}", def_id);

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "closure-detection",
        format!(
            "Checking function call: {} (DefId: {})",
            fn_name, def_id_debug
        )
    );

    // Check if this is a call to Fn::call, FnMut::call_mut, or FnOnce::call_once
    // Check the DefId debug output for the trait path
    let call_kind =
        if fn_name.as_str() == "call" && def_id_debug.contains("::ops::function::Fn::call") {
            ClosureCallKind::Fn
        } else if fn_name.as_str() == "call_mut"
            && def_id_debug.contains("::ops::function::FnMut::call_mut")
        {
            ClosureCallKind::FnMut
        } else if fn_name.as_str() == "call_once"
            && def_id_debug.contains("::ops::function::FnOnce::call_once")
        {
            ClosureCallKind::FnOnce
        } else {
            // Not a closure trait call
            return None;
        };

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "closure-detection",
        format!("Detected closure trait call: {:?}", call_kind)
    );

    // The first generic argument to the trait method should be the closure type
    // fn_args contains: [ClosureType, ArgsType]
    let closure_ty = fn_args.get(0)?;

    // Convert GenericArgKind to Ty
    let closure_ty = closure_ty.as_type()?;

    // Extract closure DefId from the closure type
    match closure_ty.kind() {
        TyKind::Closure(closure_def_id, closure_args) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "closure-detection",
                format!(
                    "Found closure: DefId={:?}, path={}",
                    closure_def_id,
                    tcx.def_path_str(closure_def_id)
                )
            );

            // SAFETY: We need to extend the lifetime. The closure_args will live as long
            // as the compilation session, so this is safe in the context of rustc.
            let closure_args_static = unsafe {
                std::mem::transmute::<GenericArgsRef<'tcx>, GenericArgsRef<'static>>(closure_args)
            };

            Some(ClosureCallInfo {
                closure_def_id: *closure_def_id,
                closure_args: closure_args_static,
                call_kind,
            })
        }
        _ => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "closure-detection",
                format!("Expected closure type, found: {:?}", closure_ty.kind())
            );
            None
        }
    }
}

/// Generate a consistent JVM-safe name for a closure function
///
/// Format: closure_<crate>_<item_path>_<unique_suffix>
pub fn generate_closure_function_name(tcx: TyCtxt<'_>, closure_def_id: DefId) -> String {
    let def_path = tcx.def_path_str(closure_def_id);

    // The def_path will be something like "closures::main::{closure#0}"
    // We want to make this JVM-safe
    let safe_name = make_jvm_safe(&def_path);

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
