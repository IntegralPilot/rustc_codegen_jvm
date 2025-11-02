//! Naming helpers for functions and monomorphized instances

use super::{place::make_jvm_safe, types::ty_to_oomir_type};
use crate::oomir;
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::Operand as MirOperand,
    ty::{GenericArgsRef, Instance, Ty, TyCtxt, TyKind},
};
use sha2::Digest;
use std::collections::HashMap;

/// Generate a JVM-safe function name for a (possibly monomorphized) function instance.
///
/// For generic instantiations, we append a short hash of the concrete type descriptors
/// to avoid very long names while keeping them unique per instantiation.
pub fn mono_fn_name_from_instance<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> String {
    let base = tcx.item_name(instance.def_id()).to_string();
    let base = make_jvm_safe(&base);

    // Collect type descriptors from generic args (types only)
    let mut data_types = HashMap::new();
    let mut desc = String::new();
    for arg in instance.args.iter() {
        if let Some(ty) = arg.as_type() {
            let t = ty_to_oomir_type(ty, tcx, &mut data_types, instance);
            desc.push_str(&t.to_jvm_descriptor());
            desc.push('_');
        }
    }

    if desc.is_empty() {
        base
    } else {
        let hash = short_hash(&desc, 10);
        format!("{base}__{hash}")
    }
}

/// Generate a JVM-safe function name from a function definition and its generic args.
pub fn mono_fn_name_from_def_args<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    args: GenericArgsRef<'tcx>,
    instance: Instance<'tcx>,
) -> String {
    let base = tcx.item_name(def_id).to_string();
    let base = make_jvm_safe(&base);

    let mut data_types = HashMap::new();
    let mut desc = String::new();
    for arg in args.iter() {
        if let Some(ty) = arg.as_type() {
            let t = ty_to_oomir_type(ty, tcx, &mut data_types, instance);
            desc.push_str(&t.to_jvm_descriptor());
            desc.push('_');
        }
    }

    if desc.is_empty() {
        base
    } else {
        let hash = short_hash(&desc, 10);
        format!("{base}__{hash}")
    }
}

/// If `func` represents a FnDef(..) constant, returns a consistent JVM-safe name
/// matching the monomorphized naming used for lowering. Otherwise returns None.
pub fn mono_fn_name_from_call_operand<'tcx>(
    func: &MirOperand<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Option<String> {
    let MirOperand::Constant(box c) = func else {
        return None;
    };
    let fn_ty: Ty<'tcx> = c.const_.ty();
    match fn_ty.kind() {
        TyKind::FnDef(def_id, args) => {
            Some(mono_fn_name_from_def_args(tcx, *def_id, *args, instance))
        }
        _ => None,
    }
}

/// Generates a short hash of the input string. Truncated to `length` hex chars.
fn short_hash(input: &str, length: usize) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(input.as_bytes());
    let full_hash = format!("{:x}", hasher.finalize());
    full_hash[..length.min(full_hash.len())].to_string()
}
