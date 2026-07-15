//! Naming helpers for functions and monomorphized instances

use super::{jvm_names, types::ty_to_oomir_type};
use rustc_hir::{LangItem, def::DefKind};
use rustc_middle::ty::{GenericParamDefKind, Instance, TyCtxt, TypeVisitableExt};
use rustc_span::sym;
use std::collections::HashMap;

const MAX_MONO_FN_NAME_LEN: usize = 128;
const WEAK_LANG_ITEMS_CLASS: &str = "org/rustlang/runtime/WeakLangItems";

#[derive(Debug, Clone)]
pub struct FnNameData {
    pub class_to_call_on: Option<String>,
    pub method_name: String,
}

pub fn associated_method_name_from_instance<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> String {
    if !instance.def_id().is_local()
        && jvm_names::is_runtime_crate(tcx, instance.def_id().krate)
        && !jvm_names::compiles_external_core_instances(tcx)
    {
        return jvm_names::method_for_function(tcx, instance.def_id());
    }

    let has_own_type_or_const_generics = tcx
        .generics_of(instance.def_id())
        .own_params
        .iter()
        .any(|param| !matches!(param.kind, GenericParamDefKind::Lifetime));

    if has_own_type_or_const_generics {
        mono_fn_name_from_instance(tcx, instance).method_name
    } else {
        jvm_names::method_for_function(tcx, instance.def_id())
    }
}

/// Generate a JVM-safe function name for a (possibly monomorphized) function instance.
///
/// Attempts to generate a readable name by appending sanitized generic type names
/// (e.g., `my_func_i32_String`). Falls back to a hash of the type descriptors if the
/// resulting name becomes too long.
pub fn mono_fn_name_from_instance<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> FnNameData {
    let is_core_panic_impl_declaration = tcx.is_foreign_item(instance.def_id())
        && tcx.opt_item_name(instance.def_id()) == Some(sym::panic_impl)
        && tcx.crate_name(instance.def_id().krate) == sym::core;
    if is_core_panic_impl_declaration || tcx.is_lang_item(instance.def_id(), LangItem::PanicImpl) {
        return FnNameData {
            class_to_call_on: Some(WEAK_LANG_ITEMS_CLASS.to_string()),
            method_name: LangItem::PanicImpl
                .link_name()
                .expect("panic_impl has a weak link name")
                .to_string(),
        };
    }

    let class = Some(jvm_names::owner_class_for_function(tcx, instance.def_id()));

    // Use only the last path segment as the method base (so "core::panicking::panic" -> "panic")
    let mut safe_base = jvm_names::method_for_function(tcx, instance.def_id());
    let parent_def_kind = tcx.def_kind(tcx.parent(instance.def_id()));
    if tcx.opt_associated_item(instance.def_id()).is_some()
        || matches!(
            parent_def_kind,
            DefKind::Fn | DefKind::AssocFn | DefKind::Closure
        )
    {
        let definition_hash =
            super::types::short_hash(&super::types::stable_def_path(tcx, instance.def_id()), 10);
        safe_base = format!("{safe_base}__{definition_hash}");
    }
    // We need a local map for the type conversion, similar to the original function
    let mut data_types = HashMap::new();

    if instance.args.has_param() || instance.args.has_escaping_bound_vars() {
        let hash = super::types::short_hash(
            &format!(
                "{}_nonconcrete_{}",
                safe_base,
                super::types::stable_instance_key(tcx, instance.def_id(), instance.args)
            ),
            10,
        );
        return FnNameData {
            class_to_call_on: class,
            method_name: format!("{}__{}", safe_base, hash),
        };
    }

    let mut generic_tokens = Vec::new();
    let mut oomir_args = Vec::new();

    // 1. Collect generics and build readable tokens
    for arg in instance.args.iter() {
        if let Some(ty) = arg.as_type() {
            // Convert to OOMIR type
            let oomir_ty = ty_to_oomir_type(ty, tcx, &mut data_types, instance);

            // Generate readable token (e.g., "i32", "MyStruct")
            let token = super::types::readable_oomir_type_name(&oomir_ty);
            generic_tokens.push(super::types::sanitize_name_token(&token));

            // Keep the OOMIR type in case we need to fallback to descriptor hashing
            oomir_args.push(oomir_ty);
        }
    }

    // 2. Construct the readable name. Rust instances can differ only in const
    // arguments, which are not present in `generic_tokens`, so include a hash of
    // the complete instance arguments whenever this is a generic instance.
    let readable_base = if generic_tokens.is_empty() {
        safe_base.clone()
    } else {
        format!("{}_{}", safe_base, generic_tokens.join("_"))
    };
    let readable_name = if instance.args.is_empty() {
        readable_base
    } else {
        let instance_hash = super::types::short_hash(
            &super::types::stable_instance_key(tcx, instance.def_id(), instance.args),
            10,
        );
        format!("{readable_base}__{instance_hash}")
    };

    // 3. Check length limit. If it fits, return the readable version.
    if readable_name.len() <= MAX_MONO_FN_NAME_LEN {
        return FnNameData {
            class_to_call_on: class,
            method_name: readable_name,
        };
    }

    // 4. Fallback: Name is too long, generate hash from descriptors
    let mut descriptor_str = String::new();
    descriptor_str.push_str(&safe_base);
    descriptor_str.push('_'); // Separator for hash generation context

    for ty in oomir_args {
        descriptor_str.push_str(&ty.to_jvm_descriptor());
        descriptor_str.push('_');
    }
    descriptor_str.push_str(&super::types::stable_instance_key(
        tcx,
        instance.def_id(),
        instance.args,
    ));

    let hash = super::types::short_hash(&descriptor_str, 10);

    // Use double underscore for hash separation to distinguish from readable parts
    FnNameData {
        class_to_call_on: class,
        method_name: format!("{}__{}", safe_base, hash),
    }
}
