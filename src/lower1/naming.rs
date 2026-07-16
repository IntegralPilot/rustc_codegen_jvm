//! Naming helpers for functions and monomorphized instances

use super::jvm_names;
use rustc_hir::{LangItem, def::DefKind};
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::ty::{GenericArg, Instance, TyCtxt, TypeVisitableExt};
use rustc_span::{def_id::LOCAL_CRATE, sym};
use std::collections::HashMap;

const MAX_MONO_FN_NAME_LEN: usize = 128;
const WEAK_LANG_ITEMS_CLASS: &str = "org/rustlang/runtime/WeakLangItems";
const GLOBAL_LINK_SYMBOLS_PACKAGE: &str = "org/rustlang/runtime/symbols";

#[derive(Debug, Clone)]
pub struct FnNameData {
    pub class_to_call_on: Option<String>,
    pub method_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmStaticImport {
    pub class_name: String,
    pub method_name: String,
    pub descriptor: String,
}

pub fn mono_owner_class<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> String {
    let def_id = instance.def_id();
    if !def_id.is_local()
        && jvm_names::is_runtime_crate(tcx, def_id.krate)
        && jvm_names::compiles_external_core_instances(tcx)
        && tcx.generics_of(def_id).requires_monomorphization(tcx)
        && !instance.args.has_param()
        && !instance.args.has_escaping_bound_vars()
        && instance.upstream_monomorphization(tcx).is_none()
    {
        let instance_key =
            super::types::stable_normalized_instance_key(tcx, instance.def_id(), instance.args);
        let bucket = super::types::short_hash(&instance_key, 1);
        format!(
            "{}/mono/MonoBucket_{}",
            jvm_names::crate_root(tcx, LOCAL_CRATE),
            bucket
        )
    } else if let Some(trait_def_id) = tcx
        .opt_associated_item(def_id)
        .and_then(|item| item.trait_container(tcx))
    {
        // A Java interface carries its dynamically dispatched method, but
        // monomorphized Rust default bodies are static functions. Put those
        // bodies beside the trait in its module rather than generating a
        // second class file for the interface name.
        jvm_names::owner_class_for_function(tcx, trait_def_id)
    } else {
        jvm_names::owner_class_for_function(tcx, def_id)
    }
}

pub fn parse_jvm_link_name(link_name: &str) -> Result<Option<JvmStaticImport>, String> {
    let Some(import) = link_name.strip_prefix("jvm:") else {
        return Ok(None);
    };

    let mut parts = import.splitn(4, ':');
    let invocation = parts.next().unwrap_or_default();
    let class_name = parts.next().unwrap_or_default();
    let method_name = parts.next().unwrap_or_default();
    let descriptor = parts.next().unwrap_or_default();

    if invocation != "static" {
        return Err(format!(
            "unsupported JVM import invocation `{invocation}`; only `jvm:static` is supported"
        ));
    }
    if class_name.is_empty() || method_name.is_empty() || descriptor.is_empty() {
        return Err(
            "malformed JVM import; expected `jvm:static:<internal-class>:<method>:<descriptor>`"
                .to_string(),
        );
    }
    if class_name.contains('.') || class_name.starts_with('/') || class_name.ends_with('/') {
        return Err(format!(
            "invalid JVM internal class name `{class_name}`; use `/` between package components"
        ));
    }
    if method_name.contains('/') || method_name.contains('.') {
        return Err(format!("invalid JVM method name `{method_name}`"));
    }

    Ok(Some(JvmStaticImport {
        class_name: class_name.to_string(),
        method_name: method_name.to_string(),
        descriptor: descriptor.to_string(),
    }))
}

pub fn jvm_static_import_from_instance<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Result<Option<JvmStaticImport>, String> {
    let Some(symbol_name) = tcx.codegen_fn_attrs(instance.def_id()).symbol_name else {
        return Ok(None);
    };
    let import = parse_jvm_link_name(symbol_name.as_str())?;
    if import.is_some() && !tcx.is_foreign_item(instance.def_id()) {
        return Err(
            "a `jvm:` link name is only supported on a function in an `extern` block".to_string(),
        );
    }
    Ok(import)
}

fn global_link_symbol_from_instance<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Option<String> {
    let attrs = tcx.codegen_fn_attrs(instance.def_id());
    let symbol_name = attrs.symbol_name.or_else(|| {
        attrs
            .flags
            .intersects(
                CodegenFnAttrFlags::NO_MANGLE | CodegenFnAttrFlags::RUSTC_STD_INTERNAL_SYMBOL,
            )
            .then(|| tcx.item_name(instance.def_id()))
    })?;
    if symbol_name.as_str().starts_with("jvm:") {
        return None;
    }
    Some(jvm_names::member_name(symbol_name.as_str()))
}

pub fn global_link_symbol_class(symbol_name: &str) -> String {
    format!("{GLOBAL_LINK_SYMBOLS_PACKAGE}/{symbol_name}")
}

pub fn is_global_link_symbol_class(class_name: &str) -> bool {
    class_name
        .strip_prefix(GLOBAL_LINK_SYMBOLS_PACKAGE)
        .is_some_and(|suffix| suffix.starts_with('/') && suffix.len() > 1)
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

    let Some(item) = tcx.opt_associated_item(instance.def_id()) else {
        return mono_fn_name_from_instance(tcx, instance).method_name;
    };
    let generics = tcx.generics_of(instance.def_id());
    let mut specialization_args = Vec::new();

    if let Some(trait_item_def_id) = item.trait_item_def_id() {
        // Use the implemented trait arguments rather than the impl arguments.
        // This makes the trait declaration and its implementation derive the
        // same JVM name, while omitting `Self`, which is represented by the
        // receiver class already.
        let impl_def_id = item
            .impl_container(tcx)
            .expect("a trait implementation item has an impl container");
        let trait_ref = tcx
            .impl_opt_trait_ref(impl_def_id)
            .expect("a trait implementation has a trait reference")
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        specialization_args.extend(trait_ref.args.iter().skip(1));
        specialization_args.extend(instance.args.iter().skip(generics.parent_count));

        return if specialization_args
            .iter()
            .all(|arg| arg.as_type().is_none() && arg.as_const().is_none())
        {
            jvm_names::method_for_function(tcx, trait_item_def_id)
        } else {
            associated_specialization_name(tcx, instance, trait_item_def_id, &specialization_args)
        };
    }

    if item.trait_container(tcx).is_some() {
        specialization_args.extend(instance.args.iter().skip(1));
    } else {
        // An inherent impl's parent generics are encoded in its receiver class.
        specialization_args.extend(instance.args.iter().skip(generics.parent_count));
    }

    if specialization_args
        .iter()
        .all(|arg| arg.as_type().is_none() && arg.as_const().is_none())
    {
        jvm_names::method_for_function(tcx, instance.def_id())
    } else {
        associated_specialization_name(tcx, instance, instance.def_id(), &specialization_args)
    }
}

fn associated_specialization_name<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    canonical_def_id: rustc_span::def_id::DefId,
    args: &[GenericArg<'tcx>],
) -> String {
    let method = jvm_names::method_for_function(tcx, canonical_def_id);
    let mut data_types = HashMap::new();
    let mut generic_tokens = Vec::new();
    for arg in args {
        if let Some(token) =
            super::types::readable_rust_generic_arg_name(*arg, tcx, &mut data_types, instance)
        {
            generic_tokens.push(super::types::sanitize_name_token(&token));
        }
    }
    let identity = format!(
        "{}:{}",
        super::types::stable_def_path(tcx, canonical_def_id),
        super::types::stable_normalized_instance_key(tcx, instance.def_id(), instance.args)
    );
    crate::stable_hash::readable_or_hashed_name(
        &method,
        &generic_tokens.join("_"),
        &identity,
        MAX_MONO_FN_NAME_LEN,
    )
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

    if let Some(method_name) = global_link_symbol_from_instance(tcx, instance) {
        return FnNameData {
            class_to_call_on: Some(global_link_symbol_class(&method_name)),
            method_name,
        };
    }

    let class = Some(mono_owner_class(tcx, instance));

    let mut safe_base = jvm_names::method_for_function(tcx, instance.def_id());
    if instance.args.has_param() || instance.args.has_escaping_bound_vars() {
        let hash = super::types::short_hash(
            &format!(
                "{}_nonconcrete_{}",
                safe_base,
                super::types::stable_normalized_instance_key(tcx, instance.def_id(), instance.args,)
            ),
            10,
        );
        return FnNameData {
            class_to_call_on: class,
            method_name: format!("{}_{}", safe_base, hash),
        };
    }
    let mut data_types = HashMap::new();
    if let Some(item) = tcx.opt_associated_item(instance.def_id()) {
        if let Some(trait_def_id) = item.trait_container(tcx) {
            safe_base = format!(
                "{}_{}",
                jvm_names::method_for_function(tcx, trait_def_id),
                safe_base
            );
        } else if let Some(impl_def_id) = item.impl_container(tcx) {
            let self_ty = tcx
                .type_of(impl_def_id)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let self_token = super::types::readable_rust_generic_arg_name(
                self_ty.into(),
                tcx,
                &mut data_types,
                instance,
            )
            .map(|token| super::types::sanitize_name_token(&token))
            .unwrap_or_else(|| "Self".to_string());
            let trait_prefix = tcx.impl_opt_trait_ref(impl_def_id).map(|trait_ref| {
                jvm_names::method_for_function(tcx, trait_ref.skip_binder().def_id)
            });
            safe_base = if let Some(trait_prefix) = trait_prefix {
                format!("{trait_prefix}_{self_token}_{safe_base}")
            } else {
                format!("{self_token}_{safe_base}")
            };
        }
    } else if matches!(
        tcx.def_kind(tcx.parent(instance.def_id())),
        DefKind::Fn | DefKind::AssocFn | DefKind::Closure
    ) {
        safe_base = jvm_names::disambiguated_def_path_token(tcx, instance.def_id());
    }

    let mut generic_tokens = Vec::new();

    // Collect type and const generics. Regions are erased by the JVM ABI.
    for arg in instance.args.iter() {
        if let Some(token) =
            super::types::readable_rust_generic_arg_name(arg, tcx, &mut data_types, instance)
        {
            generic_tokens.push(super::types::sanitize_name_token(&token));
        }
    }

    let identity =
        super::types::stable_normalized_instance_key(tcx, instance.def_id(), instance.args);
    FnNameData {
        class_to_call_on: class,
        method_name: crate::stable_hash::readable_or_hashed_name(
            &safe_base,
            &generic_tokens.join("_"),
            &identity,
            MAX_MONO_FN_NAME_LEN,
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::{JvmStaticImport, jvm_names, parse_jvm_link_name};

    #[test]
    fn generated_jvm_identifiers_preserve_rust_underscores() {
        assert_eq!(jvm_names::member_name("many___parts"), "many___parts");
        assert_eq!(
            jvm_names::member_name("__compiler_builtin"),
            "__compiler_builtin"
        );
        assert_eq!(jvm_names::member_name("part::<item>"), "part_item");
    }

    #[test]
    fn parses_static_jvm_import() {
        assert_eq!(
            parse_jvm_link_name(
                "jvm:static:org/rustlang/runtime/PanicSupport:raise:(Lorg/rustlang/runtime/Pointer;)V"
            ),
            Ok(Some(JvmStaticImport {
                class_name: "org/rustlang/runtime/PanicSupport".to_string(),
                method_name: "raise".to_string(),
                descriptor: "(Lorg/rustlang/runtime/Pointer;)V".to_string(),
            }))
        );
    }

    #[test]
    fn ignores_normal_link_names() {
        assert_eq!(parse_jvm_link_name("ordinary_native_symbol"), Ok(None));
    }

    #[test]
    fn rejects_unsupported_invocation_kind() {
        let error =
            parse_jvm_link_name("jvm:virtual:java/lang/Object:toString:()Ljava/lang/String;")
                .unwrap_err();
        assert!(error.contains("only `jvm:static` is supported"));
    }

    #[test]
    fn rejects_malformed_import() {
        let error = parse_jvm_link_name("jvm:static:java/lang/System:exit").unwrap_err();
        assert!(error.contains("malformed JVM import"));
    }
}
