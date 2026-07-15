//! Naming helpers for functions and monomorphized instances

use super::{jvm_names, types::ty_to_oomir_type};
use rustc_hir::{LangItem, def::DefKind};
use rustc_middle::ty::{GenericArg, Instance, TyCtxt, TypeVisitableExt};
use rustc_span::sym;
use std::collections::HashMap;

const MAX_MONO_FN_NAME_LEN: usize = 128;
const WEAK_LANG_ITEMS_CLASS: &str = "org/rustlang/runtime/WeakLangItems";

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
    let definition_hash =
        super::types::short_hash(&format!("{:?}", tcx.def_path_hash(canonical_def_id)), 10);
    let safe_base = format!("{method}__{definition_hash}");
    let mut data_types = HashMap::new();
    let mut generic_tokens = Vec::new();
    let mut specialization_parts = Vec::new();
    for arg in args {
        if let Some(ty) = arg.as_type() {
            let oomir_ty = ty_to_oomir_type(ty, tcx, &mut data_types, instance);
            let token = super::types::readable_oomir_type_name(&oomir_ty);
            generic_tokens.push(super::types::sanitize_name_token(&token));
            specialization_parts.push(format!("type:{token}"));
        } else if let Some(constant) = arg.as_const() {
            specialization_parts.push(format!("const:{constant:?}"));
        }
    }
    let readable_base = if generic_tokens.is_empty() {
        safe_base.clone()
    } else {
        format!("{}_{}", safe_base, generic_tokens.join("_"))
    };
    let specialization_key = specialization_parts.join(";");
    let specialization_hash = super::types::short_hash(&specialization_key, 10);

    if readable_base.len() + specialization_hash.len() + 2 <= MAX_MONO_FN_NAME_LEN {
        format!("{readable_base}__{specialization_hash}")
    } else {
        format!("{safe_base}__{specialization_hash}")
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

#[cfg(test)]
mod tests {
    use super::{JvmStaticImport, parse_jvm_link_name};

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
