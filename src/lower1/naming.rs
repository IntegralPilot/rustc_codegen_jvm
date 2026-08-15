//! Naming helpers for functions and monomorphized instances

use super::jvm_names;
use rustc_hash::FxHashMap as HashMap;
use rustc_hir::{attrs::lang_items::LangItem, def::DefKind};
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::ty::{GenericArg, Instance, InstanceKind, TyCtxt, TyKind, TypeVisitableExt};
use rustc_span::sym;

const MAX_MONO_FN_NAME_LEN: usize = 128;
const WEAK_LANG_ITEMS_CLASS: &str = "org/rustlang/runtime/WeakLangItems";
const GLOBAL_LINK_SYMBOLS_PACKAGE: &str = "org/rustlang/runtime/symbols";
const FINAL_OBJECT_METHODS: &[(&str, &str)] = &[
    ("getClass", "()Ljava/lang/Class;"),
    ("notify", "()V"),
    ("notifyAll", "()V"),
    ("wait", "()V"),
    ("wait", "(J)V"),
    ("wait", "(JI)V"),
];

#[derive(Debug, Clone)]
pub struct FnNameData {
    pub class_to_call_on: Option<String>,
    pub method_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmStaticImport {
    pub class_name: String,
    pub method_name: String,
    /// Optional legacy descriptor used only to verify the inferred Rust ABI.
    pub descriptor: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JvmVirtualImport {
    pub method_name: String,
    /// Optional descriptor used only to verify the inferred Rust ABI.
    pub descriptor: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JvmImport {
    Static(JvmStaticImport),
    Virtual(JvmVirtualImport),
}

fn validate_jvm_internal_class_name(class_name: &str) -> Result<(), String> {
    if class_name.is_empty()
        || class_name.starts_with('/')
        || class_name.ends_with('/')
        || class_name.split('/').any(str::is_empty)
        || class_name.contains(['.', ';', '['])
    {
        return Err(format!(
            "invalid JVM internal class name `{class_name}`; use `/` between non-empty package components"
        ));
    }
    Ok(())
}

/// Parses the `#[link_name]` carried by an `extern type` as a JVM class.
///
/// The plain JVM internal name is canonical. `jvm:class:` is also accepted so
/// callers can use an explicitly JVM-namespaced spelling if desired.
pub fn parse_jvm_class_link_name(link_name: &str) -> Result<String, String> {
    let class_name = if let Some(class_name) = link_name.strip_prefix("jvm:class:") {
        class_name
    } else if link_name.starts_with("jvm:") {
        return Err(format!(
            "unsupported JVM extern-type link name `{link_name}`; expected `jvm:class:<internal-class>`"
        ));
    } else {
        link_name
    };
    validate_jvm_internal_class_name(class_name)?;
    Ok(class_name.to_string())
}

fn is_external_runtime_generic<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> bool {
    let def_id = instance.def_id();
    !def_id.is_local()
        && !matches!(instance.def, InstanceKind::Intrinsic(_))
        && jvm_names::is_runtime_crate(tcx, def_id.krate)
        && jvm_names::compiles_external_core_instances(tcx)
        && tcx.generics_of(def_id).requires_monomorphization(tcx)
        && !instance.args.has_param()
        && !instance.args.has_escaping_bound_vars()
}

pub fn mono_owner_class<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> String {
    let def_id = instance.def_id();
    let external_runtime_generic = is_external_runtime_generic(tcx, instance);
    if external_runtime_generic {
        // Synthetic helpers can reference an instance without adding it to
        // the current crate's mono-item set. Give every runtime instance one
        // definition-crate owner so all downstream callers and exporters
        // agree even when the runtime crate has a different local alias.
        let instance_key = super::types::stable_instance_identity(tcx, def_id, instance.args);
        let bucket = super::types::short_hash(&instance_key, 1);
        format!(
            "{}/mono/MonoBucket_{}",
            jvm_names::crate_root(tcx, def_id.krate),
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

/// Whether a static body belongs to a Java interface, including nested
/// closures and coroutines whose immediate `DefId` is not an associated item.
pub fn instance_is_trait_interface_owned<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    owner_class: &str,
) -> bool {
    let mut current = Some(instance.def_id());
    while let Some(def_id) = current {
        let trait_def_id = matches!(tcx.def_kind(def_id), DefKind::Trait)
            .then_some(def_id)
            .or_else(|| {
                tcx.opt_associated_item(def_id)
                    .and_then(|item| item.trait_container(tcx))
            });
        if let Some(trait_def_id) = trait_def_id {
            return owner_class == jvm_names::class_for_def_id(tcx, trait_def_id);
        }
        current = tcx.opt_parent(def_id);
    }
    false
}

fn validate_jvm_method_name(method_name: &str) -> Result<(), String> {
    if method_name.is_empty() || method_name.contains(['/', '.', ':']) {
        return Err(format!("invalid JVM method name `{method_name}`"));
    }
    Ok(())
}

pub fn parse_jvm_link_name(link_name: &str) -> Result<Option<JvmImport>, String> {
    let Some(import) = link_name.strip_prefix("jvm:") else {
        return Ok(None);
    };

    let (invocation, import) = import.split_once(':').ok_or_else(|| {
        "malformed JVM import; expected `jvm:static:<internal-class>:<method>` or `jvm:virtual:<method>`"
            .to_string()
    })?;

    match invocation {
        "static" => {
            let mut parts = import.splitn(3, ':');
            let class_name = parts.next().unwrap_or_default();
            let method_name = parts.next().unwrap_or_default();
            let descriptor = parts.next();
            if class_name.is_empty() || method_name.is_empty() {
                return Err(
                    "malformed JVM import; expected `jvm:static:<internal-class>:<method>[:<descriptor>]`"
                        .to_string(),
                );
            }
            if descriptor == Some("") {
                return Err(
                    "malformed JVM import; an explicit descriptor cannot be empty".to_string(),
                );
            }
            validate_jvm_internal_class_name(class_name)?;
            validate_jvm_method_name(method_name)?;

            Ok(Some(JvmImport::Static(JvmStaticImport {
                class_name: class_name.to_string(),
                method_name: method_name.to_string(),
                descriptor: descriptor.map(str::to_string),
            })))
        }
        "virtual" => {
            let mut parts = import.splitn(2, ':');
            let method_name = parts.next().unwrap_or_default();
            let descriptor = parts.next();
            if method_name.is_empty() {
                return Err(
                    "malformed JVM import; expected `jvm:virtual:<method>[:<descriptor>]`"
                        .to_string(),
                );
            }
            if descriptor == Some("") {
                return Err(
                    "malformed JVM import; an explicit descriptor cannot be empty".to_string(),
                );
            }
            validate_jvm_method_name(method_name)?;

            Ok(Some(JvmImport::Virtual(JvmVirtualImport {
                method_name: method_name.to_string(),
                descriptor: descriptor.map(str::to_string),
            })))
        }
        _ => Err(format!(
            "unsupported JVM import invocation `{invocation}`; expected `jvm:static` or `jvm:virtual`"
        )),
    }
}

pub fn jvm_virtual_receiver_class_from_instance<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Result<String, String> {
    let signature = tcx
        .fn_sig(instance.def_id())
        .instantiate(tcx, instance.args)
        .skip_binder();
    let Some(receiver_ty) = signature.inputs().first().copied() else {
        return Err(
            "a `jvm:virtual` import requires a receiver as its first parameter".to_string(),
        );
    };
    let pointee_ty = match receiver_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        _ => {
            return Err(
                "the first parameter of a `jvm:virtual` import must be a pointer or reference to a linked `extern type`"
                    .to_string(),
            );
        }
    };
    let TyKind::Foreign(def_id) = pointee_ty.kind() else {
        return Err(
            "the first parameter of a `jvm:virtual` import must be a pointer or reference to a linked `extern type`"
                .to_string(),
        );
    };
    let Some(link_name) = rustc_hir::find_attr!(
        tcx,
        *def_id,
        LinkName { name, .. } => *name
    ) else {
        return Err(
            "the receiver extern type of a `jvm:virtual` import must have a `#[link_name]`"
                .to_string(),
        );
    };
    parse_jvm_class_link_name(link_name.as_str())
}

pub fn jvm_import_from_instance<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Result<Option<JvmImport>, String> {
    let Some(symbol_name) = tcx.codegen_fn_attrs(instance.def_id()).symbol_name else {
        return Ok(None);
    };
    let import = parse_jvm_link_name(symbol_name.as_str())?;
    if import.is_some() && !tcx.is_foreign_item(instance.def_id()) {
        return Err(
            "a `jvm:` link name is only supported on a function in an `extern` block".to_string(),
        );
    }
    if matches!(import, Some(JvmImport::Virtual(_))) {
        jvm_virtual_receiver_class_from_instance(tcx, instance)?;
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
    signature: &crate::oomir::Signature,
) -> String {
    let method_name = associated_method_base_name_from_instance(tcx, instance);
    let descriptor = signature.to_string();
    if FINAL_OBJECT_METHODS
        .iter()
        .any(|(name, final_descriptor)| method_name == *name && descriptor == *final_descriptor)
    {
        format!("{method_name}$rust")
    } else {
        method_name
    }
}

fn associated_method_base_name_from_instance<'tcx>(
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
    let mut data_types = HashMap::default();
    let mut generic_tokens = Vec::new();
    for arg in args {
        if let Some(token) =
            super::types::readable_rust_generic_arg_name(*arg, tcx, &mut data_types, instance)
        {
            generic_tokens.push(super::types::sanitize_name_token(&token));
        }
    }
    let identity = super::types::stable_def_identity(tcx, canonical_def_id);
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

    let external_runtime_generic = is_external_runtime_generic(tcx, instance);
    let needs_definition_suffix =
        external_runtime_generic && matches!(instance.def, InstanceKind::Item(_));
    let mut safe_base = if needs_definition_suffix {
        // Upstream generic bodies are grouped into a small number of
        // downstream MonoBucket classes. Their definition identity is the one
        // place a suffix is required: otherwise unrelated functions such as
        // slice::from_mut and array::from_mut can acquire the same JVM name
        // and descriptor.
        format!(
            "{}_{}",
            jvm_names::method_for_function(tcx, instance.def_id()),
            super::types::short_hash(
                &super::types::stable_def_identity(tcx, instance.def_id()),
                10,
            ),
        )
    } else {
        jvm_names::method_for_function(tcx, instance.def_id())
    };
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
    let mut data_types = HashMap::default();
    let mut generic_tokens = Vec::new();
    if needs_definition_suffix {
        // The complete definition path above already identifies traits,
        // impls, and nested items without adding redundant prefixes.
    } else if let Some(item) = tcx.opt_associated_item(instance.def_id()) {
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
                let trait_ref = trait_ref.instantiate(tcx, instance.args).skip_norm_wip();
                let self_arg = trait_ref.args[0];
                for arg in trait_ref.args.iter().skip(1) {
                    if arg == self_arg {
                        continue;
                    }
                    if let Some(token) = super::types::readable_rust_generic_arg_name(
                        arg,
                        tcx,
                        &mut data_types,
                        instance,
                    ) {
                        generic_tokens.push(super::types::sanitize_name_token(&token));
                    }
                }
                jvm_names::method_for_function(tcx, trait_ref.def_id)
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

    // Collect type and const generics. Regions are erased by the JVM ABI.
    for arg in instance.args.iter() {
        if let Some(token) =
            super::types::readable_rust_generic_arg_name(arg, tcx, &mut data_types, instance)
        {
            generic_tokens.push(super::types::sanitize_name_token(&token));
        }
    }

    // Upstream generic symbols include the instantiating crate, so use the
    // definition path plus the complete suffix as the cross-crate JVM identity.
    let identity = super::types::stable_def_identity(tcx, instance.def_id());
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
    use super::{
        JvmImport, JvmStaticImport, JvmVirtualImport, jvm_names, parse_jvm_class_link_name,
        parse_jvm_link_name,
    };

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
    fn parses_static_jvm_import_with_inferred_descriptor() {
        assert_eq!(
            parse_jvm_link_name("jvm:static:org/rustlang/runtime/PanicSupport:raise"),
            Ok(Some(JvmImport::Static(JvmStaticImport {
                class_name: "org/rustlang/runtime/PanicSupport".to_string(),
                method_name: "raise".to_string(),
                descriptor: None,
            })))
        );
    }

    #[test]
    fn parses_legacy_static_jvm_import_with_explicit_descriptor() {
        assert_eq!(
            parse_jvm_link_name(
                "jvm:static:org/rustlang/runtime/PanicSupport:raise:(Lorg/rustlang/runtime/Pointer;)V"
            ),
            Ok(Some(JvmImport::Static(JvmStaticImport {
                class_name: "org/rustlang/runtime/PanicSupport".to_string(),
                method_name: "raise".to_string(),
                descriptor: Some("(Lorg/rustlang/runtime/Pointer;)V".to_string()),
            })))
        );
    }

    #[test]
    fn parses_virtual_jvm_import_with_inferred_owner_and_descriptor() {
        assert_eq!(
            parse_jvm_link_name("jvm:virtual:getYear"),
            Ok(Some(JvmImport::Virtual(JvmVirtualImport {
                method_name: "getYear".to_string(),
                descriptor: None,
            })))
        );
    }

    #[test]
    fn parses_virtual_jvm_import_with_explicit_descriptor() {
        assert_eq!(
            parse_jvm_link_name("jvm:virtual:plusDays:(J)Ljava/time/LocalDate;"),
            Ok(Some(JvmImport::Virtual(JvmVirtualImport {
                method_name: "plusDays".to_string(),
                descriptor: Some("(J)Ljava/time/LocalDate;".to_string()),
            })))
        );
    }

    #[test]
    fn ignores_normal_link_names() {
        assert_eq!(parse_jvm_link_name("ordinary_native_symbol"), Ok(None));
    }

    #[test]
    fn rejects_unsupported_invocation_kind() {
        let error = parse_jvm_link_name("jvm:special:java/lang/Object:toString").unwrap_err();
        assert!(error.contains("expected `jvm:static` or `jvm:virtual`"));
    }

    #[test]
    fn rejects_virtual_jvm_import_with_duplicated_owner() {
        let error = parse_jvm_link_name("jvm:virtual:java/lang/Object:toString").unwrap_err();
        assert!(error.contains("invalid JVM method name"));
    }

    #[test]
    fn rejects_malformed_import() {
        let error = parse_jvm_link_name("jvm:static:java/lang/System").unwrap_err();
        assert!(error.contains("malformed JVM import"));
    }

    #[test]
    fn rejects_empty_explicit_descriptor() {
        let error = parse_jvm_link_name("jvm:static:java/lang/System:exit:").unwrap_err();
        assert!(error.contains("descriptor cannot be empty"));

        let error = parse_jvm_link_name("jvm:virtual:toString:").unwrap_err();
        assert!(error.contains("descriptor cannot be empty"));
    }

    #[test]
    fn parses_extern_type_class_link_names() {
        assert_eq!(
            parse_jvm_class_link_name("java/lang/String"),
            Ok("java/lang/String".to_string())
        );
        assert_eq!(
            parse_jvm_class_link_name("jvm:class:java/util/ArrayList"),
            Ok("java/util/ArrayList".to_string())
        );
    }

    #[test]
    fn rejects_invalid_extern_type_class_link_names() {
        for invalid in [
            "",
            "/java/lang/String",
            "java//lang/String",
            "java.lang.String",
            "jvm:static:java/lang/String",
        ] {
            assert!(parse_jvm_class_link_name(invalid).is_err(), "{invalid}");
        }
    }
}
