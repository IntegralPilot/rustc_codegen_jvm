//! Naming helpers for functions and monomorphized instances

use super::{place::make_jvm_safe, types::ty_to_oomir_type};
use rustc_hir::def_id::DefId;
use rustc_middle::ty::{Instance, TyCtxt, TypeVisitableExt};
use std::collections::HashMap;

const MAX_MONO_FN_NAME_LEN: usize = 128;

#[derive(Debug, Clone)]
pub struct FnNameData {
    pub class_to_call_on: Option<String>,
    pub method_name: String,
}

impl FnNameData {
    pub fn key(&self, fallback_class: &str) -> String {
        crate::oomir::Module::function_key_for_owner(
            self.class_to_call_on.as_deref().unwrap_or(fallback_class),
            &self.method_name,
        )
    }
}

/// Generate a JVM-safe function name for a (possibly monomorphized) function instance.
///
/// Attempts to generate a readable name by appending sanitized generic type names
/// (e.g., `my_func_i32_String`). Falls back to a hash of the type descriptors if the
/// resulting name becomes too long.
pub fn mono_fn_name_from_instance<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> FnNameData {
    let full_path = tcx.def_path_str(instance.def_id());

    // Determine class (module path) from the full path (everything before the last "::")
    let class = owner_class_from_path(tcx, instance.def_id(), &full_path);

    // Use only the last path segment as the method base (so "core::panicking::panic" -> "panic")
    let method_segment = if let Some(pos) = full_path.rfind("::") {
        &full_path[pos + 2..]
    } else {
        &full_path[..]
    };

    let safe_base = make_jvm_safe(method_segment);
    // We need a local map for the type conversion, similar to the original function
    let mut data_types = HashMap::new();

    if instance.args.has_param() || instance.args.has_escaping_bound_vars() {
        let hash = super::types::short_hash(
            &format!("{}_nonconcrete_{:?}", safe_base, instance.args),
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

    // 2. Construct the readable name
    let readable_name = if generic_tokens.is_empty() {
        safe_base.clone()
    } else {
        format!("{}_{}", safe_base, generic_tokens.join("_"))
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

    let hash = super::types::short_hash(&descriptor_str, 10);

    // Use double underscore for hash separation to distinguish from readable parts
    FnNameData {
        class_to_call_on: class,
        method_name: format!("{}__{}", safe_base, hash),
    }
}

fn owner_class_from_path<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    full_path: &str,
) -> Option<String> {
    let crate_name = make_jvm_safe(&tcx.crate_name(def_id.krate).to_string());
    let def_path = tcx.def_path(def_id);
    let mut segments: Vec<String> = def_path
        .data
        .iter()
        .filter_map(|component| component.data.get_opt_name())
        .map(|name| sanitize_class_segment(name.as_str()))
        .collect();

    let method_segment = full_path
        .rfind("::")
        .map_or(full_path, |pos| &full_path[pos + 2..]);
    let method_name = make_jvm_safe(method_segment);
    if segments
        .last()
        .is_some_and(|segment| segment == &method_name)
    {
        segments.pop();
    }

    match crate_name.as_str() {
        "core" | "alloc" | "std" => {
            let runtime_root = format!("org/rustlang/{crate_name}");
            if segments.is_empty() {
                Some(runtime_root)
            } else {
                Some(format!("{runtime_root}/{}", segments.join("/")))
            }
        }
        _ if segments.is_empty() => Some(crate_name),
        _ if def_id.is_local() => Some(segments.join("/")),
        _ => Some(format!("{crate_name}/{}", segments.join("/"))),
    }
}

fn sanitize_class_segment(seg: &str) -> String {
    let mut s = seg.trim();
    if s.starts_with('<') && s.ends_with('>') && s.len() > 2 {
        s = &s[1..s.len() - 1];
        s = s.trim();
    }
    if s.starts_with("impl") {
        // drop the "impl" prefix and leading whitespace
        s = &s["impl".len()..];
        s = s.trim_start();

        // if there are leading generic params like "<T>", skip them
        if s.starts_with('<') {
            let mut depth = 0usize;
            let mut end_idx = None;
            for (i, ch) in s.char_indices() {
                if ch == '<' {
                    depth += 1;
                } else if ch == '>' {
                    if depth == 0 {
                        continue;
                    }
                    depth -= 1;
                    if depth == 0 {
                        end_idx = Some(i);
                        break;
                    }
                }
            }
            if let Some(i) = end_idx {
                s = &s[i + 1..];
                s = s.trim_start();
            } else {
                return make_jvm_safe(seg.trim());
            }
        }

        // now we expect something like "TraitName<...> for Foo" or "TraitName"
        // drop the " for ..." portion if present
        if let Some(pos) = s.find(" for ") {
            s = &s[..pos];
        }
        // drop trait generics if present, e.g. "PartialEq<&B>" -> "PartialEq"
        if let Some(pos) = s.find('<') {
            s = &s[..pos];
        }

        return make_jvm_safe(s.trim());
    }

    // Handle "Type as Trait" pattern (without impl prefix)
    if let Some(pos) = s.find(" as ") {
        let trait_part = &s[pos + 4..];
        if let Some(gpos) = trait_part.find('<') {
            return make_jvm_safe(trait_part[..gpos].trim());
        }
        return make_jvm_safe(trait_part.trim());
    }

    make_jvm_safe(seg)
}
