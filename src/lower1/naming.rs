//! Naming helpers for functions and monomorphized instances

use super::{place::make_jvm_safe, types::ty_to_oomir_type};
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::Operand as MirOperand,
    ty::{GenericArgsRef, Instance, Ty, TyCtxt, TyKind, TypeVisitableExt},
};
use sha2::Digest;
use std::collections::HashMap;

const MAX_MONO_FN_NAME_LEN: usize = 128;

#[derive(Debug, Clone)]
pub struct FnNameData {
    pub class_to_call_on: Option<String>,
    pub method_name: String,
}

/// Generate a JVM-safe function name for a (possibly monomorphized) function instance.
///
/// Attempts to generate a readable name by appending sanitized generic type names
/// (e.g., `my_func_i32_String`). Falls back to a hash of the type descriptors if the
/// resulting name becomes too long.
pub fn mono_fn_name_from_instance<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> FnNameData {
    let full_path = tcx.def_path_str(instance.def_id());

    // Determine class (module path) from the full path (everything before the last "::")
    let class;
    if let Some(pos) = full_path.rfind("::") {
        let mod_path = &full_path[..pos];
        // replace :: with /
        let mod_path_slash = mod_path.replace("::", "/");

        // sanitize any `impl ...` segments to keep only the trait name (e.g.:
        // "impl PartialEq<&B> for &A>" -> "PartialEq")
        // Also handle "Type as Trait" segments (e.g. "SimpleAdder as Calculator" -> "Calculator")
        let sanitize_impl_segment = |seg: &str| -> String {
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
                        // malformed; fall back to original segment (but trimmed)
                        return seg.trim().to_string();
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

                return s.trim().to_string();
            }

            // Handle "Type as Trait" pattern (without impl prefix)
            if let Some(pos) = s.find(" as ") {
                let trait_part = &s[pos + 4..];
                // drop trait generics if present
                if let Some(gpos) = trait_part.find('<') {
                    return trait_part[..gpos].trim().to_string();
                }
                return trait_part.trim().to_string();
            }

            seg.to_string()
        };

        let sanitized_segments: Vec<String> = mod_path_slash
            .split('/')
            .map(|seg| sanitize_impl_segment(seg))
            .collect();

        let sanitized_path = sanitized_segments.join("/");

        // if the first word is "core" replace it with org/rustlang/core
        class = if sanitized_path.starts_with("core/") {
            Some(format!("org/rustlang/{}", sanitized_path))
        } else {
            Some(sanitized_path)
        };
    } else {
        class = None;
    }

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
