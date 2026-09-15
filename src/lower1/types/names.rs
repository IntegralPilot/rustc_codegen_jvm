use super::*;
use crate::lower1::context::Definitions;

pub(super) fn append_dynamic_generic_arg_key<'tcx>(
    key: &mut String,
    arg: rustc_middle::ty::GenericArg<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Option<String> {
    let token = readable_rust_generic_arg_name(arg, tcx, data_types, instance_context)?;
    key.push_str(if arg.as_type().is_some() {
        "type="
    } else {
        "const="
    });
    key.push_str(&token);
    key.push(';');
    Some(token)
}

/// Generates a short hash of the input string.
/// The hash is truncated to the specified length to ensure it fits within JVM class name constraints.
pub(crate) fn short_hash(input: &str, length: usize) -> String {
    crate::stable_hash::short_hash(input, length)
}

pub(crate) fn stable_def_path(tcx: TyCtxt<'_>, def_id: DefId) -> String {
    let crate_name = tcx.crate_name(def_id.krate).to_string();
    let path = with_resolve_crate_name!(with_no_trimmed_paths!(tcx.def_path_str(def_id)));
    path.strip_prefix(&format!("{crate_name}::"))
        .unwrap_or(&path)
        .to_string()
}

/// Returns an identifier for a definition that is stable across crate aliases.
///
/// Pretty-printed paths are unsuitable for ABI names because the same `core`
/// definition can be rendered through aliases such as `std` or
/// `rustc_std_workspace_core` in different rustc invocations.
pub(crate) fn stable_def_identity(tcx: TyCtxt<'_>, def_id: DefId) -> String {
    let hash_bytes = tcx.def_path_hash(def_id).0.to_le_bytes();
    crate::stable_hash::short_hash_bytes(&hash_bytes, 16)
}

/// Returns rustc's crate-alias-independent identity for a monomorphic type.
/// Normalize and erase regions before hashing so equivalent upstream and
/// downstream views have the same JVM ABI identity.
pub(crate) fn stable_type_identity<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> String {
    let ty = normalize_union_ty(tcx, ty).unwrap_or(ty);
    let hash = tcx.with_stable_hashing_context(|mut hcx| {
        let mut hasher = StableHasher::new();
        ty.stable_hash(&mut hcx, &mut hasher);
        hasher.finish::<Hash64>()
    });
    format!("{hash:016x}")
}

/// Returns a crate-alias-independent identity for a concrete instance.
pub(crate) fn stable_instance_identity<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    args: GenericArgsRef<'tcx>,
) -> String {
    let args = tcx
        .try_normalize_erasing_regions(
            TypingEnv::fully_monomorphized(),
            rustc_middle::ty::Unnormalized::new_wip(args),
        )
        .unwrap_or(args);
    let hash = tcx.with_stable_hashing_context(|mut hcx| {
        let mut hasher = StableHasher::new();
        def_id.stable_hash(&mut hcx, &mut hasher);
        args.stable_hash(&mut hcx, &mut hasher);
        hasher.finish::<Hash64>()
    });
    format!("{hash:016x}")
}

pub(super) fn readable_qualified_function_item_path(tcx: TyCtxt<'_>, def_id: DefId) -> String {
    readable_qualified_jvm_class(&jvm_names::function_item_class_for_def_id(tcx, def_id))
}

pub(super) fn readable_qualified_jvm_class(class_name: &str) -> String {
    let canonical = class_name
        .strip_prefix("org/rustlang/")
        .unwrap_or(class_name);
    sanitize_name_token(&canonical.replace('/', "_"))
}

pub(crate) fn stable_instance_key<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    args: GenericArgsRef<'tcx>,
) -> String {
    let crate_name = tcx.crate_name(def_id.krate).to_string();
    let path = with_resolve_crate_name!(with_no_trimmed_paths!(
        tcx.def_path_str_with_args(def_id, args)
    ));
    path.strip_prefix(&format!("{crate_name}::"))
        .unwrap_or(&path)
        .to_string()
}

pub(crate) fn stable_normalized_instance_key<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    args: GenericArgsRef<'tcx>,
) -> String {
    let args = tcx
        .try_normalize_erasing_regions(
            TypingEnv::fully_monomorphized(),
            rustc_middle::ty::Unnormalized::new_wip(args),
        )
        .unwrap_or(args);
    stable_instance_key(tcx, def_id, args)
}

// Produce a compact, human readable token for an OOMIR type to use in tuple class names.
pub(crate) fn readable_oomir_type_name(t: &oomir::Type) -> String {
    use oomir::Type;
    match t {
        Type::Boolean => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::I8 => "i8".to_string(),
        Type::U8 => "u8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::U16 => "u16".to_string(),
        Type::I32 => "i32".to_string(),
        Type::U32 => "u32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::U64 => "u64".to_string(),
        Type::F16 => "f16".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::Str => "Str".to_string(),
        Type::Void => "Void".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Class(name) => {
            // take last path segment for readability (e.g. java/lang/String -> String)
            name.rsplit('/').next().unwrap_or(name).to_string()
        }
        Type::Array(inner) => format!("{}Array", readable_oomir_type_name(inner)),
        Type::Slice(inner) => format!("{}Slice", readable_oomir_type_name(inner)),
        Type::Pointer(inner) => format!("Ptr{}", readable_oomir_type_name(inner)),
        Type::Reference(inner) => format!("Ref{}", readable_oomir_type_name(inner)),
        Type::MutableReference(inner) => format!("Ref{}", readable_oomir_type_name(inner)),
        Type::Interface(name) => {
            // prefix interfaces with I to avoid conflicts with classes
            let seg = name.rsplit('/').next().unwrap_or(name);
            format!("I{}", seg)
        }
    }
}

pub(super) fn readable_tuple_abi_type_name(t: &oomir::Type) -> String {
    use oomir::Type;
    match t {
        Type::Class(name) => sanitize_name_token(name),
        Type::Interface(name) => format!("I{}", sanitize_name_token(name)),
        Type::Array(inner) => format!("{}Array", readable_tuple_abi_type_name(inner)),
        Type::Slice(inner) => format!("{}Slice", readable_tuple_abi_type_name(inner)),
        Type::Pointer(inner) => format!("Ptr{}", readable_tuple_abi_type_name(inner)),
        Type::Reference(inner) | Type::MutableReference(inner) => {
            format!("Ref{}", readable_tuple_abi_type_name(inner))
        }
        _ => readable_oomir_type_name(t),
    }
}

// Naming primitive Rust types needs neither type normalization nor generated
// class schemas. The wide numeric carriers retain their established ABI tokens.
fn primitive_rust_type_name(ty: Ty<'_>) -> Option<&'static str> {
    Some(match ty.kind() {
        TyKind::Bool => "bool",
        TyKind::Char => "char",
        TyKind::Str => "Str",
        TyKind::Never => "Void",
        TyKind::Int(IntTy::I8) => "i8",
        TyKind::Int(IntTy::I16) => "i16",
        TyKind::Int(IntTy::I32) => "i32",
        TyKind::Int(IntTy::I64) => "i64",
        TyKind::Int(IntTy::I128) => "I128",
        TyKind::Int(IntTy::Isize) => "isize",
        TyKind::Uint(UintTy::U8) => "u8",
        TyKind::Uint(UintTy::U16) => "u16",
        TyKind::Uint(UintTy::U32) => "u32",
        TyKind::Uint(UintTy::U64) => "u64",
        TyKind::Uint(UintTy::U128) => "U128",
        TyKind::Uint(UintTy::Usize) => "usize",
        TyKind::Float(FloatTy::F16) => "f16",
        TyKind::Float(FloatTy::F32) => "f32",
        TyKind::Float(FloatTy::F64) => "f64",
        TyKind::Float(FloatTy::F128) => "F128",
        TyKind::Tuple(elements) if elements.is_empty() => "Unit",
        _ => return None,
    })
}

/// Produce a readable type token without erasing Rust distinctions that share a
/// JVM carrier. In particular, DST references such as `&str` are represented by
/// the same `Utf8View` carrier as `str`, but they are different generic types and
/// must not be assigned the same generated class name.
pub(crate) fn readable_rust_type_name<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    if let Some(name) = primitive_rust_type_name(ty) {
        return name.to_owned();
    }
    let instantiated = EarlyBinder::bind(tcx, ty).instantiate(tcx, instance_context.args);
    let original_ty = instantiated.skip_norm_wip();
    let ty = tcx
        .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), instantiated)
        .unwrap_or(original_ty);
    match ty.kind() {
        TyKind::Ref(_, inner, mutability) => format!(
            "{}{}",
            if mutability.is_mut() { "MutRef" } else { "Ref" },
            readable_rust_type_name(*inner, tcx, data_types, instance_context)
        ),
        TyKind::RawPtr(inner, mutability) => format!(
            "{}{}",
            if mutability.is_mut() {
                "MutPtr"
            } else {
                "ConstPtr"
            },
            readable_rust_type_name(*inner, tcx, data_types, instance_context)
        ),
        TyKind::Array(inner, length) => {
            let length = length
                .try_to_target_usize(tcx)
                .map(|length| length.to_string())
                .unwrap_or_else(|| "Unknown".to_string());
            format!(
                "{}Array{}",
                readable_rust_type_name(*inner, tcx, data_types, instance_context),
                length
            )
        }
        TyKind::Slice(inner) => format!(
            "{}Slice",
            readable_rust_type_name(*inner, tcx, data_types, instance_context)
        ),
        TyKind::Tuple(elements) if elements.is_empty() => "Unit".to_string(),
        TyKind::Tuple(elements) => format!(
            "Tuple_{}",
            elements
                .iter()
                .map(|element| readable_rust_type_name(element, tcx, data_types, instance_context,))
                .collect::<Vec<_>>()
                .join("_")
        ),
        TyKind::Dynamic(predicates, _) => {
            let mut base =
                readable_oomir_type_name(&ty_to_oomir_type(ty, tcx, data_types, instance_context));
            // Regions do not affect the JVM descriptor, but bound-region structure
            // can affect Rust-level operations such as TypeId. Preserve that hidden
            // distinction only for dynamic types whose predicates contain regions.
            if let TyKind::Dynamic(original_predicates, _) = original_ty.kind()
                && original_predicates
                    .iter()
                    .any(|predicate| match predicate.skip_binder() {
                        ExistentialPredicate::Trait(trait_ref) => {
                            trait_ref.args.iter().any(|arg| arg.as_region().is_some())
                        }
                        ExistentialPredicate::Projection(projection) => {
                            projection.args.iter().any(|arg| arg.as_region().is_some())
                        }
                        ExistentialPredicate::AutoTrait(_) => false,
                    })
            {
                let identity = with_no_trimmed_paths!(format!("{original_ty:?}"));
                base.push('_');
                base.push_str(&short_hash(&identity, 10));
            }
            let auto_traits = predicates
                .iter()
                .filter_map(|predicate| match predicate.skip_binder() {
                    ExistentialPredicate::AutoTrait(def_id) => {
                        Some(data_types.readable_class_name(tcx, def_id))
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            if auto_traits.is_empty() {
                base
            } else {
                format!("{base}_{}", auto_traits.join("_"))
            }
        }
        TyKind::Adt(adt_def, substs) => {
            // Generic arguments participate in the generated class identity.
            // Retain their qualified Rust path so unrelated types with the
            // same final segment (such as slice::Iter and btree_set::Iter) do
            // not erase to the same JVM class name.
            let base = data_types.readable_class_name(tcx, adt_def.did());
            let args = substs
                .iter()
                .filter_map(|arg| {
                    if let Some(arg_ty) = arg.as_type() {
                        Some(readable_rust_type_name(
                            arg_ty,
                            tcx,
                            data_types,
                            instance_context,
                        ))
                    } else {
                        arg.as_const().map(|constant| {
                            readable_rust_const_name(constant, tcx, instance_context)
                        })
                    }
                })
                .collect::<Vec<_>>();
            if args.is_empty() {
                base
            } else {
                format!("{}_{}", base, args.join("_"))
            }
        }
        TyKind::FnDef(def_id, _) => readable_qualified_function_item_path(tcx, *def_id),
        TyKind::Char => "char".to_string(),
        TyKind::Int(IntTy::Isize) => "isize".to_string(),
        TyKind::Uint(UintTy::Usize) => "usize".to_string(),
        _ => readable_oomir_type_name(&ty_to_oomir_type(ty, tcx, data_types, instance_context)),
    }
}

pub(super) fn readable_pointer_codec_type_name<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    if let Some(name) = primitive_rust_type_name(ty) {
        return name.to_owned();
    }
    let instantiated = EarlyBinder::bind(tcx, ty).instantiate(tcx, instance_context.args);
    let ty = tcx
        .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), instantiated)
        .unwrap_or_else(|_| instantiated.skip_norm_wip());
    match ty.kind() {
        TyKind::Ref(_, inner, mutability) => format!(
            "{}_to_{}",
            if mutability.is_mut() { "MutRef" } else { "Ref" },
            readable_pointer_codec_type_name(*inner, tcx, data_types, instance_context)
        ),
        TyKind::RawPtr(inner, mutability) => format!(
            "{}_to_{}",
            if mutability.is_mut() {
                "MutPtr"
            } else {
                "ConstPtr"
            },
            readable_pointer_codec_type_name(*inner, tcx, data_types, instance_context)
        ),
        TyKind::Array(inner, length) => {
            let length = length
                .try_to_target_usize(tcx)
                .map(|length| length.to_string())
                .unwrap_or_else(|| "Unknown".to_string());
            format!(
                "Array{}_of_{}",
                length,
                readable_pointer_codec_type_name(*inner, tcx, data_types, instance_context)
            )
        }
        TyKind::Slice(inner) => format!(
            "Slice_of_{}",
            readable_pointer_codec_type_name(*inner, tcx, data_types, instance_context)
        ),
        TyKind::Tuple(elements) if elements.is_empty() => "Unit".to_string(),
        TyKind::Tuple(elements) => format!(
            "Tuple_of_{}",
            elements
                .iter()
                .map(|element| readable_pointer_codec_type_name(
                    element,
                    tcx,
                    data_types,
                    instance_context,
                ))
                .collect::<Vec<_>>()
                .join("_and_")
        ),
        TyKind::Dynamic(predicates, _) => {
            let base =
                readable_oomir_type_name(&ty_to_oomir_type(ty, tcx, data_types, instance_context));
            let auto_traits = predicates
                .iter()
                .filter_map(|predicate| match predicate.skip_binder() {
                    ExistentialPredicate::AutoTrait(def_id) => {
                        Some(data_types.readable_class_name(tcx, def_id))
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            if auto_traits.is_empty() {
                base
            } else {
                format!("{}_and_{}", base, auto_traits.join("_and_"))
            }
        }
        TyKind::Adt(adt_def, substs) => {
            let base = data_types.readable_class_name(tcx, adt_def.did());
            let args = substs
                .iter()
                .filter_map(|arg| {
                    if let Some(arg_ty) = arg.as_type() {
                        Some(readable_pointer_codec_type_name(
                            arg_ty,
                            tcx,
                            data_types,
                            instance_context,
                        ))
                    } else {
                        arg.as_const().map(|constant| {
                            format!(
                                "Const_{}",
                                readable_rust_const_name(constant, tcx, instance_context)
                            )
                        })
                    }
                })
                .collect::<Vec<_>>();
            if args.is_empty() {
                base
            } else {
                format!("{}_of_{}", base, args.join("_and_"))
            }
        }
        TyKind::FnDef(def_id, _) => readable_qualified_function_item_path(tcx, *def_id),
        TyKind::Char => "char".to_string(),
        TyKind::Int(IntTy::Isize) => "isize".to_string(),
        TyKind::Uint(UintTy::Usize) => "usize".to_string(),
        _ => readable_oomir_type_name(&ty_to_oomir_type(ty, tcx, data_types, instance_context)),
    }
}

pub(super) fn readable_rust_const_name<'tcx>(
    constant: rustc_middle::ty::Const<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    let instantiated = EarlyBinder::bind(tcx, constant).instantiate(tcx, instance_context.args);
    let constant = tcx
        .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), instantiated)
        .unwrap_or_else(|_| instantiated.skip_norm_wip());

    constant
        .try_to_target_usize(tcx)
        .map(|value| value.to_string())
        .unwrap_or_else(|| with_no_trimmed_paths!(format!("{constant:?}")))
}

pub(crate) fn readable_rust_generic_arg_name<'tcx>(
    arg: rustc_middle::ty::GenericArg<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Option<String> {
    if let Some(arg_ty) = arg.as_type() {
        Some(readable_rust_type_name(
            arg_ty,
            tcx,
            data_types,
            instance_context,
        ))
    } else {
        arg.as_const()
            .map(|constant| readable_rust_const_name(constant, tcx, instance_context))
    }
}

// Sanitize token so it contains only ASCII alphanumeric characters and underscores.
pub(crate) fn sanitize_name_token(s: &str) -> String {
    let mut token = String::with_capacity(s.len());
    let mut previous_was_separator = false;
    for ch in s.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            token.push(ch);
            previous_was_separator = false;
        } else if !previous_was_separator && !token.is_empty() {
            token.push('_');
            previous_was_separator = true;
        }
    }
    while token.ends_with('_') {
        token.pop();
    }
    if token.is_empty() {
        "Type".to_string()
    } else {
        token
    }
}

/// Generate a JVM-safe ADT name, retaining raw tokens for the long-name hash.
/// A token may recursively name a large generic type; calculate it only once.
pub(crate) fn generate_adt_jvm_class_name<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    let base = data_types.class_name(tcx, adt_def.did());
    let tokens = substs
        .iter()
        .filter_map(|arg| readable_rust_generic_arg_name(arg, tcx, data_types, instance_context))
        .collect::<Vec<_>>();
    let mut name = base.clone();
    for token in &tokens {
        name.push('_');
        name.push_str(&sanitize_name_token(token));
    }
    if name.len() <= MAX_TUPLE_NAME_LEN {
        return name;
    }
    let mut identity = base.clone();
    identity.push('_');
    for token in tokens {
        identity.push_str(&token);
        identity.push('_');
    }
    format!("{base}_{}", short_hash(&identity, 10))
}

/// Generates a readable JVM class name for a tuple type. Rust types that share
/// the same JVM field carriers (such as `usize` and `u64`) deliberately reuse a
/// tuple class. A qualified stable hash is added only when the readable name is
/// already occupied by an ABI-incompatible tuple, such as two unrelated enums
/// both named `Ordering`.
pub(crate) fn generate_tuple_jvm_class_name<'tcx>(
    element_tys: &[Ty<'tcx>],
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>, // Needed for recursive calls
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    // First attempt: build a human-readable name like `Tuple_i32_String`.
    let mut tokens: Vec<String> = Vec::new();
    let mut oomir_element_types = Vec::new();
    for ty in element_tys {
        let oomir_ty = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
        // Downstream monomorphizations have a fresh collision registry, so keep
        // carrier paths in tuple names to prevent incompatible linker fragments.
        let token = readable_tuple_abi_type_name(&oomir_ty);
        tokens.push(sanitize_name_token(&token));
        oomir_element_types.push(oomir_ty);
    }

    let readable_name = format!("org/rustlang/core/Tuple_{}", tokens.join("_"));
    let local_incompatible_collision = match data_types.get(&readable_name) {
        Some(oomir::DataType::Class { fields, .. }) => fields
            .iter()
            .map(|(_, field_ty)| field_ty)
            .ne(oomir_element_types.iter()),
        Some(_) => true,
        None => false,
    };

    if readable_name.len() <= MAX_TUPLE_NAME_LEN {
        let crate_incompatible_collision =
            data_types.tuple_name_conflicts(&readable_name, &oomir_element_types);
        if !local_incompatible_collision && !crate_incompatible_collision {
            return readable_name;
        }
    }

    let identity = element_tys
        .iter()
        .map(|ty| readable_rust_type_name(*ty, tcx, data_types, instance_context))
        .collect::<Vec<_>>()
        .join("_");
    let hash = short_hash(&identity, 10);
    let disambiguated = format!("{readable_name}_{hash}");
    if disambiguated.len() <= MAX_TUPLE_NAME_LEN {
        disambiguated
    } else {
        format!("org/rustlang/core/Tuple_{hash}")
    }
}

// Helper to get field name from index using DataType info
pub(crate) fn get_field_name_from_index(
    owner_class_name: &str,
    index: usize,
    data_types: &HashMap<String, oomir::DataType>,
) -> Result<String, String> {
    // Return Result for error handling
    data_types
        .get(owner_class_name)
        .ok_or_else(|| format!("DataType not found for class '{}'", owner_class_name))
        .and_then(|data_type| match data_type {
            DataType::Class { fields, .. } => fields
                .get(index)
                .ok_or_else(|| {
                    format!(
                        "Field index {} out of bounds for class '{}' (has {} fields)",
                        index,
                        owner_class_name,
                        fields.len()
                    )
                })
                .map(|(name, _)| name.clone()),
            DataType::Interface { .. } => Err(format!(
                "Expected class, found interface {}",
                owner_class_name
            )),
        })
}
