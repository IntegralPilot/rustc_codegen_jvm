use super::place::make_jvm_safe;
use crate::oomir::{self, DataType, DataTypeMethod};

use rustc_middle::ty::{
    AdtDef,
    ExistentialPredicate,
    FloatTy,
    GenericArgsRef,
    IntTy,
    Ty,
    TyCtxt,
    TyKind,
    TypeVisitableExt, // Added TypeVisitableExt to check for params
    TypingEnv,
    UintTy,
};
use sha2::Digest;
use std::collections::HashMap;

/// OOMIR doesn't have a Never type (because the JVM doesn't), so we map it to Void.
/// So when we need to know if a MIR type is Never, we can use this helper.
pub fn ty_is_never<'tcx>(ty: Ty<'tcx>) -> bool {
    matches!(ty.kind(), TyKind::Never)
}

/// Converts a Rust MIR type (`Ty`) to an OOMIR type (`oomir::Type`).
pub fn ty_to_oomir_type<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Type {
    // Check if the instance args contain generic parameters.
    let has_params = instance_context.args.has_param();

    let resolved_ty = if has_params {
        rustc_middle::ty::EarlyBinder::bind(ty)
            .instantiate(tcx, instance_context.args)
            .skip_norm_wip()
    } else {
        let instantiated =
            rustc_middle::ty::EarlyBinder::bind(ty).instantiate(tcx, instance_context.args);
        match tcx.try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), instantiated) {
            Ok(normalized) => normalized,
            Err(_) => instantiated.skip_norm_wip(),
        }
    };
    match resolved_ty.kind() {
        rustc_middle::ty::TyKind::Bool => oomir::Type::Boolean,
        rustc_middle::ty::TyKind::Char => oomir::Type::Char,
        rustc_middle::ty::TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Type::I8,
            IntTy::I16 => oomir::Type::I16,
            IntTy::I32 => oomir::Type::I32,
            IntTy::I64 => oomir::Type::I64,
            IntTy::Isize => oomir::Type::I32,
            IntTy::I128 => oomir::Type::Class("java/math/BigInteger".to_string()), // doesn't fit in a primitive
        },
        rustc_middle::ty::TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Type::I16, // make it the next size up to capture full range
            UintTy::U16 => oomir::Type::I32,
            UintTy::U32 => oomir::Type::I64,
            UintTy::Usize => oomir::Type::I32, // java uses an i32 for most "usize" i.e. array indexes etc.
            UintTy::U64 => oomir::Type::Class("java/math/BigInteger".to_string()),
            UintTy::U128 => oomir::Type::Class("java/math/BigInteger".to_string()),
        },
        rustc_middle::ty::TyKind::Float(float_ty) => match float_ty {
            FloatTy::F32 => oomir::Type::F32,
            FloatTy::F64 => oomir::Type::F64,
            FloatTy::F16 => oomir::Type::F32,
            FloatTy::F128 => oomir::Type::Class("java/math/BigDecimal".to_string()),
        },
        rustc_middle::ty::TyKind::Adt(adt_def, substs) => {
            // Get the full path string for the ADT
            let full_path_str = tcx.def_path_str(adt_def.did());

            if full_path_str == "String" || full_path_str == "std::string::String" {
                oomir::Type::String
            } else {
                let safe_name = make_jvm_safe(&full_path_str);
                let base_jvm = safe_name.replace("::", "/").replace('.', "/");

                // Build readable generic tokens from substitutions (if any)
                let mut generic_tokens: Vec<String> = Vec::new();
                for arg in substs.iter() {
                    if let Some(arg_ty) = arg.as_type() {
                        let oomir_ty = ty_to_oomir_type(arg_ty, tcx, data_types, instance_context);
                        let token = readable_oomir_type_name(&oomir_ty);
                        generic_tokens.push(sanitize_name_token(&token));
                    } else {
                        // placeholder for non-type generic args (lifetimes/consts)
                        generic_tokens.push("_".to_string());
                    }
                }

                // Attach generics to the last path segment for readability
                let (prefix, last_segment) = match base_jvm.rsplit_once('/') {
                    Some((p, l)) => (p.to_string(), l.to_string()),
                    None => ("".to_string(), base_jvm.clone()),
                };

                let mut last_with_gens = last_segment.clone();
                if !generic_tokens.is_empty() {
                    last_with_gens = format!("{}_{}", last_segment, generic_tokens.join("_"));
                }

                let mut jvm_name_full = if prefix.is_empty() {
                    last_with_gens.clone()
                } else {
                    format!("{}/{}", prefix, last_with_gens)
                };

                // If the name is too long, fall back to hashed form
                if jvm_name_full.len() > MAX_TUPLE_NAME_LEN {
                    let mut name_parts = String::new();
                    name_parts.push_str(&base_jvm);
                    name_parts.push_str("_");
                    for arg in substs.iter() {
                        if let Some(arg_ty) = arg.as_type() {
                            let oomir_ty =
                                ty_to_oomir_type(arg_ty, tcx, data_types, instance_context);
                            name_parts.push_str(&oomir_ty.to_jvm_descriptor());
                            name_parts.push_str("_");
                        }
                    }
                    let hash = short_hash(&name_parts, 10);
                    let hashed_last = format!("{}_{}", last_segment, hash);
                    jvm_name_full = if prefix.is_empty() {
                        hashed_last
                    } else {
                        format!("{}/{}", prefix, hashed_last)
                    };
                }

                if adt_def.is_struct() {
                    let variant = adt_def.variant(0usize.into());
                    let oomir_fields = variant
                        .fields
                        .iter()
                        .map(|field_def| {
                            let field_name = field_def.ident(tcx).to_string();
                            let field_mir_ty = field_def.ty(tcx, substs).skip_norm_wip();
                            let field_oomir_type =
                                ty_to_oomir_type(field_mir_ty, tcx, data_types, instance_context);
                            (field_name, field_oomir_type)
                        })
                        .collect::<Vec<_>>();
                    let mut methods = HashMap::new();
                    methods.insert(
                        "eq".to_string(),
                        DataTypeMethod::AdtHelperMethod {
                            kind: oomir::AdtHelperKind::PartialEqClass {
                                fields: oomir_fields.clone(),
                            },
                        },
                    );
                    data_types.insert(
                        jvm_name_full.clone(),
                        oomir::DataType::Class {
                            fields: oomir_fields,
                            is_abstract: false,
                            methods,
                            super_class: None,
                            interfaces: vec![],
                        },
                    );
                } else if adt_def.is_enum() {
                    // the enum in general
                    if !data_types.contains_key(&jvm_name_full) {
                        let mut methods = HashMap::new();
                        methods.insert(
                            "getVariantIdx".to_string(),
                            DataTypeMethod::SimpleConstantReturn(oomir::Type::I32, None),
                        );
                        data_types.insert(
                            jvm_name_full.clone(),
                            oomir::DataType::Class {
                                fields: vec![], // No fields in the abstract class
                                is_abstract: true,
                                methods,
                                super_class: None,
                                interfaces: vec![],
                            },
                        );
                    }
                }
                oomir::Type::Class(jvm_name_full)
            }
        }
        rustc_middle::ty::TyKind::Str => oomir::Type::String,
        rustc_middle::ty::TyKind::Pat(inner_ty, _) => {
            ty_to_oomir_type(*inner_ty, tcx, data_types, instance_context)
        }
        rustc_middle::ty::TyKind::Ref(_, inner_ty, mutability) => {
            let pointee_oomir_type = ty_to_oomir_type(*inner_ty, tcx, data_types, instance_context);
            // For trait objects (&dyn Trait, &mut dyn Trait), represent as direct Interface
            // rather than using the array wrapper, since we call virtual methods on the object
            if matches!(inner_ty.kind(), rustc_middle::ty::TyKind::Dynamic(_, _)) {
                pointee_oomir_type
            } else if mutability.is_mut() {
                oomir::Type::MutableReference(Box::new(pointee_oomir_type))
            } else {
                pointee_oomir_type
            }
        }
        rustc_middle::ty::TyKind::RawPtr(ty, _mutability) => {
            if ty.is_str() {
                // A raw pointer to a string slice (*const str) is semantically a reference
                // to string data. Its OOMIR representation should be consistent with &str.
                oomir::Type::String
            } else if ty.is_slice() {
                // A raw pointer to a slice (*const [T]) should be represented as an array of T.
                let component_ty = ty.sequence_element_type(tcx);
                let oomir_component_type =
                    ty_to_oomir_type(component_ty, tcx, data_types, instance_context);
                oomir::Type::Array(Box::new(oomir_component_type))
            } else {
                // For a pointer to a sized type (*const T), use the mutable reference
                // "array hack" to represent it as a reference that can be written back to.
                let oomir_pointee_type = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
                if matches!(oomir_pointee_type, oomir::Type::Void) {
                    oomir::Type::Class("java/lang/Object".to_string())
                } else {
                    oomir::Type::MutableReference(Box::new(oomir_pointee_type))
                }
            }
        }
        rustc_middle::ty::TyKind::Array(component_ty, _) => {
            // Special case for arrays of string references
            if let TyKind::Ref(_, inner_ty, _) = component_ty.kind() {
                if inner_ty.is_str() {
                    return oomir::Type::Array(Box::new(oomir::Type::String));
                }
            }
            // Default array handling
            oomir::Type::Array(Box::new(ty_to_oomir_type(
                *component_ty,
                tcx,
                data_types,
                instance_context,
            )))
        }
        rustc_middle::ty::TyKind::Tuple(tuple_elements) => {
            // Handle the unit type () -> Void
            if tuple_elements.is_empty() {
                return oomir::Type::Void;
            }

            // Handle non-empty tuples -> generate a class
            let element_mir_tys: Vec<Ty<'tcx>> = tuple_elements.iter().collect(); // Collect MIR types

            // Generate the JVM class name for this specific tuple type
            let tuple_class_name =
                generate_tuple_jvm_class_name(&element_mir_tys, tcx, data_types, instance_context);

            // Check if we've already created the DataType for this tuple signature
            if !data_types.contains_key(&tuple_class_name) {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!(
                        "Info: Defining new tuple type class: {} for MIR type {:?}",
                        tuple_class_name, ty
                    )
                );
                // Create the fields ("field0", "field1", ...) and their OOMIR types
                let oomir_fields = element_mir_tys
                    .iter()
                    .enumerate()
                    .map(|(i, &elem_ty)| {
                        let field_name = format!("field{}", i);
                        // Recursively convert element type to OOMIR type
                        let field_oomir_type =
                            ty_to_oomir_type(elem_ty, tcx, data_types, instance_context);
                        (field_name, field_oomir_type)
                    })
                    .collect::<Vec<_>>();

                let mut methods = HashMap::new();
                methods.insert(
                    "eq".to_string(),
                    DataTypeMethod::AdtHelperMethod {
                        kind: oomir::AdtHelperKind::PartialEqClass {
                            fields: oomir_fields.clone(),
                        },
                    },
                );

                // Create and insert the DataType definition
                let tuple_data_type = oomir::DataType::Class {
                    fields: oomir_fields,
                    is_abstract: false,
                    methods,
                    super_class: None,
                    interfaces: vec![],
                };
                data_types.insert(tuple_class_name.clone(), tuple_data_type);
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!("   -> Added DataType: {:?}", data_types[&tuple_class_name])
                );
            } else {
                if let Some(oomir::DataType::Class {
                    fields, methods, ..
                }) = data_types.get_mut(&tuple_class_name)
                {
                    methods.entry("eq".to_string()).or_insert_with(|| {
                        DataTypeMethod::AdtHelperMethod {
                            kind: oomir::AdtHelperKind::PartialEqClass {
                                fields: fields.clone(),
                            },
                        }
                    });
                }
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!(
                        "Info: Reusing existing tuple type class: {}",
                        tuple_class_name
                    )
                );
            }

            // Return the OOMIR type as a Class reference
            oomir::Type::Class(tuple_class_name)
        }
        rustc_middle::ty::TyKind::Slice(component_ty) => {
            // Special case for slices of string references
            if let TyKind::Ref(_, inner_ty, _) = component_ty.kind() {
                if inner_ty.is_str() {
                    return oomir::Type::Array(Box::new(oomir::Type::String));
                }
            }
            // Default slice handling
            oomir::Type::Array(Box::new(ty_to_oomir_type(
                *component_ty,
                tcx,
                data_types,
                instance_context,
            )))
        }
        rustc_middle::ty::TyKind::Never => {
            // Handle the never type
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "type-mapping",
                "Info: Mapping Never type to OOMIR Void"
            );
            oomir::Type::Void
        }
        rustc_middle::ty::TyKind::Dynamic(bound_preds, _region) => {
            // bound_preds is a collection of `Binder<ExistentialPredicate<'tcx>>` entries.
            // Iterate and resolve trait predicates into OOMIR interface types.
            let mut resolved_types: Vec<oomir::Type> = Vec::new();
            for binder in bound_preds.iter() {
                match binder.skip_binder() {
                    ExistentialPredicate::Trait(trait_ref) => {
                        let trait_name = tcx.def_path_str(trait_ref.def_id);
                        let safe_name = make_jvm_safe(&trait_name);
                        resolved_types.push(oomir::Type::Interface(safe_name));
                    }
                    ExistentialPredicate::AutoTrait(def_id) => {
                        // Auto traits like Send/Sync — treat as interfaces as well.
                        let trait_name = tcx.def_path_str(def_id);
                        let safe_name = make_jvm_safe(&trait_name);
                        resolved_types.push(oomir::Type::Interface(safe_name));
                    }
                    ExistentialPredicate::Projection(_) => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "type-mapping",
                            format!(
                                "Warning: Unhandled dynamic projection predicate {:?}",
                                binder
                            )
                        );
                        resolved_types.push(oomir::Type::Class("java/lang/Object".to_string()));
                    }
                    _ => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "type-mapping",
                            format!("Warning: Unhandled dynamic type {:?}", binder)
                        );
                        resolved_types.push(oomir::Type::Class("java/lang/Object".to_string()));
                    }
                }
            }
            // Return the first resolved bound, or fall back to Object.
            resolved_types
                .get(0)
                .cloned()
                .unwrap_or(oomir::Type::Class("java/lang/Object".to_string()))
        }
        rustc_middle::ty::TyKind::Param(param_ty) => {
            oomir::Type::Class(make_jvm_safe(param_ty.name.as_str()))
        }
        rustc_middle::ty::TyKind::Closure(def_id, args) => {
            let full_path_str = tcx.def_path_str(*def_id);
            let safe_name = make_jvm_safe(&full_path_str);

            // Define the closure class struct if not already present
            if !data_types.contains_key(&safe_name) {
                let closure_args = args.as_closure();
                let upvar_tys = closure_args.upvar_tys();

                let mut fields = Vec::new();
                for (i, upvar_ty) in upvar_tys.iter().enumerate() {
                    let field_name = format!("arg{}", i);
                    // Recursively resolve capture types
                    let field_oomir_ty =
                        ty_to_oomir_type(upvar_ty, tcx, data_types, instance_context);
                    fields.push((field_name, field_oomir_ty));
                }

                data_types.insert(
                    safe_name.clone(),
                    oomir::DataType::Class {
                        fields,
                        is_abstract: false,
                        methods: HashMap::new(), // 'call' is handled via MIR lowering logic
                        super_class: Some("java/lang/Object".to_string()),
                        interfaces: vec![],
                    },
                );
            }
            oomir::Type::Class(safe_name)
        }
        rustc_middle::ty::TyKind::FnDef(def_id, _args) => {
            // Named functions are Zero-Sized Types (ZSTs).
            // We generate a singleton class so generics like Map<Iter, MyFunc>
            // produce unique JVM class names.
            let full_path_str = tcx.def_path_str(*def_id);
            let safe_name = make_jvm_safe(&full_path_str);

            if !data_types.contains_key(&safe_name) {
                data_types.insert(
                    safe_name.clone(),
                    oomir::DataType::Class {
                        fields: vec![], // No state
                        is_abstract: false,
                        methods: HashMap::new(),
                        super_class: Some("java/lang/Object".to_string()),
                        interfaces: vec![],
                    },
                );
            }
            oomir::Type::Class(safe_name)
        }
        rustc_middle::ty::TyKind::Alias(_alias_ty) => {
            // This handles associated types and projections (like Self::Output)
            // that couldn't be normalized because we are in a generic context.
            // We map them to java/lang/Object, effectively erasing them.
            oomir::Type::Class("java/lang/Object".to_string())
        }
        _ => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!("Warning: Unhandled type {:?}", ty)
            );
            oomir::Type::Class("java/lang/Object".to_string())
        }
    }
}

/// Generates a short hash of the input string.
/// The hash is truncated to the specified length to ensure it fits within JVM class name constraints.
pub fn short_hash(input: &str, length: usize) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(input);
    let full_hash = format!("{:x}", hasher.finalize());
    full_hash[..length].to_string()
}

// Maximum length for a readable tuple name (including the "Tuple_" prefix).
// If the human-readable name exceeds this, we fall back to the hashed name.
const MAX_TUPLE_NAME_LEN: usize = 64;

// Produce a compact, human readable token for an OOMIR type to use in tuple class names.
pub fn readable_oomir_type_name(t: &oomir::Type) -> String {
    use oomir::Type;
    match t {
        Type::Boolean => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::String => "String".to_string(),
        Type::Void => "Void".to_string(),
        Type::Class(name) => {
            // take last path segment for readability (e.g. java/lang/String -> String)
            name.rsplit('/').next().unwrap_or(name).to_string()
        }
        Type::Array(inner) => format!("{}Array", readable_oomir_type_name(inner)),
        Type::MutableReference(inner) => format!("Ref{}", readable_oomir_type_name(inner)),
        Type::Interface(name) => {
            // prefix interfaces with I to avoid conflicts with classes
            let seg = name.rsplit('/').next().unwrap_or(name);
            format!("I{}", seg)
        }
        // Fallback for any other variants
        _ => "Unsupported".to_string(),
    }
}

// Sanitize token so it contains only ASCII alphanumeric characters and underscores.
pub fn sanitize_name_token(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

/// Generate a JVM-safe class name for an ADT (struct/enum) including readable generic
/// substitution tokens. Falls back to a hashed last segment when the full name is too long.
pub fn generate_adt_jvm_class_name<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    let full_path_str = tcx.def_path_str(adt_def.did());
    let safe_name = make_jvm_safe(&full_path_str);
    let base_jvm = safe_name.replace("::", "/").replace('.', "/");

    // Build readable generic tokens from substitutions (if any)
    let mut generic_tokens: Vec<String> = Vec::new();
    for arg in substs.iter() {
        if let Some(arg_ty) = arg.as_type() {
            let oomir_ty = ty_to_oomir_type(arg_ty, tcx, data_types, instance_context);
            let token = readable_oomir_type_name(&oomir_ty);
            generic_tokens.push(sanitize_name_token(&token));
        } else {
            generic_tokens.push("_".to_string());
        }
    }

    // Attach generics to the last path segment for readability
    let (prefix, last_segment) = match base_jvm.rsplit_once('/') {
        Some((p, l)) => (p.to_string(), l.to_string()),
        None => ("".to_string(), base_jvm.clone()),
    };

    let mut last_with_gens = last_segment.clone();
    if !generic_tokens.is_empty() {
        last_with_gens = format!("{}_{}", last_segment, generic_tokens.join("_"));
    }

    let mut jvm_name_full = if prefix.is_empty() {
        last_with_gens.clone()
    } else {
        format!("{}/{}", prefix, last_with_gens)
    };

    // If the name is too long, fall back to hashed form
    if jvm_name_full.len() > MAX_TUPLE_NAME_LEN {
        let mut name_parts = String::new();
        name_parts.push_str(&base_jvm);
        name_parts.push_str("_");
        for arg in substs.iter() {
            if let Some(arg_ty) = arg.as_type() {
                let oomir_ty = ty_to_oomir_type(arg_ty, tcx, data_types, instance_context);
                name_parts.push_str(&oomir_ty.to_jvm_descriptor());
                name_parts.push_str("_");
            }
        }
        let hash = short_hash(&name_parts, 10);
        let hashed_last = format!("{}_{}", last_segment, hash);
        jvm_name_full = if prefix.is_empty() {
            hashed_last
        } else {
            format!("{}/{}", prefix, hashed_last)
        };
    }

    jvm_name_full
}

/// Generates a JVM class name for a tuple type based on its element types.
/// The name is derived from the hash of the element types to ensure uniqueness.
/// The hash is truncated to a specified length to avoid excessively long names.
pub fn generate_tuple_jvm_class_name<'tcx>(
    element_tys: &[Ty<'tcx>],
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>, // Needed for recursive calls
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    // First attempt: build a human-readable name like `Tuple_i32_String`
    let mut tokens: Vec<String> = Vec::new();
    for ty in element_tys {
        let oomir_ty = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
        let token = readable_oomir_type_name(&oomir_ty);
        tokens.push(sanitize_name_token(&token));
    }

    let readable_name = format!("Tuple_{}", tokens.join("_"));

    // If the readable name is within bounds, use it. Otherwise fall back to a hash
    if readable_name.len() <= MAX_TUPLE_NAME_LEN {
        readable_name
    } else {
        // build hashed name using JVM descriptors (previous behaviour)
        let mut name_parts = String::new();
        for ty in element_tys {
            // convert to oomir type
            let ty = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
            name_parts.push_str(&ty.to_jvm_descriptor());
            name_parts.push_str("_");
        }
        // with a length of 10, the chance of a collision is tiny
        let hash = short_hash(&name_parts, 10);
        format!("Tuple_{}", hash)
    }
}

// A helper function to convert MIR integer values to OOMIR Constants, respecting type
pub fn mir_int_to_oomir_const<'tcx>(
    value: u128,
    ty: Ty<'tcx>,
    _tcx: TyCtxt<'tcx>,
) -> oomir::Constant {
    match ty.kind() {
        TyKind::Int(int_ty) => match int_ty {
            // Cast u128 carefully to avoid panic/wrap-around if value is out of range
            IntTy::I8 => oomir::Constant::I8(value as i8),
            IntTy::I16 => oomir::Constant::I16(value as i16),
            IntTy::I32 => oomir::Constant::I32(value as i32),
            IntTy::I64 => oomir::Constant::I64(value as i64),
            IntTy::Isize => oomir::Constant::I32(value as i32), // JVM uses i32 for most "usize" tasks
            IntTy::I128 => oomir::Constant::Instance {
                class_name: "java/math/BigInteger".to_string(),
                params: vec![oomir::Constant::String(value.to_string())],
                fields: HashMap::new(),
            }, // Handle large integers
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            // JVM uses signed types, treat appropriately
            // maps to the next size up to capture full range
            UintTy::U8 => oomir::Constant::I16(value as i16),
            UintTy::U16 => oomir::Constant::I32(value as i32),
            UintTy::U32 => oomir::Constant::I64(value as i64),
            UintTy::Usize => oomir::Constant::I32(value as i32), // java uses an i32 for most "usize" tasks i.e. array indexes etc.
            UintTy::U64 | UintTy::U128 => oomir::Constant::Instance {
                class_name: "java/math/BigInteger".to_string(),
                params: vec![oomir::Constant::String(value.to_string())],
                fields: HashMap::new(),
            },
        },
        TyKind::Bool => oomir::Constant::Boolean(value != 0), // 0 is false, non-zero is true
        TyKind::Char => {
            // u128 -> u32 -> char
            oomir::Constant::Char(char::from_u32(value as u32).unwrap_or('\0')) // Handle potential invalid char
        }
        _ => {
            // This case should ideally not happen if MIR is well-typed
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Warning: Cannot convert MIR integer value {} to OOMIR constant for non-integer type {:?}",
                    value, ty
                )
            );
            oomir::Constant::I32(0) // Default fallback
        }
    }
}

// Helper to get field name from index using DataType info
pub fn get_field_name_from_index(
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
