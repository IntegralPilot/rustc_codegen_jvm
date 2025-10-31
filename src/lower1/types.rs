use super::place::make_jvm_safe;
use crate::oomir::{self, DataType, DataTypeMethod};

use rustc_middle::ty::{ExistentialPredicate, FloatTy, IntTy, Ty, TyCtxt, TyKind, UintTy};
use sha2::Digest;
use std::collections::HashMap;

/// Converts a Rust MIR type (`Ty`) to an OOMIR type (`oomir::Type`).
pub fn ty_to_oomir_type<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Type {
    match ty.kind() {
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
                let full_path_str = tcx.def_path_str(adt_def.did());
                let safe_name = make_jvm_safe(&full_path_str);
                let jvm_name = safe_name.replace("::", "/").replace('.', "/");
                if adt_def.is_struct() {
                    let variant = adt_def.variant(0usize.into());
                    let oomir_fields = variant
                        .fields
                        .iter()
                        .map(|field_def| {
                            let field_name = field_def.ident(tcx).to_string();
                            let field_mir_ty = field_def.ty(tcx, substs);
                            let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);
                            (field_name, field_oomir_type)
                        })
                        .collect::<Vec<_>>();
                    data_types.insert(
                        jvm_name.clone(),
                        oomir::DataType::Class {
                            fields: oomir_fields,
                            is_abstract: false,
                            methods: HashMap::new(),
                            super_class: None,
                            interfaces: vec![],
                        },
                    );
                } else if adt_def.is_enum() {
                    // the enum in general
                    if !data_types.contains_key(&jvm_name) {
                        let mut methods = HashMap::new();
                        methods.insert(
                            "getVariantIdx".to_string(),
                            DataTypeMethod::SimpleConstantReturn(oomir::Type::I32, None),
                        );
                        data_types.insert(
                            jvm_name.clone(),
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
                oomir::Type::Class(jvm_name)
            }
        }
        rustc_middle::ty::TyKind::Str => oomir::Type::String,
        rustc_middle::ty::TyKind::Ref(_, inner_ty, mutability) => {
            let pointee_oomir_type = ty_to_oomir_type(*inner_ty, tcx, data_types);
            if mutability.is_mut() {
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
                let oomir_component_type = ty_to_oomir_type(component_ty, tcx, data_types);
                oomir::Type::Array(Box::new(oomir_component_type))
            } else {
                // For a pointer to a sized type (*const T), use the mutable reference
                // "array hack" to represent it as a reference that can be written back to.
                let oomir_pointee_type = ty_to_oomir_type(*ty, tcx, data_types);
                oomir::Type::MutableReference(Box::new(oomir_pointee_type))
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
            oomir::Type::Array(Box::new(ty_to_oomir_type(*component_ty, tcx, data_types)))
        }
        rustc_middle::ty::TyKind::Tuple(tuple_elements) => {
            // Handle the unit type () -> Void
            if tuple_elements.is_empty() {
                return oomir::Type::Void;
            }

            // Handle non-empty tuples -> generate a class
            let element_mir_tys: Vec<Ty<'tcx>> = tuple_elements.iter().collect(); // Collect MIR types

            // Generate the JVM class name for this specific tuple type
            let tuple_class_name = generate_tuple_jvm_class_name(&element_mir_tys, tcx, data_types);

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
                        let field_oomir_type = ty_to_oomir_type(elem_ty, tcx, data_types);
                        (field_name, field_oomir_type)
                    })
                    .collect::<Vec<_>>();

                // Create and insert the DataType definition
                let tuple_data_type = oomir::DataType::Class {
                    fields: oomir_fields,
                    is_abstract: false,
                    methods: HashMap::new(),
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
            oomir::Type::Array(Box::new(ty_to_oomir_type(*component_ty, tcx, data_types)))
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
        _ => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!("Warning: Unhandled type {:?}", ty)
            );
            oomir::Type::Class("UnsupportedType".to_string())
        }
    }
}

/// Generates a short hash of the input string.
/// The hash is truncated to the specified length to ensure it fits within JVM class name constraints.
fn short_hash(input: &str, length: usize) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(input);
    let full_hash = format!("{:x}", hasher.finalize());
    full_hash[..length].to_string()
}

/// Generates a JVM class name for a tuple type based on its element types.
/// The name is derived from the hash of the element types to ensure uniqueness.
/// The hash is truncated to a specified length to avoid excessively long names.
pub fn generate_tuple_jvm_class_name<'tcx>(
    element_tys: &[Ty<'tcx>],
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>, // Needed for recursive calls
) -> String {
    let mut name_parts = String::new();
    for ty in element_tys {
        // convert to oomir type
        let ty = ty_to_oomir_type(*ty, tcx, data_types);
        name_parts.push_str(&ty.to_jvm_descriptor());
        name_parts.push_str("_");
    }

    // now hash the name parts
    // with a length of 10, the chanxce of a collision is approx 1 in 1.1 trillion
    let hash = short_hash(&name_parts, 10);

    format!("Tuple_{}", hash)
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
