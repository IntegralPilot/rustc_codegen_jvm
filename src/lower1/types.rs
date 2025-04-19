use super::place::make_jvm_safe;
use crate::oomir;

use rustc_middle::ty::{FloatTy, IntTy, Ty, TyCtxt, TyKind, UintTy};
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
                        oomir::DataType {
                            fields: oomir_fields,
                            is_abstract: false,
                            methods: HashMap::new(),
                            super_class: None,
                        },
                    );
                } else if adt_def.is_enum() {
                    // the enum in general
                    if !data_types.contains_key(&jvm_name) {
                        let mut methods = HashMap::new();
                        methods.insert("getVariantIdx".to_string(), (oomir::Type::I32, None));
                        data_types.insert(
                            jvm_name.clone(),
                            oomir::DataType {
                                fields: vec![], // No fields in the abstract class
                                is_abstract: true,
                                methods,
                                super_class: None,
                            },
                        );
                    }
                }
                oomir::Type::Class(jvm_name)
            }
        }
        rustc_middle::ty::TyKind::Str => oomir::Type::String,
        rustc_middle::ty::TyKind::Ref(_, inner_ty, _) => {
            ty_to_oomir_type(*inner_ty, tcx, data_types)
        }
        rustc_middle::ty::TyKind::RawPtr(inner_ty, _) => {
            println!("Info: Mapping RawPtr({:?}) to OOMIR I64", inner_ty); // Add logging
            oomir::Type::I64 // Represent raw pointers as integer addresses
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
                println!(
                    "Info: Defining new tuple type class: {} for MIR type {:?}",
                    tuple_class_name, ty
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
                let tuple_data_type = oomir::DataType {
                    fields: oomir_fields,
                    is_abstract: false,
                    methods: HashMap::new(),
                    super_class: None,
                };
                data_types.insert(tuple_class_name.clone(), tuple_data_type);
                println!("   -> Added DataType: {:?}", data_types[&tuple_class_name]);
            } else {
                println!(
                    "Info: Reusing existing tuple type class: {}",
                    tuple_class_name
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
        _ => {
            println!("Warning: Unhandled type {:?}", ty);
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
            println!(
                "Warning: Cannot convert MIR integer value {} to OOMIR constant for non-integer type {:?}",
                value, ty
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
        .and_then(|data_type| {
            data_type
                .fields
                .get(index)
                .ok_or_else(|| {
                    format!(
                        "Field index {} out of bounds for class '{}' (has {} fields)",
                        index,
                        owner_class_name,
                        data_type.fields.len()
                    )
                })
                .map(|(name, _)| name.clone())
        })
}
