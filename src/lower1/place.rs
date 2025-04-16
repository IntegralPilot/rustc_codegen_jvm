use super::{
    operand::convert_operand,
    types::{get_field_name_from_index, ty_to_oomir_type},
};
use crate::oomir::{self, Instruction, Operand};

use regex::Regex;
use rustc_middle::{
    mir::{Body, Operand as MirOperand, Place, ProjectionElem},
    ty::{Ty, TyCtxt},
};
use std::{collections::HashMap, sync::OnceLock};

pub fn place_to_string<'tcx>(place: &Place<'tcx>, _tcx: TyCtxt<'tcx>) -> String {
    // Base variable name (e.g., "_1")
    let name = format!("_{}", place.local.index()); // Start with base local "_N"

    // Append projections cleanly
    /*for proj_elem in place.projection.iter() {
        match proj_elem {
            rustc_middle::mir::ProjectionElem::Field(field, _ty) => {
                // Append ".index" for field access
                name.push_str(&format!(".{}", field.index()));
            }
            rustc_middle::mir::ProjectionElem::Deref => {
                name = format!("deref_{}", name); // Or maybe just skip for simple cases?
            }
            rustc_middle::mir::ProjectionElem::Index(local) => {
                name.push_str(&format!(".idx_{}", local.index()));
            }
            _ => {
                name.push_str(".?"); // Placeholder for others
            }
        }
    } */
    name // Return the simple, projected name (e.g., "_7.0")
}

// Make a rust idenfier (i.e. function name) JVM safe
// i.e. Arguments::<'_>::new_const::<1> -> arguments_new_const
// and <String as PartialEq<&str>>::eq -> eq
pub fn make_jvm_safe(input: &str) -> String {
    static RE_ANGLES: OnceLock<Regex> = OnceLock::new();
    static RE_COLONS: OnceLock<Regex> = OnceLock::new();
    static RE_TRAILING_UNDERSCORE: OnceLock<Regex> = OnceLock::new();
    // 1. Remove anything inside < > (including the angle brackets themselves),
    // but keep the inner thing if it is a Rust primitive type.
    let re_angles = RE_ANGLES.get_or_init(|| Regex::new(r"<([^>]+)>").expect("Invalid regex"));
    let without_angles = re_angles.replace_all(input, |caps: &regex::Captures| {
        let inner = &caps[1];
        match inner {
            "i8" | "i16" | "i32" | "i64" | "i128" | "u8" | "u16" | "u32" | "u64" | "u128"
            | "f32" | "f64" | "bool" | "char" | "str" => inner.to_string(),
            _ => "".to_string(),
        }
    });

    // 2. Replace occurrences of :: (or more consecutive ':') with a single underscore
    let re_colons = RE_COLONS.get_or_init(|| Regex::new(r":{2,}").expect("Invalid regex"));
    let replaced: std::borrow::Cow<'_, str> = re_colons.replace_all(&without_angles, "_");

    // 3. ensure there are no starting or trailing > or < or _
    let re_trailing_underscore = RE_TRAILING_UNDERSCORE
        .get_or_init(|| Regex::new(r"^[<>_]+|[<>_]+$").expect("Invalid regex"));
    let replaced = re_trailing_underscore.replace_all(&replaced, "");

    // 4. Convert all to lowercase
    let lower = replaced.to_lowercase();

    // 5. if the first or last character is _, rm it
    let trimmed = lower.trim_start_matches('_').trim_end_matches('_');

    trimmed.to_string()
}

// --- Helper to get OOMIR Type for a Place ---
pub fn get_place_type<'tcx>(
    place: &Place<'tcx>,
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Type {
    let place_ty = place.ty(&mir.local_decls, tcx);
    ty_to_oomir_type(place_ty.ty, tcx, data_types)
}

enum PlaceDecomposition<'tcx> {
    Field {
        base_place: Place<'tcx>,
        field_index: rustc_abi::FieldIdx,
        field_mir_ty: Ty<'tcx>,
    },
    Index {
        base_place: Place<'tcx>,
        index_operand: MirOperand<'tcx>, // Store the original MIR Operand for the index
    },
    ConstantIndex {
        base_place: Place<'tcx>,
        offset: u64,
        _min_length: u64, // Keep for potential future use (e.g., checks). Might make them later.
        from_end: bool,
    },
    Deref {
        base_place: Place<'tcx>, // The Place being dereferenced (e.g., *_1)
    },
    // Add other projections like Downcast if needed
    Simple(Place<'tcx>), // No projection, just the base local
}

fn decompose_place<'tcx>(
    tcx: TyCtxt<'tcx>, // Pass tcx
    place: &Place<'tcx>,
) -> PlaceDecomposition<'tcx> {
    if let Some(proj_elem) = place.projection.last() {
        // Create the base place (without the last projection)
        let base_projection = &place.projection[..place.projection.len() - 1];
        let base_place = Place {
            local: place.local,
            projection: tcx.mk_place_elems(base_projection),
        };

        match proj_elem {
            ProjectionElem::ConstantIndex {
                offset,
                min_length,
                from_end,
            } => {
                PlaceDecomposition::ConstantIndex {
                    base_place,
                    offset: *offset,
                    _min_length: *min_length, // Store it even if not immediately used
                    from_end: *from_end,
                }
            }
            ProjectionElem::Field(field_index, field_ty) => PlaceDecomposition::Field {
                base_place,
                field_index: *field_index,
                field_mir_ty: *field_ty,
            },
            ProjectionElem::Index(index_local) => {
                // The index is a local variable in MIR. Create a MIR Operand for it.
                let index_operand = MirOperand::Copy(Place::from(*index_local)); // Or Move? Copy is safer.
                PlaceDecomposition::Index {
                    base_place,
                    index_operand,
                }
            }
            ProjectionElem::Deref => PlaceDecomposition::Deref { base_place },
            // TODO: Handle other projections like Downcast, ConstantIndex etc.
            _ => {
                println!(
                    "Warning: Unhandled projection element in decompose_place: {:?}. Treating as Simple.",
                    proj_elem
                );
                PlaceDecomposition::Simple(*place) // Fallback for unhandled projections
            }
        }
    } else {
        // No projections, it's just the base local variable
        PlaceDecomposition::Simple(*place)
    }
}

// Helper to return the instructions you need to get the value of a place (incl. struct/tuple fields, array indexes, length)
// and then the string variable name you can use to access the value of it after running the instructions
pub fn emit_instructions_to_get_on_own<'tcx>(
    place: &Place<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> (String, Vec<Instruction>, oomir::Type) {
    let mut instructions = Vec::new();
    let result_oomir_type: oomir::Type;
    let result_var_name: String;

    match decompose_place(tcx, place) {
        PlaceDecomposition::Field {
            base_place,
            field_index,
            field_mir_ty,
        } => {
            // It's a field access (e.g., _7.1)! Generate GetField
            let base_var_name = place_to_string(&base_place, tcx); // e.g., "_7"
            // Create a unique temporary name for the result
            result_var_name = format!("{}_{}", base_var_name, field_index.index());
            let owner_oomir_type = get_place_type(&base_place, mir, tcx, data_types);
            let owner_class_name = match &owner_oomir_type {
                oomir::Type::Class(name) => name.clone(),
                // Dereference if the base is a reference to a class (common case)
                oomir::Type::Reference(inner)
                    if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                {
                    if let oomir::Type::Class(name) = inner.as_ref() {
                        name.clone()
                    } else {
                        unreachable!()
                    } // Already checked by matches!
                }
                _ => panic!(
                    "Field source base '{}' is not a class or reference-to-class type: {:?}",
                    base_var_name, owner_oomir_type
                ),
            };

            let field_name =
                match get_field_name_from_index(&owner_class_name, field_index.index(), data_types)
                {
                    Ok(name) => name,
                    Err(e) => panic!("Error getting field name for GetField: {}", e),
                };

            result_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);

            instructions.push(oomir::Instruction::GetField {
                dest: result_var_name.clone(), // Store result in the temporary variable
                object_var: base_var_name,     // Get from the base variable
                field_name,
                field_ty: result_oomir_type.clone(),
                owner_class: owner_class_name,
            });
        }
        PlaceDecomposition::Index {
            base_place,
            index_operand: mir_index_operand,
        } => {
            // It's an index access (e.g., _7[idx])! Generate ArrayGet
            let base_var_name = place_to_string(&base_place, tcx); // e.g., "_7"

            // Convert the MIR index operand to OOMIR operand
            // Need to recursively call conversion logic or handle it here
            let oomir_index_operand = super::operand::convert_operand(
                // Use the fully qualified path
                &mir_index_operand,
                tcx,
                mir,
                data_types,
            );

            // Create a unique temporary name (less predictable than before, maybe just use place string?)
            // Using the full place string might be safer if it includes index info implicitly
            // result_var_name = format!("{}_idx_{:?}", base_var_name, oomir_index_operand); // Less ideal name
            result_var_name = format!("{}_elem", place_to_string(place, tcx)); // Maybe like _7_idx_1_elem

            let array_oomir_type = get_place_type(&base_place, mir, tcx, data_types);
            result_oomir_type = match array_oomir_type {
                oomir::Type::Array(inner) => inner.as_ref().clone(),
                // Handle if base is &Array
                oomir::Type::Reference(inner)
                    if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                {
                    if let oomir::Type::Array(element_type) = inner.as_ref() {
                        element_type.as_ref().clone()
                    } else {
                        unreachable!()
                    }
                }
                _ => panic!(
                    "Index source base '{}' is not an array or reference-to-array type: {:?}",
                    base_var_name, array_oomir_type
                ),
            };

            instructions.push(oomir::Instruction::ArrayGet {
                dest: result_var_name.clone(), // Store result in temporary
                array_var: base_var_name,      // Get from the base variable
                index: oomir_index_operand,    // Use the converted OOMIR index operand
            });
        }
        PlaceDecomposition::ConstantIndex {
            base_place,
            offset,
            _min_length: _,
            from_end,
        } => {
            // It's a constant index access (e.g., slice[2] or slice[len-1])
            let base_var_name = place_to_string(&base_place, tcx); // e.g., "_7"
            result_var_name = format!("{}_elem", place_to_string(place, tcx)); // Similar naming to variable index

            // Determine the element type from the base place (array/slice)
            let array_oomir_type = get_place_type(&base_place, mir, tcx, data_types);
            result_oomir_type = match &array_oomir_type {
                oomir::Type::Array(inner) => inner.as_ref().clone(),
                oomir::Type::Reference(inner)
                    if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                {
                    if let oomir::Type::Array(element_type) = inner.as_ref() {
                        element_type.as_ref().clone()
                    } else {
                        unreachable!()
                    }
                }
                _ => panic!(
                    "ConstantIndex source base '{}' is not an array or reference-to-array type: {:?}",
                    base_var_name, array_oomir_type
                ),
            };

            let index_operand: Operand;

            if !from_end {
                // Index is simply the offset from the start
                index_operand = Operand::Constant(oomir::Constant::I32(offset as i32));
                instructions.push(oomir::Instruction::ArrayGet {
                    dest: result_var_name.clone(),
                    array_var: base_var_name,
                    index: index_operand,
                });
            } else {
                // Index is length - offset
                // 1. Get the length of the array/slice
                let len_var_name = format!("{}_len", base_var_name);
                instructions.push(oomir::Instruction::Length {
                    dest: len_var_name.clone(),
                    array_var: base_var_name.clone(),
                });

                // 2. Calculate the index: length - offset
                let index_var_name = format!("{}_calc_idx", base_var_name);
                let offset_operand = Operand::Constant(oomir::Constant::I32(offset as i32));
                instructions.push(oomir::Instruction::Sub {
                    dest: index_var_name.clone(),
                    op1: Operand::Variable {
                        name: len_var_name,
                        ty: oomir::Type::I32,
                    },
                    op2: offset_operand,
                });

                // 3. Use the calculated index variable for ArrayGet
                index_operand = Operand::Variable {
                    name: index_var_name,
                    ty: oomir::Type::I32,
                };
                instructions.push(oomir::Instruction::ArrayGet {
                    dest: result_var_name.clone(),
                    array_var: base_var_name,
                    index: index_operand, // Use the variable holding the calculated index
                });
            }
            // Note: Ignoring min_length for now. It's primarily for UB checks
            // which will be caught by errors in lower2 or asm-processor or JVM verifier.
        }
        PlaceDecomposition::Simple(simple_place)
        | PlaceDecomposition::Deref {
            base_place: simple_place,
        } => {
            // It's a simple variable access (e.g., _7)! Just use the variable name directly.
            result_var_name = place_to_string(&simple_place, tcx); // e.g., "_7"
            result_oomir_type = get_place_type(&simple_place, mir, tcx, data_types);
            // No instructions needed just to name the variable.
        }
    }

    // Return the name to use for the result, the instructions *to generate* that result,
    // and the OOMIR type *of* that result.
    (result_var_name, instructions, result_oomir_type)
}

/// Generates OOMIR instructions to store the `source_operand` value into the `dest_place`.
///
/// This handles assignments to variables, fields (of structs/tuples), and array elements.
pub fn emit_instructions_to_set_value<'tcx>(
    dest_place: &Place<'tcx>,
    source_operand: Operand, // The OOMIR value to store (already calculated)
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> Vec<Instruction> {
    let mut instructions = Vec::new();

    match decompose_place(tcx, dest_place) {
        PlaceDecomposition::Simple(simple_place) => {
            // Assignment to a simple local variable: _dest = source
            let dest_var_name = place_to_string(&simple_place, tcx);
            instructions.push(Instruction::Move {
                dest: dest_var_name,
                src: source_operand,
            });
        }

        PlaceDecomposition::Field {
            base_place,
            field_index,
            field_mir_ty,
        } => {
            // Assignment to a field: base_obj.field = source
            let base_var_name = place_to_string(&base_place, tcx);
            let owner_oomir_type = get_place_type(&base_place, mir, tcx, data_types);

            // Determine the owner class name (similar logic to get_on_own)
            let owner_class_name = match &owner_oomir_type {
                oomir::Type::Class(name) => name.clone(),
                oomir::Type::Reference(inner)
                    if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                {
                    if let oomir::Type::Class(name) = inner.as_ref() {
                        name.clone()
                    } else {
                        unreachable!()
                    }
                }
                _ => panic!(
                    "SetField source base '{}' is not a class or reference-to-class type: {:?}",
                    base_var_name, owner_oomir_type
                ),
            };

            let field_name =
                match get_field_name_from_index(&owner_class_name, field_index.index(), data_types)
                {
                    Ok(name) => name,
                    Err(e) => panic!("Error getting field name for SetField: {}", e),
                };

            let field_ty = ty_to_oomir_type(field_mir_ty, tcx, data_types);

            instructions.push(Instruction::SetField {
                object_var: base_var_name,
                field_name,
                field_ty,
                value: source_operand,
                owner_class: owner_class_name,
            });
        }

        PlaceDecomposition::Index {
            base_place,
            index_operand: mir_index_operand,
        } => {
            // Assignment to an array element (variable index): array[index] = source
            let array_var_name = place_to_string(&base_place, tcx);

            // Convert the MIR index operand to an OOMIR operand
            let oomir_index_operand = convert_operand(&mir_index_operand, tcx, mir, data_types);

            instructions.push(Instruction::ArrayStore {
                array_var: array_var_name,
                index: oomir_index_operand,
                value: source_operand,
            });
        }

        PlaceDecomposition::ConstantIndex {
            base_place,
            offset,
            _min_length: _,
            from_end,
        } => {
            // Assignment to an array element (constant index): array[const_index] = source
            let array_var_name = place_to_string(&base_place, tcx);
            let index_operand: Operand;

            if !from_end {
                // Simple constant index from the start
                index_operand = Operand::Constant(oomir::Constant::I32(offset as i32));
                // No extra instructions needed to calculate the index itself
            } else {
                // Index is calculated as length - offset
                // 1. Get length
                let len_var_name = format!("{}_len_set", array_var_name); // Temp name for length
                instructions.push(Instruction::Length {
                    dest: len_var_name.clone(),
                    array_var: array_var_name.clone(),
                });
                // 2. Calculate index
                let index_var_name = format!("{}_calc_idx_set", array_var_name); // Temp name for index
                let offset_op = Operand::Constant(oomir::Constant::I32(offset as i32));
                instructions.push(Instruction::Sub {
                    dest: index_var_name.clone(),
                    op1: Operand::Variable {
                        name: len_var_name,
                        ty: oomir::Type::I32,
                    },
                    op2: offset_op,
                });
                // 3. Use the calculated index variable
                index_operand = Operand::Variable {
                    name: index_var_name,
                    ty: oomir::Type::I32,
                };
            }

            instructions.push(Instruction::ArrayStore {
                array_var: array_var_name, // The array to store into
                index: index_operand,      // The constant or calculated index
                value: source_operand,     // The value to store
            });
        }

        PlaceDecomposition::Deref { .. } => {
            panic!("Unhandled assignment via Deref");
            // TODO: Handle assignment via Deref. To my understanding, this is incredibly uncommon in generated MIR.
        }
    }

    instructions
}
