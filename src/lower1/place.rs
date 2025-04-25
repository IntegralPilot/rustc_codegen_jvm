use super::{
    operand::convert_operand,
    types::{get_field_name_from_index, ty_to_oomir_type},
};
use crate::oomir::{self, Instruction, Operand};
use regex::Regex;
use rustc_middle::{
    mir::{Body, Operand as MirOperand, Place, ProjectionElem},
    ty::{TyCtxt, TyKind},
};
use std::{collections::HashMap, sync::OnceLock};

pub fn place_to_string<'tcx>(place: &Place<'tcx>, _tcx: TyCtxt<'tcx>) -> String {
    // Base variable name (e.g., "_1")
    format!("_{}", place.local.index()) // Start with base local "_N"
}

/// 1) Remove nested generics completely.
fn strip_generics(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut depth = 0;
    for c in s.chars() {
        match c {
            '<' => depth += 1,
            '>' if depth > 0 => depth -= 1,
            c if depth == 0 => out.push(c),
            _ => {} // inside <…>, skip
        }
    }
    out
}

pub fn make_jvm_safe(input: &str) -> String {
    // 1) Strip out all <…> (including nested)
    let mut s = strip_generics(input);

    // 2) Remove any trailing "::"
    while s.ends_with("::") {
        s.truncate(s.len() - 2);
    }

    // 3) Split on "::", drop empty segments, then:
    //    - if we have at least two parts, join first+last with "_"
    //    - else if one part, just use that
    //    - else (shouldn't happen), fall back to the full string
    let parts: Vec<_> = s.split("::").filter(|p| !p.is_empty()).collect();
    let combined = if parts.len() >= 2 {
        format!("{}_{}", parts[0], parts[parts.len() - 1])
    } else if parts.len() == 1 {
        parts[0].to_string()
    } else {
        s.clone()
    };

    // 4) Collapse any run of non‑alphanumeric/underscore into a single "_"
    static RE_NONWORD: OnceLock<Regex> = OnceLock::new();
    let re_nw = RE_NONWORD.get_or_init(|| Regex::new(r"[^\w]+").unwrap());
    let cleaned = re_nw.replace_all(&combined, "_");

    // 5) Trim leading/trailing "_" and lowercase
    cleaned.trim_matches('_').to_lowercase()
}

/// Generates the necessary OOMIR instructions to retrieve the value corresponding
/// to a given Place that may have a nested projection chain.
///
/// This function iterates over the projection chain one by one and emits the instructions
/// to “drill down” into the nested field or array.
///
/// Returns a tuple: (final variable name, generated instructions, final OOMIR type)
pub fn emit_instructions_to_get_recursive<'tcx>(
    place: &Place<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> (String, Vec<Instruction>, oomir::Type) {
    // Start with the base local.
    let current_place = Place {
        local: place.local,
        projection: tcx.mk_place_elems(&[]),
    };
    let mut current_var = place_to_string(&current_place, tcx);
    let mut current_type = get_place_type(&current_place, mir, tcx, data_types);
    let mut instructions = vec![];

    // Iterate over each projection element in the order they appear.
    for (proj_index, proj) in place.projection.iter().enumerate() {
        let type_before_proj = current_type.clone();
        match proj {
            ProjectionElem::Field(field_index, field_ty) => {
                // Get the owner class name and field name.
                let owner_class_name = match &current_type {
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
                        "Field access on non-class type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };

                let field_name = match get_field_name_from_index(
                    &owner_class_name,
                    field_index.index(),
                    data_types,
                ) {
                    Ok(name) => name,
                    Err(e) => panic!("Error getting field name: {}", e),
                };

                // Create a temporary name for the result of this field access.
                let next_var = format!("{}_{}", current_var, field_index.index());
                let obj_type = current_type.clone();
                // Update the type to the field’s type.
                current_type = ty_to_oomir_type(field_ty, tcx, data_types);
                instructions.push(oomir::Instruction::GetField {
                    dest: next_var.clone(),
                    object: Operand::Variable {
                        name: current_var.clone(),
                        ty: obj_type,
                    },
                    field_name,
                    field_ty: current_type.clone(),
                    owner_class: owner_class_name,
                });
                // Update current variable.
                current_var = next_var;
            }
            ProjectionElem::Index(index_local) => {
                let type_before_proj = current_type.clone();
                // Convert the MIR index operand.
                let index_operand = convert_operand(
                    &MirOperand::Copy(Place::from(index_local)),
                    tcx,
                    mir,
                    data_types,
                    &mut instructions,
                );
                // Create a temporary name for the array element.
                let next_var = format!("{}_elem", current_var);
                // Determine element type from the current type (which should be an array or reference-to-array).
                current_type = match &current_type {
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
                        "Index access on non-array type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };
                instructions.push(oomir::Instruction::ArrayGet {
                    dest: next_var.clone(),
                    array: Operand::Variable {
                        name: current_var.clone(),
                        ty: type_before_proj,
                    },
                    index: index_operand,
                });
                current_var = next_var;
            }
            ProjectionElem::ConstantIndex {
                offset,
                min_length: _,
                from_end,
            } => {
                let type_before_proj = current_type.clone();
                let next_var = format!("{}_elem", current_var);
                // Determine element type based on current_type being an array or reference-to-array.
                current_type = match &current_type {
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
                        "Constant index access on non-array type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };

                if !from_end {
                    // Simple constant index: array[offset]
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: next_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                        index: Operand::Constant(oomir::Constant::I32(offset as i32)),
                    });
                } else {
                    // For access from the end: calculate length - offset.
                    let len_var = format!("{}_len", current_var);
                    instructions.push(oomir::Instruction::Length {
                        dest: len_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                    });
                    let calc_idx_var = format!("{}_calc_idx", current_var);
                    instructions.push(oomir::Instruction::Sub {
                        dest: calc_idx_var.clone(),
                        op1: Operand::Variable {
                            name: len_var,
                            ty: oomir::Type::I32,
                        },
                        op2: Operand::Constant(oomir::Constant::I32(offset as i32)),
                    });
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: next_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                        index: Operand::Variable {
                            name: calc_idx_var,
                            ty: oomir::Type::I32,
                        },
                    });
                }
                current_var = next_var;
            }
            ProjectionElem::Deref => {
                // do nothing
            }
            ProjectionElem::Downcast(_, variant_idx) => {
                // A downcast changes the *effective type* for subsequent projections.

                // 1. Get the base enum OOMIR class name from the type *before* the downcast.
                let base_enum_oomir_name = match &type_before_proj {
                    oomir::Type::Class(name) => name.clone(),
                    oomir::Type::Reference(inner)
                        if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                    {
                        // If it's a reference to the enum, operate on the enum type itself
                        if let oomir::Type::Class(name) = inner.as_ref() {
                            // We should ideally verify this name corresponds to an enum base class in data_types
                            name.clone()
                        } else {
                            unreachable!() // Caught by matches!
                        }
                    }
                    _ => panic!(
                        "Downcast applied to non-enum/non-ref-enum type: var '{}' has type {:?} before downcast. Place: {:?}",
                        current_var, type_before_proj, place
                    ),
                };

                // 2. Get the AdtDef of the enum to find the variant's actual name.
                //    We need the Rust Ty of the base enum *before* the downcast.
                let base_place_proj_slice = &place.projection[..proj_index]; // Projections *before* this downcast
                let base_place_for_downcast = Place {
                    local: place.local,
                    projection: tcx.mk_place_elems(base_place_proj_slice),
                };
                let base_rust_ty = base_place_for_downcast.ty(&mir.local_decls, tcx).ty;

                let (adt_def, substs) = match base_rust_ty.kind() {
                    TyKind::Adt(adt, s) => (*adt, s),
                    TyKind::Ref(_, ty, _) => match ty.kind() {
                        // Handle reference to enum
                        TyKind::Adt(adt, s) => (*adt, s),
                        _ => panic!(
                            "Downcast base is Ref to non-ADT: {:?} ({:?})",
                            base_rust_ty, place
                        ),
                    },
                    _ => panic!(
                        "Downcast base is not an ADT: {:?} ({:?})",
                        base_rust_ty, place
                    ),
                };

                if !adt_def.is_enum() {
                    panic!(
                        "Downcast applied to non-enum ADT: {:?} ({:?})",
                        adt_def, place
                    );
                }

                // 3. Get the specific variant definition using the index.
                let variant_def = adt_def.variant(variant_idx);

                // 4. Construct the OOMIR variant class name
                let variant_class_name = format!(
                    "{}${}",
                    base_enum_oomir_name, // Use OOMIR name already derived
                    make_jvm_safe(&variant_def.name.to_string())  // Use actual variant name
                );

                // 5. Update current_type directly to the OOMIR Class representing the variant.
                current_type = oomir::Type::Class(variant_class_name.clone());

                // Verify this class exists in data_types
                if !data_types.contains_key(&variant_class_name) {
                    println!(
                        "Info: Downcast resulted in variant class name '{}' which is not yet in data_types! Will insert it.",
                        variant_class_name
                    );
                    let mut fields = vec![];
                    for (i, field) in variant_def.fields.iter().enumerate() {
                        let field_name = format!("field{}", i);
                        let field_type = ty_to_oomir_type(field.ty(tcx, substs), tcx, data_types);
                        fields.push((field_name, field_type));
                    }

                    let mut methods = HashMap::new();
                    methods.insert(
                        "getVariantIdx".to_string(),
                        (
                            oomir::Type::I32,
                            Some(oomir::Constant::I32(variant_idx.as_u32() as i32)),
                        ),
                    );

                    data_types.insert(
                        variant_class_name.clone(),
                        oomir::DataType {
                            fields,
                            is_abstract: false,
                            methods,
                            super_class: Some(base_enum_oomir_name.clone()),
                        },
                    );
                }

                println!(
                    "Info: Handled Downcast: Variant {}({}), BaseEnum='{}', New Type (Variant Class): {:?}, Var: {}",
                    variant_def.name, // Use actual name from variant_def
                    variant_idx.index(),
                    base_enum_oomir_name,
                    current_type, // Should now be Class("complexenum$userdata")
                    current_var
                );
            }
            // Will add more projection kinds when needed.
            _ => {
                println!(
                    "Warning: Unhandled projection element in nested access: {:?}. Skipping.",
                    proj
                );
            }
        }
    }

    (current_var, instructions, current_type)
}

/// Helper to get the OOMIR type for a Place.
pub fn get_place_type<'tcx>(
    place: &Place<'tcx>,
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Type {
    let place_ty = place.ty(&mir.local_decls, tcx);
    ty_to_oomir_type(place_ty.ty, tcx, data_types)
}

/// Generates OOMIR instructions to “get” the value from a Place.
/// This function now supports nested projections by calling
/// `emit_instructions_to_get_recursive`.
pub fn emit_instructions_to_get_on_own<'tcx>(
    place: &Place<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> (String, Vec<Instruction>, oomir::Type) {
    // Delegate the recursive handling.
    emit_instructions_to_get_recursive(place, tcx, mir, data_types)
}

/// Generates OOMIR instructions to store the `source_operand` value into the `dest_place`.
///
/// This function handles assignments recursively. It first generates instructions
/// to get the object or array that contains the final field/element, and then
/// generates the appropriate SetField or ArrayStore instruction.
pub fn emit_instructions_to_set_value<'tcx>(
    dest_place: &Place<'tcx>,
    source_operand: Operand, // The OOMIR value to store
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> Vec<Instruction> {
    let mut instructions = Vec::new();

    if dest_place.projection.is_empty() {
        // --- Base Case: Assignment to a simple local variable ---
        // e.g., _1 = source_operand
        let dest_var_name = place_to_string(dest_place, tcx);
        instructions.push(Instruction::Move {
            dest: dest_var_name,
            src: source_operand,
        });
    } else {
        // --- Recursive Case: Assignment involves projections (fields/indices) ---

        // 1. Separate the destination into the base and the last projection element.
        let (last_projection, base_projection_elems) = dest_place.projection.split_last().unwrap(); // Safe because we checked is_empty()
        let base_place = Place {
            local: dest_place.local,
            projection: tcx.mk_place_elems(base_projection_elems),
        };

        // 2. Generate instructions to get the value of the *base* place.
        //    This base value is the object we'll call SetField on, or the array
        //    we'll call ArrayStore on.
        //    We use `get_on_own` which internally handles recursion if base_place itself is nested.
        let (base_var_name, get_base_instructions, base_oomir_type) =
            emit_instructions_to_get_on_own(&base_place, tcx, mir, data_types);
        instructions.extend(get_base_instructions); // Add instructions to get the base

        // 3. Generate the final store instruction based on the *last* projection.
        match last_projection {
            ProjectionElem::Field(field_index, field_mir_ty) => {
                // Target is a field: base_var_name.field = source_operand
                let owner_class_name = match &base_oomir_type {
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
                        "SetField target base '{}' (Place: {:?}) is not a class or reference-to-class type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                };

                let field_name = match get_field_name_from_index(
                    &owner_class_name,
                    field_index.index(),
                    data_types,
                ) {
                    Ok(name) => name,
                    Err(e) => panic!("Error getting field name for SetField: {}", e),
                };
                let field_ty = ty_to_oomir_type(*field_mir_ty, tcx, data_types);

                instructions.push(Instruction::SetField {
                    object: base_var_name, // The object/struct retrieved in step 2
                    field_name,
                    field_ty,
                    value: source_operand, // The value we want to store
                    owner_class: owner_class_name,
                });
            }

            ProjectionElem::Index(index_local) => {
                // Target is an array element: base_var_name[index] = source_operand
                // Ensure the base is actually an array or ref-to-array
                match &base_oomir_type {
                    oomir::Type::Array(_) => {}
                    oomir::Type::Reference(t) if matches!(t.as_ref(), oomir::Type::Array(_)) => {}
                    _ => panic!(
                        "ArrayStore target base '{}' (Place: {:?}) is not an array or reference-to-array type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                }

                // Convert the MIR index operand (_local) to an OOMIR operand
                let mir_index_operand = MirOperand::Copy(Place::from(*index_local)); // Or Move? Copy usually safer.
                let oomir_index_operand =
                    convert_operand(&mir_index_operand, tcx, mir, data_types, &mut instructions);

                instructions.push(Instruction::ArrayStore {
                    array: base_var_name.clone(),
                    index: oomir_index_operand, // The index operand
                    value: source_operand,      // The value to store
                });
            }

            ProjectionElem::ConstantIndex {
                offset,
                min_length: _,
                from_end,
            } => {
                // Target is array element with constant index: base_var_name[const_idx] = source_operand
                // Ensure the base is actually an array or ref-to-array
                match &base_oomir_type {
                    oomir::Type::Array(_) => {}
                    oomir::Type::Reference(t) if matches!(t.as_ref(), oomir::Type::Array(_)) => {}
                    _ => panic!(
                        "ArrayStore target base '{}' (Place: {:?}) is not an array or reference-to-array type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                }

                let index_operand: Operand;

                if !from_end {
                    // Simple constant index from the start
                    index_operand = Operand::Constant(oomir::Constant::I32(*offset as i32));
                    // No extra instructions needed for the index itself
                } else {
                    // Index is calculated as length - offset
                    // We need to insert Length and Sub *before* the ArrayStore

                    // Temp name for length result (avoid collision)
                    let len_var_name = format!("{}_len_set", base_var_name);
                    instructions.push(Instruction::Length {
                        dest: len_var_name.clone(),
                        array: Operand::Variable {
                            name: base_var_name.clone(),
                            ty: base_oomir_type.clone(),
                        },
                    });

                    // Temp name for calculated index (avoid collision)
                    let index_var_name = format!("{}_calc_idx_set", base_var_name);
                    let offset_op = Operand::Constant(oomir::Constant::I32(*offset as i32));
                    instructions.push(Instruction::Sub {
                        dest: index_var_name.clone(),
                        op1: Operand::Variable {
                            name: len_var_name,
                            ty: oomir::Type::I32,
                        },
                        op2: offset_op,
                    });

                    // Use the calculated index variable
                    index_operand = Operand::Variable {
                        name: index_var_name,
                        ty: oomir::Type::I32,
                    };
                }

                instructions.push(Instruction::ArrayStore {
                    array: base_var_name.clone(), // The array retrieved in step 2
                    index: index_operand,         // The constant or calculated index
                    value: source_operand,        // The value to store
                });
            }

            ProjectionElem::Deref => {
                // do nothing
            }
            ProjectionElem::Downcast(..) => {
                // Downcast should not be the last element for an assignment.
                // You assign to a field/index within the downcast variant.
                panic!(
                    "Downcast cannot be the final projection element for an assignment. Place: {:?}",
                    dest_place
                );
            }
            _ => {
                panic!(
                    "Unsupported projection element type {:?} found at the end of destination Place during assignment: {:?}",
                    last_projection, dest_place
                );
            }
        }
    }

    instructions
}
