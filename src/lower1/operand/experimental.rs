use rustc_abi::{
    FieldIdx, FieldsShape, Float, Integer, Primitive, Size, TagEncoding, VariantIdx, Variants,
};
use rustc_middle::mir::interpret::{AllocRange, GlobalAlloc, Provenance, Scalar};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{AdtDef, GenericArgsRef, ScalarInt, Ty, TyCtxt, TyKind, TypingEnv};
use std::collections::HashMap;

use super::{
    super::{place::make_jvm_safe, ty_to_oomir_type, types::generate_tuple_jvm_class_name},
    scalar_int_to_oomir_constant,
};
use crate::oomir;

// Named "experimental" as it provides a new experimental (and not fully complete for every type) constant resolution engine to replace the overly hardcoded one in operand.rs
// Slowly, operations are being switched over to this new engine.
// Currently the engine is used for handling of ADTs and pointers to scalars.

/// Reads a constant value of type `ty` from the `allocation` starting at `offset`.
/// Eventually, this recursive function will replace the hardcoded/nested logic which is spagetti-like in operand.rs
/// Currently, it is used only for ADTs as a trial period.
pub fn read_constant_value_from_memory<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &rustc_middle::mir::interpret::Allocation<
        rustc_middle::mir::interpret::CtfeProvenance,
    >,
    offset: Size,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
) -> Result<oomir::Constant, String> {
    let pci = TypingEnv::fully_monomorphized().as_query_input(ty);
    let layout = tcx
        .layout_of(pci)
        .map_err(|_| "Couldn't get layout.".to_string())?;

    println!(
        "Debug: Reading constant value for type {:?} at offset {:?} with layout size {:?}",
        ty, offset, layout.size
    );

    match ty.kind() {
        // --- Primitive/Scalar Types ---
        TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_) => {
            let range = AllocRange {
                start: offset,
                size: layout.size,
            };
            // Read as ScalarInt - floats are represented by their bits
            let scalar = allocation
                .read_scalar(&tcx.data_layout, range, false)
                .map_err(|e| {
                    println!("Error reading scalar: {:?}", e);
                    "Failed to read scalar".to_string()
                })?;
            let scalar_int = match scalar {
                Scalar::Int(int) => int,
                Scalar::Ptr(_, _) => {
                    panic!("Expected scalar integer, found pointer");
                }
            };
            Ok(scalar_int_to_oomir_constant(scalar_int, &ty))
        }

        TyKind::Ref(_, inner_ty, _) if inner_ty.is_str() => {
            let size = allocation.size();
            let range = 0..size.bytes_usize();

            // Read the raw bytes, ignoring provenance and initialization checks
            // Should be okay as we are "outisde the interpreter" as the name suggests
            let bytes: &[u8] = allocation.inspect_with_uninit_and_ptr_outside_interpreter(range);
            match String::from_utf8(bytes.to_vec()) {
                Ok(s) => {
                    println!(
                        "Info: Successfully extracted string constant from allocation: \"{}\"",
                        s
                    );
                    Ok(oomir::Constant::String(s))
                }
                Err(e) => {
                    println!(
                        "Warning: Bytes from allocation for &str were not valid UTF-8: {}",
                        e
                    );
                    // TODO: make OOMIR support raw bytes?
                    Ok(oomir::Constant::String("Invalid UTF8".to_string()))
                }
            }
        }
        // --- Pointer/Reference Types (requires recursive dereference) ---
        TyKind::Ref(_, inner_ty, _) | TyKind::RawPtr(inner_ty, _) => {
            // Read the pointer scalar itself from the current allocation
            let ptr_range = AllocRange {
                start: offset,
                size: tcx.data_layout.pointer_size,
            };
            let scalar = allocation
                .read_scalar(&tcx.data_layout, ptr_range, false)
                .map_err(|e| {
                    println!("Error reading pointer scalar: {:?}", e);
                    "Failed to read pointer scalar".to_string()
                })?;

            match scalar {
                Scalar::Ptr(ptr, _) => {
                    let (inner_alloc_id, inner_offset) = ptr.into_parts();
                    match tcx.global_alloc(inner_alloc_id.get_alloc_id().unwrap()) {
                        // Assuming AllocId implements Copy
                        GlobalAlloc::Memory(inner_const_alloc) => {
                            // Recursively read the value the pointer points to
                            read_constant_value_from_memory(
                                tcx,
                                inner_const_alloc.inner(),
                                inner_offset,
                                *inner_ty,
                                oomir_data_types,
                            )
                        }
                        GlobalAlloc::Function { instance } => {
                            // Represent the function pointer, e.g., by its path
                            let func_name = tcx.def_path_str(instance.def_id());
                            println!("Info: Constant pointer to function: {}", func_name);
                            // You might need a specific oomir::Constant variant for this
                            Ok(oomir::Constant::String(format!(
                                "FunctionPtr({})",
                                func_name
                            ))) // Placeholder
                        }
                        GlobalAlloc::Static(def_id) => {
                            println!("Info: Constant pointer to static: {:?}", def_id);
                            // Need to look up the static's allocation - this might involve tcx.eval_static_initializer
                            // For now, return placeholder
                            Ok(oomir::Constant::String(format!("StaticPtr({:?})", def_id))) // Placeholder
                            // TODO: Properly evaluate or look up the static's allocation and recurse
                            // let static_alloc_id = tcx.eval_static_initializer(def_id)?; // Requires ErrorReporting + InterpCx? Complex.
                            // Simplification: Assume static maps to a known allocation ID if possible, otherwise error/placeholder.
                            // Err(ConstReadError::UnsupportedType("Pointer to Static".to_string()))
                        }
                        GlobalAlloc::VTable(..) => Err("Unsupported type: VTable".to_string()),
                    }
                }
                Scalar::Int(scalar) => Ok(scalar_int_to_oomir_constant(scalar, &ty)),
            }
        }

        // --- String Slice (&str) ---
        // This case is typically handled via ConstValue::Slice or by reading a fat pointer (Ptr+meta)
        // A direct read from memory for `str` itself isn't standard, but handle if layout allows.
        TyKind::Str => {
            Err("Unsupported type: Direct read of str from memory".to_string())
            // If `ty` was actually `&str`, it would be handled by TyKind::Ref above.
            // The pointer read there would give a fat pointer (ptr, len).
            // Need to extract len and read bytes from pointed-to allocation.
            // This logic belongs within the TyKind::Ref handler when inner_ty is str.
        }

        // --- Array ([T; N]) ---
        TyKind::Array(elem_ty, len_const) => {
            let Some(len) = len_const.try_to_target_usize(tcx) else {
                return Err("Unsupported type: Array with non-constant length".to_string());
            };
            let elem_pci = TypingEnv::fully_monomorphized().as_query_input(*elem_ty);
            let elem_layout = tcx
                .layout_of(elem_pci)
                .map_err(|_| "Couldn't get element layout.".to_string())?;
            let mut elements = Vec::with_capacity(len as usize);

            for i in 0..len {
                // Calculate offset of element `i` within the array allocation
                let elem_offset =
                    offset + elem_layout.size.checked_mul(i, &tcx.data_layout).unwrap();
                // Recursively read the element's constant value
                let elem_const = read_constant_value_from_memory(
                    tcx,
                    allocation,
                    elem_offset,
                    *elem_ty,
                    oomir_data_types,
                )?;
                elements.push(elem_const);
            }
            // Determine OOMIR element type (assuming ty_to_oomir_type exists)
            let mut data_types = HashMap::new(); // dummy data types - it's an array not an ADT so nothing will happen to it
            let oomir_elem_type = ty_to_oomir_type(*elem_ty, tcx, &mut data_types);

            // confirm data_types is empty
            if !data_types.is_empty() {
                panic!(
                    "Data types map is not empty for array type {:?}",
                    data_types
                );
            }

            // find values in the array
            let mut values = Vec::new();
            for i in 0..len {
                let elem_offset =
                    offset + elem_layout.size.checked_mul(i, &tcx.data_layout).unwrap();
                let elem_const = read_constant_value_from_memory(
                    tcx,
                    allocation,
                    elem_offset,
                    *elem_ty,
                    oomir_data_types,
                )?;
                values.push(elem_const);
            }

            // make an oomir constant
            Ok(oomir::Constant::Array(Box::new(oomir_elem_type), values))
        }

        // --- Slice ([T]) ---
        TyKind::Slice(_) => Err("Unsupported type: Direct read of slice from memory".to_string()),

        // --- ADTs (Struct, Enum) ---
        TyKind::Adt(adt_def, substs) => {
            if adt_def.is_struct() {
                handle_constant_struct(
                    tcx,
                    allocation,
                    offset,
                    layout,
                    *adt_def,
                    substs,
                    oomir_data_types,
                )
            } else if adt_def.is_enum() {
                handle_constant_enum(
                    tcx,
                    allocation,
                    offset,
                    ty,
                    layout,
                    *adt_def,
                    substs,
                    oomir_data_types,
                )
            } else {
                // Union
                Err("Unsupported type: Union".to_string())
            }
        }

        // --- Tuples ---
        TyKind::Tuple(field_tys) => {
            // Similar to structs, but fields accessed by index.
            // OOMIR might represent tuples as generic structs or have a dedicated type.
            let mut fields_map = HashMap::new();
            match layout.fields {
                FieldsShape::Arbitrary { ref offsets, .. } => {
                    for (i, field_ty) in field_tys.iter().enumerate() {
                        let field_offset = offsets[FieldIdx::from_usize(i)];
                        let field_const = read_constant_value_from_memory(
                            tcx,
                            allocation,
                            offset + field_offset,
                            field_ty,
                            oomir_data_types,
                        )?;
                        fields_map.insert(format!("field{}", i), field_const); // Use numbered field names
                    }
                }
                _ => return Err("Unsupported tuple layout".to_string()),
            }
            let tuple_class_name = generate_tuple_jvm_class_name(field_tys, tcx, oomir_data_types);
            Ok(oomir::Constant::Instance {
                class_name: tuple_class_name,
                fields: fields_map,
                params: vec![],
            })
        }

        // --- Other Types ---
        // TyKind::FnDef, TyKind::FnPtr, TyKind::Closure, TyKind::Generator, TyKind::Never, etc.
        _ => Err("Unsupported type: Complex or unknown type".to_string()),
    }
}

// --- ADT Helper Functions ---

fn handle_constant_struct<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &rustc_middle::mir::interpret::Allocation,
    offset: Size,
    layout: TyAndLayout<'tcx>,
    adt_def: AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
) -> Result<oomir::Constant, String> {
    let variant = adt_def.variant(VariantIdx::from_usize(0)); // Structs have one variant
    let mut fields_map = HashMap::new();

    for (i, field_def) in variant.fields.iter().enumerate() {
        let field_idx = FieldIdx::from_usize(i);
        let field_ty = field_def.ty(tcx, substs);
        let field_offset = layout.fields.offset(field_idx.into());
        let field_name = field_def.ident(tcx).to_string();

        println!(
            "Debug: Reading struct field '{}' ({:?}) at offset {:?}",
            field_name,
            field_ty,
            offset + field_offset
        );

        let field_const = read_constant_value_from_memory(
            tcx,
            allocation,
            offset + field_offset,
            field_ty,
            oomir_data_types,
        )?;
        fields_map.insert(field_name, field_const);
    }

    let rust_path = tcx.def_path_str(adt_def.did());
    let class_name = make_jvm_safe(&rust_path).replace("::", "/");

    Ok(oomir::Constant::Instance {
        class_name,
        fields: fields_map,
        params: vec![],
    })
}

fn handle_constant_enum<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &rustc_middle::mir::interpret::Allocation<
        rustc_middle::mir::interpret::CtfeProvenance, // Explicit provenance type
    >,
    offset: Size,
    enum_ty: Ty<'tcx>, // Keep enum_ty for context/errors if needed
    layout: TyAndLayout<'tcx>,
    adt_def: AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
) -> Result<oomir::Constant, String> {
    let active_variant_idx: VariantIdx;
    let variant_fields_shape: &FieldsShape<FieldIdx>; // Holds the layout of fields for the active variant

    match &layout.variants {
        // --- Case 1: Single Variant (Structs, Unions, Single-Variant Enums) ---
        Variants::Single { index } => {
            println!(
                "Debug: Enum {:?} has single variant layout (index {:?})",
                adt_def.did(),
                index
            );
            active_variant_idx = *index;
            // The fields are directly in the main layout
            variant_fields_shape = &layout.fields;
        }

        // --- Case 2: Multiple Variants (Standard Enums) ---
        Variants::Multiple {
            tag,
            tag_encoding,
            tag_field,
            variants: variant_layouts,
        } => {
            println!(
                "Debug: Enum {:?} has multiple variant layout",
                adt_def.did()
            );

            // --- Step 1: Locate and Read the Tag (Discriminant) ---

            // 1a. Get layout of the tag scalar itself to know its size and type
            let tag_scalar_layout = tag; // The 'tag' field in Variants::Multiple *is* the Scalar layout
            let tag_ty = match tag_scalar_layout.primitive() {
                Primitive::Int(number, signed) => {
                    // Convert to a Rust type
                    let rust_ty = match number {
                        Integer::I8 => {
                            if signed {
                                TyKind::Int(rustc_middle::ty::IntTy::I8)
                            } else {
                                TyKind::Uint(rustc_middle::ty::UintTy::U8)
                            }
                        }
                        Integer::I16 => {
                            if signed {
                                TyKind::Int(rustc_middle::ty::IntTy::I16)
                            } else {
                                TyKind::Uint(rustc_middle::ty::UintTy::U16)
                            }
                        }
                        Integer::I32 => {
                            if signed {
                                TyKind::Int(rustc_middle::ty::IntTy::I32)
                            } else {
                                TyKind::Uint(rustc_middle::ty::UintTy::U32)
                            }
                        }
                        Integer::I64 => {
                            if signed {
                                TyKind::Int(rustc_middle::ty::IntTy::I64)
                            } else {
                                TyKind::Uint(rustc_middle::ty::UintTy::U64)
                            }
                        }
                        Integer::I128 => {
                            if signed {
                                TyKind::Int(rustc_middle::ty::IntTy::I128)
                            } else {
                                TyKind::Uint(rustc_middle::ty::UintTy::U128)
                            }
                        }
                    };
                    tcx.mk_ty_from_kind(rust_ty)
                }
                Primitive::Float(number) => {
                    // Convert to a Rust type
                    let rust_ty = match number {
                        Float::F16 => TyKind::Float(rustc_middle::ty::FloatTy::F16),
                        Float::F32 => TyKind::Float(rustc_middle::ty::FloatTy::F32),
                        Float::F64 => TyKind::Float(rustc_middle::ty::FloatTy::F64),
                        Float::F128 => TyKind::Float(rustc_middle::ty::FloatTy::F128),
                    };
                    tcx.mk_ty_from_kind(rust_ty)
                }
                Primitive::Pointer(_) => {
                    // Pointer types are not valid for enum tags
                    return Err(format!(
                        "Invalid tag type for enum {:?}: Pointer type",
                        enum_ty
                    ));
                }
            };
            let tag_layout = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(tag_ty))
                .map_err(|e| {
                    format!(
                        "Failed to get layout for enum tag type {:?}: {:?}",
                        tag_ty, e
                    )
                })?;
            let tag_size = tag_layout.size;

            // 1b. Find the offset of the tag field within the enum's overall layout.
            // `tag_field` is the index of the field *within the enum's FieldsShape* that holds the tag.
            let tag_offset_in_enum = layout.fields.offset(*tag_field);
            let absolute_tag_offset = offset + tag_offset_in_enum;
            let absolute_tag_range = AllocRange {
                start: absolute_tag_offset,
                size: tag_size,
            };

            println!(
                "Debug: Reading tag for {:?} (type {:?}, size {:?}) at offset {:?} (relative offset {:?}, tag_field index {})",
                enum_ty, tag_ty, tag_size, absolute_tag_offset, tag_offset_in_enum, tag_field
            );

            // 1c. Read the tag value from memory
            let tag_scalar = allocation
                .read_scalar(&tcx.data_layout, absolute_tag_range, false)
                .map_err(|e| format!("Failed to read enum tag for {:?}: {:?}", enum_ty, e))?;
            let tag_val: ScalarInt = match tag_scalar {
                Scalar::Int(int) => int,
                Scalar::Ptr(..) => {
                    return Err(format!(
                        "Enum tag for {:?} read as pointer, expected integer",
                        enum_ty
                    ));
                }
            };

            // 1d. Sanity check the size read
            if tag_val.size() != tag_size {
                return Err(format!(
                    "Tag size mismatch for {:?}: read {:?} bytes, but type {:?} has size {:?}",
                    enum_ty,
                    tag_val.size(),
                    tag_ty,
                    tag_size
                ));
            }

            // 1e. Get the raw bits of the tag value
            let read_tag_bits = tag_val.to_bits(tag_size);
            println!(
                "Debug: Read tag value: {:?}, bits: {:#x}",
                tag_val, read_tag_bits
            );

            // --- Step 2: Determine Active Variant Index based on Tag Encoding ---

            match tag_encoding {
                TagEncoding::Direct => {
                    println!("Debug: Using Direct tag encoding");
                    // The tag value directly corresponds to the discriminant value (possibly after extension).
                    let mut found_idx = None;
                    for (v_idx, v_discr) in adt_def.discriminants(tcx) {
                        // Compare the read bits with the canonical variant discriminant value, masked to tag size.
                        let mask = (1u128 << tag_size.bits()) - 1;
                        let canonical_discr_val_masked = v_discr.val & mask;

                        println!(
                            "Debug: Comparing read_tag_bits {:#x} with variant {:?} discriminant {:#x} (masked: {:#x})",
                            read_tag_bits, v_idx, v_discr.val, canonical_discr_val_masked
                        );

                        if read_tag_bits == canonical_discr_val_masked {
                            if found_idx.is_some() {
                                return Err(format!(
                                    "Error: Tag value {:#x} matches multiple variants ({:?} and {:?}) for enum {:?}",
                                    read_tag_bits,
                                    found_idx.unwrap(),
                                    v_idx,
                                    adt_def.did()
                                ));
                            }
                            found_idx = Some(v_idx);
                            // Optimization: break early if representation is unique (usually is)
                            // Consider if enums can have aliasing discriminants in some cases.
                            // If so, we might need to continue searching to detect ambiguity.
                            // For now, assume first match is correct for Direct encoding.
                            break;
                        }
                    }
                    active_variant_idx = found_idx.ok_or_else(|| {
                        format!(
                            "Read tag value {:?} (bits: {:#x}) using Direct encoding does not match any variant discriminant for enum {:?}",
                            tag_val, read_tag_bits, adt_def.did()
                        )
                    })?;
                }

                TagEncoding::Niche {
                    untagged_variant,
                    niche_variants,
                    niche_start,
                } => {
                    println!(
                        "Debug: Using Niche tag encoding. Untagged: {:?}, Niche variants: {:?}, Niche start: {:#x}",
                        untagged_variant, niche_variants, niche_start
                    );
                    // The tag value encodes the discriminant using invalid bit patterns ("niches")
                    // of the `untagged_variant`'s field at index `tag_field`.

                    // We need to check if the `read_tag_bits` correspond to a niche value
                    // for any variant within `niche_variants`.
                    let mut found_match = false;
                    let mut matched_idx = *untagged_variant; // Default assumption

                    // Precompute discriminants for relevant variants for lookup
                    let discriminants: HashMap<VariantIdx, u128> = adt_def
                        .discriminants(tcx)
                        .map(|(idx, discr)| (idx, discr.val))
                        .collect();

                    for v_idx_int in niche_variants.start().index()..=niche_variants.end().index() {
                        let v_idx = VariantIdx::from_usize(v_idx_int);
                        if v_idx == *untagged_variant {
                            continue;
                        } // Skip untagged variant itself in niche check

                        let d = match discriminants.get(&v_idx) {
                            Some(discr_info) => discr_info,
                            None => {
                                return Err(format!(
                                    "Internal Error: Could not find discriminant for niche variant {:?} in enum {:?}",
                                    v_idx,
                                    adt_def.did()
                                ));
                            }
                        };

                        // Calculate the expected niche value for discriminant `d.val`
                        // Formula: (d - niche_variants.start).wrapping_add(niche_start)
                        // Note: Ensure arithmetic uses u128 for wrapping and range.
                        let niche_offset = d.wrapping_sub(
                            *discriminants.get(niche_variants.start()).ok_or_else(|| {
                                format!(
                                    "Missing discriminant for niche start {:?}",
                                    niche_variants.start()
                                )
                            })?,
                        );
                        let expected_niche_val_u128 = niche_offset.wrapping_add(*niche_start);

                        // Mask the expected niche value to the actual tag size for comparison
                        let mask = (1u128 << tag_size.bits()) - 1;
                        let expected_niche_bits = expected_niche_val_u128 & mask;

                        println!(
                            "Debug: Checking Niche for {:?} (Discr: {:#x}). Expected Niche Bits: {:#x}",
                            v_idx, d, expected_niche_bits
                        );

                        if read_tag_bits == expected_niche_bits {
                            println!("Debug: Match found for niche variant {:?}", v_idx);
                            if found_match && matched_idx != v_idx {
                                // This indicates an overlap or error in niche calculation/definition
                                return Err(format!(
                                    "Error: Niche tag value {:#x} matches multiple niche variants ({:?} and {:?}) for enum {:?}",
                                    read_tag_bits,
                                    matched_idx,
                                    v_idx,
                                    adt_def.did()
                                ));
                            }
                            matched_idx = v_idx;
                            found_match = true;
                            // Don't break early here, as multiple discriminants *could* theoretically map
                            // to the same niche value if the tag size is small, although unlikely/error.
                            // Let the check for multiple matches handle ambiguity.
                        }
                    }

                    if found_match {
                        active_variant_idx = matched_idx;
                    } else {
                        // If it didn't match any niche, it must be the untagged variant
                        println!(
                            "Debug: No niche match found, assuming untagged variant {:?}",
                            untagged_variant
                        );
                        active_variant_idx = *untagged_variant;
                    }
                }
            } // End match tag_encoding

            // --- Step 3: Get Layout for the Active Variant ---
            variant_fields_shape = &variant_layouts[active_variant_idx].fields;
        } // End Variants::Multiple

        // --- Case 3: Empty Enum (Uninhabited) ---
        Variants::Empty => {
            return Err(format!(
                "Cannot read constant value for uninhabited enum type {:?}",
                enum_ty
            ));
        }
    } // End match layout.variants

    println!(
        "Debug: Determined active variant index: {:?}",
        active_variant_idx
    );

    // --- Start: Steps 3 & 4 (Reading Fields - common logic) ---

    // Get the definition for the *active* variant
    let variant_def = adt_def.variant(active_variant_idx);

    // 3. Read the fields of the active variant using its specific layout (`variant_fields_shape`)
    let mut fields_map = HashMap::new();
    for (i, field_def) in variant_def.fields.iter().enumerate() {
        let field_idx = FieldIdx::from_usize(i);
        let field_ty = field_def.ty(tcx, substs);

        // Calculate the offset of the field *within the variant's data area*.
        // `variant_fields_shape.offset()` gives the offset relative to the start of *this variant's fields*.
        // For Multiple variants, this data area might not start at offset 0 of the enum.
        // However, the overall enum layout `layout` should place the variant fields correctly
        // relative to the enum's start `offset`. We need the *absolute* offset.

        // Determine the absolute offset of the field in the main allocation.
        // The `variant_fields_shape.offset` is relative to the start of the *variant's data*.
        // We need to know where the variant's data starts relative to the enum's start `offset`.
        // For Single variant, it's 0.
        // For Multiple variants, the base offset of the variant's non-tag fields might differ.
        // Let's re-check: `layout.fields.offset(idx)` gives offset from the *start* of the enum layout.
        // If field `idx` belongs to the active variant, this offset should be correct.
        // BUT: Field indices might restart from 0 for each variant internally.
        // Let's use the `variant_fields_shape` to get the *relative* offset within the variant,
        // and assume the *absolute* offset calculation needs care.

        // Revised approach: The offset returned by `variant_fields_shape.offset(field_idx)`
        // should be the offset relative to the start of the *variant's specific layout data*.
        // The tricky part is finding the start of *that data* within the overall enum allocation.
        // Usually, for non-C-like enums, variants overlay, sharing the start offset (after the tag).
        // Let's *assume* the variant data starts at `offset` (the start of the enum allocation)
        // and `variant_fields_shape.offset` gives the correct offset from *that* start.
        // This works if the tag is handled separately or is field 0, and other fields follow.
        // If variants have different base alignments/offsets, this assumption breaks.

        // Safer assumption: Use the overall `layout.fields.offset()` if we can map
        // the variant's `field_idx` (0, 1, ...) to the correct index in `layout.fields`.
        // This mapping isn't directly available.

        // Sticking with the previous logic: relative offset within variant shape.
        let field_offset_in_variant_shape = variant_fields_shape.offset(field_idx.into());

        // Calculate absolute offset relative to the start of the *whole allocation* `offset`.
        let absolute_field_offset = offset + field_offset_in_variant_shape;

        let field_name = format!("field{}", i); // Using index as field name for enum variant

        println!(
            "Debug: Reading enum variant {:?} field '{}' index {} ({:?}) at absolute offset {:?} (relative offset in variant shape: {:?})",
            variant_def.name,
            field_name,
            i,
            field_ty,
            absolute_field_offset,
            field_offset_in_variant_shape
        );

        let field_const = read_constant_value_from_memory(
            tcx,
            allocation,
            absolute_field_offset, // Use the absolute offset in the allocation
            field_ty,
            oomir_data_types,
        )?;
        fields_map.insert(field_name, field_const);
    }

    // 4. Construct the OOMIR constant
    let base_enum_name = make_jvm_safe(&tcx.def_path_str(adt_def.did())).replace("::", "/");
    let variant_class_name = format!(
        "{}${}", // Using '$' as inner class separator is common in JVM
        base_enum_name,
        make_jvm_safe(&variant_def.ident(tcx).to_string()) // Use ident for correct name
    );

    Ok(oomir::Constant::Instance {
        class_name: variant_class_name,
        fields: fields_map,
        params: vec![],
    })
}
