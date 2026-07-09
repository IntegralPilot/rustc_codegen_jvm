use rustc_abi::{FieldIdx, FieldsShape, Size, TagEncoding, VariantIdx, Variants};
use rustc_middle::mir::interpret::{
    AllocRange, Allocation, CtfeProvenance, GlobalAlloc, Pointer, Provenance, Scalar,
};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{
    AdtDef, FloatTy, GenericArgsRef, Instance, IntTy, PseudoCanonicalInput, ScalarInt, Ty, TyCtxt,
    TyKind, TypingEnv, UintTy, util::IntTypeExt,
};
use std::collections::HashMap;

use super::super::{
    place::make_jvm_safe,
    ty_to_oomir_type,
    types::{generate_adt_jvm_class_name, generate_tuple_jvm_class_name},
};
use super::float::f128_to_string;
use crate::oomir::{self, DataTypeMethod};

type ConstAllocation = Allocation<CtfeProvenance>;

pub fn read_scalar_int_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    scalar_int: ScalarInt,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    if let TyKind::Adt(adt_def, substs) = ty.kind() {
        if adt_def.is_enum() {
            let repr_type = adt_def.repr().discr_type();
            let int_ty = repr_type.to_ty(tcx);
            return Ok(scalar_int_to_oomir_constant(scalar_int, int_ty));
        }

        let adt_name = match ty_to_oomir_type(ty, tcx, oomir_data_types, instance) {
            oomir::Type::Class(class_name) => class_name,
            other => {
                return Err(format!(
                    "Expected class type for scalar ADT constant {:?}, got {:?}",
                    ty, other
                ));
            }
        };

        let variant = adt_def
            .variants()
            .iter()
            .next()
            .ok_or_else(|| format!("Transparent ADT {:?} has no variants", ty))?;
        let non_zst_fields = variant
            .fields
            .iter()
            .filter(|field_def| {
                !tcx.layout_of(PseudoCanonicalInput {
                    typing_env: TypingEnv::post_analysis(tcx, field_def.did),
                    value: field_def.ty(tcx, substs).skip_norm_wip(),
                })
                .map(|layout| layout.is_zst())
                .unwrap_or(false)
            })
            .collect::<Vec<_>>();

        if non_zst_fields.len() != 1 {
            return Err(format!(
                "Transparent ADT {:?} has {} non-ZST fields, expected exactly one",
                ty,
                non_zst_fields.len()
            ));
        }

        let field_def = non_zst_fields[0];
        let field_name = field_def.ident(tcx).name.to_string();
        let field_ty = tcx
            .normalize_erasing_regions(TypingEnv::fully_monomorphized(), field_def.ty(tcx, substs));
        let inner_constant =
            read_scalar_int_constant(tcx, scalar_int, field_ty, oomir_data_types, instance)?;
        let mut fields = HashMap::new();
        fields.insert(field_name, inner_constant.clone());
        return Ok(oomir::Constant::Instance {
            class_name: adt_name,
            fields,
            params: vec![inner_constant],
        });
    }

    Ok(scalar_int_to_oomir_constant(scalar_int, ty))
}

pub fn read_pointer_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    match ty.kind() {
        TyKind::Ref(_, inner_ty, _) | TyKind::RawPtr(inner_ty, _) => {
            read_pointee_constant(tcx, pointer, *inner_ty, oomir_data_types, instance)
        }
        _ => read_pointee_constant(tcx, pointer, ty, oomir_data_types, instance),
    }
}

fn read_pointer_from_memory<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
) -> Result<Pointer<CtfeProvenance>, String> {
    let pointer_size = tcx.data_layout.pointer_size();
    let ptr_range = AllocRange {
        start: offset,
        size: pointer_size,
    };
    match allocation
        .read_scalar(&tcx.data_layout, ptr_range, true)
        .map_err(|e| format!("Failed to read pointer scalar at {:?}: {:?}", offset, e))?
    {
        Scalar::Ptr(ptr, _) => Ok(ptr),
        Scalar::Int(int) => Err(format!(
            "Expected pointer scalar at {:?}, found integer {:?}",
            offset, int
        )),
    }
}

fn read_pointee_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    pointee_ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let (provenance, offset) = pointer.into_raw_parts();
    let alloc_id = provenance
        .get_alloc_id()
        .ok_or_else(|| format!("Pointer provenance {:?} has no allocation id", provenance))?;

    match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(const_alloc) => {
            let allocation = const_alloc.inner();
            if pointee_ty.is_str() {
                read_string_from_allocation(allocation, offset, None)
            } else {
                read_constant_value_from_memory(
                    tcx,
                    allocation,
                    offset,
                    pointee_ty,
                    oomir_data_types,
                    instance,
                )
            }
        }
        GlobalAlloc::Function { instance } => {
            let func_name = tcx.def_path_str(instance.def_id());
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Info: Constant pointer to function: {}", func_name)
            );
            Ok(oomir::Constant::String(format!(
                "FunctionPtr({})",
                func_name
            )))
        }
        GlobalAlloc::Static(def_id) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Info: Constant pointer to static: {:?}", def_id)
            );
            Ok(oomir::Constant::String(format!("StaticPtr({:?})", def_id)))
        }
        GlobalAlloc::VTable(..) => Err("Unsupported constant pointer to vtable".to_string()),
        GlobalAlloc::TypeId { ty } => Err(format!("Unsupported constant pointer to TypeId {ty:?}")),
    }
}

fn read_str_from_fat_pointer<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
) -> Result<oomir::Constant, String> {
    let pointer_size = tcx.data_layout.pointer_size();
    let data_ptr = read_pointer_from_memory(tcx, allocation, offset)?;
    let len_range = AllocRange {
        start: offset + pointer_size,
        size: pointer_size,
    };
    let len_scalar = allocation
        .read_scalar(&tcx.data_layout, len_range, false)
        .map_err(|e| format!("Failed to read str length at {:?}: {:?}", offset, e))?;
    let len = match len_scalar {
        Scalar::Int(len) => len.to_target_usize(tcx) as usize,
        Scalar::Ptr(..) => {
            return Err(format!(
                "Expected integer str length at {:?}, found pointer",
                offset + pointer_size
            ));
        }
    };

    let (provenance, data_offset) = data_ptr.into_raw_parts();
    let alloc_id = provenance.get_alloc_id().ok_or_else(|| {
        format!(
            "String data pointer provenance {:?} has no allocation id",
            provenance
        )
    })?;
    match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(const_alloc) => {
            read_string_from_allocation(const_alloc.inner(), data_offset, Some(len))
        }
        other => Err(format!(
            "String data pointer referenced non-memory allocation {:?}",
            other
        )),
    }
}

fn read_string_from_allocation(
    allocation: &ConstAllocation,
    offset: Size,
    len: Option<usize>,
) -> Result<oomir::Constant, String> {
    let start = offset.bytes_usize();
    let alloc_size = allocation.size().bytes_usize();
    let end = match len {
        Some(len) => start
            .checked_add(len)
            .ok_or_else(|| format!("String byte range starting at {} overflowed", start))?,
        None => alloc_size,
    };
    if end > alloc_size {
        return Err(format!(
            "String byte range {}..{} is outside allocation size {}",
            start, end, alloc_size
        ));
    }

    let bytes = allocation.inspect_with_uninit_and_ptr_outside_interpreter(start..end);
    match String::from_utf8(bytes.to_vec()) {
        Ok(s) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Info: Successfully extracted string constant: \"{}\"", s)
            );
            Ok(oomir::Constant::String(s))
        }
        Err(e) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "const-eval",
                format!("Warning: String bytes were not valid UTF-8: {}", e)
            );
            Ok(oomir::Constant::String("Invalid UTF8".to_string()))
        }
    }
}

/// Reads a constant value of type `ty` from the `allocation` starting at `offset`.
pub fn read_constant_value_from_memory<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let pci = TypingEnv::fully_monomorphized().as_query_input(ty);
    let layout = tcx
        .layout_of(pci)
        .map_err(|_| "Couldn't get layout.".to_string())?;

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "const-eval",
        format!(
            "Debug: Reading constant value for type {:?} at offset {:?} with layout size {:?}",
            ty, offset, layout.size
        )
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
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Error,
                        "const-eval",
                        format!("Error reading scalar: {:?}", e)
                    );
                    "Failed to read scalar".to_string()
                })?;
            let scalar_int = match scalar {
                Scalar::Int(int) => int,
                Scalar::Ptr(_, _) => {
                    return Err(format!(
                        "Expected scalar integer for type {:?}, found pointer",
                        ty
                    ));
                }
            };
            read_scalar_int_constant(tcx, scalar_int, ty, oomir_data_types, instance)
        }

        // --- Pointer/Reference Types (requires recursive dereference) ---
        TyKind::Ref(_, inner_ty, _) | TyKind::RawPtr(inner_ty, _) => {
            if inner_ty.is_str() {
                read_str_from_fat_pointer(tcx, allocation, offset)
            } else {
                let ptr = read_pointer_from_memory(tcx, allocation, offset)?;
                read_pointee_constant(tcx, ptr, *inner_ty, oomir_data_types, instance)
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
            // Determine OOMIR element type (assuming ty_to_oomir_type exists)
            let oomir_elem_type = ty_to_oomir_type(*elem_ty, tcx, oomir_data_types, instance);

            // find values in the array
            let mut values = Vec::with_capacity(len as usize);
            for i in 0..len {
                let elem_offset =
                    offset + elem_layout.size.checked_mul(i, &tcx.data_layout).unwrap();
                let elem_const = read_constant_value_from_memory(
                    tcx,
                    allocation,
                    elem_offset,
                    *elem_ty,
                    oomir_data_types,
                    instance,
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
                    instance,
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
                    instance,
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
            let mut params = Vec::new();
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
                            instance,
                        )?;
                        params.push(field_const.clone());
                        fields_map.insert(format!("field{}", i), field_const); // Use numbered field names
                    }
                }
                _ => return Err("Unsupported tuple layout".to_string()),
            }
            let tuple_class_name =
                generate_tuple_jvm_class_name(field_tys, tcx, oomir_data_types, instance);
            Ok(oomir::Constant::Instance {
                class_name: tuple_class_name,
                fields: fields_map,
                params,
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
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let variant = adt_def.variant(VariantIdx::from_usize(0)); // Structs have one variant
    let mut fields_map = HashMap::new();
    let mut params = Vec::new();

    for (i, field_def) in variant.fields.iter().enumerate() {
        let field_idx = FieldIdx::from_usize(i);
        let field_ty = field_def.ty(tcx, substs).skip_norm_wip();
        let field_offset = layout.fields.offset(field_idx.into());
        let field_name = field_def.ident(tcx).to_string();

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "const-eval",
            format!(
                "Debug: Reading struct field '{}' ({:?}) at offset {:?}",
                field_name,
                field_ty,
                offset + field_offset
            )
        );

        let field_const = read_constant_value_from_memory(
            tcx,
            allocation,
            offset + field_offset,
            field_ty,
            oomir_data_types,
            instance,
        )?;
        params.push(field_const.clone());
        fields_map.insert(field_name, field_const);
    }

    let class_name = generate_adt_jvm_class_name(&adt_def, substs, tcx, oomir_data_types, instance);

    Ok(oomir::Constant::Instance {
        class_name,
        fields: fields_map,
        params,
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
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let active_variant_idx: VariantIdx;
    match &layout.variants {
        // --- Case 1: Single Variant (Structs, Unions, Single-Variant Enums) ---
        Variants::Single { index } => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Enum {:?} has single variant layout (index {:?})",
                    adt_def.did(),
                    index
                )
            );
            active_variant_idx = *index;
        }

        // --- Case 2: Multiple Variants (Standard Enums) ---
        Variants::Multiple {
            tag, // This is the Scalar layout for the tag's storage location
            tag_encoding,
            tag_field, // Index within layout.fields where the tag is stored
            variants: _variant_layouts,
        } => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Enum {:?} has multiple variant layout. Tag Encoding: {:?}",
                    adt_def.did(),
                    tag_encoding
                )
            );

            // --- Step 1: Locate and Read the Tag/Niche Value ---

            // 1a. Get layout information for the tag's storage location
            // The 'tag' field in Variants::Multiple *is* the Scalar layout
            let tag_scalar_layout = tag;
            let tag_size = tag_scalar_layout.size(&tcx.data_layout); // Get size from Scalar layout

            // 1b. Find the offset of the tag field within the enum's overall layout.
            let tag_offset_in_enum = layout.fields.offset((*tag_field).into());
            let absolute_tag_offset = offset + tag_offset_in_enum;
            let absolute_tag_range = AllocRange {
                start: absolute_tag_offset,
                size: tag_size,
            };

            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Reading tag/niche value for {:?} (storage type {:?}, size {:?}) at offset {:?} (relative offset {:?}, tag_field index {})",
                    enum_ty,
                    tag_scalar_layout.primitive(),
                    tag_size,
                    absolute_tag_offset,
                    tag_offset_in_enum,
                    usize::from(*tag_field)
                )
            );

            // 1c. Read the tag value from memory (could be Int or Ptr)
            let tag_scalar = allocation
                .read_scalar(&tcx.data_layout, absolute_tag_range, false)
                .map_err(|e| format!("Failed to read enum tag/niche for {:?}: {:?}", enum_ty, e))?;

            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Debug: Read tag scalar: {:?}", tag_scalar)
            );

            // --- Step 2: Determine Active Variant Index based on Tag Encoding ---

            match tag_encoding {
                TagEncoding::Direct => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        "Debug: Using Direct tag encoding"
                    );
                    // Tag value must be an integer for Direct encoding
                    let tag_val = match tag_scalar {
                        Scalar::Int(int) => int,
                        Scalar::Ptr(..) => {
                            return Err(format!(
                                "Enum tag for {:?} with Direct encoding read as pointer, expected integer",
                                enum_ty
                            ));
                        }
                    };

                    // Sanity check the size read
                    if tag_val.size() != tag_size {
                        return Err(format!(
                            "Direct Tag size mismatch for {:?}: read {:?} bytes, but expected size {:?}",
                            enum_ty,
                            tag_val.size(),
                            tag_size
                        ));
                    }
                    let read_tag_bits = tag_val.to_bits(tag_size);
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        format!(
                            "Debug: Read Direct tag value: {:?}, bits: {:#x}",
                            tag_val, read_tag_bits
                        )
                    );

                    // --- Find matching variant (existing logic seems okay) ---
                    let mut found_idx = None;
                    for (v_idx, v_discr) in adt_def.discriminants(tcx) {
                        let mask = (1u128 << tag_size.bits()) - 1;
                        let canonical_discr_val_masked = v_discr.val & mask;
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "const-eval",
                            format!(
                                "Debug: Comparing read_tag_bits {:#x} with variant {:?} discriminant {:#x} (masked: {:#x})",
                                read_tag_bits, v_idx, v_discr.val, canonical_discr_val_masked
                            )
                        );
                        if read_tag_bits == canonical_discr_val_masked {
                            if found_idx.is_some() {
                                return Err(format!("Ambiguous match found for enum variant"));
                            }
                            found_idx = Some(v_idx);
                            break;
                        }
                    }
                    active_variant_idx =
                        found_idx.ok_or_else(|| "No matching variant found".to_string())?;
                } // End Direct Encoding

                TagEncoding::Niche {
                    untagged_variant,
                    niche_variants,
                    niche_start,
                } => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        format!(
                            "Debug: Using Niche tag encoding. Untagged: {:?}, Niche variants: {:?}, Niche start: {:#x}",
                            untagged_variant, niche_variants, niche_start
                        )
                    );

                    // Extract the bits from the read scalar (Int or Ptr)
                    // For pointers in niche encoding, we compare the address bits.
                    let read_value_bits = match tag_scalar {
                        Scalar::Int(int) => {
                            if int.size() != tag_size {
                                return Err(format!(
                                    "Niche integer tag size mismatch for {:?}: read {:?} bytes, but expected size {:?}",
                                    enum_ty,
                                    int.size(),
                                    tag_size
                                ));
                            }
                            int.to_bits(tag_size)
                        }
                        Scalar::Ptr(ptr, _meta) => {
                            // Pointer size must match tag size for niche encoding
                            if tag_size != tcx.data_layout.pointer_size() {
                                return Err(format!(
                                    "Niche pointer tag size mismatch for {:?}: pointer size is {:?}, but tag size is {:?}",
                                    enum_ty,
                                    tcx.data_layout.pointer_size(),
                                    tag_size
                                ));
                            }
                            // Use the address part of the pointer for comparison.
                            // The address is usually u64, safely convert to u128.
                            ptr.into_raw_parts().1.bytes() as u128
                        }
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        format!("Debug: Read Niche value bits: {:#x}", read_value_bits)
                    );

                    // --- Compare read_value_bits with expected niche values (existing logic seems okay) ---
                    let mut found_match = false;
                    let mut matched_idx = *untagged_variant; // Default assumption

                    let discriminants: HashMap<VariantIdx, u128> = adt_def
                        .discriminants(tcx)
                        .map(|(idx, discr)| (idx, discr.val))
                        .collect();

                    for v_idx in niche_variants.clone() {
                        if v_idx == *untagged_variant {
                            continue;
                        }

                        let d = match discriminants.get(&v_idx) {
                            Some(discr) => discr,
                            None => {
                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Warn,
                                    "const-eval",
                                    format!(
                                        "Warning: No discriminant found for variant {:?}",
                                        v_idx
                                    )
                                );
                                continue; // Skip this variant if no discriminant is found
                            }
                        };
                        let niche_offset = d.wrapping_sub(*niche_start);
                        let expected_niche_val_u128 = niche_offset.wrapping_add(*niche_start);
                        let mask = (1u128 << tag_size.bits()) - 1;
                        let expected_niche_bits = expected_niche_val_u128 & mask;

                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "const-eval",
                            format!(
                                "Debug: Checking Niche for {:?} (Discr: {:#x}). Expected Niche Bits: {:#x}",
                                v_idx, d, expected_niche_bits
                            )
                        );

                        if read_value_bits == expected_niche_bits {
                            // ... (handle match, check ambiguity) ...
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "const-eval",
                                format!("Debug: Match found for niche variant {:?}", v_idx)
                            );
                            if found_match && matched_idx != v_idx {
                                return Err(format!("Ambiguous match found for enum variant"));
                            }
                            matched_idx = v_idx;
                            found_match = true;
                        }
                    }

                    if found_match {
                        active_variant_idx = matched_idx;
                    } else {
                        // If it didn't match any niche, it must be the untagged variant,
                        // *and* the read value should be valid for the untagged variant's field.
                        // (We implicitly assume this if no niche matches).
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "const-eval",
                            format!(
                                "Debug: No niche match found for bits {:#x}, assuming untagged variant {:?}",
                                read_value_bits, untagged_variant
                            )
                        );
                        active_variant_idx = *untagged_variant;
                    }
                } // End Niche Encoding
            } // End match tag_encoding

            // --- Step 3: Get Layout for the Active Variant ---
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Determined active variant index: {:?}",
                    active_variant_idx
                )
            );
        } // End Variants::Multiple

        // --- Case 3: Empty Enum (Uninhabited) ---
        Variants::Empty => {
            return Err(format!(
                "Cannot read constant value for uninhabited enum type {:?}",
                enum_ty
            ));
        }
    } // End match layout.variants

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "const-eval",
        format!(
            "Debug: Determined active variant index: {:?}",
            active_variant_idx
        )
    );

    // --- Start: Steps 3 & 4 (Reading Fields - common logic) ---

    // Get the definition for the *active* variant
    let variant_def = adt_def.variant(active_variant_idx);

    // 3. Read the fields of the active variant using its specific layout (`variant_fields_shape`)
    let mut fields_map = HashMap::new();
    let mut params = Vec::new();
    for (i, field_def) in variant_def.fields.iter().enumerate() {
        let field_idx = FieldIdx::from_usize(i);
        let field_ty = field_def.ty(tcx, substs).skip_norm_wip();

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
        let field_offset_in_variant_shape = match &layout.variants {
            Variants::Single { .. } => layout.fields.offset(field_idx.into()),
            Variants::Multiple {
                variants: variant_layouts,
                ..
            } => variant_layouts[active_variant_idx].field_offsets[field_idx],
            Variants::Empty => unreachable!("empty enums have no active variant fields"),
        };

        // Calculate absolute offset relative to the start of the *whole allocation* `offset`.
        let absolute_field_offset = offset + field_offset_in_variant_shape;

        let field_name = format!("field{}", i); // Using index as field name for enum variant

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "const-eval",
            format!(
                "Debug: Reading enum variant {:?} field '{}' index {} ({:?}) at absolute offset {:?} (relative offset in variant shape: {:?})",
                variant_def.name,
                field_name,
                i,
                field_ty,
                absolute_field_offset,
                field_offset_in_variant_shape
            )
        );

        let field_const = read_constant_value_from_memory(
            tcx,
            allocation,
            absolute_field_offset, // Use the absolute offset in the allocation
            field_ty,
            oomir_data_types,
            instance,
        )?;
        params.push(field_const.clone());
        fields_map.insert(field_name, field_const);
    }

    // 4. Construct the OOMIR constant
    let base_enum_name =
        generate_adt_jvm_class_name(&adt_def, substs, tcx, oomir_data_types, instance);
    let variant_class_name = format!(
        "{}${}", // Using '$' as inner class separator is common in JVM
        base_enum_name,
        make_jvm_safe(&variant_def.ident(tcx).to_string()) // Use ident for correct name
    );

    // the enum in general
    if !oomir_data_types.contains_key(&base_enum_name) {
        let mut methods = HashMap::new();
        methods.insert(
            "getVariantIdx".to_string(),
            DataTypeMethod::SimpleConstantReturn(oomir::Type::I32, None),
        );
        oomir_data_types.insert(
            base_enum_name.clone(),
            oomir::DataType::Class {
                fields: vec![], // No fields in the abstract class
                is_abstract: true,
                methods,
                super_class: None,
                interfaces: vec![],
            },
        );
    }

    // this variant
    if !oomir_data_types.contains_key(&variant_class_name) {
        let mut fields = vec![];
        for (i, field) in variant_def.fields.iter().enumerate() {
            let field_name = format!("field{}", i);
            let field_type = ty_to_oomir_type(
                field.ty(tcx, substs).skip_norm_wip(),
                tcx,
                oomir_data_types,
                instance,
            );
            fields.push((field_name, field_type));
        }

        let mut methods = HashMap::new();
        methods.insert(
            "getVariantIdx".to_string(),
            DataTypeMethod::SimpleConstantReturn(
                oomir::Type::I32,
                Some(oomir::Constant::I32(active_variant_idx.as_u32() as i32)),
            ),
        );

        oomir_data_types.insert(
            variant_class_name.clone(),
            oomir::DataType::Class {
                fields,
                is_abstract: false,
                methods,
                super_class: Some(base_enum_name.clone()),
                interfaces: vec![],
            },
        );
    }

    Ok(oomir::Constant::Instance {
        class_name: variant_class_name,
        fields: fields_map,
        params,
    })
}

/// Converts a Rust MIR Scalar::Int into the appropriate OOMIR constant.
pub fn scalar_int_to_oomir_constant(scalar_int: ScalarInt, ty: Ty<'_>) -> oomir::Constant {
    match ty.kind() {
        TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Constant::I8(scalar_int.to_i8() as i8),
            IntTy::I16 => oomir::Constant::I16(scalar_int.to_i16() as i16),
            IntTy::I32 => oomir::Constant::I32(scalar_int.to_i32() as i32),
            IntTy::Isize => oomir::Constant::I32(scalar_int.to_i64() as i32),
            IntTy::I64 => oomir::Constant::I64(scalar_int.to_i64()),
            IntTy::I128 => {
                let param = oomir::Constant::String(scalar_int.to_i128().to_string());
                oomir::Constant::Instance {
                    class_name: "java/math/BigInteger".into(),
                    fields: HashMap::new(),
                    params: vec![param],
                }
            }
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Constant::I16(scalar_int.to_u8() as i16),
            UintTy::U16 => oomir::Constant::I32(scalar_int.to_u16() as i32),
            UintTy::U32 => oomir::Constant::I64(scalar_int.to_u32() as i64),
            UintTy::Usize => oomir::Constant::I32(scalar_int.to_u64() as i32),
            UintTy::U64 => {
                let param = oomir::Constant::String(scalar_int.to_u64().to_string());
                oomir::Constant::Instance {
                    class_name: "java/math/BigInteger".into(),
                    fields: HashMap::new(),
                    params: vec![param],
                }
            }
            UintTy::U128 => {
                let param = oomir::Constant::String(scalar_int.to_u128().to_string());
                oomir::Constant::Instance {
                    class_name: "java/math/BigInteger".into(),
                    fields: HashMap::new(),
                    params: vec![param],
                }
            }
        },
        TyKind::Bool => oomir::Constant::Boolean(scalar_int.try_to_bool().unwrap_or(false)),
        TyKind::Char => oomir::Constant::Char(char::from_u32(scalar_int.to_u32()).unwrap_or('\0')),
        TyKind::Float(float_ty) => match float_ty {
            FloatTy::F16 => {
                let f16_val = half::f16::from_bits(scalar_int.to_u16());
                oomir::Constant::F32(f16_val.to_f32())
            }
            FloatTy::F32 => oomir::Constant::F32(f32::from_bits(scalar_int.to_u32())),
            FloatTy::F64 => oomir::Constant::F64(f64::from_bits(scalar_int.to_u64())),
            FloatTy::F128 => {
                let val = f128::from_bits(scalar_int.to_u128());
                if val.is_nan() || val.is_infinite() {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "const-eval",
                        "Warning: Attempt to store NaN/inf as BigDecimal/F128. BigDecimal does not support it. Using 0 as placeholder."
                    );
                    oomir::Constant::Instance {
                        class_name: "java/math/BigDecimal".into(),
                        fields: HashMap::new(),
                        params: vec![oomir::Constant::String("0".to_string())],
                    }
                } else {
                    oomir::Constant::Instance {
                        class_name: "java/math/BigDecimal".into(),
                        fields: HashMap::new(),
                        params: vec![oomir::Constant::String(f128_to_string(val))],
                    }
                }
            }
        },
        TyKind::Str => oomir::Constant::String(scalar_int.to_u64().to_string()),
        TyKind::Ref(_, inner_ty, _) => {
            scalar_int_to_oomir_constant(scalar_int.to_u64().into(), *inner_ty)
        }
        _ => panic!("Unsupported type for ScalarInt conversion: {:?}", ty),
    }
}
