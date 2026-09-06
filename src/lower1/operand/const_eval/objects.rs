//! Compile-time objects decoding.
use super::*;

pub(crate) fn constant_contains_function_pointer(constant: &oomir::Constant) -> bool {
    match constant {
        oomir::Constant::FunctionPointer { .. } => true,
        oomir::Constant::Array(_, values) | oomir::Constant::Slice(_, values) => {
            values.iter().any(constant_contains_function_pointer)
        }
        oomir::Constant::SliceRef { backing, .. } => constant_contains_function_pointer(backing),
        oomir::Constant::Instance { fields, params, .. } => {
            fields.values().any(constant_contains_function_pointer)
                || params.iter().any(constant_contains_function_pointer)
        }
        oomir::Constant::StaticCall { args, .. } => {
            args.iter().any(constant_contains_function_pointer)
        }
        _ => false,
    }
}

pub(crate) fn handle_constant_struct<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &rustc_middle::mir::interpret::Allocation,
    offset: Size,
    layout: TyAndLayout<'tcx>,
    adt_def: AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let variant = adt_def.variant(VariantIdx::from_usize(0)); // Structs have one variant
    let mut fields_map = HashMap::default();
    let mut params = Vec::new();
    let mut param_types = Vec::new();

    for (i, field_def) in variant.fields.iter().enumerate() {
        let field_idx = FieldIdx::from_usize(i);
        let unnormalized_field_ty = field_def.ty(tcx, substs);
        let field_ty = tcx
            .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), unnormalized_field_ty)
            .map_err(|error| {
                format!(
                    "Could not normalize constant field {} of type {:?}: {:?}",
                    field_def.ident(tcx),
                    unnormalized_field_ty,
                    error
                )
            })?;
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
        param_types.push(ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance));
        params.push(field_const.clone());
        fields_map.insert(field_name, field_const);
    }

    let class_name = generate_adt_jvm_class_name(&adt_def, substs, tcx, oomir_data_types, instance);

    Ok(oomir::Constant::Instance {
        class_name,
        fields: fields_map,
        params,
        param_types,
    })
}

pub(crate) fn handle_constant_enum<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &rustc_middle::mir::interpret::Allocation<
        rustc_middle::mir::interpret::CtfeProvenance, // Explicit provenance type
    >,
    offset: Size,
    enum_ty: Ty<'tcx>, // Keep enum_ty for context/errors if needed
    layout: TyAndLayout<'tcx>,
    adt_def: AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let active_variant_idx: VariantIdx;
    match &layout.variants {
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

            let tag_scalar_layout = tag;
            let tag_size = tag_scalar_layout.size(&tcx.data_layout);

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

            // Preserve provenance only when the niche can physically be a
            // pointer. CTFE requires provenance reads to be exactly pointer
            // sized, while ordinary direct tags are commonly just one byte.
            let read_provenance = matches!(tag_encoding, TagEncoding::Niche { .. })
                && tag_size == tcx.data_layout.pointer_size();
            let tag_scalar = allocation
                .read_scalar(&tcx.data_layout, absolute_tag_range, read_provenance)
                .map_err(|e| format!("Failed to read enum tag/niche for {:?}: {:?}", enum_ty, e))?;

            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Debug: Read tag scalar: {:?}", tag_scalar)
            );

            match tag_encoding {
                TagEncoding::Direct => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        "Debug: Using Direct tag encoding"
                    );
                    let tag_val = match tag_scalar {
                        Scalar::Int(int) => int,
                        Scalar::Ptr(..) => {
                            return Err(format!(
                                "Enum tag for {:?} with Direct encoding read as pointer, expected integer",
                                enum_ty
                            ));
                        }
                    };

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

                    let mut found_idx = None;
                    for (v_idx, v_discr) in adt_def.discriminants(tcx) {
                        let mask = if tag_size.bits() == 128 {
                            u128::MAX
                        } else {
                            (1u128 << tag_size.bits()) - 1
                        };
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
                            Some(int.to_bits(tag_size))
                        }
                        Scalar::Ptr(_, _meta) => {
                            if tag_size != tcx.data_layout.pointer_size() {
                                return Err(format!(
                                    "Niche pointer tag size mismatch for {:?}: pointer size is {:?}, but tag size is {:?}",
                                    enum_ty,
                                    tcx.data_layout.pointer_size(),
                                    tag_size
                                ));
                            }
                            // A provenance-carrying CTFE pointer is a valid,
                            // non-null pointer even when its allocation-relative
                            // offset is zero. Its numeric runtime address is not
                            // available during compilation, but it cannot encode
                            // one of the invalid-pointer niche variants.
                            None
                        }
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        format!("Debug: Read Niche value bits: {:?}", read_value_bits)
                    );

                    if let Some(read_value_bits) = read_value_bits {
                        let tag_bits = tag_size.bits();
                        let tag_mask = if tag_bits == 128 {
                            u128::MAX
                        } else {
                            (1u128 << tag_bits) - 1
                        };
                        let relative = read_value_bits.wrapping_sub(*niche_start) & tag_mask;
                        let first = niche_variants.start.as_u32();
                        let relative_max = niche_variants.last.as_u32() - first;
                        if relative <= u128::from(relative_max) {
                            active_variant_idx = VariantIdx::from_u32(
                                first + u32::try_from(relative).expect("bounded by relative_max"),
                            );
                        } else {
                            active_variant_idx = *untagged_variant;
                        }
                    } else {
                        active_variant_idx = *untagged_variant;
                    }
                }
            }

            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Determined active variant index: {:?}",
                    active_variant_idx
                )
            );
        }

        Variants::Empty => {
            return Err(format!(
                "Cannot read constant value for uninhabited enum type {:?}",
                enum_ty
            ));
        }
    }

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "const-eval",
        format!(
            "Debug: Determined active variant index: {:?}",
            active_variant_idx
        )
    );

    let variant_def = adt_def.variant(active_variant_idx);

    let mut fields_map = HashMap::default();
    let mut params = Vec::new();
    let mut param_types = Vec::new();
    for (i, field_def) in variant_def.fields.iter().enumerate() {
        let field_idx = FieldIdx::from_usize(i);
        let field_ty = field_def.ty(tcx, substs).skip_norm_wip();
        let field_oomir_ty = ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance);
        if !field_oomir_ty.has_jvm_value() {
            continue;
        }

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

        let field_name = enum_variant_field_name(variant_def, i, tcx);

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
        param_types.push(field_oomir_ty);
        params.push(field_const.clone());
        fields_map.insert(field_name, field_const);
    }

    // 4. Construct the OOMIR constant
    let base_enum_name =
        generate_adt_jvm_class_name(&adt_def, substs, tcx, oomir_data_types, instance);
    force_define_named_adt(enum_ty, tcx, oomir_data_types, instance);
    if jvm_subtype_payload_ty(&adt_def, variant_def, substs, tcx).is_some() {
        return params.into_iter().next().ok_or_else(|| {
            "`#[jvm_codegen::subtype]` constant payload has no JVM value".to_string()
        });
    }
    let variant_class_name = format!(
        "{}${}", // Using '$' as inner class separator is common in JVM
        base_enum_name,
        jvm_names::member_name(&variant_def.ident(tcx).to_string())
    );

    Ok(oomir::Constant::Instance {
        class_name: variant_class_name,
        fields: fields_map,
        params,
        param_types,
    })
}
