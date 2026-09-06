//! Compile-time memory decoding.
use super::*;

/// Reads a constant value of type `ty` from the `allocation` starting at `offset`.
pub(crate) fn read_constant_value_from_memory<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
    ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let pci = TypingEnv::fully_monomorphized().as_query_input(ty);
    let layout = tcx
        .layout_of(pci)
        .map_err(|_| "Couldn't get layout.".to_string())?;

    // A reference to a ZST can point at a zero-byte allocation, so there are
    // no bytes to decode. Reconstruct its nominal JVM value from the type.
    if layout.is_zst() {
        return read_zero_sized_constant(tcx, ty, oomir_data_types, instance);
    }

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "const-eval",
        format!(
            "Debug: Reading constant value for type {:?} at offset {:?} with layout size {:?}",
            ty, offset, layout.size
        )
    );

    match ty.kind() {
        TyKind::Pat(base_ty, _) => read_constant_value_from_memory(
            tcx,
            allocation,
            offset,
            *base_ty,
            oomir_data_types,
            instance,
        ),
        TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_) => {
            let range = AllocRange {
                start: offset,
                size: layout.size,
            };
            // Read as ScalarInt - floats are represented by their bits
            let scalar = allocation
                .read_scalar(&tcx.data_layout, range, false)
                .map_err(|error| format!("Failed to read scalar: {error:?}"))?;
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

        TyKind::Ref(_, inner_ty, _) => {
            if inner_ty.is_str() || inner_ty.is_slice() {
                let value = if inner_ty.is_str() {
                    read_str_from_fat_pointer(tcx, allocation, offset)?
                } else {
                    read_slice_from_fat_pointer(
                        tcx,
                        allocation,
                        offset,
                        *inner_ty,
                        oomir_data_types,
                        instance,
                    )?
                };
                Ok(value)
            } else if {
                let tail = tcx.struct_tail_for_codegen(*inner_ty, TypingEnv::fully_monomorphized());
                tail.is_str() || tail.is_slice()
            } {
                read_slice_tailed_pointer_from_fat_pointer(
                    tcx,
                    allocation,
                    offset,
                    *inner_ty,
                    oomir_data_types,
                    instance,
                )
            } else if matches!(inner_ty.kind(), TyKind::Dynamic(..)) {
                read_trait_object_reference_from_memory(
                    tcx,
                    allocation,
                    offset,
                    *inner_ty,
                    oomir_data_types,
                    instance,
                )
            } else {
                let ptr = read_pointer_from_memory(tcx, allocation, offset)?;
                let value = read_pointee_constant(tcx, ptr, *inner_ty, oomir_data_types, instance)?;
                if inner_ty.is_array() {
                    let backing = interned_pointer_for_full_allocation(
                        tcx,
                        ptr,
                        *inner_ty,
                        value.clone(),
                        oomir_data_types,
                        instance,
                    )?
                    .unwrap_or(value);
                    return array_reference_to_slice(
                        tcx,
                        *inner_ty,
                        backing,
                        oomir_data_types,
                        instance,
                    );
                }
                if pointer_references_static(tcx, ptr) {
                    // References whose allocation is the static itself already
                    // are its canonical stable Pointer. An ordinary allocation
                    // containing a reference still needs another Pointer layer.
                    return Ok(value);
                }
                pointer_constant_for_pointee(tcx, ptr, *inner_ty, value, oomir_data_types, instance)
            }
        }

        TyKind::RawPtr(inner_ty, _) => {
            if inner_ty.is_str() {
                read_str_from_fat_pointer(tcx, allocation, offset)
            } else if inner_ty.is_slice() {
                read_slice_from_fat_pointer(
                    tcx,
                    allocation,
                    offset,
                    *inner_ty,
                    oomir_data_types,
                    instance,
                )
            } else if {
                let tail = tcx.struct_tail_for_codegen(*inner_ty, TypingEnv::fully_monomorphized());
                tail.is_str() || tail.is_slice()
            } {
                read_slice_tailed_pointer_from_fat_pointer(
                    tcx,
                    allocation,
                    offset,
                    *inner_ty,
                    oomir_data_types,
                    instance,
                )
            } else {
                let pointer_size = tcx.data_layout.pointer_size();
                let scalar = allocation
                    .read_scalar(
                        &tcx.data_layout,
                        AllocRange {
                            start: offset,
                            size: pointer_size,
                        },
                        true,
                    )
                    .map_err(|error| {
                        format!("Failed to read raw pointer {ty:?} at {offset:?}: {error:?}")
                    })?;
                match scalar {
                    Scalar::Int(address) => {
                        read_scalar_int_constant(tcx, address, ty, oomir_data_types, instance)
                    }
                    Scalar::Ptr(pointer, _) => {
                        if let Some(pointer) = anonymous_memory_pointer_constant(
                            tcx,
                            pointer,
                            *inner_ty,
                            oomir_data_types,
                            instance,
                        )? {
                            return Ok(pointer);
                        }
                        let (provenance, pointer_offset) = pointer.into_raw_parts();
                        if provenance.get_alloc_id().is_some_and(|alloc_id| {
                            matches!(tcx.global_alloc(alloc_id), GlobalAlloc::TypeId { .. })
                        }) {
                            let pointee_layout = tcx
                                .layout_of(
                                    TypingEnv::fully_monomorphized().as_query_input(*inner_ty),
                                )
                                .map_err(|error| {
                                    format!(
                                        "Could not determine pointee layout for {ty:?}: {error:?}"
                                    )
                                })?;
                            Ok(oomir::Constant::PointerAddress {
                                // TypeId encodes its hash bytes in the offsets of
                                // provenance-only pointers. At runtime those are
                                // ordinary exposed-address pointer bits.
                                address: pointer_offset.bytes(),
                                view_size: pointee_layout.size.bytes(),
                                pointee: Box::new(ty_to_oomir_type(
                                    *inner_ty,
                                    tcx,
                                    oomir_data_types,
                                    instance,
                                )),
                            })
                        } else {
                            let value = read_pointee_constant(
                                tcx,
                                pointer,
                                *inner_ty,
                                oomir_data_types,
                                instance,
                            )?;
                            if pointer_references_static(tcx, pointer)
                                || pointer_references_vtable(tcx, pointer)
                            {
                                // Statics and vtables already have canonical runtime Pointer
                                // carriers. Anonymous allocations containing a pointer still
                                // require their own outer pointer layer.
                                Ok(value)
                            } else {
                                pointer_constant_for_pointee(
                                    tcx,
                                    pointer,
                                    *inner_ty,
                                    value,
                                    oomir_data_types,
                                    instance,
                                )
                            }
                        }
                    }
                }
            }
        }

        TyKind::FnPtr(..) => {
            let pointer = read_pointer_from_memory(tcx, allocation, offset)?;
            read_function_pointer_constant(tcx, pointer, ty, oomir_data_types, instance)
        }

        TyKind::Str => Err("Unsupported type: Direct read of str from memory".to_string()),

        TyKind::Array(elem_ty, _) => {
            let FieldsShape::Array { count: len, .. } = layout.fields else {
                return Err(format!(
                    "Array type {:?} had non-array layout {:?}",
                    ty, layout
                ));
            };
            let elem_pci = TypingEnv::fully_monomorphized().as_query_input(*elem_ty);
            let elem_layout = tcx
                .layout_of(elem_pci)
                .map_err(|_| "Couldn't get element layout.".to_string())?;
            let oomir_elem_type = ty_to_oomir_type(*elem_ty, tcx, oomir_data_types, instance);
            if !oomir_elem_type.has_jvm_value() {
                return Ok(oomir::Constant::Array(
                    Box::new(oomir_elem_type),
                    Vec::new(),
                ));
            }

            let len = usize::try_from(len)
                .ok()
                .filter(|length| *length <= i32::MAX as usize)
                .ok_or_else(|| format!("constant array length {len} exceeds JVM limits"))?;
            let mut values = Vec::with_capacity(len);
            for i in 0..len {
                let elem_offset = offset
                    + elem_layout
                        .size
                        .checked_mul(i as u64, &tcx.data_layout)
                        .ok_or_else(|| format!("constant array offset overflow at element {i}"))?;
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

            Ok(oomir::Constant::Array(Box::new(oomir_elem_type), values))
        }

        TyKind::Slice(_) => Err("Unsupported type: Direct read of slice from memory".to_string()),

        TyKind::Adt(adt_def, substs) => {
            if crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
                && matches!(
                    ty_to_oomir_type(ty, tcx, oomir_data_types, instance),
                    oomir::Type::Pointer(_)
                )
            {
                let field = adt_def
                    .variant(VariantIdx::from_usize(0))
                    .fields
                    .iter()
                    .next()
                    .ok_or_else(|| "NonNull constant has no pointer field".to_string())?;
                let field_ty = tcx
                    .try_normalize_erasing_regions(
                        TypingEnv::fully_monomorphized(),
                        field.ty(tcx, substs),
                    )
                    .map_err(|error| {
                        format!("Could not normalize NonNull constant field: {error:?}")
                    })?;
                read_constant_value_from_memory(
                    tcx,
                    allocation,
                    offset,
                    field_ty,
                    oomir_data_types,
                    instance,
                )
            } else if adt_def.is_struct() {
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
            } else if adt_def.is_union() {
                let class_name =
                    ensure_union_data_type(adt_def, substs, tcx, oomir_data_types, instance);
                let variant = adt_def.variant(VariantIdx::from_usize(0));
                let has_function_provenance =
                    allocation
                        .provenance()
                        .ptrs()
                        .iter()
                        .any(|&(pointer_offset, provenance)| {
                            pointer_offset >= offset
                                && pointer_offset < offset + layout.size
                                && provenance.get_alloc_id().is_some_and(|alloc_id| {
                                    matches!(
                                        tcx.global_alloc(alloc_id),
                                        GlobalAlloc::Function { .. }
                                    )
                                })
                        });
                let mut candidates = Vec::new();
                for field in &variant.fields {
                    let raw_field_ty = field.ty(tcx, substs).skip_norm_wip();
                    let instantiated_field_ty = EarlyBinder::bind(tcx, raw_field_ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let field_ty = tcx
                        .try_normalize_erasing_regions(
                            TypingEnv::fully_monomorphized(),
                            Unnormalized::new_wip(instantiated_field_ty),
                        )
                        .unwrap_or(instantiated_field_ty);
                    let Ok(value) = read_constant_value_from_memory(
                        tcx,
                        allocation,
                        offset,
                        field_ty,
                        oomir_data_types,
                        instance,
                    ) else {
                        continue;
                    };
                    let preserves_function = constant_contains_function_pointer(&value);
                    candidates.push((
                        preserves_function,
                        field.ident(tcx).to_string(),
                        field_ty,
                        value,
                    ));
                }
                if let Some((_, field_name, field_ty, value)) =
                    candidates
                        .into_iter()
                        .max_by_key(|(preserves_function, ..)| {
                            usize::from(*preserves_function == has_function_provenance)
                        })
                {
                    let field_oomir_ty =
                        ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance);
                    return Ok(oomir::Constant::StaticCall {
                        owner_class: class_name.clone(),
                        method_name: union_from_method_name(&field_name),
                        args: field_oomir_ty
                            .has_jvm_value()
                            .then_some(value)
                            .into_iter()
                            .collect(),
                        param_types: Vec::new(),
                        ty: oomir::Type::Class(class_name),
                    });
                }
                let start = offset.bytes_usize();
                let end = start
                    .checked_add(layout.size.bytes_usize())
                    .ok_or_else(|| format!("Union constant range overflow for {ty:?}"))?;
                let bytes = allocation
                    .inspect_with_uninit_and_ptr_outside_interpreter(start..end)
                    .iter()
                    .map(|byte| oomir::Constant::I8(*byte as i8))
                    .collect::<Vec<_>>();
                let objects = (0..layout.size.bytes_usize())
                    .map(|_| {
                        oomir::Constant::Null(oomir::Type::Class("java/lang/Object".to_string()))
                    })
                    .collect::<Vec<_>>();
                let bytes = oomir::Constant::Array(Box::new(oomir::Type::I8), bytes);
                let objects = oomir::Constant::Array(
                    Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                    objects,
                );
                Ok(oomir::Constant::Instance {
                    class_name,
                    fields: HashMap::from_iter([
                        (UNION_BYTES_FIELD.to_string(), bytes.clone()),
                        (UNION_OBJECTS_FIELD.to_string(), objects.clone()),
                    ]),
                    params: vec![bytes, objects],
                    param_types: Vec::new(),
                })
            } else {
                Err(format!("Unsupported ADT constant type: {ty:?}"))
            }
        }

        TyKind::Tuple(field_tys) => {
            if field_tys.is_empty() {
                return Ok(oomir::Constant::Unit);
            }
            let mut fields_map = HashMap::default();
            let mut params = Vec::new();
            let mut param_types = Vec::new();
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
                        param_types.push(ty_to_oomir_type(
                            field_ty,
                            tcx,
                            oomir_data_types,
                            instance,
                        ));
                        params.push(field_const.clone());
                        fields_map.insert(format!("field{}", i), field_const);
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
                param_types,
            })
        }

        TyKind::Closure(_, closure_args) => {
            let class_name = match ty_to_oomir_type(ty, tcx, oomir_data_types, instance) {
                oomir::Type::Class(class_name) => class_name,
                other => {
                    return Err(format!(
                        "Closure constant {ty:?} did not map to a JVM class: {other:?}"
                    ));
                }
            };
            let capture_tys = closure_args.as_closure().upvar_tys();
            let mut fields = HashMap::default();
            let mut params = Vec::new();
            let mut param_types = Vec::new();
            for (index, capture_ty) in capture_tys.iter().enumerate() {
                let capture_offset = layout.fields.offset(index);
                let capture = read_constant_value_from_memory(
                    tcx,
                    allocation,
                    offset + capture_offset,
                    capture_ty,
                    oomir_data_types,
                    instance,
                )?;
                param_types.push(ty_to_oomir_type(
                    capture_ty,
                    tcx,
                    oomir_data_types,
                    instance,
                ));
                fields.insert(format!("arg{index}"), capture.clone());
                params.push(capture);
            }
            Ok(oomir::Constant::Instance {
                class_name,
                fields,
                params,
                param_types,
            })
        }

        _ => Err(format!("Unsupported constant type: {:?}", ty)),
    }
}
