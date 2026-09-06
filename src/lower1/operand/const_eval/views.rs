//! Compile-time views decoding.
use super::*;

pub(crate) fn read_slice_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    alloc_id: AllocId,
    len: u64,
    pointee_ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let allocation = match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(allocation) => allocation.inner(),
        GlobalAlloc::Static(def_id) => {
            return read_slice_from_static(
                tcx,
                def_id,
                Size::ZERO,
                len,
                pointee_ty,
                oomir_data_types,
                instance,
            );
        }
        other => {
            return Err(format!(
                "slice data referred to non-memory allocation {:?}",
                other
            ));
        }
    };

    if pointee_ty != tcx.struct_tail_for_codegen(pointee_ty, TypingEnv::fully_monomorphized()) {
        return slice_tailed_pointer_constant(
            tcx,
            alloc_id,
            allocation,
            Size::ZERO,
            len,
            pointee_ty,
            oomir_data_types,
            instance,
        );
    }

    let value = read_slice_backed_value(
        tcx,
        allocation,
        Size::ZERO,
        len,
        pointee_ty,
        oomir_data_types,
        instance,
    )?;

    preserve_slice_allocation(
        tcx,
        alloc_id,
        allocation,
        Size::ZERO,
        len,
        pointee_ty,
        value,
        oomir_data_types,
        instance,
    )
}

pub(crate) fn slice_tailed_pointer_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    alloc_id: AllocId,
    allocation: &ConstAllocation,
    data_offset: Size,
    len: u64,
    pointee_ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let tail_ty = tcx.struct_tail_for_codegen(pointee_ty, TypingEnv::fully_monomorphized());
    let (element_ty, tail_view_class) = match tail_ty.kind() {
        TyKind::Str => (tcx.types.u8, oomir::UTF8_VIEW_CLASS),
        TyKind::Slice(element_ty) => (*element_ty, oomir::SLICE_VIEW_CLASS),
        _ => {
            return Err(format!(
                "{pointee_ty:?} does not have a string or slice tail"
            ));
        }
    };
    if pointee_ty == tail_ty {
        return Err(format!(
            "direct slice tail {pointee_ty:?} does not require a struct-tail pointer"
        ));
    }

    let tail_value = read_slice_backed_value(
        tcx,
        allocation,
        data_offset,
        len,
        tail_ty,
        oomir_data_types,
        instance,
    )?;
    let tail_value = preserve_slice_allocation(
        tcx,
        alloc_id,
        allocation,
        data_offset,
        len,
        tail_ty,
        tail_value,
        oomir_data_types,
        instance,
    )?;
    let element_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(element_ty))
        .map_err(|error| format!("could not determine slice-tail element layout: {error:?}"))?;
    let pointee_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(pointee_ty))
        .map_err(|error| format!("could not determine slice-tailed layout: {error:?}"))?;
    let element_type = ty_to_oomir_type(element_ty, tcx, oomir_data_types, instance);
    let pointee_type = ty_to_oomir_type(pointee_ty, tcx, oomir_data_types, instance);
    let oomir::Type::Class(target_class) = &pointee_type else {
        return Err(format!(
            "slice-tailed pointee {pointee_ty:?} mapped to non-class type {pointee_type:?}"
        ));
    };
    let element_codec =
        match pointer_memory_codec_operand(element_ty, tcx, oomir_data_types, instance) {
            oomir::Operand::Constant(codec) => codec,
            other => {
                return Err(format!(
                    "slice-tail element codec was not constant: {other:?}"
                ));
            }
        };
    let java_object = oomir::Type::Class("java/lang/Object".to_string());
    let java_string = oomir::Type::java_string();
    let data_pointer_type = oomir::Type::Pointer(Box::new(element_type));
    let data_pointer = oomir::Constant::StaticCall {
        owner_class: oomir::POINTER_CLASS.to_string(),
        method_name: "fromSlice".to_string(),
        args: vec![
            tail_value,
            oomir::Constant::U64(element_layout.size.bytes()),
            element_codec.clone(),
        ],
        param_types: vec![java_object, oomir::Type::U64, java_string.clone()],
        ty: data_pointer_type.clone(),
    };
    Ok(oomir::Constant::StaticCall {
        owner_class: oomir::POINTER_CLASS.to_string(),
        method_name: "unsizeStructTail".to_string(),
        args: vec![
            data_pointer,
            oomir::Constant::U64(pointee_layout.size.bytes()),
            oomir::Constant::String(target_class.clone()),
            oomir::Constant::String(tail_view_class.to_string()),
            oomir::Constant::U64(element_layout.size.bytes()),
            element_codec,
            oomir::Constant::U64(len),
        ],
        param_types: vec![
            data_pointer_type,
            oomir::Type::U64,
            java_string.clone(),
            java_string.clone(),
            oomir::Type::U64,
            java_string,
            oomir::Type::U64,
        ],
        ty: oomir::Type::Pointer(Box::new(pointee_type)),
    })
}

pub(crate) fn preserve_slice_allocation<'tcx>(
    tcx: TyCtxt<'tcx>,
    alloc_id: AllocId,
    allocation: &ConstAllocation,
    data_offset: Size,
    len: u64,
    slice_ty: Ty<'tcx>,
    value: oomir::Constant,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let TyKind::Slice(element_ty) = slice_ty.kind() else {
        return Ok(value);
    };
    let (element_type, elements) = match value {
        oomir::Constant::Slice(element_type, elements) => (element_type, elements),
        other => return Ok(other),
    };
    let element_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(*element_ty))
        .map_err(|error| format!("could not get constant slice element layout: {error:?}"))?;
    if data_offset != Size::ZERO
        || element_layout.size == Size::ZERO
        || element_layout
            .size
            .checked_mul(len, &tcx.data_layout)
            .is_none_or(|size| size != allocation.size())
    {
        return Ok(oomir::Constant::Slice(element_type, elements));
    }

    // Keep CTFE slice data in the same allocation cache as sized references.
    // In particular, `const S: &[T] = from_ref(const R: &T)` must give
    // `R` and `&S[0]` exactly the same runtime address.
    let identity_value = if elements.len() == 1 {
        elements[0].clone()
    } else {
        oomir::Constant::Array(element_type.clone(), elements.clone())
    };
    let allocation_value = oomir::Constant::Array(element_type.clone(), elements);
    let view_codec =
        match pointer_memory_codec_operand(*element_ty, tcx, oomir_data_types, instance) {
            oomir::Operand::Constant(codec) => codec,
            other => {
                return Err(format!(
                    "constant slice element codec was not constant: {other:?}"
                ));
            }
        };
    let pointee = element_type.clone();
    let identity_candidate =
        anonymous_memory_identity(tcx, allocation, instance, &allocation_value).unwrap_or_else(
            || {
                anonymous_allocation_identity(
                    tcx,
                    &identity_value,
                    element_layout.size.bytes(),
                    allocation.align.bytes(),
                    &view_codec,
                    &pointee,
                )
            },
        );
    let identity = oomir_data_types.allocation_identity(alloc_id, identity_candidate);
    let backing = oomir::Constant::InternedPointer {
        identity,
        value: Box::new(allocation_value),
        array_backed: true,
        allocation_size: allocation.size().bytes(),
        offset: 0,
        view_size: element_layout.size.bytes(),
        alignment: allocation.align.bytes(),
        view_codec: Box::new(view_codec),
        pointee: element_type.clone(),
    };
    Ok(oomir::Constant::SliceRef {
        backing: Box::new(backing),
        element_type,
        offset: 0,
        length: len,
    })
}

pub(crate) fn read_slice_backed_value<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    base_offset: Size,
    len: u64,
    ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    match ty.kind() {
        TyKind::Str => read_string_from_allocation(
            allocation,
            base_offset,
            Some(
                usize::try_from(len)
                    .map_err(|_| format!("string length {len} exceeds the host address space"))?,
            ),
        ),
        TyKind::Slice(element_ty) => {
            let element_layout = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(*element_ty))
                .map_err(|error| {
                    format!(
                        "could not get slice element layout for {:?}: {:?}",
                        element_ty, error
                    )
                })?;
            let element_type = ty_to_oomir_type(*element_ty, tcx, oomir_data_types, instance);
            if !element_type.has_jvm_value() {
                return Ok(oomir::Constant::SliceRef {
                    backing: Box::new(oomir::Constant::Array(
                        Box::new(element_type.clone()),
                        Vec::new(),
                    )),
                    element_type: Box::new(element_type),
                    offset: 0,
                    length: len,
                });
            }
            let len = usize::try_from(len)
                .ok()
                .filter(|length| *length <= i32::MAX as usize)
                .ok_or_else(|| format!("constant slice length {len} exceeds JVM limits"))?;
            let mut elements = Vec::with_capacity(len);

            for index in 0..len {
                let element_offset = element_layout
                    .size
                    .checked_mul(index as u64, &tcx.data_layout)
                    .ok_or_else(|| format!("slice offset overflow at element {}", index))?;
                elements.push(read_constant_value_from_memory(
                    tcx,
                    allocation,
                    base_offset + element_offset,
                    *element_ty,
                    oomir_data_types,
                    instance,
                )?);
            }

            Ok(oomir::Constant::Slice(Box::new(element_type), elements))
        }
        TyKind::Adt(adt_def, args) if adt_def.is_struct() && adt_def.repr().transparent() => {
            let variant = adt_def.variant(VariantIdx::from_usize(0));
            if variant.fields.len() != 1 {
                return Err(format!(
                    "transparent slice-tailed type {:?} has {} fields; only single-field wrappers are currently representable",
                    ty,
                    variant.fields.len()
                ));
            }

            let field = &variant.fields[FieldIdx::from_usize(0)];
            let field_ty = tcx
                .normalize_erasing_regions(TypingEnv::fully_monomorphized(), field.ty(tcx, args));
            let expected_tail = tcx.struct_tail_for_codegen(ty, TypingEnv::fully_monomorphized());
            let field_tail =
                tcx.struct_tail_for_codegen(field_ty, TypingEnv::fully_monomorphized());
            if field_tail != expected_tail {
                return Err(format!(
                    "transparent field {:?} does not contain the slice tail {:?}",
                    field_ty, expected_tail
                ));
            }

            let inner = read_slice_backed_value(
                tcx,
                allocation,
                base_offset,
                len,
                field_ty,
                oomir_data_types,
                instance,
            )?;
            let class_name = match ty_to_oomir_type(ty, tcx, oomir_data_types, instance) {
                oomir::Type::Class(class_name) => class_name,
                other => {
                    return Err(format!(
                        "slice-tailed wrapper {:?} mapped to non-class type {:?}",
                        ty, other
                    ));
                }
            };
            let mut fields = HashMap::default();
            fields.insert(field.ident(tcx).to_string(), inner.clone());
            Ok(oomir::Constant::Instance {
                class_name,
                fields,
                params: vec![inner],
                param_types: vec![ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance)],
            })
        }
        _ => Err(format!(
            "unsupported slice-backed pointee type {:?}; expected str, a slice, or a single-field transparent wrapper",
            ty
        )),
    }
}

pub(crate) fn array_reference_to_slice<'tcx>(
    tcx: TyCtxt<'tcx>,
    array_ty: Ty<'tcx>,
    value: oomir::Constant,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let TyKind::Array(element_ty, _) = array_ty.kind() else {
        return Err(format!("Expected array type, found {array_ty:?}"));
    };
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(array_ty))
        .map_err(|error| format!("Could not get array layout for {array_ty:?}: {error:?}"))?;
    let FieldsShape::Array { count, .. } = layout.fields else {
        return Err(format!("Array type {array_ty:?} had layout {layout:?}"));
    };
    let value = match value {
        oomir::Constant::Array(element_type, elements)
            if u64::try_from(elements.len()).ok() == Some(count) =>
        {
            return Ok(oomir::Constant::Slice(element_type, elements));
        }
        other => other,
    };
    slice_ref_constant(
        tcx,
        value,
        *element_ty,
        0,
        count,
        oomir_data_types,
        instance,
    )
}

pub(crate) fn slice_ref_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    backing: oomir::Constant,
    element_ty: Ty<'tcx>,
    offset: u64,
    length: u64,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let element_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(element_ty))
        .map_err(|error| format!("Could not determine constant slice element layout: {error:?}"))?;
    let element_type = ty_to_oomir_type(element_ty, tcx, oomir_data_types, instance);
    let backing = if matches!(
        oomir::Type::from_constant(&backing),
        oomir::Type::Pointer(_)
    ) {
        let element_codec =
            match pointer_view_codec_operand(element_ty, tcx, oomir_data_types, instance) {
                oomir::Operand::Constant(codec) => codec,
                other => {
                    return Err(format!(
                        "Constant slice element codec was not constant: {other:?}"
                    ));
                }
            };
        oomir::Constant::StaticCall {
            owner_class: oomir::POINTER_CLASS.to_string(),
            method_name: "retype".to_string(),
            args: vec![
                backing,
                oomir::Constant::U64(element_layout.size.bytes()),
                element_codec,
            ],
            param_types: Vec::new(),
            ty: oomir::Type::Pointer(Box::new(element_type.clone())),
        }
    } else {
        backing
    };
    Ok(oomir::Constant::SliceRef {
        backing: Box::new(backing),
        element_type: Box::new(element_type),
        offset,
        length,
    })
}

pub(crate) fn read_slice_from_static<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: rustc_span::def_id::DefId,
    data_offset: Size,
    len: u64,
    slice_ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let TyKind::Slice(element_ty) = slice_ty.kind() else {
        return Err(format!(
            "Named static-backed constant has unsupported slice type {slice_ty:?}"
        ));
    };
    let element_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(*element_ty))
        .map_err(|error| format!("Could not determine static slice element layout: {error:?}"))?;
    let element_size = element_layout.size.bytes();
    if element_size == 0 {
        if data_offset != Size::ZERO {
            return Err("A zero-sized static slice had a nonzero data offset".to_string());
        }
    } else if data_offset.bytes() % element_size != 0 {
        return Err(format!(
            "Static slice byte offset {} is not aligned to its {}-byte element size",
            data_offset.bytes(),
            element_size
        ));
    }
    let offset = if element_size == 0 {
        0
    } else {
        data_offset.bytes() / element_size
    };
    slice_ref_constant(
        tcx,
        super::super::super::statics::static_ref_constant(tcx, def_id, oomir_data_types, instance),
        *element_ty,
        offset,
        len,
        oomir_data_types,
        instance,
    )
}

pub(crate) fn read_str_from_fat_pointer<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
) -> Result<oomir::Constant, String> {
    let pointer_size = tcx.data_layout.pointer_size();
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
    if len == 0 {
        return Ok(oomir::Constant::Str(String::new()));
    }

    let data_ptr = read_pointer_from_memory(tcx, allocation, offset)?;

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

pub(crate) fn read_slice_from_fat_pointer<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
    slice_ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let pointer_size = tcx.data_layout.pointer_size();
    let len_scalar = allocation
        .read_scalar(
            &tcx.data_layout,
            AllocRange {
                start: offset + pointer_size,
                size: pointer_size,
            },
            false,
        )
        .map_err(|error| format!("Failed to read slice length at {:?}: {:?}", offset, error))?;
    let len = match len_scalar {
        Scalar::Int(len) => len.to_target_usize(tcx),
        Scalar::Ptr(..) => {
            return Err(format!(
                "Expected integer slice length at {:?}, found pointer",
                offset + pointer_size
            ));
        }
    };
    let data_scalar = allocation
        .read_scalar(
            &tcx.data_layout,
            AllocRange {
                start: offset,
                size: pointer_size,
            },
            true,
        )
        .map_err(|error| format!("Failed to read slice data pointer at {offset:?}: {error:?}"))?;
    let data_ptr = match data_scalar {
        Scalar::Ptr(pointer, _) => pointer,
        Scalar::Int(address) => {
            let TyKind::Slice(element_ty) = slice_ty.kind() else {
                return Err(format!("Expected slice type, found {slice_ty:?}"));
            };
            let element_layout = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(*element_ty))
                .map_err(|error| format!("Could not get slice element layout: {error:?}"))?;
            let element_type = ty_to_oomir_type(*element_ty, tcx, oomir_data_types, instance);
            return Ok(oomir::Constant::SliceRef {
                backing: Box::new(oomir::Constant::PointerAddress {
                    address: address.to_target_usize(tcx) as u64,
                    view_size: element_layout.size.bytes(),
                    pointee: Box::new(element_type.clone()),
                }),
                element_type: Box::new(element_type),
                offset: 0,
                length: len as u64,
            });
        }
    };

    let (provenance, data_offset) = data_ptr.into_raw_parts();
    let alloc_id = provenance.get_alloc_id().ok_or_else(|| {
        format!(
            "Slice data pointer provenance {:?} has no allocation id",
            provenance
        )
    })?;
    match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(const_alloc) => {
            let const_alloc = const_alloc.inner();
            let value = read_slice_backed_value(
                tcx,
                const_alloc,
                data_offset,
                len,
                slice_ty,
                oomir_data_types,
                instance,
            )?;
            preserve_slice_allocation(
                tcx,
                alloc_id,
                const_alloc,
                data_offset,
                len,
                slice_ty,
                value,
                oomir_data_types,
                instance,
            )
        }
        GlobalAlloc::Static(def_id) => read_slice_from_static(
            tcx,
            def_id,
            data_offset,
            len,
            slice_ty,
            oomir_data_types,
            instance,
        ),
        other => Err(format!(
            "Slice data pointer referenced non-memory allocation {:?}",
            other
        )),
    }
}

pub(crate) fn read_slice_tailed_pointer_from_fat_pointer<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
    pointee_ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let pointer_size = tcx.data_layout.pointer_size();
    let len_scalar = allocation
        .read_scalar(
            &tcx.data_layout,
            AllocRange {
                start: offset + pointer_size,
                size: pointer_size,
            },
            false,
        )
        .map_err(|error| {
            format!(
                "failed to read slice-tail length at {:?}: {:?}",
                offset + pointer_size,
                error
            )
        })?;
    let len = match len_scalar {
        Scalar::Int(len) => len.to_target_usize(tcx),
        Scalar::Ptr(..) => {
            return Err(format!(
                "expected integer slice-tail length at {:?}, found pointer",
                offset + pointer_size
            ));
        }
    };
    let data_ptr = read_pointer_from_memory(tcx, allocation, offset)?;
    let (provenance, data_offset) = data_ptr.into_raw_parts();
    let alloc_id = provenance.get_alloc_id().ok_or_else(|| {
        format!(
            "slice-tail data pointer provenance {:?} has no allocation id",
            provenance
        )
    })?;
    match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(const_allocation) => slice_tailed_pointer_constant(
            tcx,
            alloc_id,
            const_allocation.inner(),
            data_offset,
            len,
            pointee_ty,
            oomir_data_types,
            instance,
        ),
        other => Err(format!(
            "slice-tail data pointer referenced unsupported allocation {other:?}"
        )),
    }
}

pub(crate) fn read_string_from_allocation(
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
            Ok(oomir::Constant::Str(s))
        }
        Err(e) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "const-eval",
                format!("Warning: String bytes were not valid UTF-8: {}", e)
            );
            Ok(oomir::Constant::Str("Invalid UTF8".to_string()))
        }
    }
}
