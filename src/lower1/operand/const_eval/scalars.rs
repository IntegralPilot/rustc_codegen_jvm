//! Compile-time scalars decoding.
use super::*;

pub(crate) fn read_scalar_int_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    scalar_int: ScalarInt,
    ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let ty = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    if tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map(|layout| layout.is_zst())
        .unwrap_or(false)
    {
        return read_zero_sized_constant(tcx, ty, oomir_data_types, instance);
    }

    if let TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) = ty.kind()
        && matches!(
            ty_to_oomir_type(ty, tcx, oomir_data_types, instance),
            oomir::Type::Pointer(_)
        )
    {
        let layout = tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(*pointee))
            .map_err(|error| format!("Could not determine pointee layout for {ty:?}: {error:?}"))?;
        return Ok(oomir::Constant::PointerAddress {
            address: scalar_int.to_target_usize(tcx) as u64,
            view_size: layout.size.bytes(),
            pointee: Box::new(ty_to_oomir_type(*pointee, tcx, oomir_data_types, instance)),
        });
    }

    if let TyKind::Closure(_, closure_args) = ty.kind() {
        let class_name = match ty_to_oomir_type(ty, tcx, oomir_data_types, instance) {
            oomir::Type::Class(class_name) => class_name,
            other => {
                return Err(format!(
                    "Scalar closure {ty:?} did not map to a JVM class: {other:?}"
                ));
            }
        };
        let mut fields = HashMap::default();
        let mut params = Vec::new();
        let mut param_types = Vec::new();
        let mut non_zst_captures = 0usize;
        for (index, capture_ty) in closure_args.as_closure().upvar_tys().iter().enumerate() {
            let capture_ty = EarlyBinder::bind(tcx, capture_ty)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let capture_layout = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(capture_ty))
                .map_err(|error| {
                    format!(
                        "Could not determine closure capture layout for {capture_ty:?}: {error:?}"
                    )
                })?;
            let capture_jvm_ty = ty_to_oomir_type(capture_ty, tcx, oomir_data_types, instance);
            if !capture_jvm_ty.has_jvm_value() {
                continue;
            }
            let capture = if capture_layout.is_zst() {
                read_zero_sized_constant(tcx, capture_ty, oomir_data_types, instance)?
            } else {
                non_zst_captures += 1;
                read_scalar_int_constant(tcx, scalar_int, capture_ty, oomir_data_types, instance)?
            };
            fields.insert(format!("arg{index}"), capture.clone());
            params.push(capture);
            param_types.push(capture_jvm_ty);
        }
        if non_zst_captures != 1 {
            return Err(format!(
                "Scalar closure {ty:?} has {non_zst_captures} non-ZST captures, expected exactly one"
            ));
        }
        return Ok(oomir::Constant::Instance {
            class_name,
            fields,
            params,
            param_types,
        });
    }

    let scalar_carrier_ty = || match scalar_int.size().bytes() {
        1 => Ok(tcx.types.u8),
        2 => Ok(tcx.types.u16),
        4 => Ok(tcx.types.u32),
        8 => Ok(tcx.types.u64),
        16 => Ok(tcx.types.u128),
        size => Err(format!("Unsupported {size}-byte scalar carrier for {ty:?}")),
    };

    if matches!(ty.kind(), TyKind::Tuple(elements) if !elements.is_empty()) {
        let layout = tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
            .map_err(|error| format!("Could not determine tuple layout for {ty:?}: {error:?}"))?;
        if !matches!(layout.backend_repr, BackendRepr::Scalar(_)) {
            return Err(format!(
                "Scalar constant tuple {ty:?} does not have a scalar ABI representation"
            ));
        }
        return scalar_int_to_oomir_constant(tcx, scalar_int, scalar_carrier_ty()?);
    }

    if let TyKind::Adt(adt_def, _) = ty.kind() {
        if adt_def.is_enum() {
            // A scalar enum constant is the enum's physical ABI carrier, not
            // necessarily its source-level discriminant. Niche-encoded enums
            // in optimized MIR rely on both the exact bits and their width.
            // Preserve that width so value adaptation can reconstruct the enum
            // through the exact-layout codec instead of widening it to i64.
            return scalar_int_to_oomir_constant(tcx, scalar_int, scalar_carrier_ty()?);
        }

        let field_ty = scalar_struct_field_ty(tcx, ty)?
            .ok_or_else(|| format!("Scalar constant ADT {ty:?} did not have one non-ZST field"))?;
        // A scalar ADT is carried using the bits of its one non-ZST field.
        // Keep that physical carrier here and let value-representation
        // adaptation reconstruct the nominal JVM object at the use site.
        // Constructing the wrapper eagerly is incorrect when its field has a
        // nominal JVM representation of its own (for example `Alignment`,
        // which is transparent over `AlignmentEnum`): the physical integer
        // bits are not a valid argument to the generated object constructor.
        return read_scalar_int_constant(tcx, scalar_int, field_ty, oomir_data_types, instance);
    }

    scalar_int_to_oomir_constant(tcx, scalar_int, ty)
}

pub(crate) fn scalar_struct_field_ty<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
) -> Result<Option<Ty<'tcx>>, String> {
    let TyKind::Adt(adt_def, substs) = ty.kind() else {
        return Ok(None);
    };
    if !adt_def.is_struct() {
        return Ok(None);
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|error| format!("Could not determine constant layout for {ty:?}: {error:?}"))?;
    if !matches!(layout.backend_repr, BackendRepr::Scalar(_)) {
        return Ok(None);
    }

    let variant = adt_def
        .variants()
        .iter()
        .next()
        .ok_or_else(|| format!("Scalar ADT {ty:?} has no variants"))?;
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
    let [field_def] = non_zst_fields.as_slice() else {
        return Err(format!(
            "Scalar ADT {ty:?} has {} non-ZST fields, expected exactly one",
            non_zst_fields.len()
        ));
    };
    let field_ty = field_def.ty(tcx, substs);
    tcx.try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), field_ty)
        .map(Some)
        .map_err(|error| {
            format!(
                "Could not normalize constant field {} of type {field_ty:?}: {error:?}",
                field_def.ident(tcx)
            )
        })
}

pub(crate) fn instance_constant_with_declared_fields(
    class_name: String,
    named_values: Vec<(String, oomir::Constant)>,
    oomir_data_types: &HashMap<String, oomir::DataType>,
) -> oomir::Constant {
    let declared_fields: &[(String, oomir::Type)] = match oomir_data_types.get(&class_name) {
        Some(oomir::DataType::Class { fields, .. }) => fields.as_slice(),
        _ => &[],
    };
    let param_types = named_values
        .iter()
        .map(|(name, value)| {
            declared_fields
                .iter()
                .find_map(|(field_name, ty)| (field_name == name).then(|| ty.clone()))
                .unwrap_or_else(|| oomir::Type::from_constant(value))
        })
        .collect();
    let fields = named_values.iter().cloned().collect::<HashMap<_, _>>();
    let params = named_values.into_iter().map(|(_, value)| value).collect();
    oomir::Constant::Instance {
        class_name,
        fields,
        params,
        param_types,
    }
}

pub(crate) fn read_zero_sized_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let ty = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let ty = tcx
        .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), Unnormalized::new_wip(ty))
        .unwrap_or(ty);
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|error| format!("Could not determine ZST layout for {ty:?}: {error:?}"))?;
    if !layout.is_zst() {
        return Err(format!("Type {ty:?} is not zero-sized"));
    }

    let oomir_ty = ty_to_oomir_type(ty, tcx, oomir_data_types, instance);
    if !oomir_ty.has_jvm_value() {
        return Ok(oomir::Constant::Unit);
    }

    match ty.kind() {
        TyKind::FnDef(..) => {
            let oomir::Type::Class(class_name) = oomir_ty else {
                return Err(format!("ZST function item {ty:?} did not map to a class"));
            };
            Ok(instance_constant_with_declared_fields(
                class_name,
                Vec::new(),
                oomir_data_types,
            ))
        }
        TyKind::Closure(_, closure_args) => {
            let oomir::Type::Class(class_name) = oomir_ty else {
                return Err(format!("ZST closure {ty:?} did not map to a class"));
            };
            let mut values = Vec::new();
            for (index, capture_ty) in closure_args.as_closure().upvar_tys().iter().enumerate() {
                let capture_ty = EarlyBinder::bind(tcx, capture_ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                let capture_jvm_ty = ty_to_oomir_type(capture_ty, tcx, oomir_data_types, instance);
                if capture_jvm_ty.has_jvm_value() {
                    values.push((
                        format!("arg{index}"),
                        read_zero_sized_constant(tcx, capture_ty, oomir_data_types, instance)?,
                    ));
                }
            }
            Ok(instance_constant_with_declared_fields(
                class_name,
                values,
                oomir_data_types,
            ))
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            let oomir::Type::Class(class_name) = oomir_ty else {
                return Err(format!("ZST struct {ty:?} did not map to a class"));
            };
            let mut values = Vec::new();
            for field in &adt_def.variant(VariantIdx::from_usize(0)).fields {
                let field_ty = EarlyBinder::bind(tcx, field.ty(tcx, substs).skip_norm_wip())
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                let field_jvm_ty = ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance);
                if field_jvm_ty.has_jvm_value() {
                    values.push((
                        field.ident(tcx).to_string(),
                        read_zero_sized_constant(tcx, field_ty, oomir_data_types, instance)?,
                    ));
                }
            }
            Ok(instance_constant_with_declared_fields(
                class_name,
                values,
                oomir_data_types,
            ))
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_union() => {
            let class_name =
                ensure_union_data_type(adt_def, substs, tcx, oomir_data_types, instance);
            let bytes = oomir::Constant::Array(Box::new(oomir::Type::I8), Vec::new());
            let objects = oomir::Constant::Array(
                Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                vec![oomir::Constant::Null(oomir::Type::Class(
                    "java/lang/Object".to_string(),
                ))],
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
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_enum() => {
            let Variants::Single { index } = layout.variants else {
                return Err(format!(
                    "ZST enum {ty:?} does not have one known active variant"
                ));
            };
            let oomir::Type::Class(base_class) = oomir_ty else {
                return Err(format!("ZST enum {ty:?} did not map to a class"));
            };
            let variant = adt_def.variant(index);
            let class_name = format!(
                "{}${}",
                base_class,
                jvm_names::member_name(&variant.name.to_string())
            );
            let mut values = Vec::new();
            for (field_index, field) in variant.fields.iter().enumerate() {
                let field_ty = EarlyBinder::bind(tcx, field.ty(tcx, substs).skip_norm_wip())
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                let field_jvm_ty = ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance);
                if field_jvm_ty.has_jvm_value() {
                    values.push((
                        enum_variant_field_name(variant, field_index, tcx),
                        read_zero_sized_constant(tcx, field_ty, oomir_data_types, instance)?,
                    ));
                }
            }
            Ok(instance_constant_with_declared_fields(
                class_name,
                values,
                oomir_data_types,
            ))
        }
        TyKind::Tuple(elements) => {
            let element_tys = elements.iter().collect::<Vec<_>>();
            if element_tys.is_empty() {
                return Ok(oomir::Constant::Unit);
            }
            let oomir::Type::Class(class_name) = oomir_ty else {
                return Err(format!("ZST tuple {ty:?} did not map to a class"));
            };
            let mut values = Vec::new();
            for (index, element_ty) in element_tys.into_iter().enumerate() {
                let element_jvm_ty = ty_to_oomir_type(element_ty, tcx, oomir_data_types, instance);
                if element_jvm_ty.has_jvm_value() {
                    values.push((
                        format!("field{index}"),
                        read_zero_sized_constant(tcx, element_ty, oomir_data_types, instance)?,
                    ));
                }
            }
            Ok(instance_constant_with_declared_fields(
                class_name,
                values,
                oomir_data_types,
            ))
        }
        TyKind::Array(element_ty, length) => {
            let length = length
                .try_to_target_usize(tcx)
                .ok_or_else(|| format!("ZST array length is not concrete for {ty:?}"))?;
            let element_jvm_ty = ty_to_oomir_type(*element_ty, tcx, oomir_data_types, instance);
            if !element_jvm_ty.has_jvm_value() {
                return Ok(oomir::Constant::Array(Box::new(element_jvm_ty), Vec::new()));
            }
            let Some(length) = usize::try_from(length)
                .ok()
                .filter(|length| *length <= i32::MAX as usize)
            else {
                // The array occupies no Rust storage, so its JVM carrier may
                // be elided even when its logical length exceeds JVM limits.
                // Borrow lowering restores that compile-time length in the
                // SliceView metadata rather than asking the backing array.
                return Ok(oomir::Constant::Array(Box::new(element_jvm_ty), Vec::new()));
            };
            let mut elements = Vec::with_capacity(length);
            for _ in 0..length {
                elements.push(read_zero_sized_constant(
                    tcx,
                    *element_ty,
                    oomir_data_types,
                    instance,
                )?);
            }
            Ok(oomir::Constant::Array(Box::new(element_jvm_ty), elements))
        }
        _ => Err(format!("Unsupported nominal ZST constant type: {ty:?}")),
    }
}

/// Converts a Rust MIR Scalar::Int into the appropriate OOMIR constant.
pub(crate) fn scalar_int_to_oomir_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    scalar_int: ScalarInt,
    ty: Ty<'tcx>,
) -> Result<oomir::Constant, String> {
    let bits = scalar_int.to_bits(scalar_int.size());
    let bit_width = scalar_int.size().bits() as u32;
    let signed = if bit_width == 128 {
        bits as i128
    } else {
        ((bits << (128 - bit_width)) as i128) >> (128 - bit_width)
    };

    let constant = match ty.kind() {
        TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Constant::I8(signed as i8),
            IntTy::I16 => oomir::Constant::I16(signed as i16),
            IntTy::I32 => oomir::Constant::I32(signed as i32),
            IntTy::Isize => oomir::Constant::I64(signed as i64),
            IntTy::I64 => oomir::Constant::I64(signed as i64),
            IntTy::I128 => {
                let param = oomir::Constant::String(signed.to_string());
                oomir::Constant::Instance {
                    class_name: crate::lower2::I128_CLASS.into(),
                    fields: HashMap::default(),
                    params: vec![param],
                    param_types: Vec::new(),
                }
            }
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Constant::U8(bits as u8),
            UintTy::U16 => oomir::Constant::U16(bits as u16),
            UintTy::U32 => oomir::Constant::U32(bits as u32),
            UintTy::Usize | UintTy::U64 => oomir::Constant::U64(bits as u64),
            UintTy::U128 => {
                let param = oomir::Constant::String(bits.to_string());
                oomir::Constant::Instance {
                    class_name: crate::lower2::U128_CLASS.into(),
                    fields: HashMap::default(),
                    params: vec![param],
                    param_types: Vec::new(),
                }
            }
        },
        TyKind::Bool => oomir::Constant::Boolean(scalar_int.try_to_bool().unwrap_or(false)),
        TyKind::Char => oomir::Constant::I32(scalar_int.to_u32() as i32),
        TyKind::Float(float_ty) => match float_ty {
            FloatTy::F16 => oomir::Constant::F16(scalar_int.to_u16()),
            FloatTy::F32 => oomir::Constant::F32(f32::from_bits(scalar_int.to_u32())),
            FloatTy::F64 => oomir::Constant::F64(f64::from_bits(scalar_int.to_u64())),
            FloatTy::F128 => {
                let bits = scalar_int.to_u128();
                oomir::Constant::Instance {
                    class_name: crate::lower2::F128_CLASS.into(),
                    fields: HashMap::default(),
                    params: vec![
                        oomir::Constant::I64((bits >> 64) as i64),
                        oomir::Constant::I64(bits as i64),
                    ],
                    param_types: Vec::new(),
                }
            }
        },
        TyKind::Str => oomir::Constant::Str(scalar_int.to_u64().to_string()),
        TyKind::RawPtr(..) | TyKind::Ref(..) => {
            return Err(format!(
                "Pointer scalar {ty:?} requires typed constant lowering context"
            ));
        }
        TyKind::Pat(base_ty, _) => return scalar_int_to_oomir_constant(tcx, scalar_int, *base_ty),
        _ => return Err(format!("Unsupported type for ScalarInt conversion: {ty:?}")),
    };
    Ok(constant)
}
