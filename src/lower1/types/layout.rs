use super::*;
use crate::lower1::context::Definitions;

pub(crate) fn layout_size_bytes<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Result<usize, String> {
    let ty = normalize_union_ty(tcx, ty)?;
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|err| format!("could not get layout for {:?}: {:?}", ty, err))?;
    Ok(layout.size.bytes_usize())
}

pub(crate) fn layout_align_bytes<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Result<usize, String> {
    let ty = normalize_union_ty(tcx, ty)?;
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|err| format!("could not get layout for {:?}: {:?}", ty, err))?;
    Ok(layout.align.abi.bytes_usize())
}

pub(super) fn normalize_union_ty<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
) -> Result<Ty<'tcx>, String> {
    tcx.try_normalize_erasing_regions(
        TypingEnv::fully_monomorphized(),
        rustc_middle::ty::Unnormalized::new_wip(ty),
    )
    .map_err(|err| format!("could not normalize union storage type {ty:?}: {err:?}"))
}

pub(super) fn resolve_union_ty<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<Ty<'tcx>, String> {
    let instantiated = rustc_middle::ty::EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance_context.args)
        .skip_norm_wip();
    Ok(normalize_union_ty(tcx, instantiated).unwrap_or(instantiated))
}

pub(super) fn needs_union_object_storage_inner<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    visiting: &mut HashSet<Ty<'tcx>>,
) -> Result<bool, String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    if layout_size_bytes(tcx, ty)? == 0 {
        return Ok(false);
    }
    if !visiting.insert(ty) {
        return Ok(true);
    }
    let needs_objects = match ty.kind() {
        TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_) => false,
        TyKind::Pat(inner, _) | TyKind::Array(inner, _) => {
            needs_union_object_storage_inner(*inner, tcx, instance_context, visiting)?
        }
        TyKind::Tuple(elements) => {
            let mut needs_objects = false;
            for element in elements.iter() {
                needs_objects |=
                    needs_union_object_storage_inner(element, tcx, instance_context, visiting)?;
                if needs_objects {
                    break;
                }
            }
            needs_objects
        }
        TyKind::Closure(_, args) => {
            let mut needs_objects = false;
            for capture in args.as_closure().upvar_tys() {
                needs_objects |=
                    needs_union_object_storage_inner(capture, tcx, instance_context, visiting)?;
                if needs_objects {
                    break;
                }
            }
            needs_objects
        }
        TyKind::Adt(adt_def, args)
            if adt_def.is_struct() || adt_def.is_enum() || adt_def.is_union() =>
        {
            let mut needs_objects = false;
            for field in adt_def
                .variants()
                .iter()
                .flat_map(|variant| variant.fields.iter())
            {
                let field_ty = field.ty(tcx, args).skip_norm_wip();
                needs_objects |=
                    needs_union_object_storage_inner(field_ty, tcx, instance_context, visiting)?;
                if needs_objects {
                    break;
                }
            }
            needs_objects
        }
        // These carriers retain JVM objects in addition to their encoded address.
        TyKind::RawPtr(_, _)
        | TyKind::Ref(_, _, _)
        | TyKind::FnPtr(..)
        | TyKind::Dynamic(..)
        | TyKind::Coroutine(..) => true,
        _ => true,
    };
    visiting.remove(&ty);
    Ok(needs_objects)
}

pub(super) fn union_object_storage_size<'tcx>(
    ty: Ty<'tcx>,
    rust_size: usize,
    tcx: TyCtxt<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> usize {
    let mut visiting = HashSet::default();
    if needs_union_object_storage_inner(ty, tcx, instance_context, &mut visiting).unwrap_or(true) {
        rust_size.max(1)
    } else {
        0
    }
}

pub(crate) fn simple_enum_union_size<'tcx>(
    adt_def: &AdtDef<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> Result<usize, String> {
    if !adt_def.is_enum() || adt_def.variants().is_empty() {
        return Err("only non-empty enums are supported in unions".to_string());
    }
    if adt_def
        .variants()
        .iter()
        .any(|variant| !variant.fields.is_empty())
    {
        return Err("only fieldless enums are supported in unions".to_string());
    }

    if !enum_union_discriminant_supported(adt_def, tcx) {
        return Err(
            "enum discriminant does not fit the JVM union integer representation".to_string(),
        );
    }

    let enum_ty = tcx
        .type_of(adt_def.did())
        .instantiate_identity()
        .skip_norm_wip();
    let size = layout_size_bytes(tcx, enum_ty)?;
    if !matches!(size, 1 | 2 | 4 | 8) {
        return Err(format!(
            "unsupported enum discriminant layout size: {} bytes",
            size
        ));
    }
    Ok(size)
}

pub(crate) fn enum_union_discriminant_supported<'tcx>(
    adt_def: &AdtDef<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> bool {
    let Some((_, discriminant)) = adt_def.discriminants(tcx).next() else {
        return false;
    };
    let discriminant_ty = discriminant.ty;
    match discriminant_ty.kind() {
        TyKind::Int(IntTy::I128) | TyKind::Uint(UintTy::U128) => false,
        TyKind::Int(_) | TyKind::Uint(_) => true,
        _ => false,
    }
}

pub(super) fn masked_enum_discriminant_bits(value: u128, size: usize) -> u128 {
    if size >= 16 {
        value
    } else {
        let mask = (1u128 << (size * 8)) - 1;
        value & mask
    }
}

pub(super) fn masked_enum_discriminant(value: u128, size: usize) -> i64 {
    masked_enum_discriminant_bits(value, size) as i64
}

pub(super) fn scalar_bits_type(rust_size: usize, oomir_ty: &oomir::Type) -> oomir::Type {
    if matches!(oomir_ty, oomir::Type::U64) {
        oomir::Type::U64
    } else if matches!(oomir_ty, oomir::Type::I64 | oomir::Type::F64) || rust_size > 4 {
        oomir::Type::I64
    } else {
        oomir::Type::I32
    }
}

pub(super) fn int_constant_for_type(value: i64, ty: &oomir::Type) -> oomir::Constant {
    if matches!(ty, oomir::Type::U64) {
        oomir::Constant::U64(value as u64)
    } else if matches!(ty, oomir::Type::I64) {
        oomir::Constant::I64(value)
    } else {
        oomir::Constant::I32(value as i32)
    }
}

pub(super) fn union_aggregate_layout<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<Option<UnionAggregateLayout<'tcx>>, String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|err| format!("could not get layout for {:?}: {:?}", ty, err))?;

    let (class_name, raw_fields): (String, Vec<(Ty<'tcx>, String, usize)>) = match ty.kind() {
        TyKind::Tuple(elements) if !elements.is_empty() => {
            let element_tys: Vec<_> = elements.iter().collect();
            let class_name =
                generate_tuple_jvm_class_name(&element_tys, tcx, data_types, instance_context);
            let fields = element_tys
                .into_iter()
                .enumerate()
                .map(|(index, field_ty)| {
                    (
                        field_ty,
                        format!("field{index}"),
                        layout
                            .fields
                            .offset(FieldIdx::from_usize(index).into())
                            .bytes_usize(),
                    )
                })
                .collect();
            (class_name, fields)
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            let class_name =
                generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance_context);
            let fields = adt_def
                .variant(VariantIdx::from_usize(0))
                .fields
                .iter()
                .enumerate()
                .map(|(index, field)| {
                    Ok((
                        resolve_union_ty(
                            tcx,
                            field.ty(tcx, substs).skip_norm_wip(),
                            instance_context,
                        )?,
                        field.ident(tcx).to_string(),
                        layout
                            .fields
                            .offset(FieldIdx::from_usize(index).into())
                            .bytes_usize(),
                    ))
                })
                .collect::<Result<Vec<_>, String>>()?;
            (class_name, fields)
        }
        TyKind::Closure(_, closure_args) => {
            let oomir::Type::Class(class_name) =
                ty_to_oomir_type(ty, tcx, data_types, instance_context)
            else {
                return Err(format!("closure {ty:?} did not map to a JVM class"));
            };
            let fields = closure_args
                .as_closure()
                .upvar_tys()
                .iter()
                .enumerate()
                .map(|(index, field_ty)| {
                    Ok((
                        resolve_union_ty(tcx, field_ty, instance_context)?,
                        format!("arg{index}"),
                        layout
                            .fields
                            .offset(FieldIdx::from_usize(index).into())
                            .bytes_usize(),
                    ))
                })
                .collect::<Result<Vec<_>, String>>()?;
            (class_name, fields)
        }
        _ => return Ok(None),
    };

    // The generated carrier is the ABI authority. In particular, closure
    // captures can contain function-item ZSTs whose already-instantiated type
    // must not be substituted again in the codec's enclosing instance.
    let defined_field_types = match data_types.get(&class_name) {
        Some(oomir::DataType::Class { fields, .. }) => {
            fields.iter().cloned().collect::<HashMap<_, _>>()
        }
        _ => HashMap::default(),
    };
    let mut fields = Vec::new();
    for (rust_ty, jvm_name, offset) in raw_fields {
        let jvm_ty = defined_field_types
            .get(&jvm_name)
            .cloned()
            .unwrap_or_else(|| ty_to_oomir_type(rust_ty, tcx, data_types, instance_context));
        if !jvm_ty.has_jvm_value() {
            continue;
        }
        fields.push(UnionAggregateField {
            rust_ty,
            jvm_ty,
            jvm_name,
            offset,
        });
    }
    Ok(Some(UnionAggregateLayout { class_name, fields }))
}

pub(super) fn emit_u128_constant(
    value: u128,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> oomir::Operand {
    let dest = next_union_temp("enum_u128_tag_constant", temp_counter);
    instructions.push(oomir::Instruction::ConstructObject {
        dest: dest.clone(),
        class_name: crate::lower2::U128_CLASS.to_string(),
        args: vec![
            (
                oomir::Operand::Constant(oomir::Constant::I64((value >> 64) as i64)),
                oomir::Type::I64,
            ),
            (
                oomir::Operand::Constant(oomir::Constant::I64(value as i64)),
                oomir::Type::I64,
            ),
        ],
    });
    operand_var(
        dest,
        oomir::Type::Class(crate::lower2::U128_CLASS.to_string()),
    )
}

pub(super) fn emit_u128_tag_to_union_bytes(
    value: u128,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) {
    for byte_index in 0..16 {
        let index = storage.byte_index(base_offset + byte_index, instructions, temp_counter);
        instructions.push(oomir::Instruction::ArrayStore {
            array: oomir::Operand::Variable {
                name: storage.bytes_var.clone(),
                ty: oomir::Type::Array(Box::new(oomir::Type::I8)),
            },
            index,
            value: oomir::Operand::Constant(oomir::Constant::I8((value >> (byte_index * 8)) as i8)),
            copy_value: false,
        });
    }
}

pub(super) fn emit_u128_tag_from_union_bytes(
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> oomir::Operand {
    let offset = storage.byte_index(base_offset, instructions, temp_counter);
    let dest = next_union_temp("enum_u128_tag", temp_counter);
    let ty = oomir::Type::Class(crate::lower2::U128_CLASS.to_string());
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest.clone()),
        class_name: MEMORY_BYTES_CLASS.into(),
        method_name: "readU128".into(),
        method_ty: oomir::Signature {
            params: vec![
                ("bytes".to_string(), byte_array_type()),
                ("offset".to_string(), oomir::Type::I32),
            ],
            ret: Box::new(ty.clone()),
            is_static: true,
        },
        args: vec![
            operand_var(storage.bytes_var.clone(), byte_array_type()),
            offset,
        ],
    });
    operand_var(dest, ty)
}

pub(super) fn insert_u128_enum_dispatch(
    storage: &JvmUnionStorage,
    offset: usize,
    comparisons: Vec<(u128, String)>,
    otherwise: String,
    basic_blocks: &mut HashMap<String, oomir::BasicBlock>,
) -> String {
    let entry = "wide_tag_check_0".to_string();
    let mut initial_instructions = Vec::new();
    let mut temp_counter = 0;
    let tag = emit_u128_tag_from_union_bytes(
        storage,
        offset,
        &mut initial_instructions,
        &mut temp_counter,
    );
    if comparisons.is_empty() {
        initial_instructions.push(oomir::Instruction::Jump { target: otherwise });
        basic_blocks.insert(
            entry.clone(),
            oomir::BasicBlock {
                label: entry.clone(),
                instructions: initial_instructions,
            },
        );
        return entry;
    }

    let comparison_count = comparisons.len();
    for (index, (expected, target)) in comparisons.into_iter().enumerate() {
        let block_name = format!("wide_tag_check_{index}");
        let mut instructions = if index == 0 {
            std::mem::take(&mut initial_instructions)
        } else {
            Vec::new()
        };
        let expected = emit_u128_constant(expected, &mut instructions, &mut temp_counter);
        let matches = next_union_temp("enum_u128_tag_matches", &mut temp_counter);
        instructions.push(oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Eq,
            dest: matches.clone(),
            op1: tag.clone(),
            op2: expected,
        });
        instructions.push(oomir::Instruction::Branch {
            condition: operand_var(matches, oomir::Type::Boolean),
            true_block: target,
            false_block: if index + 1 == comparison_count {
                otherwise.clone()
            } else {
                format!("wide_tag_check_{}", index + 1)
            },
        });
        basic_blocks.insert(
            block_name.clone(),
            oomir::BasicBlock {
                label: block_name,
                instructions,
            },
        );
    }
    entry
}

pub(super) fn union_enum_tag<'tcx>(
    layout: &TyAndLayout<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> Result<UnionEnumTag, String> {
    match &layout.variants {
        Variants::Single { index } => Ok(UnionEnumTag::Single { variant: *index }),
        Variants::Multiple {
            tag,
            tag_encoding,
            tag_field,
            ..
        } => {
            let size = tag.size(&tcx.data_layout).bytes_usize();
            if size == 0 || size > 16 {
                return Err(format!("unsupported enum tag size: {size} bytes"));
            }
            let offset = layout.fields.offset((*tag_field).into()).bytes_usize();
            match tag_encoding {
                TagEncoding::Direct => Ok(UnionEnumTag::Direct { offset, size }),
                TagEncoding::Niche {
                    untagged_variant,
                    niche_variants,
                    niche_start,
                } => Ok(UnionEnumTag::Niche {
                    offset,
                    size,
                    untagged_variant: *untagged_variant,
                    niche_start_variant: niche_variants.start,
                    niche_end_variant: niche_variants.last,
                    niche_start: *niche_start,
                }),
            }
        }
        Variants::Empty => Err("uninhabited enums cannot be stored in unions".to_string()),
    }
}

pub(super) fn union_enum_variant_layout<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_class: &str,
    layout: &TyAndLayout<'tcx>,
    variant_idx: VariantIdx,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<UnionAggregateLayout<'tcx>, String> {
    let variant = adt_def.variant(variant_idx);
    let variant_class = format!(
        "{}${}",
        enum_class,
        jvm_names::member_name(&variant.name.to_string())
    );
    let mut fields = Vec::new();
    for (field_index, field) in variant.fields.iter().enumerate() {
        let rust_ty =
            resolve_union_ty(tcx, field.ty(tcx, substs).skip_norm_wip(), instance_context)?;
        let jvm_ty = ty_to_oomir_type(rust_ty, tcx, data_types, instance_context);
        if !jvm_ty.has_jvm_value() {
            continue;
        }
        let offset = match &layout.variants {
            Variants::Single { .. } => layout.fields.offset(field_index).bytes_usize(),
            Variants::Multiple { variants, .. } => {
                variants[variant_idx].field_offsets[FieldIdx::from_usize(field_index)].bytes_usize()
            }
            Variants::Empty => unreachable!(),
        };
        fields.push(UnionAggregateField {
            rust_ty,
            jvm_ty,
            jvm_name: enum_variant_field_name(variant, field_index, tcx),
            offset,
        });
    }
    Ok(UnionAggregateLayout {
        class_name: variant_class,
        fields,
    })
}

pub(super) fn enum_union_write_signature(receiver_class: &str) -> oomir::Signature {
    oomir::Signature {
        params: vec![
            (
                "self".to_string(),
                oomir::Type::Class(receiver_class.to_string()),
            ),
            ("bytes".to_string(), byte_array_type()),
            ("objects".to_string(), object_array_type()),
            ("offset".to_string(), oomir::Type::I32),
        ],
        ret: Box::new(oomir::Type::Void),
        is_static: false,
    }
}

pub(super) fn enum_union_read_signature(enum_class: &str) -> oomir::Signature {
    oomir::Signature {
        params: vec![
            ("bytes".to_string(), byte_array_type()),
            ("objects".to_string(), object_array_type()),
            ("offset".to_string(), oomir::Type::I32),
        ],
        ret: Box::new(oomir::Type::Class(enum_class.to_string())),
        is_static: true,
    }
}

pub(super) fn exact_bytes_supported<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<(), String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    // ZSTs contribute no bits to an enclosing layout. Their nominal JVM
    // carriers are reconstructed when decoding, so every materializable ZST is
    // safe to traverse without requiring a byte codec of its own.
    if layout_size_bytes(tcx, ty)? == 0 {
        return Ok(());
    }
    match ty.kind() {
        TyKind::Bool
        | TyKind::Char
        | TyKind::Int(
            IntTy::I8 | IntTy::I16 | IntTy::I32 | IntTy::I64 | IntTy::I128 | IntTy::Isize,
        )
        | TyKind::Uint(
            UintTy::U8 | UintTy::U16 | UintTy::U32 | UintTy::U64 | UintTy::U128 | UintTy::Usize,
        )
        | TyKind::Float(FloatTy::F16 | FloatTy::F32 | FloatTy::F64 | FloatTy::F128) => Ok(()),
        TyKind::RawPtr(_, _) | TyKind::Ref(_, _, _) | TyKind::FnPtr(..) => Ok(()),
        TyKind::Pat(inner, _) => exact_bytes_supported(*inner, tcx, instance_context),
        TyKind::Tuple(elements) => elements
            .iter()
            .try_for_each(|element| exact_bytes_supported(element, tcx, instance_context)),
        TyKind::Closure(_, closure_args) => closure_args
            .as_closure()
            .upvar_tys()
            .iter()
            .try_for_each(|capture| exact_bytes_supported(capture, tcx, instance_context)),
        TyKind::Coroutine(def_id, args) => {
            args.as_coroutine()
                .upvar_tys()
                .iter()
                .try_for_each(|capture| exact_bytes_supported(capture, tcx, instance_context))?;
            tcx.coroutine_layout(*def_id, args)
                .map_err(|error| format!("could not get coroutine layout for {ty:?}: {error:?}"))?
                .field_tys
                .iter()
                .try_for_each(|saved| {
                    let saved_ty = EarlyBinder::bind(tcx, saved.ty)
                        .instantiate(tcx, args)
                        .skip_norm_wip();
                    exact_bytes_supported(saved_ty, tcx, instance_context)
                })
        }
        TyKind::Array(element, length) => {
            length
                .try_to_target_usize(tcx)
                .ok_or_else(|| format!("array length is not concrete for {ty:?}"))?;
            exact_bytes_supported(*element, tcx, instance_context)
        }
        TyKind::Adt(adt_def, substs)
            if adt_def.is_struct() || adt_def.is_enum() || adt_def.is_union() =>
        {
            adt_def
                .variants()
                .iter()
                .flat_map(|variant| variant.fields.iter())
                .try_for_each(|field| {
                    let field_ty = resolve_union_ty(
                        tcx,
                        field.ty(tcx, substs).skip_norm_wip(),
                        instance_context,
                    )?;
                    if matches!(field_ty.kind(), TyKind::Dynamic(..)) {
                        Ok(())
                    } else {
                        exact_bytes_supported(field_ty, tcx, instance_context)
                    }
                })
        }
        _ => Err(format!(
            "type {ty:?} is not fully byte-addressable; references, raw pointers, and JVM object carriers need an explicit bit representation"
        )),
    }
}
