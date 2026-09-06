use super::*;
use crate::lower1::context::Definitions;

pub(super) fn enum_variant_union_writer<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_class: &str,
    layout: &TyAndLayout<'tcx>,
    tag: &UnionEnumTag,
    variant_idx: VariantIdx,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<(String, oomir::Function), String> {
    let variant = adt_def.variant(variant_idx);
    let transparent_payload = jvm_subtype_payload_ty(adt_def, variant, substs, tcx);
    let variant_layout = transparent_payload
        .is_none()
        .then(|| {
            union_enum_variant_layout(
                adt_def,
                substs,
                enum_class,
                layout,
                variant_idx,
                tcx,
                data_types,
                instance_context,
            )
        })
        .transpose()?;
    let receiver_class = if let Some(payload_ty) = transparent_payload {
        ty_to_oomir_type(payload_ty, tcx, data_types, instance_context)
            .get_class_name()
            .ok_or_else(|| "transparent enum subtype payload is not a JVM reference".to_string())?
            .to_string()
    } else {
        variant_layout
            .as_ref()
            .expect("ordinary enum variant layout disappeared")
            .class_name
            .clone()
    };
    let storage = JvmUnionStorage::at_offset("_2", "_3", operand_var("_4", oomir::Type::I32));
    let mut instructions = Vec::new();
    let mut temp_counter = 0;

    if let Some(payload_ty) = transparent_payload {
        let payload_offset = match &layout.variants {
            Variants::Single { .. } => layout.fields.offset(0).bytes_usize(),
            Variants::Multiple { variants, .. } => {
                variants[variant_idx].field_offsets[FieldIdx::from_usize(0)].bytes_usize()
            }
            Variants::Empty => unreachable!(),
        };
        emit_ty_to_union_bytes(
            payload_ty,
            operand_var("_1", oomir::Type::Class(receiver_class.clone())),
            &storage,
            payload_offset,
            tcx,
            data_types,
            instance_context,
            &mut instructions,
            &mut temp_counter,
        )?;
    } else {
        let variant_layout = variant_layout
            .as_ref()
            .expect("ordinary enum variant layout disappeared");
        emit_aggregate_to_union_bytes(
            variant_layout,
            operand_var("_1", oomir::Type::Class(receiver_class.clone())),
            &storage,
            0,
            tcx,
            data_types,
            instance_context,
            &mut instructions,
            &mut temp_counter,
        )?;
    }

    let tag_value = match tag {
        UnionEnumTag::Single { .. } => None,
        UnionEnumTag::Direct { offset, size } => {
            let discriminant = adt_def
                .discriminants(tcx)
                .find(|(index, _)| *index == variant_idx)
                .ok_or_else(|| format!("missing discriminant for variant {variant_idx:?}"))?
                .1;
            Some((
                *offset,
                *size,
                masked_enum_discriminant_bits(discriminant.val, *size),
            ))
        }
        UnionEnumTag::Niche {
            offset,
            size,
            untagged_variant,
            niche_start_variant,
            niche_end_variant,
            niche_start,
        } if variant_idx != *untagged_variant => {
            if variant_idx < *niche_start_variant || variant_idx > *niche_end_variant {
                return Err(format!(
                    "variant {variant_idx:?} is neither the untagged nor a niche variant"
                ));
            }
            let relative = variant_idx.as_u32() - niche_start_variant.as_u32();
            Some((
                *offset,
                *size,
                masked_enum_discriminant_bits(niche_start.wrapping_add(relative.into()), *size),
            ))
        }
        UnionEnumTag::Niche { .. } => None,
    };
    if let Some((offset, size, value)) = tag_value {
        if size == 16 {
            emit_u128_tag_to_union_bytes(
                value,
                &storage,
                offset,
                &mut instructions,
                &mut temp_counter,
            );
        } else {
            emit_bits_to_union_bytes(
                oomir::Operand::Constant(oomir::Constant::I64(value as i64)),
                size,
                &storage,
                offset,
                &mut instructions,
                &mut temp_counter,
            )?;
        }
    }
    instructions.push(oomir::Instruction::Return { operand: None });

    Ok((
        receiver_class.clone(),
        oomir::Function {
            name: enum_scoped_method_name(enum_class, ENUM_WRITE_UNION_STORAGE_METHOD),
            owner_class: None,
            debug_variables: Vec::new(),
            signature: enum_union_write_signature(&receiver_class),
            body: simple_body(instructions).into(),
        },
    ))
}

pub(super) fn enum_union_reader<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_class: &str,
    layout: &TyAndLayout<'tcx>,
    tag: &UnionEnumTag,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<oomir::Function, String> {
    let storage = JvmUnionStorage::at_offset("_1", "_2", operand_var("_3", oomir::Type::I32));
    let mut basic_blocks = HashMap::default();

    for variant_index in 0..adt_def.variants().len() {
        let variant_idx = VariantIdx::from_usize(variant_index);
        let mut instructions = Vec::new();
        let mut temp_counter = variant_idx.as_usize() * 10_000;
        let variant = adt_def.variant(variant_idx);
        let value = if let Some(payload_ty) = jvm_subtype_payload_ty(adt_def, variant, substs, tcx)
        {
            let payload_offset = match &layout.variants {
                Variants::Single { .. } => layout.fields.offset(0).bytes_usize(),
                Variants::Multiple { variants, .. } => {
                    variants[variant_idx].field_offsets[FieldIdx::from_usize(0)].bytes_usize()
                }
                Variants::Empty => unreachable!(),
            };
            let payload = emit_ty_from_union_bytes(
                payload_ty,
                &storage,
                payload_offset,
                tcx,
                data_types,
                instance_context,
                &mut instructions,
                &mut temp_counter,
            )?;
            let dest = next_union_temp("transparent_enum_value", &mut temp_counter);
            instructions.push(oomir::Instruction::Cast {
                op: payload,
                ty: oomir::Type::Class(enum_class.to_string()),
                dest: dest.clone(),
            });
            operand_var(dest, oomir::Type::Class(enum_class.to_string()))
        } else {
            let variant_layout = union_enum_variant_layout(
                adt_def,
                substs,
                enum_class,
                layout,
                variant_idx,
                tcx,
                data_types,
                instance_context,
            )?;
            emit_aggregate_from_union_bytes(
                &variant_layout,
                &storage,
                0,
                tcx,
                data_types,
                instance_context,
                &mut instructions,
                &mut temp_counter,
            )?
        };
        instructions.push(oomir::Instruction::Return {
            operand: Some(value),
        });
        let block_name = format!("variant_{}", variant_idx.as_u32());
        basic_blocks.insert(
            block_name.clone(),
            oomir::BasicBlock {
                label: block_name,
                instructions,
            },
        );
    }

    let entry = match tag {
        UnionEnumTag::Single { variant } => format!("variant_{}", variant.as_u32()),
        UnionEnumTag::Direct { offset, size } if *size == 16 => {
            let targets = adt_def
                .discriminants(tcx)
                .map(|(variant, discriminant)| {
                    (
                        masked_enum_discriminant_bits(discriminant.val, *size),
                        format!("variant_{}", variant.as_u32()),
                    )
                })
                .collect();
            basic_blocks.insert(
                "invalid".to_string(),
                oomir::BasicBlock {
                    label: "invalid".to_string(),
                    instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                        exception_class: "java/lang/IllegalArgumentException".to_string(),
                        message: format!(
                            "invalid discriminant while reading enum {enum_class} from union storage"
                        ),
                    }],
                },
            );
            insert_u128_enum_dispatch(
                &storage,
                *offset,
                targets,
                "invalid".to_string(),
                &mut basic_blocks,
            )
        }
        UnionEnumTag::Direct { offset, size } => {
            let mut instructions = Vec::new();
            let mut temp_counter = 0;
            let discriminant = emit_bits_from_union_bytes(
                oomir::Type::I64,
                *size,
                &storage,
                *offset,
                &mut instructions,
                &mut temp_counter,
            );
            let targets = adt_def
                .discriminants(tcx)
                .map(|(variant, discriminant)| {
                    (
                        oomir::Constant::I64(masked_enum_discriminant(discriminant.val, *size)),
                        format!("variant_{}", variant.as_u32()),
                    )
                })
                .collect();
            instructions.push(oomir::Instruction::Switch {
                discr: discriminant,
                targets,
                otherwise: "invalid".to_string(),
            });
            basic_blocks.insert(
                "entry".to_string(),
                oomir::BasicBlock {
                    label: "entry".to_string(),
                    instructions,
                },
            );
            basic_blocks.insert(
                "invalid".to_string(),
                oomir::BasicBlock {
                    label: "invalid".to_string(),
                    instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                        exception_class: "java/lang/IllegalArgumentException".to_string(),
                        message: format!(
                            "invalid discriminant while reading enum {enum_class} from union storage"
                        ),
                    }],
                },
            );
            "entry".to_string()
        }
        UnionEnumTag::Niche {
            offset,
            size,
            untagged_variant,
            niche_start_variant,
            niche_end_variant,
            niche_start,
        } if *size == 16 => {
            let targets = (niche_start_variant.as_u32()..=niche_end_variant.as_u32())
                .map(|variant| {
                    let relative = variant - niche_start_variant.as_u32();
                    (
                        masked_enum_discriminant_bits(
                            niche_start.wrapping_add(relative.into()),
                            *size,
                        ),
                        format!("variant_{variant}"),
                    )
                })
                .collect();
            insert_u128_enum_dispatch(
                &storage,
                *offset,
                targets,
                format!("variant_{}", untagged_variant.as_u32()),
                &mut basic_blocks,
            )
        }
        UnionEnumTag::Niche {
            offset,
            size,
            untagged_variant,
            niche_start_variant,
            niche_end_variant,
            niche_start,
        } => {
            let mut instructions = Vec::new();
            let mut temp_counter = 0;
            let niche = emit_bits_from_union_bytes(
                oomir::Type::I64,
                *size,
                &storage,
                *offset,
                &mut instructions,
                &mut temp_counter,
            );
            let targets = (niche_start_variant.as_u32()..=niche_end_variant.as_u32())
                .map(|variant| {
                    let relative = variant - niche_start_variant.as_u32();
                    (
                        oomir::Constant::I64(masked_enum_discriminant(
                            niche_start.wrapping_add(relative.into()),
                            *size,
                        )),
                        format!("variant_{variant}"),
                    )
                })
                .collect();
            instructions.push(oomir::Instruction::Switch {
                discr: niche,
                targets,
                otherwise: format!("variant_{}", untagged_variant.as_u32()),
            });
            basic_blocks.insert(
                "entry".to_string(),
                oomir::BasicBlock {
                    label: "entry".to_string(),
                    instructions,
                },
            );
            "entry".to_string()
        }
    };

    Ok(oomir::Function {
        name: ENUM_READ_UNION_STORAGE_METHOD.to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: enum_union_read_signature(enum_class),
        body: oomir::CodeBlock {
            entry,
            basic_blocks,
        }
        .into(),
    })
}

pub(super) fn ensure_enum_union_codec<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_ty: Ty<'tcx>,
    enum_class: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<(), String> {
    let writer_method = enum_scoped_method_name(enum_class, ENUM_WRITE_UNION_STORAGE_METHOD);
    if !data_types.contains_key(enum_class) {
        ensure_enum_data_types(
            adt_def,
            substs,
            enum_class,
            tcx,
            data_types,
            instance_context,
        );
    }
    if let Some(DataType::Interface { methods, .. }) = data_types.get(enum_class) {
        if methods.contains_key(ENUM_READ_UNION_STORAGE_METHOD) {
            return Ok(());
        }
        if methods.contains_key(&writer_method) {
            // The writer is installed before recursively constructing variant
            // codecs. Encountering it without the reader means this enum is
            // already being generated through a recursive field.
            return Ok(());
        }
    }

    if let Some(DataType::Interface { methods, .. }) = data_types.get_mut(enum_class) {
        let mut abstract_writer_signature = enum_union_write_signature(enum_class);
        abstract_writer_signature.params.remove(0);
        methods.insert(
            writer_method.clone(),
            DataTypeMethod::Abstract(abstract_writer_signature),
        );
    } else {
        return Err(format!("enum JVM interface {enum_class} was not defined"));
    }

    let generated = (|| {
        let layout = tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(enum_ty))
            .map_err(|err| format!("could not get layout for {enum_ty:?}: {err:?}"))?;
        let tag = union_enum_tag(&layout, tcx)?;
        let mut writers = Vec::new();
        for variant_index in 0..adt_def.variants().len() {
            let variant_idx = VariantIdx::from_usize(variant_index);
            writers.push(enum_variant_union_writer(
                adt_def,
                substs,
                enum_class,
                &layout,
                &tag,
                variant_idx,
                tcx,
                data_types,
                instance_context,
            )?);
        }
        let reader = enum_union_reader(
            adt_def,
            substs,
            enum_class,
            &layout,
            &tag,
            tcx,
            data_types,
            instance_context,
        )?;
        Ok::<_, String>((writers, reader))
    })();
    let (writers, reader) = match generated {
        Ok(generated) => generated,
        Err(error) => {
            if let Some(DataType::Interface { methods, .. }) = data_types.get_mut(enum_class) {
                methods.remove(&writer_method);
            }
            return Err(error);
        }
    };

    for (variant_class, writer) in writers {
        match data_types.get_mut(&variant_class) {
            Some(DataType::Class { methods, .. }) | Some(DataType::Interface { methods, .. }) => {
                methods.insert(writer_method.clone(), DataTypeMethod::Function(writer));
            }
            None => {
                return Err(format!(
                    "enum variant JVM type {variant_class} was not defined"
                ));
            }
        }
    }
    let Some(DataType::Interface { methods, .. }) = data_types.get_mut(enum_class) else {
        return Err(format!("enum JVM interface {enum_class} was not defined"));
    };
    methods.insert(
        ENUM_READ_UNION_STORAGE_METHOD.to_string(),
        DataTypeMethod::Function(reader),
    );
    Ok(())
}
