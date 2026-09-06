use super::*;
use crate::lower1::context::Definitions;

pub(super) fn coroutine_memory_layout<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<CoroutineMemoryLayout<'tcx>, String> {
    let TyKind::Coroutine(def_id, args) = ty.kind() else {
        return Err(format!("expected a coroutine, found {ty:?}"));
    };
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|error| format!("could not get coroutine layout for {ty:?}: {error:?}"))?;
    let coroutine = tcx
        .coroutine_layout(*def_id, args)
        .map_err(|error| format!("could not get coroutine fields for {ty:?}: {error:?}"))?;
    let Variants::Multiple {
        tag,
        tag_encoding: TagEncoding::Direct,
        tag_field,
        variants,
    } = &layout.variants
    else {
        return Err(format!(
            "coroutine {ty:?} does not have a direct state-tag layout"
        ));
    };
    let oomir::Type::Class(class_name) = ty_to_oomir_type(ty, tcx, data_types, instance_context)
    else {
        return Err(format!("coroutine {ty:?} did not map to a JVM class"));
    };
    let defined_fields = match data_types.get(&class_name) {
        Some(oomir::DataType::Class { fields, .. }) => {
            fields.iter().cloned().collect::<HashMap<_, _>>()
        }
        _ => return Err(format!("coroutine carrier {class_name} was not defined")),
    };

    let mut upvars = Vec::new();
    for (index, upvar_ty) in args.as_coroutine().upvar_tys().iter().enumerate() {
        let rust_ty = resolve_union_ty(tcx, upvar_ty, instance_context)?;
        let jvm_name = format!("arg{index}");
        let jvm_ty = defined_fields
            .get(&jvm_name)
            .cloned()
            .unwrap_or_else(|| ty_to_oomir_type(rust_ty, tcx, data_types, instance_context));
        if !jvm_ty.has_jvm_value() {
            continue;
        }
        upvars.push(UnionAggregateField {
            rust_ty,
            jvm_ty,
            jvm_name,
            offset: layout
                .fields
                .offset(FieldIdx::from_usize(index).into())
                .bytes_usize(),
        });
    }

    let mut saved = Vec::new();
    let mut saved_indices = HashMap::default();
    for (saved_local, saved_ty) in coroutine.field_tys.iter_enumerated() {
        let rust_ty = resolve_union_ty(
            tcx,
            EarlyBinder::bind(tcx, saved_ty.ty)
                .instantiate(tcx, args)
                .skip_norm_wip(),
            instance_context,
        )?;
        let jvm_name = format!("state{}", saved_local.as_usize());
        let jvm_ty = defined_fields
            .get(&jvm_name)
            .cloned()
            .unwrap_or_else(|| ty_to_oomir_type(rust_ty, tcx, data_types, instance_context));
        if !jvm_ty.has_jvm_value() {
            continue;
        }
        saved_indices.insert(saved_local.as_usize(), saved.len());
        saved.push(UnionAggregateField {
            rust_ty,
            jvm_ty,
            jvm_name,
            offset: 0,
        });
    }

    let variant_saved_offsets = coroutine
        .variant_fields
        .iter_enumerated()
        .map(|(variant, fields)| {
            fields
                .iter_enumerated()
                .filter_map(|(field, saved_local)| {
                    saved_indices
                        .get(&saved_local.as_usize())
                        .map(|saved_index| {
                            (
                                *saved_index,
                                variants[variant].field_offsets[field].bytes_usize(),
                            )
                        })
                })
                .collect()
        })
        .collect();

    Ok(CoroutineMemoryLayout {
        class_name,
        size: layout.size.bytes_usize(),
        state_offset: layout.fields.offset((*tag_field).into()).bytes_usize(),
        state_size: tag.size(&tcx.data_layout).bytes_usize(),
        upvars,
        saved,
        variant_saved_offsets,
    })
}

pub(super) fn coroutine_codec_function(
    name: &str,
    signature: oomir::Signature,
    entry: Vec<oomir::Instruction>,
    variant_blocks: Vec<Vec<oomir::Instruction>>,
) -> oomir::Function {
    let targets = variant_blocks
        .iter()
        .enumerate()
        .map(|(index, _)| {
            (
                oomir::Constant::I32(index as i32),
                format!("variant_{index}"),
            )
        })
        .collect();
    let mut entry = entry;
    entry.push(oomir::Instruction::Switch {
        discr: operand_var("_coroutine_state", oomir::Type::I32),
        targets,
        otherwise: "invalid_state".to_string(),
    });
    let mut basic_blocks = HashMap::from_iter([
        (
            "entry".to_string(),
            oomir::BasicBlock {
                label: "entry".to_string(),
                instructions: entry,
            },
        ),
        (
            "invalid_state".to_string(),
            oomir::BasicBlock {
                label: "invalid_state".to_string(),
                instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/IllegalStateException".to_string(),
                    message: "invalid Rust coroutine state".to_string(),
                }],
            },
        ),
    ]);
    for (index, instructions) in variant_blocks.into_iter().enumerate() {
        let label = format!("variant_{index}");
        basic_blocks.insert(
            label.clone(),
            oomir::BasicBlock {
                label,
                instructions,
            },
        );
    }
    oomir::Function {
        name: name.to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature,
        body: oomir::CodeBlock {
            entry: "entry".to_string(),
            basic_blocks,
        }
        .into(),
    }
}

pub(super) fn coroutine_pointer_codec_methods<'tcx>(
    ty: Ty<'tcx>,
    value_ty: oomir::Type,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<HashMap<String, DataTypeMethod>, String> {
    let layout = coroutine_memory_layout(ty, tcx, data_types, instance_context)?;
    let bytes_ty = byte_array_type();
    let mut encode_entry = vec![
        oomir::Instruction::NewArray {
            dest: "_bytes".to_string(),
            element_type: oomir::Type::I8,
            size: oomir::Operand::Constant(oomir::Constant::I32(layout.size as i32)),
        },
        oomir::Instruction::NewArray {
            dest: "_objects".to_string(),
            element_type: oomir::Type::Class("java/lang/Object".to_string()),
            size: oomir::Operand::Constant(oomir::Constant::I32(layout.size.max(1) as i32)),
        },
    ];
    let storage = JvmUnionStorage::at_start("_bytes", "_objects");
    let mut counter = 0;
    emit_aggregate_to_union_bytes(
        &UnionAggregateLayout {
            class_name: layout.class_name.clone(),
            fields: layout.upvars.clone(),
        },
        operand_var("_1", value_ty.clone()),
        &storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut encode_entry,
        &mut counter,
    )?;
    encode_entry.push(oomir::Instruction::GetField {
        dest: "_coroutine_state".to_string(),
        object: operand_var("_1", value_ty.clone()),
        field_name: "__state".to_string(),
        field_ty: oomir::Type::I32,
        owner_class: layout.class_name.clone(),
    });
    emit_bits_to_union_bytes(
        operand_var("_coroutine_state", oomir::Type::I32),
        layout.state_size,
        &storage,
        layout.state_offset,
        &mut encode_entry,
        &mut counter,
    )?;
    let mut encode_variants = Vec::new();
    for active in &layout.variant_saved_offsets {
        let fields = active
            .iter()
            .map(|(saved_index, offset)| {
                let mut field = layout.saved[*saved_index].clone();
                field.offset = *offset;
                field
            })
            .collect();
        let mut instructions = Vec::new();
        emit_aggregate_to_union_bytes(
            &UnionAggregateLayout {
                class_name: layout.class_name.clone(),
                fields,
            },
            operand_var("_1", value_ty.clone()),
            &storage,
            0,
            tcx,
            data_types,
            instance_context,
            &mut instructions,
            &mut counter,
        )?;
        instructions.push(oomir::Instruction::Return {
            operand: Some(operand_var("_bytes", bytes_ty.clone())),
        });
        encode_variants.push(instructions);
    }
    let encode = coroutine_codec_function(
        "encode",
        oomir::Signature {
            params: vec![("value".to_string(), value_ty.clone())],
            ret: Box::new(bytes_ty.clone()),
            is_static: true,
        },
        encode_entry,
        encode_variants,
    );

    let mut decode_entry = vec![oomir::Instruction::NewArray {
        dest: "_objects".to_string(),
        element_type: oomir::Type::Class("java/lang/Object".to_string()),
        size: oomir::Operand::Constant(oomir::Constant::I32(layout.size.max(1) as i32)),
    }];
    let decode_storage = JvmUnionStorage::at_start("_1", "_objects");
    let decoded_state = emit_bits_from_union_bytes(
        oomir::Type::I32,
        layout.state_size,
        &decode_storage,
        layout.state_offset,
        &mut decode_entry,
        &mut counter,
    );
    decode_entry.push(oomir::Instruction::Move {
        dest: "_coroutine_state".to_string(),
        src: decoded_state,
    });
    let mut decode_variants = Vec::new();
    for active in &layout.variant_saved_offsets {
        let active = active.iter().copied().collect::<HashMap<_, _>>();
        let mut instructions = Vec::new();
        let mut constructor_args = Vec::new();
        for field in &layout.upvars {
            let value = emit_ty_from_union_bytes(
                field.rust_ty,
                &decode_storage,
                field.offset,
                tcx,
                data_types,
                instance_context,
                &mut instructions,
                &mut counter,
            )?;
            constructor_args.push((value, field.jvm_ty.clone()));
        }
        constructor_args.push((
            operand_var("_coroutine_state", oomir::Type::I32),
            oomir::Type::I32,
        ));
        for (saved_index, field) in layout.saved.iter().enumerate() {
            let value = if let Some(offset) = active.get(&saved_index) {
                emit_ty_from_union_bytes(
                    field.rust_ty,
                    &decode_storage,
                    *offset,
                    tcx,
                    data_types,
                    instance_context,
                    &mut instructions,
                    &mut counter,
                )?
            } else {
                default_operand_for_codec(&field.jvm_ty)
            };
            constructor_args.push((value, field.jvm_ty.clone()));
        }
        instructions.push(oomir::Instruction::ConstructObject {
            dest: "_coroutine_value".to_string(),
            class_name: layout.class_name.clone(),
            args: constructor_args,
        });
        instructions.push(oomir::Instruction::Return {
            operand: Some(operand_var("_coroutine_value", value_ty.clone())),
        });
        decode_variants.push(instructions);
    }
    let decode = coroutine_codec_function(
        "decode",
        oomir::Signature {
            params: vec![("bytes".to_string(), bytes_ty)],
            ret: Box::new(value_ty.clone()),
            is_static: true,
        },
        decode_entry,
        decode_variants,
    );

    let pointer_ty = oomir::Type::Pointer(Box::new(value_ty.clone()));
    let mut bind_entry = Vec::new();
    emit_aggregate_memory_bindings(
        &UnionAggregateLayout {
            class_name: layout.class_name.clone(),
            fields: layout.upvars.clone(),
        },
        operand_var("_1", pointer_ty.clone()),
        operand_var("_2", value_ty.clone()),
        tcx,
        data_types,
        instance_context,
        &mut bind_entry,
        &mut counter,
    )?;
    bind_entry.push(oomir::Instruction::GetField {
        dest: "_coroutine_state".to_string(),
        object: operand_var("_2", value_ty.clone()),
        field_name: "__state".to_string(),
        field_ty: oomir::Type::I32,
        owner_class: layout.class_name.clone(),
    });
    let mut bind_variants = Vec::new();
    for active in &layout.variant_saved_offsets {
        let fields = active
            .iter()
            .map(|(saved_index, offset)| {
                let mut field = layout.saved[*saved_index].clone();
                field.offset = *offset;
                field
            })
            .collect();
        let mut instructions = Vec::new();
        emit_aggregate_memory_bindings(
            &UnionAggregateLayout {
                class_name: layout.class_name.clone(),
                fields,
            },
            operand_var("_1", pointer_ty.clone()),
            operand_var("_2", value_ty.clone()),
            tcx,
            data_types,
            instance_context,
            &mut instructions,
            &mut counter,
        )?;
        instructions.push(oomir::Instruction::Return { operand: None });
        bind_variants.push(instructions);
    }
    let bind = coroutine_codec_function(
        "bind",
        oomir::Signature {
            params: vec![
                ("pointer".to_string(), pointer_ty),
                ("value".to_string(), value_ty),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        bind_entry,
        bind_variants,
    );
    Ok(HashMap::from_iter([
        ("encode".to_string(), DataTypeMethod::Function(encode)),
        ("decode".to_string(), DataTypeMethod::Function(decode)),
        ("bind".to_string(), DataTypeMethod::Function(bind)),
    ]))
}
