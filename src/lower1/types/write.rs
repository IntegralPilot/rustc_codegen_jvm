use super::*;
use crate::lower1::context::Definitions;

pub(super) fn emit_ty_to_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    if layout_size_bytes(tcx, ty)? == 0 {
        return Ok(());
    }
    if is_direct_union_scalar(ty) {
        return emit_scalar_to_union_bytes(
            ty,
            source,
            storage,
            base_offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        );
    }
    if let Some(codec) = fat_pointer_codec_operand(ty, tcx, data_types, instance_context) {
        let offset = storage.byte_index(base_offset, instructions, temp_counter);
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: None,
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "encodeFatPointerMemory".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "value".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    ("bytes".to_string(), byte_array_type()),
                    ("offset".to_string(), oomir::Type::I32),
                    ("size".to_string(), oomir::Type::I32),
                    ("codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![
                source,
                operand_var(storage.bytes_var.clone(), byte_array_type()),
                offset,
                oomir::Operand::Constant(oomir::Constant::I32(
                    i32::try_from(layout_size_bytes(tcx, ty)?)
                        .map_err(|_| "fat-pointer layout exceeds JVM address space")?,
                )),
                codec,
            ],
        });
        return Ok(());
    }
    if let TyKind::Adt(adt_def, args) = ty.kind()
        && crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
        && matches!(
            ty_to_oomir_type(ty, tcx, data_types, instance_context),
            oomir::Type::Pointer(_)
        )
    {
        let pointee = args
            .iter()
            .find_map(|arg| arg.as_type())
            .ok_or_else(|| format!("NonNull pointer has no pointee type: {ty:?}"))?;
        return emit_direct_pointer_to_union_bytes(
            ty,
            pointee,
            source,
            storage,
            base_offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        );
    }
    if matches!(ty.kind(), TyKind::Coroutine(_, _)) {
        let codec = ensure_pointer_memory_codec(ty, tcx, data_types, instance_context)?
            .ok_or_else(|| format!("coroutine {ty:?} has no exact memory codec"))?;
        let encoded = next_union_temp("nested_coroutine_bytes", temp_counter);
        let nested_objects = next_union_temp("nested_coroutine_objects", temp_counter);
        let size = layout_size_bytes(tcx, ty)?;
        let value_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(encoded.clone()),
            class_name: codec.class_name,
            method_name: "encode".to_string(),
            method_ty: oomir::Signature {
                params: vec![("value".to_string(), value_ty)],
                ret: Box::new(byte_array_type()),
                is_static: true,
            },
            args: vec![source],
        });
        instructions.push(oomir::Instruction::NewArray {
            dest: nested_objects.clone(),
            element_type: oomir::Type::Class("java/lang/Object".to_string()),
            size: oomir::Operand::Constant(oomir::Constant::I32(size.max(1) as i32)),
        });
        emit_union_storage_copy(
            &JvmUnionStorage::at_start(encoded, nested_objects),
            0,
            storage,
            base_offset,
            size,
            instructions,
            temp_counter,
        );
        return Ok(());
    }

    match ty.kind() {
        TyKind::Pat(inner, _) => emit_ty_to_union_bytes(
            *inner,
            source,
            storage,
            base_offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        ),
        TyKind::Ref(_, inner, _)
            if let TyKind::Array(element, _) = inner.kind()
                && matches!(source.get_type(), Some(oomir::Type::Slice(_))) =>
        {
            // `&[T; N]` is a thin Rust pointer even though its JVM carrier is a
            // SliceView. Transmutes (notably fmt::Arguments::new) must encode the
            // data address, not the managed identity of the SliceView wrapper.
            let element_size = layout_size_bytes(tcx, *element)?;
            let pointer_ty = oomir::Type::Pointer(Box::new(ty_to_oomir_type(
                *element,
                tcx,
                data_types,
                instance_context,
            )));
            let pointer_dest = next_union_temp("union_array_reference_pointer", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(pointer_dest.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "fromSlice".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        (
                            "slice".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("element_size".to_string(), oomir::Type::U64),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(pointer_ty.clone()),
                    is_static: true,
                },
                args: vec![
                    source,
                    oomir::Operand::Constant(oomir::Constant::U64(
                        u64::try_from(element_size)
                            .map_err(|_| "Rust array element layout exceeds u64")?,
                    )),
                    pointer_view_codec_operand(*element, tcx, data_types, instance_context),
                ],
            });
            let address_dest = next_union_temp("union_array_reference_address", temp_counter);
            let pointer_codec =
                pointer_builtin_codec_operand(ty, tcx, data_types, instance_context)
                    .expect("fixed-array reference must have a pointer codec");
            let encoded_size = layout_size_bytes(tcx, ty)?;
            let storage_offset = storage.byte_index(base_offset, instructions, temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(address_dest.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "encodedAddress".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("pointer".to_string(), pointer_ty),
                        (
                            "owner".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("owner_offset".to_string(), oomir::Type::I32),
                        ("encoded_size".to_string(), oomir::Type::I32),
                        ("pointer_codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir::Type::U64),
                    is_static: true,
                },
                args: vec![
                    operand_var(
                        pointer_dest,
                        oomir::Type::Pointer(Box::new(ty_to_oomir_type(
                            *element,
                            tcx,
                            data_types,
                            instance_context,
                        ))),
                    ),
                    operand_var(storage.bytes_var.clone(), byte_array_type()),
                    storage_offset,
                    oomir::Operand::Constant(oomir::Constant::I32(
                        i32::try_from(encoded_size)
                            .map_err(|_| "pointer layout exceeds JVM address space")?,
                    )),
                    pointer_codec,
                ],
            });
            emit_bits_to_union_bytes(
                operand_var(address_dest, oomir::Type::U64),
                encoded_size,
                storage,
                base_offset,
                instructions,
                temp_counter,
            )
        }
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)
            if matches!(
                ty_to_oomir_type(ty, tcx, data_types, instance_context),
                oomir::Type::Pointer(_)
            ) =>
        {
            let pointer_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            let pointer_codec =
                pointer_builtin_codec_operand(ty, tcx, data_types, instance_context);
            let uses_typed_address =
                !matches!(pointee.kind(), TyKind::Dynamic(..)) && pointer_codec.is_some();
            let encoded_size = layout_size_bytes(tcx, ty)?;
            let storage_offset = uses_typed_address
                .then(|| storage.byte_index(base_offset, instructions, temp_counter));
            let address_dest = next_union_temp("union_pointer_address", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(address_dest.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: if matches!(pointee.kind(), TyKind::Dynamic(..)) {
                    "erasedAddress".to_string()
                } else {
                    "encodedAddress".to_string()
                },
                method_ty: oomir::Signature {
                    params: if matches!(pointee.kind(), TyKind::Dynamic(..)) {
                        vec![("pointer".to_string(), pointer_ty)]
                    } else if uses_typed_address {
                        vec![
                            ("pointer".to_string(), pointer_ty),
                            (
                                "owner".to_string(),
                                oomir::Type::Class("java/lang/Object".to_string()),
                            ),
                            ("owner_offset".to_string(), oomir::Type::I32),
                            ("encoded_size".to_string(), oomir::Type::I32),
                            ("pointer_codec".to_string(), oomir::Type::java_string()),
                        ]
                    } else {
                        vec![
                            ("pointer".to_string(), pointer_ty),
                            (
                                "owner".to_string(),
                                oomir::Type::Class("java/lang/Object".to_string()),
                            ),
                        ]
                    },
                    ret: Box::new(oomir::Type::U64),
                    is_static: true,
                },
                args: if matches!(pointee.kind(), TyKind::Dynamic(..)) {
                    vec![source]
                } else if uses_typed_address {
                    vec![
                        source,
                        operand_var(storage.bytes_var.clone(), byte_array_type()),
                        storage_offset.expect("typed pointer storage offset disappeared"),
                        oomir::Operand::Constant(oomir::Constant::I32(
                            i32::try_from(encoded_size)
                                .map_err(|_| "pointer layout exceeds JVM address space")?,
                        )),
                        pointer_codec.expect("typed pointer codec disappeared"),
                    ]
                } else {
                    vec![
                        source,
                        operand_var(storage.bytes_var.clone(), byte_array_type()),
                    ]
                },
            });
            emit_bits_to_union_bytes(
                operand_var(address_dest, oomir::Type::U64),
                encoded_size,
                storage,
                base_offset,
                instructions,
                temp_counter,
            )
        }
        TyKind::Float(FloatTy::F128) => {
            for (method_name, offset) in [("lowBits", 0), ("highBits", 8)] {
                let bits_dest = next_union_temp("union_f128_bits", temp_counter);
                instructions.push(oomir::Instruction::InvokeVirtual {
                    dest: Some(bits_dest.clone()),
                    class_name: crate::lower2::F128_CLASS.to_string(),
                    method_name: method_name.to_string(),
                    method_ty: oomir::Signature {
                        params: Vec::new(),
                        ret: Box::new(oomir::Type::I64),
                        is_static: false,
                    },
                    args: Vec::new(),
                    operand: source.clone(),
                });
                emit_bits_to_union_bytes(
                    operand_var(bits_dest, oomir::Type::I64),
                    8,
                    storage,
                    base_offset + offset,
                    instructions,
                    temp_counter,
                )?;
            }
            Ok(())
        }
        TyKind::Tuple(elements) if elements.is_empty() => Ok(()),
        TyKind::Array(element_ty, _) => {
            let element_size = layout_size_bytes(tcx, *element_ty)?;
            let offset = storage.byte_index(base_offset, instructions, temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: None,
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "encodeArrayMemory".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        (
                            "array".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("bytes".to_string(), byte_array_type()),
                        ("offset".to_string(), oomir::Type::I32),
                        ("element_size".to_string(), oomir::Type::I32),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir::Type::Void),
                    is_static: true,
                },
                args: vec![
                    source,
                    operand_var(storage.bytes_var.clone(), byte_array_type()),
                    offset,
                    oomir::Operand::Constant(oomir::Constant::I32(element_size as i32)),
                    pointer_view_codec_operand(*element_ty, tcx, data_types, instance_context),
                ],
            });
            Ok(())
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_enum() => {
            let enum_oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            let oomir::Type::Class(enum_class) = &enum_oomir_ty else {
                return Err(format!("enum {ty:?} did not map to a JVM class"));
            };
            ensure_enum_union_codec(
                adt_def,
                substs,
                ty,
                enum_class,
                tcx,
                data_types,
                instance_context,
            )?;
            let offset = storage.byte_index(base_offset, instructions, temp_counter);
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: None,
                class_name: enum_class.clone(),
                method_name: enum_scoped_method_name(enum_class, ENUM_WRITE_UNION_STORAGE_METHOD),
                method_ty: enum_union_write_signature(enum_class),
                args: vec![
                    operand_var(storage.bytes_var.clone(), byte_array_type()),
                    operand_var(storage.objects_var.clone(), object_array_type()),
                    offset,
                ],
                operand: source,
            });
            Ok(())
        }
        TyKind::Adt(adt_def, _) if adt_def.is_union() => {
            let union_size = layout_size_bytes(tcx, ty)?;
            let union_oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            let oomir::Type::Class(union_class) = union_oomir_ty else {
                return Err(format!("union {ty:?} did not map to a JVM class"));
            };
            let bytes_dest = next_union_temp("nested_union_bytes", temp_counter);
            let objects_dest = next_union_temp("nested_union_objects", temp_counter);
            instructions.push(oomir::Instruction::GetField {
                dest: bytes_dest.clone(),
                object: source.clone(),
                field_name: UNION_BYTES_FIELD.to_string(),
                field_ty: byte_array_type(),
                owner_class: union_class.clone(),
            });
            instructions.push(oomir::Instruction::GetField {
                dest: objects_dest.clone(),
                object: source,
                field_name: UNION_OBJECTS_FIELD.to_string(),
                field_ty: object_array_type(),
                owner_class: union_class,
            });
            emit_union_storage_copy(
                &JvmUnionStorage::at_start(bytes_dest, objects_dest),
                0,
                storage,
                base_offset,
                union_size,
                instructions,
                temp_counter,
            );
            Ok(())
        }
        TyKind::Tuple(elements) if !elements.is_empty() => {
            let aggregate = union_aggregate_layout(ty, tcx, data_types, instance_context)?
                .expect("non-empty tuples have aggregate layouts");
            emit_aggregate_to_union_bytes(
                &aggregate,
                source,
                storage,
                base_offset,
                tcx,
                data_types,
                instance_context,
                instructions,
                temp_counter,
            )
        }
        TyKind::Closure(_, _) => {
            let aggregate = union_aggregate_layout(ty, tcx, data_types, instance_context)?
                .expect("closures have aggregate layouts");
            emit_aggregate_to_union_bytes(
                &aggregate,
                source,
                storage,
                base_offset,
                tcx,
                data_types,
                instance_context,
                instructions,
                temp_counter,
            )
        }
        TyKind::Adt(adt_def, _) if adt_def.is_struct() => {
            match union_aggregate_layout(ty, tcx, data_types, instance_context) {
                Ok(Some(aggregate)) => emit_aggregate_to_union_bytes(
                    &aggregate,
                    source,
                    storage,
                    base_offset,
                    tcx,
                    data_types,
                    instance_context,
                    instructions,
                    temp_counter,
                ),
                Err(_) => {
                    let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
                    if !jvm_ty.is_jvm_reference_type() {
                        return Err(format!("cannot store unresolved struct {ty:?} in a union"));
                    }
                    emit_object_to_union_storage(
                        source,
                        storage,
                        base_offset,
                        instructions,
                        temp_counter,
                    );
                    Ok(())
                }
                Ok(None) => unreachable!(),
            }
        }
        _ => {
            let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            if jvm_ty.is_jvm_reference_type() {
                emit_managed_object_to_union_bytes(
                    source,
                    layout_size_bytes(tcx, ty)?,
                    storage,
                    base_offset,
                    instructions,
                    temp_counter,
                );
                Ok(())
            } else {
                Err(format!(
                    "unsupported union field type {ty:?}: JVM representation {jvm_ty:?} is neither byte-addressable nor a reference"
                ))
            }
        }
    }
}
