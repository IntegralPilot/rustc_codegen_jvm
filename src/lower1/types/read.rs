use super::*;
use crate::lower1::context::Definitions;

pub(super) fn emit_ty_from_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<oomir::Operand, String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    if layout_size_bytes(tcx, ty)? == 0 {
        let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
        if !jvm_ty.has_jvm_value() {
            return Ok(oomir::Operand::Constant(oomir::Constant::Unit));
        }
        if let Ok(constant) = crate::lower1::operand::const_eval::read_zero_sized_constant(
            tcx,
            ty,
            data_types,
            instance_context,
        ) {
            return Ok(oomir::Operand::Constant(constant));
        }
        return crate::lower1::value_repr::materialize_implicit_zst(
            ty,
            &next_union_temp("union_zst_value", temp_counter),
            tcx,
            instance_context,
            data_types,
            instructions,
        )
        .ok_or_else(|| format!("cannot materialize zero-sized value {ty:?} from Rust storage"));
    }
    if is_direct_union_scalar(ty) {
        return emit_scalar_from_union_bytes(
            ty,
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
        let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
        let offset = storage.byte_index(base_offset, instructions, temp_counter);
        let object_dest = next_union_temp("union_fat_pointer_object", temp_counter);
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(object_dest.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "decodeFatPointerMemory".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("bytes".to_string(), byte_array_type()),
                    ("offset".to_string(), oomir::Type::I32),
                    ("size".to_string(), oomir::Type::I32),
                    ("codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                is_static: true,
            },
            args: vec![
                operand_var(storage.bytes_var.clone(), byte_array_type()),
                offset,
                oomir::Operand::Constant(oomir::Constant::I32(
                    i32::try_from(layout_size_bytes(tcx, ty)?)
                        .map_err(|_| "fat-pointer layout exceeds JVM address space")?,
                )),
                codec,
            ],
        });
        let typed_dest = next_union_temp("union_fat_pointer", temp_counter);
        instructions.push(oomir::Instruction::Cast {
            op: operand_var(
                object_dest,
                oomir::Type::Class("java/lang/Object".to_string()),
            ),
            ty: jvm_ty.clone(),
            dest: typed_dest.clone(),
        });
        return Ok(operand_var(typed_dest, jvm_ty));
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
        return emit_direct_pointer_from_union_bytes(
            ty,
            pointee,
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
        let nested_bytes = next_union_temp("nested_coroutine_bytes", temp_counter);
        let nested_objects = next_union_temp("nested_coroutine_objects", temp_counter);
        let size = layout_size_bytes(tcx, ty)?;
        instructions.push(oomir::Instruction::NewArray {
            dest: nested_bytes.clone(),
            element_type: oomir::Type::I8,
            size: oomir::Operand::Constant(oomir::Constant::I32(size as i32)),
        });
        instructions.push(oomir::Instruction::NewArray {
            dest: nested_objects.clone(),
            element_type: oomir::Type::Class("java/lang/Object".to_string()),
            size: oomir::Operand::Constant(oomir::Constant::I32(size.max(1) as i32)),
        });
        let nested_storage = JvmUnionStorage::at_start(nested_bytes.clone(), nested_objects);
        emit_union_storage_copy(
            storage,
            base_offset,
            &nested_storage,
            0,
            size,
            instructions,
            temp_counter,
        );
        let value_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
        let decoded = next_union_temp("nested_coroutine_value", temp_counter);
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(decoded.clone()),
            class_name: codec.class_name,
            method_name: "decode".to_string(),
            method_ty: oomir::Signature {
                params: vec![("bytes".to_string(), byte_array_type())],
                ret: Box::new(value_ty.clone()),
                is_static: true,
            },
            args: vec![operand_var(nested_bytes, byte_array_type())],
        });
        return Ok(operand_var(decoded, value_ty));
    }

    match ty.kind() {
        TyKind::Pat(inner, _) => emit_ty_from_union_bytes(
            *inner,
            storage,
            base_offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        ),
        TyKind::Ref(_, inner, _)
            if let TyKind::Array(element, length) = inner.kind()
                && matches!(
                    ty_to_oomir_type(ty, tcx, data_types, instance_context),
                    oomir::Type::Slice(_)
                ) =>
        {
            let length = length
                .try_to_target_usize(tcx)
                .ok_or_else(|| format!("array length is not concrete for {inner:?}"))?;
            let element_oomir_ty = ty_to_oomir_type(*element, tcx, data_types, instance_context);
            let slice_ty = oomir::Type::Slice(Box::new(element_oomir_ty.clone()));
            let pointer_ty = oomir::Type::Pointer(Box::new(element_oomir_ty));
            let address = emit_bits_from_union_bytes(
                oomir::Type::U64,
                layout_size_bytes(tcx, ty)?,
                storage,
                base_offset,
                instructions,
                temp_counter,
            );
            let pointer_dest = next_union_temp("union_array_reference_pointer", temp_counter);
            let pointer_codec =
                pointer_builtin_codec_operand(ty, tcx, data_types, instance_context)
                    .expect("fixed-array reference must have a pointer codec");
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(pointer_dest.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "fromEncodedAddress".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("address".to_string(), oomir::Type::U64),
                        ("view_size".to_string(), oomir::Type::U64),
                        ("view_codec".to_string(), oomir::Type::java_string()),
                        ("pointer_codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(pointer_ty.clone()),
                    is_static: true,
                },
                args: vec![
                    address,
                    oomir::Operand::Constant(oomir::Constant::U64(
                        u64::try_from(layout_size_bytes(tcx, *element)?)
                            .map_err(|_| "Rust array element layout exceeds u64")?,
                    )),
                    pointer_view_codec_operand(*element, tcx, data_types, instance_context),
                    pointer_codec,
                ],
            });
            let object_dest = next_union_temp("union_array_reference_view", temp_counter);
            instructions.push(oomir::Instruction::ConstructObject {
                dest: object_dest.clone(),
                class_name: oomir::SLICE_VIEW_CLASS.to_string(),
                args: vec![
                    (
                        operand_var(pointer_dest, pointer_ty),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    (
                        oomir::Operand::Constant(oomir::Constant::I32(0)),
                        oomir::Type::I32,
                    ),
                    (
                        oomir::Operand::Constant(oomir::Constant::U64(length)),
                        oomir::Type::U64,
                    ),
                ],
            });
            let slice_dest = next_union_temp("union_array_reference_slice", temp_counter);
            instructions.push(oomir::Instruction::Cast {
                op: operand_var(
                    object_dest,
                    oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
                ),
                ty: slice_ty.clone(),
                dest: slice_dest.clone(),
            });
            Ok(operand_var(slice_dest, slice_ty))
        }
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)
            if matches!(
                ty_to_oomir_type(ty, tcx, data_types, instance_context),
                oomir::Type::Pointer(_)
            ) =>
        {
            let pointer_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            let is_erased_trait_object = matches!(pointee.kind(), TyKind::Dynamic(..));
            let pointer_codec = (!is_erased_trait_object)
                .then(|| pointer_builtin_codec_operand(ty, tcx, data_types, instance_context))
                .flatten();
            let uses_typed_address = pointer_codec.is_some();
            let bits = emit_bits_from_union_bytes(
                oomir::Type::I64,
                layout_size_bytes(tcx, ty)?,
                storage,
                base_offset,
                instructions,
                temp_counter,
            );
            let pointer_dest = next_union_temp("union_pointer_value", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(pointer_dest.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: if is_erased_trait_object {
                    "fromErasedAddress".to_string()
                } else if uses_typed_address {
                    "fromEncodedAddress".to_string()
                } else {
                    "fromAddress".to_string()
                },
                method_ty: oomir::Signature {
                    params: if is_erased_trait_object {
                        vec![("address".to_string(), oomir::Type::U64)]
                    } else if uses_typed_address {
                        vec![
                            ("address".to_string(), oomir::Type::U64),
                            ("view_size".to_string(), oomir::Type::U64),
                            ("view_codec".to_string(), oomir::Type::java_string()),
                            ("pointer_codec".to_string(), oomir::Type::java_string()),
                        ]
                    } else {
                        vec![
                            ("address".to_string(), oomir::Type::U64),
                            ("view_size".to_string(), oomir::Type::U64),
                            ("view_codec".to_string(), oomir::Type::java_string()),
                        ]
                    },
                    ret: Box::new(pointer_ty.clone()),
                    is_static: true,
                },
                args: if is_erased_trait_object {
                    vec![bits]
                } else if uses_typed_address {
                    vec![
                        bits,
                        oomir::Operand::Constant(oomir::Constant::U64(
                            u64::try_from(layout_size_bytes(tcx, *pointee)?)
                                .map_err(|_| "Rust pointer pointee layout exceeds u64")?,
                        )),
                        pointer_view_codec_operand(*pointee, tcx, data_types, instance_context),
                        pointer_codec.expect("non-erased pointer codec disappeared"),
                    ]
                } else {
                    vec![
                        bits,
                        oomir::Operand::Constant(oomir::Constant::U64(
                            u64::try_from(layout_size_bytes(tcx, *pointee)?)
                                .map_err(|_| "Rust pointer pointee layout exceeds u64")?,
                        )),
                        pointer_view_codec_operand(*pointee, tcx, data_types, instance_context),
                    ]
                },
            });
            Ok(operand_var(pointer_dest, pointer_ty))
        }
        TyKind::Float(FloatTy::F128) => {
            let low = emit_bits_from_union_bytes(
                oomir::Type::I64,
                8,
                storage,
                base_offset,
                instructions,
                temp_counter,
            );
            let high = emit_bits_from_union_bytes(
                oomir::Type::I64,
                8,
                storage,
                base_offset + 8,
                instructions,
                temp_counter,
            );
            let dest = next_union_temp("union_f128_value", temp_counter);
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.clone(),
                class_name: crate::lower2::F128_CLASS.to_string(),
                args: vec![(high, oomir::Type::I64), (low, oomir::Type::I64)],
            });
            Ok(operand_var(
                dest,
                oomir::Type::Class(crate::lower2::F128_CLASS.to_string()),
            ))
        }
        TyKind::Array(element_ty, length) => {
            let length = length
                .try_to_target_usize(tcx)
                .ok_or_else(|| format!("array length is not concrete for {:?}", ty))?
                as usize;
            let element_size = layout_size_bytes(tcx, *element_ty)?;
            let element_oomir_ty = ty_to_oomir_type(*element_ty, tcx, data_types, instance_context);
            let array_ty = oomir::Type::Array(Box::new(element_oomir_ty.clone()));
            let array_dest = next_union_temp("union_array_value", temp_counter);
            instructions.push(oomir::Instruction::NewArray {
                dest: array_dest.clone(),
                element_type: element_oomir_ty,
                size: oomir::Operand::Constant(oomir::Constant::I32(length as i32)),
            });
            let offset = storage.byte_index(base_offset, instructions, temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: None,
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "decodeArrayMemory".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("bytes".to_string(), byte_array_type()),
                        ("offset".to_string(), oomir::Type::I32),
                        (
                            "array".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("element_size".to_string(), oomir::Type::I32),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir::Type::Void),
                    is_static: true,
                },
                args: vec![
                    operand_var(storage.bytes_var.clone(), byte_array_type()),
                    offset,
                    operand_var(array_dest.clone(), array_ty.clone()),
                    oomir::Operand::Constant(oomir::Constant::I32(element_size as i32)),
                    pointer_view_codec_operand(*element_ty, tcx, data_types, instance_context),
                ],
            });
            Ok(operand_var(array_dest, array_ty))
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
            let enum_dest = next_union_temp("union_enum_value", temp_counter);
            let offset = storage.byte_index(base_offset, instructions, temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(enum_dest.clone()),
                class_name: enum_class.clone(),
                method_name: ENUM_READ_UNION_STORAGE_METHOD.to_string(),
                method_ty: enum_union_read_signature(enum_class),
                args: vec![
                    operand_var(storage.bytes_var.clone(), byte_array_type()),
                    operand_var(storage.objects_var.clone(), object_array_type()),
                    offset,
                ],
            });
            Ok(operand_var(enum_dest, enum_oomir_ty))
        }
        TyKind::Adt(adt_def, _) if adt_def.is_union() => {
            let union_size = layout_size_bytes(tcx, ty)?;
            let object_storage_size =
                union_object_storage_size(ty, union_size, tcx, instance_context);
            let union_oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            let oomir::Type::Class(union_class) = &union_oomir_ty else {
                return Err(format!("union {ty:?} did not map to a JVM class"));
            };
            let bytes_dest = next_union_temp("nested_union_bytes", temp_counter);
            let objects_dest = next_union_temp("nested_union_objects", temp_counter);
            instructions.push(oomir::Instruction::NewArray {
                dest: bytes_dest.clone(),
                element_type: oomir::Type::I8,
                size: oomir::Operand::Constant(oomir::Constant::I32(union_size as i32)),
            });
            instructions.push(allocate_union_object_storage(
                objects_dest.clone(),
                object_storage_size,
            ));
            let nested_storage =
                JvmUnionStorage::at_start(bytes_dest.clone(), objects_dest.clone());
            emit_union_storage_copy(
                storage,
                base_offset,
                &nested_storage,
                0,
                union_size,
                instructions,
                temp_counter,
            );
            let union_dest = next_union_temp("nested_union_value", temp_counter);
            instructions.push(oomir::Instruction::ConstructObject {
                dest: union_dest.clone(),
                class_name: union_class.clone(),
                args: vec![
                    (
                        operand_var(bytes_dest, byte_array_type()),
                        byte_array_type(),
                    ),
                    (
                        operand_var(objects_dest, object_array_type()),
                        object_array_type(),
                    ),
                ],
            });
            Ok(operand_var(union_dest, union_oomir_ty))
        }
        TyKind::Tuple(elements) if elements.is_empty() => {
            Ok(oomir::Operand::Constant(oomir::Constant::Unit))
        }
        TyKind::Tuple(elements) if !elements.is_empty() => {
            let aggregate = union_aggregate_layout(ty, tcx, data_types, instance_context)?
                .expect("non-empty tuples have aggregate layouts");
            emit_aggregate_from_union_bytes(
                &aggregate,
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
            emit_aggregate_from_union_bytes(
                &aggregate,
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
                Ok(Some(aggregate)) => emit_aggregate_from_union_bytes(
                    &aggregate,
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
                        return Err(format!("cannot load unresolved struct {ty:?} from a union"));
                    }
                    Ok(emit_object_from_union_storage(
                        jvm_ty,
                        storage,
                        base_offset,
                        instructions,
                        temp_counter,
                    ))
                }
                Ok(None) => unreachable!(),
            }
        }
        _ => {
            let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            if jvm_ty.is_jvm_reference_type() {
                Ok(emit_managed_object_from_union_bytes(
                    jvm_ty,
                    layout_size_bytes(tcx, ty)?,
                    storage,
                    base_offset,
                    instructions,
                    temp_counter,
                ))
            } else {
                Err(format!(
                    "unsupported union field type {ty:?}: JVM representation {jvm_ty:?} is neither byte-addressable nor a reference"
                ))
            }
        }
    }
}
