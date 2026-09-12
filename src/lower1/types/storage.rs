use super::*;
use crate::lower1::context::Definitions;

pub(super) fn byte_array_type() -> oomir::Type {
    oomir::Type::Array(Box::new(oomir::Type::I8))
}

pub(super) fn object_array_type() -> oomir::Type {
    oomir::Type::Array(Box::new(oomir::Type::Class("java/lang/Object".to_string())))
}

pub(super) fn allocate_union_object_storage(
    dest: impl Into<String>,
    size: usize,
) -> oomir::Instruction {
    let dest = dest.into();
    if size == 0 {
        oomir::Instruction::InvokeStatic {
            dest: Some(dest),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "emptyUnionObjectStorage".to_string(),
            method_ty: oomir::Signature {
                params: Vec::new(),
                ret: Box::new(object_array_type()),
                is_static: true,
            },
            args: Vec::new(),
        }
    } else {
        oomir::Instruction::NewArray {
            dest,
            element_type: oomir::Type::Class("java/lang/Object".to_string()),
            size: oomir::Operand::Constant(oomir::Constant::I32(size as i32)),
        }
    }
}

pub(super) fn next_union_temp(prefix: &str, counter: &mut usize) -> String {
    let temp = format!("{}_{}", prefix, *counter);
    *counter += 1;
    temp
}

pub(super) fn operand_var(name: impl Into<String>, ty: oomir::Type) -> oomir::Operand {
    oomir::Operand::Variable {
        name: name.into(),
        ty,
    }
}

pub(super) fn emit_aggregate_to_union_bytes<'tcx>(
    aggregate: &UnionAggregateLayout<'tcx>,
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    for field in &aggregate.fields {
        if matches!(field.rust_ty.kind(), TyKind::Dynamic(..)) {
            continue;
        }
        let field_dest = next_union_temp("union_aggregate_field", temp_counter);
        instructions.push(oomir::Instruction::GetField {
            dest: field_dest.clone(),
            object: source.clone(),
            field_name: field.jvm_name.clone(),
            field_ty: field.jvm_ty.clone(),
            owner_class: aggregate.class_name.clone(),
        });
        emit_ty_to_union_bytes(
            field.rust_ty,
            operand_var(field_dest, field.jvm_ty.clone()),
            storage,
            base_offset + field.offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        )?;
    }
    Ok(())
}

pub(super) fn emit_aggregate_from_union_bytes<'tcx>(
    aggregate: &UnionAggregateLayout<'tcx>,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<oomir::Operand, String> {
    let mut constructor_args = Vec::new();
    for field in &aggregate.fields {
        let value = if matches!(field.rust_ty.kind(), TyKind::Dynamic(..)) {
            default_operand_for_codec(&field.jvm_ty)
        } else {
            emit_ty_from_union_bytes(
                field.rust_ty,
                storage,
                base_offset + field.offset,
                tcx,
                data_types,
                instance_context,
                instructions,
                temp_counter,
            )?
        };
        constructor_args.push((value, field.jvm_ty.clone()));
    }

    let dest = next_union_temp("union_aggregate_value", temp_counter);
    instructions.push(oomir::Instruction::ConstructObject {
        dest: dest.clone(),
        class_name: aggregate.class_name.clone(),
        args: constructor_args,
    });
    Ok(operand_var(
        dest,
        oomir::Type::Class(aggregate.class_name.clone()),
    ))
}

pub(super) fn needs_nested_memory_binding(ty: Ty<'_>) -> bool {
    match ty.kind() {
        TyKind::Tuple(elements) => !elements.is_empty(),
        TyKind::Array(_, _) | TyKind::Closure(_, _) => true,
        TyKind::Adt(adt_def, _) => adt_def.is_struct() || adt_def.is_enum() || adt_def.is_union(),
        TyKind::Pat(inner, _) => needs_nested_memory_binding(*inner),
        _ => false,
    }
}

pub(super) fn emit_aggregate_memory_bindings<'tcx>(
    aggregate: &UnionAggregateLayout<'tcx>,
    pointer: oomir::Operand,
    source: oomir::Operand,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    let pointer_ty = pointer
        .get_type()
        .ok_or_else(|| "memory-view binding pointer has no OOMIR type".to_string())?;
    for field in &aggregate.fields {
        if !needs_nested_memory_binding(field.rust_ty) || !field.jvm_ty.is_jvm_reference_type() {
            continue;
        }
        let field_dest = next_union_temp("memory_view_field", temp_counter);
        instructions.push(oomir::Instruction::GetField {
            dest: field_dest.clone(),
            object: source.clone(),
            field_name: field.jvm_name.clone(),
            field_ty: field.jvm_ty.clone(),
            owner_class: aggregate.class_name.clone(),
        });
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: None,
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "bindNestedMemoryView".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), pointer_ty.clone()),
                    (
                        "value".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    ("relative_offset".to_string(), oomir::Type::U64),
                    ("size".to_string(), oomir::Type::U64),
                    ("codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: false,
            },
            args: vec![
                operand_var(field_dest, field.jvm_ty.clone()),
                oomir::Operand::Constant(oomir::Constant::U64(field.offset as u64)),
                oomir::Operand::Constant(oomir::Constant::U64(
                    layout_size_bytes(tcx, field.rust_ty)? as u64,
                )),
                pointer_view_codec_operand(field.rust_ty, tcx, data_types, instance_context),
            ],
            operand: pointer.clone(),
        });
    }
    Ok(())
}

pub(super) fn emit_memory_view_bindings<'tcx>(
    ty: Ty<'tcx>,
    pointer: oomir::Operand,
    source: oomir::Operand,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    match ty.kind() {
        TyKind::Pat(inner, _) => emit_memory_view_bindings(
            *inner,
            pointer,
            source,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        ),
        TyKind::Array(element_ty, _) if needs_nested_memory_binding(*element_ty) => {
            let pointer_ty = pointer
                .get_type()
                .ok_or_else(|| "array memory-view binding pointer has no OOMIR type".to_string())?;
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: None,
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "bindArrayMemoryViews".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("self".to_string(), pointer_ty),
                        (
                            "array".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("element_size".to_string(), oomir::Type::U64),
                        ("element_codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir::Type::Void),
                    is_static: false,
                },
                args: vec![
                    source,
                    oomir::Operand::Constant(oomir::Constant::U64(layout_size_bytes(
                        tcx,
                        *element_ty,
                    )? as u64)),
                    pointer_view_codec_operand(*element_ty, tcx, data_types, instance_context),
                ],
                operand: pointer,
            });
            Ok(())
        }
        TyKind::Tuple(elements) if !elements.is_empty() => {
            if let Some(aggregate) = union_aggregate_layout(ty, tcx, data_types, instance_context)?
            {
                emit_aggregate_memory_bindings(
                    &aggregate,
                    pointer,
                    source,
                    tcx,
                    data_types,
                    instance_context,
                    instructions,
                    temp_counter,
                )?;
            }
            Ok(())
        }
        TyKind::Closure(_, _) | TyKind::Adt(_, _) => {
            if let Some(aggregate) = union_aggregate_layout(ty, tcx, data_types, instance_context)?
            {
                emit_aggregate_memory_bindings(
                    &aggregate,
                    pointer,
                    source,
                    tcx,
                    data_types,
                    instance_context,
                    instructions,
                    temp_counter,
                )?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

pub(super) fn emit_object_to_union_storage(
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) {
    let index = storage.byte_index(base_offset, instructions, temp_counter);
    instructions.push(oomir::Instruction::ArrayStore {
        array: oomir::Operand::Variable {
            name: storage.objects_var.clone(),
            ty: oomir::Type::Array(Box::new(oomir::Type::Class("java/lang/Object".into()))),
        },
        index,
        value: source,
        copy_value: false,
    });
}

pub(super) fn emit_managed_object_to_union_bytes(
    source: oomir::Operand,
    rust_size: usize,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) {
    let address_dest = next_union_temp("union_managed_object_address", temp_counter);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(address_dest.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "managedObjectAddress".to_string(),
        method_ty: oomir::Signature {
            params: vec![(
                "value".to_string(),
                oomir::Type::Class("java/lang/Object".to_string()),
            )],
            ret: Box::new(oomir::Type::U64),
            is_static: true,
        },
        args: vec![source.clone()],
    });
    let address_bytes = rust_size.min(8);
    emit_bits_to_union_bytes(
        operand_var(address_dest, oomir::Type::U64),
        address_bytes,
        storage,
        base_offset,
        instructions,
        temp_counter,
    )
    .expect("an eight-byte managed address is representable");
    for byte_index in address_bytes..rust_size {
        let index = storage.byte_index(base_offset + byte_index, instructions, temp_counter);
        instructions.push(oomir::Instruction::ArrayStore {
            array: oomir::Operand::Variable {
                name: storage.bytes_var.clone(),
                ty: oomir::Type::Array(Box::new(oomir::Type::I8)),
            },
            index,
            value: oomir::Operand::Constant(oomir::Constant::I8(0)),
            copy_value: false,
        });
    }
    // Keep the side table populated for existing union helpers as well. Raw
    // pointer codecs can reconstruct solely from the byte address.
    emit_object_to_union_storage(source, storage, base_offset, instructions, temp_counter);
}

pub(super) fn emit_object_from_union_storage(
    target_ty: oomir::Type,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> oomir::Operand {
    let index = storage.byte_index(base_offset, instructions, temp_counter);
    let object_dest = next_union_temp("union_object", temp_counter);
    instructions.push(oomir::Instruction::ArrayGet {
        dest: object_dest.clone(),
        array: operand_var(storage.objects_var.clone(), object_array_type()),
        index,
    });
    if target_ty == oomir::Type::Class("java/lang/Object".to_string()) {
        return operand_var(object_dest, target_ty);
    }

    let typed_dest = next_union_temp("union_typed_object", temp_counter);
    instructions.push(oomir::Instruction::Cast {
        op: operand_var(
            object_dest,
            oomir::Type::Class("java/lang/Object".to_string()),
        ),
        ty: target_ty.clone(),
        dest: typed_dest.clone(),
    });
    operand_var(typed_dest, target_ty)
}

pub(super) fn emit_managed_object_from_union_bytes(
    target_ty: oomir::Type,
    rust_size: usize,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> oomir::Operand {
    let bits = emit_bits_from_union_bytes(
        oomir::Type::I64,
        rust_size.min(8),
        storage,
        base_offset,
        instructions,
        temp_counter,
    );
    let object_dest = next_union_temp("union_managed_object", temp_counter);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(object_dest.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "managedObjectFromAddress".to_string(),
        method_ty: oomir::Signature {
            params: vec![("address".to_string(), oomir::Type::U64)],
            ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
            is_static: true,
        },
        args: vec![bits],
    });
    if target_ty == oomir::Type::Class("java/lang/Object".to_string()) {
        return operand_var(object_dest, target_ty);
    }
    let typed_dest = next_union_temp("union_typed_managed_object", temp_counter);
    instructions.push(oomir::Instruction::Cast {
        op: operand_var(
            object_dest,
            oomir::Type::Class("java/lang/Object".to_string()),
        ),
        ty: target_ty.clone(),
        dest: typed_dest.clone(),
    });
    operand_var(typed_dest, target_ty)
}

pub(super) fn emit_union_storage_copy(
    source: &JvmUnionStorage,
    source_offset: usize,
    target: &JvmUnionStorage,
    target_offset: usize,
    size: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) {
    let source_index = source.byte_index(source_offset, instructions, temp_counter);
    let target_index = target.byte_index(target_offset, instructions, temp_counter);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: None,
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "copyUnionStorage".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("source_bytes".to_string(), byte_array_type()),
                ("source_objects".to_string(), object_array_type()),
                ("source_offset".to_string(), oomir::Type::I32),
                ("target_bytes".to_string(), byte_array_type()),
                ("target_objects".to_string(), object_array_type()),
                ("target_offset".to_string(), oomir::Type::I32),
                ("size".to_string(), oomir::Type::I32),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args: vec![
            operand_var(source.bytes_var.clone(), byte_array_type()),
            operand_var(source.objects_var.clone(), object_array_type()),
            source_index,
            operand_var(target.bytes_var.clone(), byte_array_type()),
            operand_var(target.objects_var.clone(), object_array_type()),
            target_index,
            oomir::Operand::Constant(oomir::Constant::I32(
                i32::try_from(size).expect("union storage exceeds the JVM runtime address space"),
            )),
        ],
    });
}

pub(super) fn emit_direct_pointer_to_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    pointee: Ty<'tcx>,
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    let pointer_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
    let pointer_codec = pointer_builtin_codec_operand(ty, tcx, data_types, instance_context);
    let uses_typed_address =
        !matches!(pointee.kind(), TyKind::Dynamic(..)) && pointer_codec.is_some();
    let encoded_size = layout_size_bytes(tcx, ty)?;
    let storage_offset =
        uses_typed_address.then(|| storage.byte_index(base_offset, instructions, temp_counter));
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

pub(super) fn emit_direct_pointer_from_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    pointee: Ty<'tcx>,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<oomir::Operand, String> {
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
                    u64::try_from(layout_size_bytes(tcx, pointee)?)
                        .map_err(|_| "Rust pointer pointee layout exceeds u64")?,
                )),
                pointer_view_codec_operand(pointee, tcx, data_types, instance_context),
                pointer_codec.expect("non-erased pointer codec disappeared"),
            ]
        } else {
            vec![
                bits,
                oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(layout_size_bytes(tcx, pointee)?)
                        .map_err(|_| "Rust pointer pointee layout exceeds u64")?,
                )),
                pointer_view_codec_operand(pointee, tcx, data_types, instance_context),
            ]
        },
    });
    Ok(operand_var(pointer_dest, pointer_ty))
}
