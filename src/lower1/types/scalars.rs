use super::*;
use crate::lower1::context::Definitions;

pub(super) fn is_direct_union_scalar<'tcx>(ty: Ty<'tcx>) -> bool {
    matches!(
        ty.kind(),
        TyKind::Bool
            | TyKind::Char
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(FloatTy::F16 | FloatTy::F32 | FloatTy::F64)
    )
}

pub(super) fn scalar_bit_operand_for_union<'tcx>(
    ty: Ty<'tcx>,
    source: oomir::Operand,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(oomir::Operand, oomir::Type, usize), String> {
    let rust_size = layout_size_bytes(tcx, ty)?;
    let oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);

    match ty.kind() {
        TyKind::Float(FloatTy::F16) => {
            let dest = next_union_temp("union_f16_bits", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "org/rustlang/runtime/Numbers".to_string(),
                method_name: "f16ToBits".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("value".to_string(), oomir::Type::F16)],
                    ret: Box::new(oomir::Type::U16),
                    is_static: true,
                },
                args: vec![source],
            });
            Ok((
                operand_var(dest, oomir::Type::U16),
                oomir::Type::U16,
                rust_size,
            ))
        }
        TyKind::Float(FloatTy::F32) => {
            let dest = next_union_temp("union_f32_bits", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "java/lang/Float".to_string(),
                method_name: "floatToRawIntBits".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("value".to_string(), oomir::Type::F32)],
                    ret: Box::new(oomir::Type::I32),
                    is_static: true,
                },
                args: vec![source],
            });
            Ok((
                operand_var(dest, oomir::Type::I32),
                oomir::Type::I32,
                rust_size,
            ))
        }
        TyKind::Float(FloatTy::F64) => {
            let dest = next_union_temp("union_f64_bits", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "java/lang/Double".to_string(),
                method_name: "doubleToRawLongBits".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("value".to_string(), oomir::Type::F64)],
                    ret: Box::new(oomir::Type::I64),
                    is_static: true,
                },
                args: vec![source],
            });
            Ok((
                operand_var(dest, oomir::Type::I64),
                oomir::Type::I64,
                rust_size,
            ))
        }
        TyKind::Float(_) => Err(format!("unsupported float width in union field: {:?}", ty)),
        TyKind::Int(IntTy::I128) | TyKind::Uint(UintTy::U128) => {
            Err(format!("unsupported wide integer union field: {:?}", ty))
        }
        TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) => {
            let bits_ty = scalar_bits_type(rust_size, &oomir_ty);
            if source.get_type().as_ref() == Some(&bits_ty) {
                Ok((source, bits_ty, rust_size))
            } else {
                let dest = next_union_temp("union_bits", temp_counter);
                instructions.push(oomir::Instruction::Cast {
                    op: source,
                    ty: bits_ty.clone(),
                    dest: dest.clone(),
                });
                Ok((operand_var(dest, bits_ty.clone()), bits_ty, rust_size))
            }
        }
        _ => Err(format!("unsupported scalar union field: {:?}", ty)),
    }
}

pub(super) fn emit_scalar_to_union_bytes<'tcx>(
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
    if matches!(
        ty.kind(),
        TyKind::Int(IntTy::I128) | TyKind::Uint(UintTy::U128)
    ) {
        let integer_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
        let offset = storage.byte_index(base_offset, instructions, temp_counter);
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: None,
            class_name: MEMORY_BYTES_CLASS.into(),
            method_name: if matches!(ty.kind(), TyKind::Int(IntTy::I128)) {
                "writeI128".into()
            } else {
                "writeU128".into()
            },
            method_ty: oomir::Signature {
                params: vec![
                    ("bytes".into(), byte_array_type()),
                    ("offset".into(), oomir::Type::I32),
                    ("value".into(), integer_ty),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![
                operand_var(storage.bytes_var.clone(), byte_array_type()),
                offset,
                source,
            ],
        });
        return Ok(());
    }

    let (bits_operand, _, rust_size) = scalar_bit_operand_for_union(
        ty,
        source,
        tcx,
        data_types,
        instance_context,
        instructions,
        temp_counter,
    )?;
    emit_bits_to_union_bytes(
        bits_operand,
        rust_size,
        storage,
        base_offset,
        instructions,
        temp_counter,
    )
}

pub(super) fn emit_scalar_from_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<oomir::Operand, String> {
    let rust_size = layout_size_bytes(tcx, ty)?;
    let oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
    if matches!(
        ty.kind(),
        TyKind::Int(IntTy::I128) | TyKind::Uint(UintTy::U128)
    ) {
        let offset = storage.byte_index(base_offset, instructions, temp_counter);
        let dest = next_union_temp("union_big_integer_value", temp_counter);
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest.clone()),
            class_name: MEMORY_BYTES_CLASS.into(),
            method_name: if matches!(ty.kind(), TyKind::Int(IntTy::I128)) {
                "readI128".into()
            } else {
                "readU128".into()
            },
            method_ty: oomir::Signature {
                params: vec![
                    ("bytes".to_string(), byte_array_type()),
                    ("offset".to_string(), oomir::Type::I32),
                ],
                ret: Box::new(oomir_ty.clone()),
                is_static: true,
            },
            args: vec![
                operand_var(storage.bytes_var.clone(), byte_array_type()),
                offset,
            ],
        });
        return Ok(operand_var(dest, oomir_ty));
    }

    let bits_ty = match ty.kind() {
        TyKind::Float(FloatTy::F16) => oomir::Type::U16,
        TyKind::Float(FloatTy::F32) => oomir::Type::I32,
        TyKind::Float(FloatTy::F64) => oomir::Type::I64,
        TyKind::Float(_) => {
            return Err(format!("unsupported float width in union field: {:?}", ty));
        }
        TyKind::Int(IntTy::I128) | TyKind::Uint(UintTy::U128) => {
            return Err(format!("unsupported wide integer union field: {:?}", ty));
        }
        TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) => {
            scalar_bits_type(rust_size, &oomir_ty)
        }
        _ => return Err(format!("unsupported scalar union field: {:?}", ty)),
    };
    let bits_operand = emit_bits_from_union_bytes(
        bits_ty.clone(),
        rust_size,
        storage,
        base_offset,
        instructions,
        temp_counter,
    );

    match ty.kind() {
        TyKind::Float(FloatTy::F16) => {
            let dest = next_union_temp("union_f16_value", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "org/rustlang/runtime/Numbers".to_string(),
                method_name: "f16FromBits".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("bits".to_string(), oomir::Type::U16)],
                    ret: Box::new(oomir::Type::F16),
                    is_static: true,
                },
                args: vec![bits_operand],
            });
            Ok(operand_var(dest, oomir::Type::F16))
        }
        TyKind::Float(FloatTy::F32) => {
            let dest = next_union_temp("union_f32_value", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "java/lang/Float".to_string(),
                method_name: "intBitsToFloat".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("bits".to_string(), oomir::Type::I32)],
                    ret: Box::new(oomir::Type::F32),
                    is_static: true,
                },
                args: vec![bits_operand],
            });
            Ok(operand_var(dest, oomir::Type::F32))
        }
        TyKind::Float(FloatTy::F64) => {
            let dest = next_union_temp("union_f64_value", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "java/lang/Double".to_string(),
                method_name: "longBitsToDouble".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("bits".to_string(), oomir::Type::I64)],
                    ret: Box::new(oomir::Type::F64),
                    is_static: true,
                },
                args: vec![bits_operand],
            });
            Ok(operand_var(dest, oomir::Type::F64))
        }
        _ if oomir_ty == bits_ty => Ok(bits_operand),
        _ => {
            let dest = next_union_temp("union_scalar_value", temp_counter);
            instructions.push(oomir::Instruction::Cast {
                op: bits_operand,
                ty: oomir_ty.clone(),
                dest: dest.clone(),
            });
            Ok(operand_var(dest, oomir_ty))
        }
    }
}
