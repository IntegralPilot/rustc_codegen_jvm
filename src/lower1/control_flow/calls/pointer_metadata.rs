//! Pointer metadata.
use super::*;

pub(super) fn with_metadata<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    fn_output: Ty<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
) {
    if let Some(dest) = effective_dest {
        let data = oomir_operands[0].clone();
        let metadata = oomir_operands[1].clone();
        let target_pointee = match fn_output.kind() {
            TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
            other => panic!("with_metadata_of returned non-pointer type {other:?}"),
        };
        let element_ty = if target_pointee.is_str() {
            tcx.types.u8
        } else {
            target_pointee.sequence_element_type(tcx)
        };
        let data = emit_retyped_slice_data_pointer(
            data,
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(
                    crate::lower1::types::layout_size_bytes(tcx, element_ty)
                        .expect("slice element has a layout"),
                )
                .expect("Rust slice element layout exceeds u64"),
            )),
            crate::lower1::types::pointer_view_codec_operand(element_ty, tcx, data_types, instance),
            &format!("{label}_metadata_data"),
            &mut instructions,
        );
        let (backing, offset) =
            emit_pointer_slice_parts(data, &format!("{label}_metadata_data"), &mut instructions);
        let length = format!("{label}_metadata_length");
        instructions.push(oomir::Instruction::GetField {
            dest: length.clone(),
            object: metadata,
            field_name: "rustLength".to_string(),
            field_ty: oomir::Type::U64,
            owner_class: oomir::SLICE_VIEW_CLASS.to_string(),
        });
        instructions.push(oomir::Instruction::ConstructObject {
            dest,
            class_name: if target_pointee.is_str() {
                oomir::UTF8_VIEW_CLASS.to_string()
            } else {
                oomir::SLICE_VIEW_CLASS.to_string()
            },
            args: vec![
                (backing, oomir::Type::Class("java/lang/Object".to_string())),
                (offset, oomir::Type::I32),
                (
                    oomir::Operand::Variable {
                        name: length,
                        ty: oomir::Type::U64,
                    },
                    oomir::Type::U64,
                ),
            ],
        });
    }
}
pub(super) fn cast<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    fn_output: Ty<'tcx>,
    oomir_output_type: oomir::Type,
    effective_dest: Option<String>,
    receiver_operand: oomir::Operand,
    resolved_receiver_mir_ty: Ty<'tcx>,
) {
    let source_pointee = match resolved_receiver_mir_ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
            if pointee.is_slice() {
                pointee.sequence_element_type(tcx)
            } else {
                tcx.types.u8
            }
        }
        other => panic!("fat pointer cast receiver has unexpected type {other:?}"),
    };
    let target_pointee = match fn_output.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
        other => panic!("fat pointer cast returns unexpected type {other:?}"),
    };
    let source_element_size = crate::lower1::types::layout_size_bytes(tcx, source_pointee)
        .expect("fat pointer element has a concrete layout");
    let data_pointer = format!("{label}_fat_cast_data");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(data_pointer.clone()),
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
            ret: Box::new(oomir_output_type.clone()),
            is_static: true,
        },
        args: vec![
            receiver_operand,
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(source_element_size).expect("Rust slice element layout exceeds u64"),
            )),
            crate::lower1::types::pointer_view_codec_operand(
                source_pointee,
                tcx,
                data_types,
                instance,
            ),
        ],
    });
    instructions.push(oomir::Instruction::InvokeVirtual {
        dest: effective_dest,
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "retype".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("self".to_string(), oomir_output_type.clone()),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(oomir_output_type.clone()),
            is_static: false,
        },
        args: vec![
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(
                    crate::lower1::types::layout_size_bytes(tcx, target_pointee)
                        .expect("fat pointer cast target has a concrete layout"),
                )
                .expect("Rust pointer target layout exceeds u64"),
            )),
            crate::lower1::types::pointer_view_codec_operand(
                target_pointee,
                tcx,
                data_types,
                instance,
            ),
        ],
        operand: oomir::Operand::Variable {
            name: data_pointer,
            ty: oomir_output_type.clone(),
        },
    });
}

pub(super) fn from_raw_parts<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    fn_output: Ty<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    method_signature: oomir::Signature,
) {
    let pointee = match fn_output.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
        other => {
            panic!("from_raw_parts returned non-pointer type {other:?}")
        }
    };
    let source_ty = oomir_operands[0]
        .get_type()
        .expect("from_raw_parts data pointer is typed");
    if matches!(pointee.kind(), TyKind::Dynamic(_, _)) {
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: effective_dest.clone(),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "fromRawTraitParts".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), source_ty),
                    (
                        "metadata".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                ],
                ret: method_signature.ret.clone(),
                is_static: true,
            },
            args: vec![oomir_operands[0].clone(), oomir_operands[1].clone()],
        });
    } else {
        let tail = tcx.struct_tail_for_codegen(pointee, TypingEnv::fully_monomorphized());
        let carries_slice_metadata = tail.is_slice() || tail.is_str();
        let pointee_size =
            crate::lower1::types::layout_size_bytes(tcx, pointee).unwrap_or_else(|error| {
                panic!("could not determine from_raw_parts pointee size: {error}")
            });
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: effective_dest.clone(),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: if carries_slice_metadata {
                "retypeWithMetadata".to_string()
            } else {
                "retype".to_string()
            },
            method_ty: oomir::Signature {
                params: {
                    let mut params = vec![
                        ("pointer".to_string(), source_ty),
                        ("view_size".to_string(), oomir::Type::U64),
                        ("view_codec".to_string(), oomir::Type::java_string()),
                    ];
                    if carries_slice_metadata {
                        params.push(("metadata".to_string(), oomir::Type::U64));
                    }
                    params
                },
                ret: method_signature.ret.clone(),
                is_static: true,
            },
            args: {
                let mut args = vec![
                    oomir_operands[0].clone(),
                    oomir::Operand::Constant(oomir::Constant::U64(
                        u64::try_from(pointee_size)
                            .expect("Rust pointer target layout exceeds u64"),
                    )),
                    crate::lower1::types::pointer_view_codec_operand(
                        pointee, tcx, data_types, instance,
                    ),
                ];
                if carries_slice_metadata {
                    args.push(oomir_operands[1].clone());
                }
                args
            },
        });
    }
}
