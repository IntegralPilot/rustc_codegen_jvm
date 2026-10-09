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
            crate::lower1::types::ty_to_oomir_type(element_ty, tcx, data_types, instance),
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
    instructions.push(oomir::Instruction::ViewAddress {
        dest: Some(data_pointer.clone()),
        source: receiver_operand,
        layout: Box::new(oomir::AddressLayout {
            pointer_type: oomir_output_type.clone(),
            size: oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(source_element_size).expect("Rust slice element layout exceeds u64"),
            )),
            codec: crate::lower1::types::pointer_view_codec_operand(
                source_pointee,
                tcx,
                data_types,
                instance,
            ),
        }),
    });
    instructions.push(oomir::Instruction::AddressRetype {
        dest: effective_dest,
        source: oomir::Operand::Variable {
            name: data_pointer,
            ty: oomir_output_type.clone(),
        },
        layout: Box::new(oomir::AddressLayout {
            pointer_type: oomir_output_type.clone(),
            size: oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(
                    crate::lower1::types::layout_size_bytes(tcx, target_pointee)
                        .expect("fat pointer cast target has a concrete layout"),
                )
                .expect("Rust pointer target layout exceeds u64"),
            )),
            codec: crate::lower1::types::pointer_view_codec_operand(
                target_pointee,
                tcx,
                data_types,
                instance,
            ),
        }),
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
    emit_raw_pointer_from_parts(
        tcx,
        instance,
        data_types,
        pointee,
        *method_signature.ret,
        oomir_operands[0].clone(),
        oomir_operands[1].clone(),
        effective_dest,
        instructions,
    );
}
