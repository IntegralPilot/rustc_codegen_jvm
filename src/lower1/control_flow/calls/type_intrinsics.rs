//! Type intrinsics.
use super::*;

pub(super) fn static_layout<'tcx>(
    tcx: TyCtxt<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    intrinsic_name: String,
    is_size_of: bool,
    dest: String,
) {
    let measured_ty = func_instance
        .args
        .types()
        .next()
        .expect("size/alignment intrinsic has a type argument");
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(measured_ty))
        .unwrap_or_else(|error| {
            panic!("could not determine layout for {intrinsic_name}::<{measured_ty:?}>: {error:?}")
        });
    let value = if is_size_of {
        layout.size.bytes()
    } else {
        layout.align.abi.bytes()
    };
    instructions.push(oomir::Instruction::Move {
        dest,
        src: oomir::Operand::Constant(oomir::Constant::U64(value)),
    });
}
pub(super) fn size_of_val<'tcx>(
    tcx: TyCtxt<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    dest: String,
) {
    let measured_ty = func_instance
        .args
        .types()
        .next()
        .expect("size_of_val intrinsic has a type argument");
    let tail = tcx.struct_tail_for_codegen(measured_ty, TypingEnv::fully_monomorphized());
    let tail_element = match tail.kind() {
        TyKind::Slice(element_ty) => Some(*element_ty),
        TyKind::Str => Some(tcx.types.u8),
        _ => None,
    };
    if matches!(tail.kind(), TyKind::Dynamic(..)) {
        let layout = tcx
        .layout_of(
            TypingEnv::fully_monomorphized()
                .as_query_input(measured_ty),
        )
        .unwrap_or_else(|error| {
            panic!(
                "could not determine size_of_val trait DST layout for {measured_ty:?}: {error:?}"
            )
        });
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "sizeOfTraitTailed".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "value".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    ("prefix_size".to_string(), oomir::Type::U64),
                    ("prefix_alignment".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(oomir_output_type.clone()),
                is_static: true,
            },
            args: vec![
                oomir_operands[0].clone(),
                oomir::Operand::Constant(oomir::Constant::U64(layout.size.bytes())),
                oomir::Operand::Constant(oomir::Constant::U64(layout.align.abi.bytes())),
            ],
        });
    } else if let Some(element_ty) = tail_element {
        let layout = tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(measured_ty))
            .unwrap_or_else(|error| {
                panic!("could not determine size_of_val DST layout for {measured_ty:?}: {error:?}")
            });
        let element_size =
            crate::lower1::types::layout_size_bytes(tcx, element_ty).unwrap_or_else(|error| {
                panic!("could not determine size_of_val tail element layout: {error}")
            });
        let alignment = layout.align.abi.bytes().max(
            crate::lower1::types::layout_align_bytes(tcx, element_ty)
                .expect("DST tail element has an alignment") as u64,
        );
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "sizeOfSliceTailed".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "value".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    ("prefix_size".to_string(), oomir::Type::U64),
                    ("element_size".to_string(), oomir::Type::U64),
                    ("alignment".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(oomir_output_type.clone()),
                is_static: true,
            },
            args: vec![
                oomir_operands[0].clone(),
                oomir::Operand::Constant(oomir::Constant::U64(layout.size.bytes())),
                oomir::Operand::Constant(oomir::Constant::U64(element_size as u64)),
                oomir::Operand::Constant(oomir::Constant::U64(alignment)),
            ],
        });
    } else {
        let size = crate::lower1::types::layout_size_bytes(tcx, measured_ty)
            .unwrap_or_else(|error| panic!("could not determine size_of_val layout: {error}"));
        instructions.push(oomir::Instruction::Move {
            dest,
            src: oomir::Operand::Constant(oomir::Constant::U64(size as u64)),
        });
    }
}
pub(super) fn align_of_val<'tcx>(
    tcx: TyCtxt<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    dest: String,
) {
    let measured_ty = func_instance
        .args
        .types()
        .next()
        .expect("align_of_val intrinsic has a type argument");
    let tail = tcx.struct_tail_for_codegen(measured_ty, TypingEnv::fully_monomorphized());
    if matches!(tail.kind(), TyKind::Dynamic(..)) {
        let layout = tcx
        .layout_of(
            TypingEnv::fully_monomorphized()
                .as_query_input(measured_ty),
        )
        .unwrap_or_else(|error| {
            panic!(
                "could not determine align_of_val trait DST layout for {measured_ty:?}: {error:?}"
            )
        });
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "alignOfTraitTailed".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "value".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    ("prefix_alignment".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(oomir_output_type.clone()),
                is_static: true,
            },
            args: vec![
                oomir_operands[0].clone(),
                oomir::Operand::Constant(oomir::Constant::U64(layout.align.abi.bytes())),
            ],
        });
    } else {
        let alignment = match measured_ty.kind() {
            TyKind::Slice(element_ty) => crate::lower1::types::layout_align_bytes(tcx, *element_ty),
            TyKind::Str => Ok(1),
            _ => crate::lower1::types::layout_align_bytes(tcx, measured_ty),
        }
        .unwrap_or_else(|error| {
            panic!("could not determine align_of_val layout for {measured_ty:?}: {error}")
        });
        instructions.push(oomir::Instruction::Move {
            dest,
            src: oomir::Operand::Constant(oomir::Constant::U64(alignment as u64)),
        });
    }
}
pub(super) fn type_id_eq<'tcx>(
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    dest: String,
) {
    let type_id_ty = oomir_operands[0]
        .get_type()
        .expect("TypeId equality operands are typed");
    let oomir::Type::Class(type_id_class) = &type_id_ty else {
        panic!("type_id_eq received non-class operand {type_id_ty:?}");
    };
    let data_ty = match data_types.get(type_id_class) {
        Some(oomir::DataType::Class { fields, .. }) => fields
            .iter()
            .find(|(name, _)| name == "data")
            .map(|(_, ty)| ty.clone())
            .expect("TypeId class has a data field"),
        _ => panic!("TypeId class {type_id_class} is not defined"),
    };
    let oomir::Type::Array(pointer_ty) = &data_ty else {
        panic!("TypeId data field is not an array: {data_ty:?}");
    };
    let left_data = format!("{label}_type_id_left_data");
    let right_data = format!("{label}_type_id_right_data");
    instructions.push(oomir::Instruction::GetField {
        dest: left_data.clone(),
        object: oomir_operands[0].clone(),
        field_name: "data".to_string(),
        field_ty: data_ty.clone(),
        owner_class: type_id_class.clone(),
    });
    instructions.push(oomir::Instruction::GetField {
        dest: right_data.clone(),
        object: oomir_operands[1].clone(),
        field_name: "data".to_string(),
        field_ty: data_ty.clone(),
        owner_class: type_id_class.clone(),
    });

    let limb_count = 16usize
        / usize::try_from(tcx.data_layout.pointer_size().bytes()).expect("pointer size fits usize");
    let mut equality = None;
    for limb in 0..limb_count {
        let left_limb = format!("{label}_type_id_left_{limb}");
        let right_limb = format!("{label}_type_id_right_{limb}");
        let limb_equal = format!("{label}_type_id_equal_{limb}");
        let index = oomir::Operand::Constant(oomir::Constant::I32(limb as i32));
        instructions.push(oomir::Instruction::ArrayGet {
            dest: left_limb.clone(),
            array: oomir::Operand::Variable {
                name: left_data.clone(),
                ty: data_ty.clone(),
            },
            index: index.clone(),
        });
        instructions.push(oomir::Instruction::ArrayGet {
            dest: right_limb.clone(),
            array: oomir::Operand::Variable {
                name: right_data.clone(),
                ty: data_ty.clone(),
            },
            index,
        });
        instructions.push(oomir::Instruction::InvokeVirtual {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "sameAddress".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), pointer_ty.as_ref().clone()),
                    ("other".to_string(), pointer_ty.as_ref().clone()),
                ],
                ret: Box::new(oomir::Type::Boolean),
                is_static: false,
            },
            args: vec![oomir::Operand::Variable {
                name: right_limb,
                ty: pointer_ty.as_ref().clone(),
            }],
            dest: Some(limb_equal.clone()),
            operand: oomir::Operand::Variable {
                name: left_limb,
                ty: pointer_ty.as_ref().clone(),
            },
        });
        let limb_equal = oomir::Operand::Variable {
            name: limb_equal,
            ty: oomir::Type::Boolean,
        };
        equality = Some(if let Some(previous) = equality {
            let combined = format!("{label}_type_id_combined_{limb}");
            instructions.push(oomir::Instruction::Binary {
                op: crate::oomir::BinaryOp::BitAnd,
                dest: combined.clone(),
                op1: previous,
                op2: limb_equal,
            });
            oomir::Operand::Variable {
                name: combined,
                ty: oomir::Type::Boolean,
            }
        } else {
            limb_equal
        });
    }
    instructions.push(oomir::Instruction::Move {
        dest,
        src: equality.unwrap_or(oomir::Operand::Constant(oomir::Constant::Boolean(true))),
    });
}
