//! Memory intrinsics.
use super::*;

pub(super) fn arith_offset<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
) {
    let pointer_ty = oomir_operands[0]
        .get_type()
        .expect("arith_offset pointer operand is typed");
    if !matches!(pointer_ty, oomir::Type::Pointer(_)) {
        panic!("arith_offset requires a pointer operand, found {pointer_ty:?}");
    }
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: effective_dest.clone(),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "offset".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), pointer_ty.clone()),
                ("count".to_string(), oomir::Type::I64),
            ],
            ret: Box::new(oomir_output_type.clone()),
            is_static: true,
        },
        args: vec![oomir_operands[0].clone(), oomir_operands[1].clone()],
    });
}
pub(super) fn raw_eq<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
    dest: String,
) {
    let compared_ty = func_instance
        .args
        .types()
        .next()
        .expect("raw_eq has a compared type argument");
    let byte_count = crate::lower1::types::layout_size_bytes(tcx, compared_ty)
        .expect("raw_eq type has a concrete layout");
    if byte_count == 0 {
        instructions.push(oomir::Instruction::Move {
            dest,
            src: oomir::Operand::Constant(oomir::Constant::Boolean(true)),
        });
    } else {
        let left = emit_raw_eq_pointer(
            oomir_operands[0].clone(),
            compared_ty,
            &format!("{label}_raw_eq_left"),
            tcx,
            instance,
            data_types,
            &mut instructions,
        );
        let right = emit_raw_eq_pointer(
            oomir_operands[1].clone(),
            compared_ty,
            &format!("{label}_raw_eq_right"),
            tcx,
            instance,
            data_types,
            &mut instructions,
        );
        let comparison = format!("{label}_raw_eq_comparison");
        let pointer_ty = left.get_type().expect("raw_eq pointer operand is typed");
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(comparison.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "compareBytes".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("left".to_string(), pointer_ty.clone()),
                    ("right".to_string(), pointer_ty),
                    ("length".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(oomir::Type::I32),
                is_static: true,
            },
            args: vec![
                left,
                right,
                oomir::Operand::Constant(oomir::Constant::U64(byte_count as u64)),
            ],
        });
        instructions.push(oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Eq,
            dest,
            op1: oomir::Operand::Variable {
                name: comparison,
                ty: oomir::Type::I32,
            },
            op2: oomir::Operand::Constant(oomir::Constant::I32(0)),
        });
    }
}

pub(super) fn compare_bytes<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
) {
    let left_ty = oomir_operands[0]
        .get_type()
        .expect("compare_bytes left operand is typed");
    let right_ty = oomir_operands[1]
        .get_type()
        .expect("compare_bytes right operand is typed");
    if !matches!(left_ty, oomir::Type::Pointer(_)) || !matches!(right_ty, oomir::Type::Pointer(_)) {
        panic!("compare_bytes requires pointer operands, found {left_ty:?} and {right_ty:?}");
    }
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: effective_dest.clone(),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "compareBytes".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("left".to_string(), left_ty),
                ("right".to_string(), right_ty),
                ("length".to_string(), oomir::Type::U64),
            ],
            ret: Box::new(oomir::Type::I32),
            is_static: true,
        },
        args: vec![
            oomir_operands[0].clone(),
            oomir_operands[1].clone(),
            oomir_operands[2].clone(),
        ],
    });
}
