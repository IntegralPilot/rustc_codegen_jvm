//! Numeric methods.
use super::*;

pub(super) fn overflowing<'tcx>(
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    declared_method_name: String,
    dispatch_receiver_ty: oomir::Type,
) {
    if let Some(dest) = effective_dest {
        let tuple_class = oomir_output_type
            .get_class_name()
            .expect("overflowing integer result must be a tuple class")
            .to_string();
        let operation = declared_method_name
            .strip_prefix("overflowing_")
            .expect("overflowing operation prefix");
        let (generated, pair, _, _) = checked_ops::emit_checked_arithmetic_oomir_instructions(
            data_types,
            &dest,
            &oomir_operands[0],
            &oomir_operands[1],
            &dispatch_receiver_ty,
            operation,
            instructions.len(),
            &tuple_class,
        );
        instructions.extend(generated);
        instructions.push(oomir::Instruction::Move {
            dest,
            src: oomir::Operand::Variable {
                name: pair,
                ty: oomir::Type::Class(tuple_class),
            },
        });
    }
}
pub(super) fn wrapping<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    declared_method_name: String,
) {
    if let Some(dest) = effective_dest {
        let op1 = oomir_operands[0].clone();
        let op2 = oomir_operands[1].clone();
        let instruction = match declared_method_name.as_str() {
            "wrapping_add" => oomir::Instruction::Binary {
                op: crate::oomir::BinaryOp::Add,
                dest,
                op1,
                op2,
            },
            "wrapping_sub" => oomir::Instruction::Binary {
                op: crate::oomir::BinaryOp::Sub,
                dest,
                op1,
                op2,
            },
            "wrapping_mul" => oomir::Instruction::Binary {
                op: crate::oomir::BinaryOp::Mul,
                dest,
                op1,
                op2,
            },
            _ => unreachable!(),
        };
        instructions.push(instruction);
    }
}
pub(super) fn comparison<'tcx>(
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    fn_inputs: Vec<Ty<'tcx>>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    declared_method_name: String,
    direct_non_null_fat_equality: bool,
) {
    if let Some(dest) = effective_dest {
        let left = emit_comparison_value(
            oomir_operands[0].clone(),
            fn_inputs[0],
            &format!("{label}_comparison_left"),
            tcx,
            data_types,
            &mut instructions,
        );
        let right = emit_comparison_value(
            oomir_operands[1].clone(),
            fn_inputs[1],
            &format!("{label}_comparison_right"),
            tcx,
            data_types,
            &mut instructions,
        );
        if matches!(
            left.get_type(),
            Some(oomir::Type::Slice(_) | oomir::Type::Str)
        ) && direct_non_null_fat_equality
        {
            let equality_result = if declared_method_name == "ne" {
                format!("{dest}_fat_pointer_eq")
            } else {
                dest.clone()
            };
            let object_ty = oomir::Type::Class("java/lang/Object".to_string());
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(equality_result.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "fatPointerEquals".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("left".to_string(), object_ty.clone()),
                        ("right".to_string(), object_ty),
                    ],
                    ret: Box::new(oomir::Type::Boolean),
                    is_static: true,
                },
                args: vec![left, right],
            });
            if declared_method_name == "ne" {
                instructions.push(oomir::Instruction::Not {
                    dest,
                    src: oomir::Operand::Variable {
                        name: equality_result,
                        ty: oomir::Type::Boolean,
                    },
                });
            }
        } else if matches!(left.get_type(), Some(oomir::Type::Pointer(_))) {
            let pointer_result = if declared_method_name == "ne" {
                format!("{dest}_pointer_eq")
            } else {
                dest.clone()
            };
            let method_name = match declared_method_name.as_str() {
                "eq" | "ne" => "samePointer",
                "lt" => "lessThan",
                "le" => "lessOrEqual",
                "gt" => "greaterThan",
                "ge" => "greaterOrEqual",
                _ => unreachable!(),
            };
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: Some(pointer_result.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: method_name.to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("self".to_string(), left.get_type().unwrap()),
                        ("other".to_string(), right.get_type().unwrap()),
                    ],
                    ret: Box::new(oomir::Type::Boolean),
                    is_static: false,
                },
                args: vec![right],
                operand: left,
            });
            if declared_method_name == "ne" {
                instructions.push(oomir::Instruction::Not {
                    dest,
                    src: oomir::Operand::Variable {
                        name: pointer_result,
                        ty: oomir::Type::Boolean,
                    },
                });
            }
        } else {
            let instruction = match declared_method_name.as_str() {
                "eq" => oomir::Instruction::Binary {
                    op: crate::oomir::BinaryOp::Eq,
                    dest,
                    op1: left,
                    op2: right,
                },
                "ne" => oomir::Instruction::Binary {
                    op: crate::oomir::BinaryOp::Ne,
                    dest,
                    op1: left,
                    op2: right,
                },
                "lt" => oomir::Instruction::Binary {
                    op: crate::oomir::BinaryOp::Lt,
                    dest,
                    op1: left,
                    op2: right,
                },
                "le" => oomir::Instruction::Binary {
                    op: crate::oomir::BinaryOp::Le,
                    dest,
                    op1: left,
                    op2: right,
                },
                "gt" => oomir::Instruction::Binary {
                    op: crate::oomir::BinaryOp::Gt,
                    dest,
                    op1: left,
                    op2: right,
                },
                "ge" => oomir::Instruction::Binary {
                    op: crate::oomir::BinaryOp::Ge,
                    dest,
                    op1: left,
                    op2: right,
                },
                _ => unreachable!(),
            };
            instructions.push(instruction);
        }
    }
}
