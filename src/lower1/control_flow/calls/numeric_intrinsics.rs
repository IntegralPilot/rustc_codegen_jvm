//! Numeric intrinsics.
use super::*;

pub(super) fn algebraic_float<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    intrinsic_name: String,
    dest: String,
) {
    let left = oomir_operands[0].clone();
    let right = oomir_operands[1].clone();
    instructions.push(match intrinsic_name.as_str() {
        "fadd_algebraic" | "fadd_fast" => oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Add,
            dest,
            op1: left,
            op2: right,
        },
        "fsub_algebraic" | "fsub_fast" => oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Sub,
            dest,
            op1: left,
            op2: right,
        },
        "fmul_algebraic" | "fmul_fast" => oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Mul,
            dest,
            op1: left,
            op2: right,
        },
        "fdiv_algebraic" | "fdiv_fast" => oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Div,
            dest,
            op1: left,
            op2: right,
        },
        "frem_algebraic" | "frem_fast" => oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Rem,
            dest,
            op1: left,
            op2: right,
        },
        _ => unreachable!(),
    });
}
pub(super) fn simd_splat<'tcx>(
    tcx: TyCtxt<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    fn_inputs: Vec<Ty<'tcx>>,
    fn_output: Ty<'tcx>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    dest: String,
) {
    let oomir::Type::Class(result_class) = &oomir_output_type else {
        panic!("simd_splat result is not a generated SIMD carrier");
    };
    let vector_size = crate::lower1::types::layout_size_bytes(tcx, fn_output)
        .expect("SIMD result must have a layout");
    let lane_size = crate::lower1::types::layout_size_bytes(tcx, fn_inputs[0])
        .expect("SIMD lane must have a layout");
    let lane_count = vector_size
        .checked_div(lane_size)
        .filter(|count| *count != 0)
        .expect("SIMD lane layout must be nonzero");
    let object_ty = oomir::Type::Class("java/lang/Object".to_string());
    let result_object = format!("{dest}_simd_object");
    let lane_ty = oomir_operands[0]
        .get_type()
        .expect("simd_splat lane is typed");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: "org/rustlang/runtime/Intrinsics".to_string(),
        method_name: "simdSplat".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("value".to_string(), lane_ty),
                ("result_class".to_string(), oomir::Type::java_string()),
                ("lane_count".to_string(), oomir::Type::U32),
            ],
            ret: Box::new(object_ty.clone()),
            is_static: true,
        },
        args: vec![
            oomir_operands[0].clone(),
            oomir::Operand::Constant(oomir::Constant::String(result_class.clone())),
            oomir::Operand::Constant(oomir::Constant::U32(
                u32::try_from(lane_count).expect("SIMD lane count exceeds u32"),
            )),
        ],
        dest: Some(result_object.clone()),
    });
    instructions.push(oomir::Instruction::Cast {
        op: oomir::Operand::Variable {
            name: result_object,
            ty: object_ty,
        },
        ty: oomir_output_type.clone(),
        dest,
    });
}
pub(super) fn simd_bitwise<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    intrinsic_name: String,
) {
    let object_ty = oomir::Type::Class("java/lang/Object".to_string());
    let dest = effective_dest
        .clone()
        .expect("SIMD bitwise operation has a result");
    let result_object = format!("{dest}_simd_object");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: "org/rustlang/runtime/Intrinsics".to_string(),
        method_name: "simdBitwise".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("operation".to_string(), oomir::Type::java_string()),
                ("left".to_string(), object_ty.clone()),
                ("right".to_string(), object_ty.clone()),
            ],
            ret: Box::new(object_ty.clone()),
            is_static: true,
        },
        args: vec![
            oomir::Operand::Constant(oomir::Constant::String(intrinsic_name.clone())),
            oomir_operands[0].clone(),
            oomir_operands[1].clone(),
        ],
        dest: Some(result_object.clone()),
    });
    instructions.push(oomir::Instruction::Cast {
        op: oomir::Operand::Variable {
            name: result_object,
            ty: object_ty,
        },
        ty: oomir_output_type.clone(),
        dest,
    });
}
pub(super) fn simd_comparison<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    intrinsic_name: String,
    dest: String,
) {
    let oomir::Type::Class(result_class) = &oomir_output_type else {
        panic!("SIMD comparison result is not a generated SIMD carrier");
    };
    let object_ty = oomir::Type::Class("java/lang/Object".to_string());
    let result_object = format!("{dest}_simd_object");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: "org/rustlang/runtime/Intrinsics".to_string(),
        method_name: "simdCompare".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("operation".to_string(), oomir::Type::java_string()),
                ("left".to_string(), object_ty.clone()),
                ("right".to_string(), object_ty.clone()),
                ("result_class".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(object_ty.clone()),
            is_static: true,
        },
        args: vec![
            oomir::Operand::Constant(oomir::Constant::String(intrinsic_name.clone())),
            oomir_operands[0].clone(),
            oomir_operands[1].clone(),
            oomir::Operand::Constant(oomir::Constant::String(result_class.clone())),
        ],
        dest: Some(result_object.clone()),
    });
    instructions.push(oomir::Instruction::Cast {
        op: oomir::Operand::Variable {
            name: result_object,
            ty: object_ty,
        },
        ty: oomir_output_type.clone(),
        dest,
    });
}
pub(super) fn simd_unary<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    mut oomir_operands: Vec<oomir::Operand>,
    intrinsic_name: String,
    dest: String,
) {
    let object_ty = oomir::Type::Class("java/lang/Object".to_string());
    let result_object = format!("{dest}_simd_object");
    let (method_name, params) = if intrinsic_name == "simd_mul" {
        (
            "simdMultiply",
            vec![
                ("left".to_string(), object_ty.clone()),
                ("right".to_string(), object_ty.clone()),
            ],
        )
    } else {
        oomir_operands.insert(
            0,
            oomir::Operand::Constant(oomir::Constant::String(intrinsic_name.clone())),
        );
        (
            "simdUnary",
            vec![
                ("operation".to_string(), oomir::Type::java_string()),
                ("vector".to_string(), object_ty.clone()),
            ],
        )
    };
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: "org/rustlang/runtime/Intrinsics".to_string(),
        method_name: method_name.to_string(),
        method_ty: oomir::Signature {
            params,
            ret: Box::new(object_ty.clone()),
            is_static: true,
        },
        args: oomir_operands.clone(),
        dest: Some(result_object.clone()),
    });
    instructions.push(oomir::Instruction::Cast {
        op: oomir::Operand::Variable {
            name: result_object,
            ty: object_ty,
        },
        ty: oomir_output_type.clone(),
        dest,
    });
}
pub(super) fn rotate<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    intrinsic_name: String,
) {
    let runtime_signature = oomir::Signature {
        params: oomir_operands
            .iter()
            .enumerate()
            .map(|(index, operand)| {
                (
                    format!("arg{index}"),
                    operand
                        .get_type()
                        .expect("integer intrinsic operand is typed"),
                )
            })
            .collect(),
        ret: Box::new(oomir_output_type.clone()),
        is_static: true,
    };
    let method_name = match intrinsic_name.as_str() {
        "rotate_left" => "rotateLeft",
        "rotate_right" => "rotateRight",
        "bitreverse" => "bitReverse",
        _ => unreachable!(),
    };
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: "org/rustlang/runtime/Numbers".to_string(),
        method_name: method_name.to_string(),
        method_ty: runtime_signature,
        args: oomir_operands.clone(),
        dest: effective_dest.clone(),
    });
}
pub(super) fn float_math<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    intrinsic_name: String,
) {
    let runtime_signature = oomir::Signature {
        params: oomir_operands
            .iter()
            .enumerate()
            .map(|(index, operand)| {
                (
                    format!("arg{index}"),
                    operand
                        .get_type()
                        .expect("float intrinsic operand is typed"),
                )
            })
            .collect(),
        ret: Box::new(oomir_output_type.clone()),
        is_static: true,
    };
    let runtime_method_name = if matches!(
        intrinsic_name.as_str(),
        "sin" | "cos" | "exp" | "exp2" | "log" | "log2" | "log10"
    ) {
        let suffix = match &oomir_output_type {
            oomir::Type::F16 => "f16",
            oomir::Type::F32 => "f32",
            oomir::Type::F64 => "f64",
            oomir::Type::Class(class_name) if class_name == crate::lower2::F128_CLASS => "f128",
            other => panic!("generic float intrinsic {intrinsic_name} returned {other:?}"),
        };
        format!("{intrinsic_name}{suffix}")
    } else {
        intrinsic_name
    };
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: "org/rustlang/runtime/Intrinsics".to_string(),
        method_name: runtime_method_name,
        method_ty: runtime_signature,
        args: oomir_operands.clone(),
        dest: effective_dest.clone(),
    });
}

pub(super) fn integer_min<'tcx>(
    mut instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    intrinsic_name: String,
    dest: String,
) {
    let left = oomir_operands[0].clone();
    let right = oomir_operands[1].clone();
    let choose_left = format!("{dest}_choose_left");
    let (lesser, greater) = if intrinsic_name == "integer_min" {
        (left.clone(), right.clone())
    } else {
        (right.clone(), left.clone())
    };
    instructions.push(oomir::Instruction::Binary {
        op: crate::oomir::BinaryOp::Lt,
        dest: choose_left.clone(),
        op1: lesser,
        op2: greater,
    });
    emit_value_selection(
        oomir::Operand::Variable {
            name: choose_left,
            ty: oomir::Type::Boolean,
        },
        left,
        right,
        &oomir_output_type,
        dest,
        &mut instructions,
    );
}
pub(super) fn carrying_mul_add<'tcx>(
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    dest: String,
) {
    let input_ty = oomir_operands[0]
        .get_type()
        .expect("carrying_mul_add operand is typed");
    let (suffix, low_ty) = match &input_ty {
        oomir::Type::I8 => ("I8", oomir::Type::U8),
        oomir::Type::I16 => ("I16", oomir::Type::U16),
        oomir::Type::I32 => ("I32", oomir::Type::U32),
        oomir::Type::I64 => ("I64", oomir::Type::U64),
        oomir::Type::Class(class_name) if class_name == crate::lower2::I128_CLASS => (
            "I128",
            oomir::Type::Class(crate::lower2::U128_CLASS.to_string()),
        ),
        _ => unreachable!("signed carrying_mul_add type was checked"),
    };
    let params = (0..4)
        .map(|index| (format!("arg{index}"), input_ty.clone()))
        .collect::<Vec<_>>();
    let low_dest = format!("{label}_carrying_low");
    let high_dest = format!("{label}_carrying_high");
    for (method_name, result_ty, result_dest) in [
        (
            format!("carryingLow{suffix}"),
            low_ty.clone(),
            low_dest.clone(),
        ),
        (
            format!("carryingHigh{suffix}"),
            input_ty.clone(),
            high_dest.clone(),
        ),
    ] {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: "org/rustlang/runtime/Numbers".to_string(),
            method_name,
            method_ty: oomir::Signature {
                params: params.clone(),
                ret: Box::new(result_ty),
                is_static: true,
            },
            args: oomir_operands.clone(),
            dest: Some(result_dest),
        });
    }
    instructions.push(oomir::Instruction::ConstructObject {
        dest,
        class_name: oomir_output_type
            .get_class_name()
            .expect("carrying_mul_add returns a tuple class")
            .to_string(),
        args: vec![
            (
                oomir::Operand::Variable {
                    name: low_dest,
                    ty: low_ty.clone(),
                },
                low_ty,
            ),
            (
                oomir::Operand::Variable {
                    name: high_dest,
                    ty: input_ty.clone(),
                },
                input_ty,
            ),
        ],
    });
}
pub(super) fn ctpop<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    dest: String,
) {
    let operand = oomir_operands
        .first()
        .cloned()
        .expect("ctpop has an integer operand");
    let operand_ty = operand
        .get_type()
        .expect("ctpop integer operand has a JVM type");
    let (method_name, parameter_ty, call_args) = match operand_ty {
        oomir::Type::I8 | oomir::Type::U8 => (
            "bitCount32",
            oomir::Type::I32,
            vec![operand, oomir::Operand::Constant(oomir::Constant::I32(8))],
        ),
        oomir::Type::I16 | oomir::Type::U16 => (
            "bitCount32",
            oomir::Type::I32,
            vec![operand, oomir::Operand::Constant(oomir::Constant::I32(16))],
        ),
        oomir::Type::I32 | oomir::Type::U32 => (
            "bitCount32",
            oomir::Type::I32,
            vec![operand, oomir::Operand::Constant(oomir::Constant::I32(32))],
        ),
        oomir::Type::I64 | oomir::Type::U64 => ("bitCount64", oomir::Type::I64, vec![operand]),
        oomir::Type::Class(ref class_name) if class_name == crate::lower2::I128_CLASS => {
            ("bitCountI128", operand_ty.clone(), vec![operand])
        }
        oomir::Type::Class(ref class_name) if class_name == crate::lower2::U128_CLASS => {
            ("bitCountU128", operand_ty.clone(), vec![operand])
        }
        other => panic!("unsupported ctpop integer carrier {other:?}"),
    };
    let mut params = vec![("value".to_string(), parameter_ty)];
    if call_args.len() == 2 {
        params.push(("bit_width".to_string(), oomir::Type::I32));
    }
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest),
        class_name: "org/rustlang/runtime/Numbers".to_string(),
        method_name: method_name.to_string(),
        method_ty: oomir::Signature {
            params,
            ret: Box::new(oomir::Type::U32),
            is_static: true,
        },
        args: call_args,
    });
}
pub(super) fn count_zero_bits<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    intrinsic_name: String,
    dest: String,
) {
    let operand = oomir_operands
        .first()
        .cloned()
        .expect("zero-count intrinsic has an integer operand");
    let operand_ty = operand
        .get_type()
        .expect("zero-count integer operand has a JVM type");
    let leading = intrinsic_name.starts_with("ctlz");
    let operation = if leading {
        "leadingZeros"
    } else {
        "trailingZeros"
    };
    let (method_name, parameter_ty, call_args) = match operand_ty {
        oomir::Type::I8 | oomir::Type::U8 => (
            format!("{operation}32"),
            oomir::Type::I32,
            vec![operand, oomir::Operand::Constant(oomir::Constant::I32(8))],
        ),
        oomir::Type::I16 | oomir::Type::U16 => (
            format!("{operation}32"),
            oomir::Type::I32,
            vec![operand, oomir::Operand::Constant(oomir::Constant::I32(16))],
        ),
        oomir::Type::I32 | oomir::Type::U32 => (
            format!("{operation}32"),
            oomir::Type::I32,
            vec![operand, oomir::Operand::Constant(oomir::Constant::I32(32))],
        ),
        oomir::Type::I64 | oomir::Type::U64 => {
            (format!("{operation}64"), oomir::Type::I64, vec![operand])
        }
        oomir::Type::Class(ref class_name) if class_name == crate::lower2::I128_CLASS => (
            format!("{operation}I128"),
            operand_ty.clone(),
            vec![operand],
        ),
        oomir::Type::Class(ref class_name) if class_name == crate::lower2::U128_CLASS => (
            format!("{operation}U128"),
            operand_ty.clone(),
            vec![operand],
        ),
        other => {
            panic!("unsupported zero-count integer carrier {other:?}")
        }
    };
    let mut params = vec![("value".to_string(), parameter_ty)];
    if call_args.len() == 2 {
        params.push(("bit_width".to_string(), oomir::Type::I32));
    }
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest),
        class_name: "org/rustlang/runtime/Numbers".to_string(),
        method_name,
        method_ty: oomir::Signature {
            params,
            ret: Box::new(oomir::Type::U32),
            is_static: true,
        },
        args: call_args,
    });
}
pub(super) fn saturating_arithmetic<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    intrinsic_name: String,
    dest: String,
) {
    let operation = if intrinsic_name == "saturating_add" {
        "Add"
    } else {
        "Sub"
    };
    let suffix = match &oomir_output_type {
        oomir::Type::I8 => "I8",
        oomir::Type::U8 => "U8",
        oomir::Type::I16 => "I16",
        oomir::Type::U16 => "U16",
        oomir::Type::I32 => "I32",
        oomir::Type::U32 => "U32",
        oomir::Type::I64 => "I64",
        oomir::Type::U64 => "U64",
        oomir::Type::Class(class_name) if class_name == crate::lower2::I128_CLASS => "I128",
        oomir::Type::Class(class_name) if class_name == crate::lower2::U128_CLASS => "U128",
        ref other => {
            panic!("unsupported saturating intrinsic carrier {other:?}")
        }
    };
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest),
        class_name: "org/rustlang/runtime/Numbers".to_string(),
        method_name: format!("saturating{operation}{suffix}"),
        method_ty: oomir::Signature {
            params: vec![
                ("left".to_string(), oomir_output_type.clone()),
                ("right".to_string(), oomir_output_type.clone()),
            ],
            ret: Box::new(oomir_output_type.clone()),
            is_static: true,
        },
        args: oomir_operands.clone(),
    });
}
