use crate::oomir::{Constant, Instruction, Operand, Type};

pub fn emit_checked_arithmetic_oomir_instructions(
    dest_base_name: &str,
    op1: &Operand,
    op2: &Operand,
    op_ty: &Type,
    operation: &str,         // "add", "sub", "mul"
    unique_id_offset: usize, // Used to ensure unique labels/temps
) -> (Vec<Instruction>, String, String) {
    let mut generated_instructions = Vec::new();
    let unique_id = unique_id_offset; // Use offset for uniqueness

    let tmp_result = format!("{}_{}_chk_res_{}", dest_base_name, operation, unique_id);
    let tmp_overflow = format!("{}_{}_chk_ovf_{}", dest_base_name, operation, unique_id);

    if matches!(op_ty, Type::Class(c) if c == crate::lower2::BIG_INTEGER_CLASS || c == crate::lower2::BIG_DECIMAL_CLASS)
    {
        // For BigInt/BigDec, overflow doesn't happen in the fixed-size sense.
        // Perform the regular operation and always set overflow to false.

        // 1. Perform the actual operation
        let op_instr = match operation {
            "add" => Instruction::Add {
                dest: tmp_result.clone(),
                op1: op1.clone(),
                op2: op2.clone(),
            },
            "sub" => Instruction::Sub {
                dest: tmp_result.clone(),
                op1: op1.clone(),
                op2: op2.clone(),
            },
            "mul" => Instruction::Mul {
                dest: tmp_result.clone(),
                op1: op1.clone(),
                op2: op2.clone(),
            },
            _ => panic!(
                "Unsupported checked operation for BigInt/BigDec: {}",
                operation
            ),
        };
        generated_instructions.push(op_instr);

        // 2. Set overflow flag to false
        generated_instructions.push(Instruction::Move {
            dest: tmp_overflow.clone(),
            src: Operand::Constant(Constant::Boolean(false)),
        });

        return (generated_instructions, tmp_result, tmp_overflow);
    }

    // --- Handle Primitive Integer/Float Types ---

    // Generate unique temporary variable names and labels (only needed for primitives)
    let tmp_a = format!("{}_{}_chk_a_{}", dest_base_name, operation, unique_id);
    let tmp_b = format!("{}_{}_chk_b_{}", dest_base_name, operation, unique_id);

    // Labels for control flow within this sequence
    let label_check_neg = format!(
        "label_{}_{}_chk_neg_{}",
        dest_base_name, operation, unique_id
    );
    let label_overflow = format!(
        "label_{}_{}_chk_ovf_{}",
        dest_base_name, operation, unique_id
    );
    let label_no_overflow = format!(
        "label_{}_{}_chk_no_ovf_{}",
        dest_base_name, operation, unique_id
    );
    let label_end = format!(
        "label_{}_{}_chk_end_{}",
        dest_base_name, operation, unique_id
    );

    // Labels for intermediate targets within the checks
    let lbl_pos_check_b_non_neg = format!(
        "lbl_{}_{}_pos_chk_b_non_neg_{}",
        dest_base_name, operation, unique_id
    );
    let lbl_pos_check_final_cmp = format!(
        "lbl_{}_{}_pos_chk_final_cmp_{}",
        dest_base_name, operation, unique_id
    );
    let lbl_neg_check_b_non_pos = format!(
        "lbl_{}_{}_neg_chk_b_non_pos_{}",
        dest_base_name, operation, unique_id
    );
    let lbl_neg_check_final_cmp = format!(
        "lbl_{}_{}_neg_chk_final_cmp_{}",
        dest_base_name, operation, unique_id
    );

    // Get MIN/MAX constants and the type-specific zero constant for primitives
    let (const_max_op, const_min_op, zero_const_op) = match op_ty {
        Type::I8 => (
            Some(Constant::I8(i8::MAX)),
            Some(Constant::I8(i8::MIN)),
            Some(Constant::I8(0)),
        ),
        Type::I16 => (
            Some(Constant::I16(i16::MAX)),
            Some(Constant::I16(i16::MIN)),
            Some(Constant::I16(0)),
        ),
        Type::I32 => (
            Some(Constant::I32(i32::MAX)),
            Some(Constant::I32(i32::MIN)),
            Some(Constant::I32(0)),
        ),
        Type::I64 => (
            Some(Constant::I64(i64::MAX)),
            Some(Constant::I64(i64::MIN)),
            Some(Constant::I64(0)),
        ),
        _ => panic!(
            "Checked arithmetic MIN/MAX/Zero constants not defined for OOMIR type {:?}",
            op_ty
        ),
    };

    // Ensure we have constants for integer types before proceeding with integer logic
    let (const_max, const_min, zero_const) = match (const_max_op, const_min_op, zero_const_op) {
        (Some(max), Some(min), Some(zero)) => (max, min, zero),
        (_, _, _) => {
            // currently impossible for good for correct error reporting in future
            panic!(
                "Checked arithmetic MAX constant not defined for OOMIR type {:?}",
                op_ty
            )
        }
    };

    // --- Load operands into temporary variables (for primitive integer logic) ---
    generated_instructions.push(Instruction::Move {
        dest: tmp_a.clone(),
        src: op1.clone(),
    });
    generated_instructions.push(Instruction::Move {
        dest: tmp_b.clone(),
        src: op2.clone(),
    });

    let op1_var = Operand::Variable {
        name: tmp_a.clone(),
        ty: op_ty.clone(),
    };
    let op2_var = Operand::Variable {
        name: tmp_b.clone(),
        ty: op_ty.clone(),
    };

    // --- Start Positive Overflow Check (Integer Logic) ---
    let tmp_cmp1 = format!("{}_{}_chk_cmp1_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(Instruction::Gt {
        dest: tmp_cmp1.clone(),
        op1: op1_var.clone(),
        op2: Operand::Constant(zero_const.clone()),
    });
    generated_instructions.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp1.clone(),
            ty: Type::Boolean,
        },
        true_block: lbl_pos_check_b_non_neg.clone(),
        false_block: label_check_neg.clone(),
    });

    // --- Positive Check: Check B ---
    generated_instructions.push(Instruction::Label {
        name: lbl_pos_check_b_non_neg.clone(),
    });
    let tmp_cmp2 = format!("{}_{}_chk_cmp2_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(Instruction::Gt {
        dest: tmp_cmp2.clone(),
        op1: op2_var.clone(),
        op2: Operand::Constant(zero_const.clone()),
    });
    generated_instructions.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp2.clone(),
            ty: Type::Boolean,
        },
        true_block: lbl_pos_check_final_cmp.clone(),
        false_block: label_check_neg.clone(), // If b <= 0, can't positive overflow, check neg
    });

    // --- Positive Check: Final Comparison (b > MAX - a) ---
    generated_instructions.push(Instruction::Label {
        name: lbl_pos_check_final_cmp.clone(),
    });
    let tmp_max_minus_a = format!(
        "{}_{}_chk_max_minus_a_{}",
        dest_base_name, operation, unique_id
    );
    let tmp_cmp3 = format!("{}_{}_chk_cmp3_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(Instruction::Sub {
        dest: tmp_max_minus_a.clone(),
        op1: Operand::Constant(const_max.clone()), // MAX
        op2: op1_var.clone(),                      // a
    });
    generated_instructions.push(Instruction::Gt {
        dest: tmp_cmp3.clone(),
        op1: op2_var.clone(), // b
        op2: Operand::Variable {
            name: tmp_max_minus_a.clone(),
            ty: op_ty.clone(),
        }, // MAX - a
    });
    generated_instructions.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp3.clone(),
            ty: Type::Boolean,
        },
        true_block: label_overflow.clone(), // If b > MAX - a, OVERFLOW
        false_block: label_check_neg.clone(), // Else, check negative overflow
    });

    // --- Start Negative Overflow Check ---
    generated_instructions.push(Instruction::Label {
        name: label_check_neg.clone(),
    });
    let tmp_cmp4 = format!("{}_{}_chk_cmp4_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(Instruction::Lt {
        dest: tmp_cmp4.clone(),
        op1: op1_var.clone(),
        op2: Operand::Constant(zero_const.clone()),
    });
    generated_instructions.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp4.clone(),
            ty: Type::Boolean,
        },
        true_block: lbl_neg_check_b_non_pos.clone(), // If a < 0, check b
        false_block: label_no_overflow.clone(), // If a >= 0, can't negative overflow (already checked pos)
    });

    // --- Negative Check: Check B ---
    generated_instructions.push(Instruction::Label {
        name: lbl_neg_check_b_non_pos.clone(),
    });
    let tmp_cmp5 = format!("{}_{}_chk_cmp5_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(Instruction::Lt {
        dest: tmp_cmp5.clone(),
        op1: op2_var.clone(),
        op2: Operand::Constant(zero_const.clone()),
    });
    generated_instructions.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp5.clone(),
            ty: Type::Boolean,
        },
        true_block: lbl_neg_check_final_cmp.clone(), // If b < 0, do final check
        false_block: label_no_overflow.clone(),      // If b >= 0, can't negative overflow
    });

    // --- Negative Check: Final Comparison (b < MIN - a) ---
    generated_instructions.push(Instruction::Label {
        name: lbl_neg_check_final_cmp.clone(),
    });
    let tmp_min_minus_a = format!(
        "{}_{}_chk_min_minus_a_{}",
        dest_base_name, operation, unique_id
    );
    let tmp_cmp6 = format!("{}_{}_chk_cmp6_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(Instruction::Sub {
        dest: tmp_min_minus_a.clone(),
        op1: Operand::Constant(const_min.clone()), // MIN
        op2: op1_var.clone(),                      // a
    });
    generated_instructions.push(Instruction::Lt {
        dest: tmp_cmp6.clone(),
        op1: op2_var.clone(), // b
        op2: Operand::Variable {
            name: tmp_min_minus_a.clone(),
            ty: op_ty.clone(),
        }, // MIN - a
    });
    generated_instructions.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp6.clone(),
            ty: Type::Boolean,
        },
        true_block: label_overflow.clone(), // If b < MIN - a, OVERFLOW
        false_block: label_no_overflow.clone(), // Else, NO overflow
    });

    // --- Overflow Path ---
    generated_instructions.push(Instruction::Label {
        name: label_overflow.clone(),
    });
    generated_instructions.push(Instruction::Move {
        dest: tmp_overflow.clone(),
        src: Operand::Constant(Constant::Boolean(true)), // Set overflow flag
    });
    // Store zero in result on overflow (consistent with Rust's checked_add etc.)
    generated_instructions.push(Instruction::Move {
        dest: tmp_result.clone(),
        src: Operand::Constant(zero_const.clone()),
    });
    generated_instructions.push(Instruction::Jump {
        target: label_end.clone(),
    });

    // --- No Overflow Path ---
    generated_instructions.push(Instruction::Label {
        name: label_no_overflow.clone(),
    });
    generated_instructions.push(Instruction::Move {
        dest: tmp_overflow.clone(),
        src: Operand::Constant(Constant::Boolean(false)), // Clear overflow flag
    });
    // Perform actual operation
    let op_instr = match operation {
        "add" => Instruction::Add {
            dest: tmp_result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        },
        "sub" => Instruction::Sub {
            dest: tmp_result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        },
        "mul" => Instruction::Mul {
            dest: tmp_result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        },
        // Add other checked operations (div, rem, shl, shr?) here if needed
        _ => panic!(
            "Unsupported checked operation for Primitives: {}",
            operation
        ),
    };
    generated_instructions.push(op_instr);
    generated_instructions.push(Instruction::Jump {
        target: label_end.clone(),
    });

    // --- End Path ---
    generated_instructions.push(Instruction::Label {
        name: label_end.clone(),
    });
    // Result is in tmp_result, overflow flag in tmp_overflow.

    (generated_instructions, tmp_result, tmp_overflow)
}
