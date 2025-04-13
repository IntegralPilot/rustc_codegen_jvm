use crate::oomir;

pub fn emit_checked_arithmetic_oomir_instructions(
    dest_base_name: &str,
    op1: &oomir::Operand,
    op2: &oomir::Operand,
    op_ty: &oomir::Type,
    operation: &str,
    unique_id_offset: usize, // Used to ensure unique labels/temps
) -> (Vec<oomir::Instruction>, String, String) {
    let mut generated_instructions = Vec::new();

    // --- Generate unique temporary variable names and labels ---
    let unique_id = unique_id_offset; // Use offset for uniqueness
    let tmp_a = format!("{}_{}_chk_a_{}", dest_base_name, operation, unique_id);
    let tmp_b = format!("{}_{}_chk_b_{}", dest_base_name, operation, unique_id);
    let tmp_result = format!("{}_{}_chk_res_{}", dest_base_name, operation, unique_id);
    let tmp_overflow = format!("{}_{}_chk_ovf_{}", dest_base_name, operation, unique_id);

    // Labels for control flow *within* this sequence
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

    // Labels for intermediate targets within the checks to avoid unstructured jumps
    let lbl_pos_check_b_non_neg = format!(
        "lbl_{}_{}_pos_chk_b_non_neg_{}",
        dest_base_name, operation, unique_id
    );
    let lbl_pos_check_final_cmp = format!(
        "lbl_{}_{}_pos_check_final_cmp_{}",
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

    // --- Load operands into temporary variables ---
    generated_instructions.push(oomir::Instruction::Move {
        dest: tmp_a.clone(),
        src: op1.clone(),
    });
    generated_instructions.push(oomir::Instruction::Move {
        dest: tmp_b.clone(),
        src: op2.clone(),
    });

    let op1_var = oomir::Operand::Variable {
        name: tmp_a.clone(),
        ty: op_ty.clone(),
    };
    let op2_var = oomir::Operand::Variable {
        name: tmp_b.clone(),
        ty: op_ty.clone(),
    };

    // --- Get MIN/MAX constants ---
    let (const_max, const_min) = match op_ty {
        oomir::Type::I32 => (
            oomir::Constant::I32(i32::MAX),
            oomir::Constant::I32(i32::MIN),
        ),
        // TODO: Add other types like I64
        _ => panic!(
            "Checked arithmetic not implemented for OOMIR type {:?}",
            op_ty
        ),
    };

    // --- Start Positive Overflow Check ---
    let tmp_cmp1 = format!("{}_{}_chk_cmp1_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Gt {
        dest: tmp_cmp1.clone(),
        op1: op1_var.clone(),
        op2: oomir::Operand::Constant(oomir::Constant::I32(0)),
    });
    generated_instructions.push(oomir::Instruction::Branch {
        condition: oomir::Operand::Variable {
            name: tmp_cmp1.clone(),
            ty: oomir::Type::Boolean,
        },
        true_block: lbl_pos_check_b_non_neg.clone(),
        false_block: label_check_neg.clone(),
    }); // If a > 0 goto check b, else goto neg check

    // --- Positive Check: Check B --- (Label: lbl_pos_check_b_non_neg)
    generated_instructions.push(oomir::Instruction::Label {
        name: lbl_pos_check_b_non_neg.clone(),
    });
    let tmp_cmp2 = format!("{}_{}_chk_cmp2_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Gt {
        dest: tmp_cmp2.clone(),
        op1: op2_var.clone(),
        op2: oomir::Operand::Constant(oomir::Constant::I32(0)),
    });
    generated_instructions.push(oomir::Instruction::Branch {
        condition: oomir::Operand::Variable {
            name: tmp_cmp2.clone(),
            ty: oomir::Type::Boolean,
        },
        true_block: lbl_pos_check_final_cmp.clone(),
        false_block: label_check_neg.clone(),
    }); // If b > 0 goto final check, else goto neg check

    // --- Positive Check: Final Comparison --- (Label: lbl_pos_check_final_cmp)
    generated_instructions.push(oomir::Instruction::Label {
        name: lbl_pos_check_final_cmp.clone(),
    });
    let tmp_max_minus_a = format!(
        "{}_{}_chk_max_minus_a_{}",
        dest_base_name, operation, unique_id
    );
    let tmp_cmp3 = format!("{}_{}_chk_cmp3_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Sub {
        dest: tmp_max_minus_a.clone(),
        op1: oomir::Operand::Constant(const_max),
        op2: op1_var.clone(),
    });
    generated_instructions.push(oomir::Instruction::Gt {
        dest: tmp_cmp3.clone(),
        op1: op2_var.clone(),
        op2: oomir::Operand::Variable {
            name: tmp_max_minus_a.clone(),
            ty: op_ty.clone(),
        },
    });
    generated_instructions.push(oomir::Instruction::Branch {
        condition: oomir::Operand::Variable {
            name: tmp_cmp3.clone(),
            ty: oomir::Type::Boolean,
        },
        true_block: label_overflow.clone(),
        false_block: label_check_neg.clone(),
    }); // If b > MAX-a goto overflow, else goto neg check

    // --- Start Negative Overflow Check --- (Label: label_check_neg)
    generated_instructions.push(oomir::Instruction::Label {
        name: label_check_neg.clone(),
    });
    let tmp_cmp4 = format!("{}_{}_chk_cmp4_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Lt {
        dest: tmp_cmp4.clone(),
        op1: op1_var.clone(),
        op2: oomir::Operand::Constant(oomir::Constant::I32(0)),
    });
    generated_instructions.push(oomir::Instruction::Branch {
        condition: oomir::Operand::Variable {
            name: tmp_cmp4.clone(),
            ty: oomir::Type::Boolean,
        },
        true_block: lbl_neg_check_b_non_pos.clone(),
        false_block: label_no_overflow.clone(),
    }); // If a < 0 goto check b, else goto no_overflow

    // --- Negative Check: Check B --- (Label: lbl_neg_check_b_non_pos)
    generated_instructions.push(oomir::Instruction::Label {
        name: lbl_neg_check_b_non_pos.clone(),
    });
    let tmp_cmp5 = format!("{}_{}_chk_cmp5_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Lt {
        dest: tmp_cmp5.clone(),
        op1: op2_var.clone(),
        op2: oomir::Operand::Constant(oomir::Constant::I32(0)),
    });
    generated_instructions.push(oomir::Instruction::Branch {
        condition: oomir::Operand::Variable {
            name: tmp_cmp5.clone(),
            ty: oomir::Type::Boolean,
        },
        true_block: lbl_neg_check_final_cmp.clone(),
        false_block: label_no_overflow.clone(),
    }); // If b < 0 goto final check, else goto no_overflow

    // --- Negative Check: Final Comparison --- (Label: lbl_neg_check_final_cmp)
    generated_instructions.push(oomir::Instruction::Label {
        name: lbl_neg_check_final_cmp.clone(),
    });
    let tmp_min_minus_a = format!(
        "{}_{}_chk_min_minus_a_{}",
        dest_base_name, operation, unique_id
    );
    let tmp_cmp6 = format!("{}_{}_chk_cmp6_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Sub {
        dest: tmp_min_minus_a.clone(),
        op1: oomir::Operand::Constant(const_min),
        op2: op1_var.clone(),
    });
    generated_instructions.push(oomir::Instruction::Lt {
        dest: tmp_cmp6.clone(),
        op1: op2_var.clone(),
        op2: oomir::Operand::Variable {
            name: tmp_min_minus_a.clone(),
            ty: op_ty.clone(),
        },
    });
    generated_instructions.push(oomir::Instruction::Branch {
        condition: oomir::Operand::Variable {
            name: tmp_cmp6.clone(),
            ty: oomir::Type::Boolean,
        },
        true_block: label_overflow.clone(),
        false_block: label_no_overflow.clone(),
    }); // If b < MIN-a goto overflow, else goto no_overflow

    // --- Overflow Path --- (Label: label_overflow)
    generated_instructions.push(oomir::Instruction::Label {
        name: label_overflow.clone(),
    });
    generated_instructions.push(oomir::Instruction::Move {
        dest: tmp_overflow.clone(),
        src: oomir::Operand::Constant(oomir::Constant::Boolean(true)),
    });
    // Determine zero value for the type
    let zero_val = match op_ty {
        oomir::Type::I8 => oomir::Constant::I8(0),
        oomir::Type::I16 => oomir::Constant::I16(0),
        oomir::Type::I32 => oomir::Constant::I32(0),
        oomir::Type::I64 => oomir::Constant::I64(0),
        _ => oomir::Constant::I32(0), // Fallback or panic?
    };
    generated_instructions.push(oomir::Instruction::Move {
        dest: tmp_result.clone(),
        src: oomir::Operand::Constant(zero_val),
    });
    generated_instructions.push(oomir::Instruction::Jump {
        target: label_end.clone(),
    });

    // --- No Overflow Path --- (Label: label_no_overflow)
    generated_instructions.push(oomir::Instruction::Label {
        name: label_no_overflow.clone(),
    });
    generated_instructions.push(oomir::Instruction::Move {
        dest: tmp_overflow.clone(),
        src: oomir::Operand::Constant(oomir::Constant::Boolean(false)),
    });
    // Perform actual operation
    match operation {
        "add" => generated_instructions.push(oomir::Instruction::Add {
            dest: tmp_result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        }),
        "sub" => generated_instructions.push(oomir::Instruction::Sub {
            dest: tmp_result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        }),
        "mul" => generated_instructions.push(oomir::Instruction::Mul {
            dest: tmp_result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        }),
        _ => panic!("Unsupported checked operation: {}", operation),
    }
    generated_instructions.push(oomir::Instruction::Jump {
        target: label_end.clone(),
    }); // Or just fall through to end label? Jump is safer.

    // --- End Path --- (Label: label_end)
    generated_instructions.push(oomir::Instruction::Label {
        name: label_end.clone(),
    });
    // Result is in tmp_result, overflow flag in tmp_overflow. No instruction needed here.

    // Return the generated instructions and the names of the temporary variables
    (generated_instructions, tmp_result, tmp_overflow)
}
