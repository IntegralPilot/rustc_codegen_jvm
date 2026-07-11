// checked_intrinsics.rs
// This module emits reusable OOMIR checked arithmetic functions for integer types.
use crate::oomir::{CodeBlock, DataType, DataTypeMethod, Function, Signature};
use crate::oomir::{Constant, Instruction, Operand, Type};
use sha2::Digest;
use std::collections::HashMap;

/// Generate the intrinsic function name for a checked operation
pub fn get_intrinsic_function_name(
    operation: &str,
    ty_suffix: &str,
    result_struct_name: &str,
) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(result_struct_name.as_bytes());
    let hash = format!("{:x}", hasher.finalize());
    format!("__oomir_checked_{}_{}_{}", operation, ty_suffix, &hash[..8])
}

/// Emits a checked arithmetic intrinsic function for the given integer type
/// The function returns a CheckedArithmeticResult struct with result and overflow fields
pub fn emit_checked_arithmetic_intrinsic(
    operation: &str, // "add", "sub", "mul"
    ty: &Type,
    ty_suffix: &str, // "i32", "i64", etc.
    result_struct_name: &str,
) -> Function {
    let fn_name = get_intrinsic_function_name(operation, ty_suffix, result_struct_name);

    // Use MIR naming convention for parameters
    let a = "_1".to_string();
    let b = "_2".to_string();
    let result = "result".to_string();
    let overflow = "overflow".to_string();
    let tmp_struct = "tmp_struct".to_string();

    let mut instrs = Vec::new();

    let (const_max, const_min, zero_const) = match ty {
        Type::I8 => (
            Constant::I8(i8::MAX),
            Constant::I8(i8::MIN),
            Constant::I8(0),
        ),
        Type::I16 => (
            Constant::I16(i16::MAX),
            Constant::I16(i16::MIN),
            Constant::I16(0),
        ),
        Type::I32 => (
            Constant::I32(i32::MAX),
            Constant::I32(i32::MIN),
            Constant::I32(0),
        ),
        Type::I64 => (
            Constant::I64(i64::MAX),
            Constant::I64(i64::MIN),
            Constant::I64(0),
        ),
        _ => panic!(
            "Unsupported type for checked arithmetic intrinsic: {:?}",
            ty
        ),
    };

    let op1_var = Operand::Variable {
        name: a.clone(),
        ty: ty.clone(),
    };
    let op2_var = Operand::Variable {
        name: b.clone(),
        ty: ty.clone(),
    };

    // Generate unique labels for this function
    let label_check_neg = format!("label_{}_chk_neg", fn_name);
    let label_overflow = format!("label_{}_chk_ovf", fn_name);
    let label_no_overflow = format!("label_{}_chk_no_ovf", fn_name);
    let label_end = format!("label_{}_chk_end", fn_name);
    let lbl_pos_check_b_non_neg = format!("lbl_{}_pos_chk_b_non_neg", fn_name);
    let lbl_pos_check_final_cmp = format!("lbl_{}_pos_chk_final_cmp", fn_name);
    let lbl_neg_check_b_non_pos = format!("lbl_{}_neg_chk_b_non_pos", fn_name);
    let lbl_neg_check_final_cmp = format!("lbl_{}_neg_chk_final_cmp", fn_name);

    let tmp_cmp1 = format!("{}_chk_cmp1", fn_name);
    instrs.push(Instruction::Gt {
        dest: tmp_cmp1.clone(),
        op1: op1_var.clone(),
        op2: Operand::Constant(zero_const.clone()),
    });
    instrs.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp1.clone(),
            ty: Type::Boolean,
        },
        true_block: lbl_pos_check_b_non_neg.clone(),
        false_block: label_check_neg.clone(),
    });

    instrs.push(Instruction::Label {
        name: lbl_pos_check_b_non_neg.clone(),
    });
    let tmp_cmp2 = format!("{}_chk_cmp2", fn_name);
    instrs.push(Instruction::Gt {
        dest: tmp_cmp2.clone(),
        op1: op2_var.clone(),
        op2: Operand::Constant(zero_const.clone()),
    });
    instrs.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp2.clone(),
            ty: Type::Boolean,
        },
        true_block: lbl_pos_check_final_cmp.clone(),
        false_block: label_check_neg.clone(),
    });

    instrs.push(Instruction::Label {
        name: lbl_pos_check_final_cmp.clone(),
    });
    let tmp_max_minus_a = format!("{}_chk_max_minus_a", fn_name);
    let tmp_cmp3 = format!("{}_chk_cmp3", fn_name);
    instrs.push(Instruction::Sub {
        dest: tmp_max_minus_a.clone(),
        op1: Operand::Constant(const_max.clone()),
        op2: op1_var.clone(),
    });
    instrs.push(Instruction::Gt {
        dest: tmp_cmp3.clone(),
        op1: op2_var.clone(),
        op2: Operand::Variable {
            name: tmp_max_minus_a.clone(),
            ty: ty.clone(),
        },
    });
    instrs.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp3.clone(),
            ty: Type::Boolean,
        },
        true_block: label_overflow.clone(),
        false_block: label_check_neg.clone(),
    });

    instrs.push(Instruction::Label {
        name: label_check_neg.clone(),
    });
    let tmp_cmp4 = format!("{}_chk_cmp4", fn_name);
    instrs.push(Instruction::Lt {
        dest: tmp_cmp4.clone(),
        op1: op1_var.clone(),
        op2: Operand::Constant(zero_const.clone()),
    });
    instrs.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp4.clone(),
            ty: Type::Boolean,
        },
        true_block: lbl_neg_check_b_non_pos.clone(),
        false_block: label_no_overflow.clone(),
    });

    instrs.push(Instruction::Label {
        name: lbl_neg_check_b_non_pos.clone(),
    });
    let tmp_cmp5 = format!("{}_chk_cmp5", fn_name);
    instrs.push(Instruction::Lt {
        dest: tmp_cmp5.clone(),
        op1: op2_var.clone(),
        op2: Operand::Constant(zero_const.clone()),
    });
    instrs.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp5.clone(),
            ty: Type::Boolean,
        },
        true_block: lbl_neg_check_final_cmp.clone(),
        false_block: label_no_overflow.clone(),
    });

    instrs.push(Instruction::Label {
        name: lbl_neg_check_final_cmp.clone(),
    });
    let tmp_min_minus_a = format!("{}_chk_min_minus_a", fn_name);
    let tmp_cmp6 = format!("{}_chk_cmp6", fn_name);
    instrs.push(Instruction::Sub {
        dest: tmp_min_minus_a.clone(),
        op1: Operand::Constant(const_min.clone()),
        op2: op1_var.clone(),
    });
    instrs.push(Instruction::Lt {
        dest: tmp_cmp6.clone(),
        op1: op2_var.clone(),
        op2: Operand::Variable {
            name: tmp_min_minus_a.clone(),
            ty: ty.clone(),
        },
    });
    instrs.push(Instruction::Branch {
        condition: Operand::Variable {
            name: tmp_cmp6.clone(),
            ty: Type::Boolean,
        },
        true_block: label_overflow.clone(),
        false_block: label_no_overflow.clone(),
    });

    instrs.push(Instruction::Label {
        name: label_overflow.clone(),
    });
    instrs.push(Instruction::Move {
        dest: overflow.clone(),
        src: Operand::Constant(Constant::Boolean(true)),
    });
    instrs.push(Instruction::Move {
        dest: result.clone(),
        src: Operand::Constant(zero_const.clone()),
    });
    instrs.push(Instruction::Jump {
        target: label_end.clone(),
    });

    instrs.push(Instruction::Label {
        name: label_no_overflow.clone(),
    });
    instrs.push(Instruction::Move {
        dest: overflow.clone(),
        src: Operand::Constant(Constant::Boolean(false)),
    });

    // Perform actual operation
    let op_instr = match operation {
        "add" => Instruction::Add {
            dest: result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        },
        "sub" => Instruction::Sub {
            dest: result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        },
        "mul" => Instruction::Mul {
            dest: result.clone(),
            op1: op1_var.clone(),
            op2: op2_var.clone(),
        },
        _ => panic!("Unsupported checked operation: {}", operation),
    };
    instrs.push(op_instr);
    instrs.push(Instruction::Jump {
        target: label_end.clone(),
    });

    instrs.push(Instruction::Label {
        name: label_end.clone(),
    });

    // Construct tuple object (Tuple_i32_bool, etc.)
    instrs.push(Instruction::ConstructObject {
        dest: tmp_struct.clone(),
        class_name: result_struct_name.to_string(),
        args: vec![
            (
                Operand::Variable {
                    name: result.clone(),
                    ty: ty.clone(),
                },
                ty.clone(),
            ),
            (
                Operand::Variable {
                    name: overflow.clone(),
                    ty: Type::Boolean,
                },
                Type::Boolean,
            ),
        ],
    });
    instrs.push(Instruction::Return {
        operand: Some(Operand::Variable {
            name: tmp_struct.clone(),
            ty: Type::Class(result_struct_name.to_string()),
        }),
    });

    Function {
        name: fn_name.clone(),
        owner_class: None,
        signature: Signature {
            params: vec![(a, ty.clone()), (b, ty.clone())],
            ret: Box::new(Type::Class(result_struct_name.to_string())),
            is_static: true,
        },
        body: CodeBlock {
            basic_blocks: {
                let mut bbs = HashMap::new();
                bbs.insert(
                    "entry".to_string(),
                    crate::oomir::BasicBlock {
                        label: "entry".to_string(),
                        instructions: instrs,
                    },
                );
                bbs
            },
            entry: "entry".to_string(),
        },
    }
}

/// Emit all needed checked arithmetic intrinsics
/// Returns the intrinsic class
pub fn emit_all_needed_intrinsics(needed_intrinsics: &[(String, String, String)]) -> DataType {
    // Create RustcCodegenJVMIntrinsics class with static methods
    let mut intrinsic_methods = HashMap::new();

    for (operation, ty_suffix, result_struct_name) in needed_intrinsics {
        let ty = match ty_suffix.as_str() {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            _ => panic!("Unsupported type suffix: {}", ty_suffix),
        };

        let function =
            emit_checked_arithmetic_intrinsic(operation, &ty, ty_suffix, result_struct_name);
        intrinsic_methods.insert(function.name.clone(), DataTypeMethod::Function(function));
    }

    let intrinsic_class = DataType::Class {
        is_abstract: false,
        super_class: Some("java/lang/Object".to_string()),
        fields: vec![],
        methods: intrinsic_methods,
        interfaces: vec![],
    };

    intrinsic_class
}
