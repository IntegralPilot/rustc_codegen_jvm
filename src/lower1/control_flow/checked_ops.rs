use super::{checked_intrinsic_registry, checked_intrinsics};
use crate::oomir::{Constant, Instruction, Operand, Type};

pub fn checked_arithmetic_tuple_local_name(op_ty: &Type) -> Option<&'static str> {
    match op_ty {
        Type::I8 => Some("Tuple_i8_bool"),
        Type::I16 => Some("Tuple_i16_bool"),
        Type::I32 => Some("Tuple_i32_bool"),
        Type::I64 => Some("Tuple_i64_bool"),
        Type::Class(c) if c == crate::lower2::BIG_INTEGER_CLASS => Some("Tuple_BigInteger_bool"),
        Type::Class(c) if c == crate::lower2::BIG_DECIMAL_CLASS => Some("Tuple_BigDecimal_bool"),
        _ => None,
    }
}

pub fn emit_checked_arithmetic_oomir_instructions(
    dest_base_name: &str,
    op1: &Operand,
    op2: &Operand,
    op_ty: &Type,
    operation: &str,         // "add", "sub", "mul"
    unique_id_offset: usize, // Used to ensure unique labels/temps
    result_tuple_class: &str,
) -> (Vec<Instruction>, String, String, String) {
    // Instead of inlining, emit a call to a reusable checked arithmetic intrinsic function.
    // The intrinsic function should be emitted once per type/operation elsewhere (e.g., at module init).
    let mut generated_instructions = Vec::new();
    let unique_id = unique_id_offset;
    let tmp_pair = format!("{}_{}_chk_pair_{}", dest_base_name, operation, unique_id);
    let tmp_result = format!("{}_{}_chk_res_{}", dest_base_name, operation, unique_id);
    let tmp_overflow = format!("{}_{}_chk_ovf_{}", dest_base_name, operation, unique_id);

    // For BigInt/BigDec, overflow doesn't happen in the fixed-size sense.
    if matches!(op_ty, Type::Class(c) if c == crate::lower2::BIG_INTEGER_CLASS || c == crate::lower2::BIG_DECIMAL_CLASS)
    {
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
        generated_instructions.push(Instruction::Move {
            dest: tmp_overflow.clone(),
            src: Operand::Constant(Constant::Boolean(false)),
        });

        generated_instructions.push(Instruction::ConstructObject {
            dest: tmp_pair.clone(),
            class_name: result_tuple_class.to_string(),
            args: vec![
                (
                    Operand::Variable {
                        name: tmp_result.clone(),
                        ty: op_ty.clone(),
                    },
                    op_ty.clone(),
                ),
                (
                    Operand::Variable {
                        name: tmp_overflow.clone(),
                        ty: Type::Boolean,
                    },
                    Type::Boolean,
                ),
            ],
        });

        return (generated_instructions, tmp_pair, tmp_result, tmp_overflow);
    }

    // --- For primitive integer types, emit a call to the checked arithmetic intrinsic ---
    let ty_suffix = match op_ty {
        Type::I32 => "i32",
        Type::I64 => "i64",
        Type::I16 => "i16",
        Type::I8 => "i8",
        _ => panic!(
            "Unsupported checked arithmetic operation/type: {} {:?}",
            operation, op_ty
        ),
    };
    let fn_name =
        checked_intrinsics::get_intrinsic_function_name(operation, ty_suffix, result_tuple_class);

    // Register that this intrinsic is needed
    checked_intrinsic_registry::register_intrinsic(operation, ty_suffix, result_tuple_class);

    // Emit a call to the intrinsic static method: pair = RustcCodegenJVMIntrinsics.fn_name(a, b)
    generated_instructions.push(Instruction::InvokeStatic {
        dest: Some(tmp_pair.clone()),
        class_name: "RustcCodegenJVMIntrinsics".to_string(),
        method_name: fn_name,
        method_ty: crate::oomir::Signature {
            params: vec![
                ("a".to_string(), op_ty.clone()),
                ("b".to_string(), op_ty.clone()),
            ],
            ret: Box::new(Type::Class(result_tuple_class.to_string())),
            is_static: true,
        },
        args: vec![op1.clone(), op2.clone()],
    });
    (generated_instructions, tmp_pair, tmp_result, tmp_overflow)
}
