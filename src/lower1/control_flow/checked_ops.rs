use crate::oomir::{Constant, Instruction, Operand, Type};
use super::checked_intrinsic_registry;

pub fn emit_checked_arithmetic_oomir_instructions(
    dest_base_name: &str,
    op1: &Operand,
    op2: &Operand,
    op_ty: &Type,
    operation: &str,         // "add", "sub", "mul"
    unique_id_offset: usize, // Used to ensure unique labels/temps
) -> (Vec<Instruction>, String, String, String) {
    // Instead of inlining, emit a call to a reusable checked arithmetic intrinsic function.
    // The intrinsic function should be emitted once per type/operation elsewhere (e.g., at module init).
    let mut generated_instructions = Vec::new();
    let unique_id = unique_id_offset;
    let tmp_pair = format!("{}_{}_chk_pair_{}", dest_base_name, operation, unique_id);
    let tmp_result = format!("{}_{}_chk_res_{}", dest_base_name, operation, unique_id);
    let tmp_overflow = format!("{}_{}_chk_ovf_{}", dest_base_name, operation, unique_id);

    // For BigInt/BigDec, overflow doesn't happen in the fixed-size sense.
    if matches!(op_ty, Type::Class(c) if c == crate::lower2::BIG_INTEGER_CLASS || c == crate::lower2::BIG_DECIMAL_CLASS) {
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
            _ => panic!("Unsupported checked operation for BigInt/BigDec: {}", operation),
        };
        generated_instructions.push(op_instr);
        generated_instructions.push(Instruction::Move {
            dest: tmp_overflow.clone(),
            src: Operand::Constant(Constant::Boolean(false)),
        });
        
        // Construct a tuple for consistency with the primitive integer case
        // Determine the tuple type based on the BigInteger/BigDecimal type
        // Note: Use the readable name form that matches types.rs:readable_oomir_type_name
        let class_name = match op_ty {
            Type::Class(c) if c == crate::lower2::BIG_INTEGER_CLASS => "Tuple_BigInteger_bool",
            Type::Class(c) if c == crate::lower2::BIG_DECIMAL_CLASS => "Tuple_BigDecimal_bool",
            _ => unreachable!(),
        };
        
        generated_instructions.push(Instruction::ConstructObject {
            dest: tmp_pair.clone(),
            class_name: class_name.to_string(),
        });
        generated_instructions.push(Instruction::SetField {
            object: tmp_pair.clone(),
            field_name: "field0".to_string(),
            value: Operand::Variable { name: tmp_result.clone(), ty: op_ty.clone() },
            field_ty: op_ty.clone(),
            owner_class: class_name.to_string(),
        });
        generated_instructions.push(Instruction::SetField {
            object: tmp_pair.clone(),
            field_name: "field1".to_string(),
            value: Operand::Variable { name: tmp_overflow.clone(), ty: Type::Boolean },
            field_ty: Type::Boolean,
            owner_class: class_name.to_string(),
        });
        
        return (generated_instructions, tmp_pair, tmp_result, tmp_overflow);
    }

    // --- For primitive integer types, emit a call to the checked arithmetic intrinsic ---
    // The function name is e.g. "__oomir_checked_add_i32"
    let (fn_name, ty_suffix) = match (operation, op_ty) {
        ("add", Type::I32) => ("__oomir_checked_add_i32", "i32"),
        ("add", Type::I64) => ("__oomir_checked_add_i64", "i64"),
        ("add", Type::I16) => ("__oomir_checked_add_i16", "i16"),
        ("add", Type::I8) => ("__oomir_checked_add_i8", "i8"),
        ("sub", Type::I32) => ("__oomir_checked_sub_i32", "i32"),
        ("sub", Type::I64) => ("__oomir_checked_sub_i64", "i64"),
        ("sub", Type::I16) => ("__oomir_checked_sub_i16", "i16"),
        ("sub", Type::I8) => ("__oomir_checked_sub_i8", "i8"),
        ("mul", Type::I32) => ("__oomir_checked_mul_i32", "i32"),
        ("mul", Type::I64) => ("__oomir_checked_mul_i64", "i64"),
        ("mul", Type::I16) => ("__oomir_checked_mul_i16", "i16"),
        ("mul", Type::I8) => ("__oomir_checked_mul_i8", "i8"),
        _ => panic!("Unsupported checked arithmetic operation/type: {} {:?}", operation, op_ty),
    };

    // Register that this intrinsic is needed
    checked_intrinsic_registry::register_intrinsic(operation, ty_suffix);

    // Determine the result tuple type (e.g. Tuple_i32_bool)
    let tuple_type_name = format!("Tuple_{}_bool", ty_suffix);

    // Emit a call to the intrinsic static method: pair = RustcCodegenJVMIntrinsics.fn_name(a, b)
    generated_instructions.push(Instruction::InvokeStatic {
        dest: Some(tmp_pair.clone()),
        class_name: "RustcCodegenJVMIntrinsics".to_string(),
        method_name: fn_name.to_string(),
        method_ty: crate::oomir::Signature {
            params: vec![("a".to_string(), op_ty.clone()), ("b".to_string(), op_ty.clone())],
            ret: Box::new(Type::Class(tuple_type_name.clone())),
        },
        args: vec![op1.clone(), op2.clone()],
    });
    // Extract result and overflow from the pair (assume tuple struct with .0 and .1)
    // Assume the intrinsic returns a struct with fields "result" and "overflow"
    // Use GetField to extract them
    generated_instructions.push(Instruction::GetField {
        dest: tmp_result.clone(),
        object: Operand::Variable { name: tmp_pair.clone(), ty: Type::Class(tuple_type_name.clone()) },
        field_name: "field0".to_string(),
        field_ty: op_ty.clone(),
        owner_class: tuple_type_name.clone(),
    });
    generated_instructions.push(Instruction::GetField {
        dest: tmp_overflow.clone(),
        object: Operand::Variable { name: tmp_pair.clone(), ty: Type::Class(tuple_type_name.clone()) },
        field_name: "field1".to_string(),
        field_ty: Type::Boolean,
        owner_class: tuple_type_name.clone(),
    });
    (generated_instructions, tmp_pair, tmp_result, tmp_overflow)
}
