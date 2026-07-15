use super::{checked_intrinsic_registry, checked_intrinsics};
use crate::oomir::{Instruction, Operand, Type};

pub fn checked_arithmetic_tuple_local_name(op_ty: &Type) -> Option<&'static str> {
    match op_ty {
        Type::I8 => Some("Tuple_i8_bool"),
        Type::U8 => Some("Tuple_u8_bool"),
        Type::I16 => Some("Tuple_i16_bool"),
        Type::U16 => Some("Tuple_u16_bool"),
        Type::I32 => Some("Tuple_i32_bool"),
        Type::U32 => Some("Tuple_u32_bool"),
        Type::I64 => Some("Tuple_i64_bool"),
        Type::U64 => Some("Tuple_u64_bool"),
        Type::Class(c) if c == crate::lower2::I128_CLASS => Some("Tuple_I128_bool"),
        Type::Class(c) if c == crate::lower2::U128_CLASS => Some("Tuple_U128_bool"),
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

    let ty_suffix = match op_ty {
        Type::I32 => "i32",
        Type::U32 => "u32",
        Type::I64 => "i64",
        Type::U64 => "u64",
        Type::I16 => "i16",
        Type::U16 => "u16",
        Type::I8 => "i8",
        Type::U8 => "u8",
        Type::Class(c) if c == crate::lower2::I128_CLASS => "i128",
        Type::Class(c) if c == crate::lower2::U128_CLASS => "u128",
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
