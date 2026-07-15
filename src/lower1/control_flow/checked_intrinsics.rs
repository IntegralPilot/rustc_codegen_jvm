// Reusable OOMIR checked arithmetic functions for fixed-width integer types.
use crate::oomir::{
    CodeBlock, Constant, DataType, DataTypeMethod, Function, Instruction, Operand, Signature, Type,
};
use std::collections::HashMap;

pub fn get_intrinsic_function_name(
    operation: &str,
    ty_suffix: &str,
    result_struct_name: &str,
) -> String {
    let hash = crate::stable_hash::short_hash(result_struct_name, 8);
    format!("__oomir_checked_{operation}_{ty_suffix}_{hash}")
}

fn variable(name: &str, ty: &Type) -> Operand {
    Operand::Variable {
        name: name.to_string(),
        ty: ty.clone(),
    }
}

fn integer_constants(ty: &Type) -> (Constant, Constant, Constant) {
    let wide = |class_name: &str, value: &str| Constant::Instance {
        class_name: class_name.to_string(),
        fields: HashMap::new(),
        params: vec![Constant::String(value.to_string())],
    };
    match ty {
        Type::I8 => (Constant::I8(0), Constant::I8(i8::MIN), Constant::I8(-1)),
        Type::U8 => (Constant::U8(0), Constant::U8(0), Constant::U8(u8::MAX)),
        Type::I16 => (Constant::I16(0), Constant::I16(i16::MIN), Constant::I16(-1)),
        Type::U16 => (Constant::U16(0), Constant::U16(0), Constant::U16(u16::MAX)),
        Type::I32 => (Constant::I32(0), Constant::I32(i32::MIN), Constant::I32(-1)),
        Type::U32 => (Constant::U32(0), Constant::U32(0), Constant::U32(u32::MAX)),
        Type::I64 => (Constant::I64(0), Constant::I64(i64::MIN), Constant::I64(-1)),
        Type::U64 => (Constant::U64(0), Constant::U64(0), Constant::U64(u64::MAX)),
        Type::Class(class_name) if class_name == crate::lower2::I128_CLASS => (
            wide(class_name, "0"),
            wide(class_name, "-170141183460469231731687303715884105728"),
            wide(class_name, "-1"),
        ),
        Type::Class(class_name) if class_name == crate::lower2::U128_CLASS => (
            wide(class_name, "0"),
            wide(class_name, "0"),
            wide(class_name, "340282366920938463463374607431768211455"),
        ),
        _ => panic!("unsupported checked integer type: {ty:?}"),
    }
}

fn is_unsigned(ty: &Type) -> bool {
    matches!(ty, Type::U8 | Type::U16 | Type::U32 | Type::U64)
        || matches!(ty, Type::Class(class_name) if class_name == crate::lower2::U128_CLASS)
}

pub fn emit_checked_arithmetic_intrinsic(
    operation: &str,
    ty: &Type,
    ty_suffix: &str,
    result_struct_name: &str,
) -> Function {
    let fn_name = get_intrinsic_function_name(operation, ty_suffix, result_struct_name);
    let a = "_1";
    let b = "_2";
    let result = "result";
    let overflow = "overflow";
    let tmp_struct = "tmp_struct";
    let (zero, min, minus_one) = integer_constants(ty);
    let a_op = variable(a, ty);
    let b_op = variable(b, ty);
    let result_op = variable(result, ty);
    let mut instrs = Vec::new();

    instrs.push(match operation {
        "add" => Instruction::Add {
            dest: result.into(),
            op1: a_op.clone(),
            op2: b_op.clone(),
        },
        "sub" => Instruction::Sub {
            dest: result.into(),
            op1: a_op.clone(),
            op2: b_op.clone(),
        },
        "mul" => Instruction::Mul {
            dest: result.into(),
            op1: a_op.clone(),
            op2: b_op.clone(),
        },
        _ => panic!("unsupported checked operation: {operation}"),
    });

    match (operation, is_unsigned(ty)) {
        ("add", true) => instrs.push(Instruction::Lt {
            dest: overflow.into(),
            op1: result_op.clone(),
            op2: a_op.clone(),
        }),
        ("sub", true) => instrs.push(Instruction::Lt {
            dest: overflow.into(),
            op1: a_op.clone(),
            op2: b_op.clone(),
        }),
        ("add" | "sub", false) => {
            // add: ((a ^ result) & (b ^ result)) < 0
            // sub: ((a ^ b)      & (a ^ result)) < 0
            let xor_left = "overflow_xor_left";
            let xor_right = "overflow_xor_right";
            let sign_bits = "overflow_sign_bits";
            instrs.push(Instruction::BitXor {
                dest: xor_left.into(),
                op1: a_op.clone(),
                op2: if operation == "add" {
                    result_op.clone()
                } else {
                    b_op.clone()
                },
            });
            instrs.push(Instruction::BitXor {
                dest: xor_right.into(),
                op1: if operation == "add" {
                    b_op.clone()
                } else {
                    a_op.clone()
                },
                op2: result_op.clone(),
            });
            instrs.push(Instruction::BitAnd {
                dest: sign_bits.into(),
                op1: variable(xor_left, ty),
                op2: variable(xor_right, ty),
            });
            instrs.push(Instruction::Lt {
                dest: overflow.into(),
                op1: variable(sign_bits, ty),
                op2: Operand::Constant(zero.clone()),
            });
        }
        ("mul", _) => {
            // When neither operand is zero, wrapped multiplication overflowed iff
            // `result / b != a`.  JVM's MIN / -1 wraps, so account for that pair.
            let a_zero = "mul_a_zero";
            let b_zero = "mul_b_zero";
            let either_zero = "mul_either_zero";
            let check = format!("{fn_name}_mul_check");
            let no_overflow = format!("{fn_name}_mul_no_overflow");
            let end = format!("{fn_name}_mul_end");
            instrs.push(Instruction::Eq {
                dest: a_zero.into(),
                op1: a_op.clone(),
                op2: Operand::Constant(zero.clone()),
            });
            instrs.push(Instruction::Eq {
                dest: b_zero.into(),
                op1: b_op.clone(),
                op2: Operand::Constant(zero.clone()),
            });
            instrs.push(Instruction::BitOr {
                dest: either_zero.into(),
                op1: variable(a_zero, &Type::Boolean),
                op2: variable(b_zero, &Type::Boolean),
            });
            instrs.push(Instruction::Branch {
                condition: variable(either_zero, &Type::Boolean),
                true_block: no_overflow.clone(),
                false_block: check.clone(),
            });
            instrs.push(Instruction::Label { name: check });
            let quotient = "mul_quotient";
            let quotient_differs = "mul_quotient_differs";
            instrs.push(Instruction::Div {
                dest: quotient.into(),
                op1: result_op.clone(),
                op2: b_op.clone(),
            });
            instrs.push(Instruction::Ne {
                dest: quotient_differs.into(),
                op1: variable(quotient, ty),
                op2: a_op.clone(),
            });
            if is_unsigned(ty) || matches!(ty, Type::I8 | Type::I16) {
                instrs.push(Instruction::Move {
                    dest: overflow.into(),
                    src: variable(quotient_differs, &Type::Boolean),
                });
            } else {
                let a_min = "mul_a_min";
                let b_minus_one = "mul_b_minus_one";
                let min_times_minus_one = "mul_min_times_minus_one";
                instrs.push(Instruction::Eq {
                    dest: a_min.into(),
                    op1: a_op.clone(),
                    op2: Operand::Constant(min),
                });
                instrs.push(Instruction::Eq {
                    dest: b_minus_one.into(),
                    op1: b_op.clone(),
                    op2: Operand::Constant(minus_one),
                });
                instrs.push(Instruction::BitAnd {
                    dest: min_times_minus_one.into(),
                    op1: variable(a_min, &Type::Boolean),
                    op2: variable(b_minus_one, &Type::Boolean),
                });
                instrs.push(Instruction::BitOr {
                    dest: overflow.into(),
                    op1: variable(quotient_differs, &Type::Boolean),
                    op2: variable(min_times_minus_one, &Type::Boolean),
                });
            }
            instrs.push(Instruction::Jump {
                target: end.clone(),
            });
            instrs.push(Instruction::Label { name: no_overflow });
            instrs.push(Instruction::Move {
                dest: overflow.into(),
                src: Operand::Constant(Constant::Boolean(false)),
            });
            instrs.push(Instruction::Label { name: end });
        }
        _ => unreachable!(),
    }

    instrs.push(Instruction::ConstructObject {
        dest: tmp_struct.into(),
        class_name: result_struct_name.to_string(),
        args: vec![
            (result_op, ty.clone()),
            (variable(overflow, &Type::Boolean), Type::Boolean),
        ],
    });
    instrs.push(Instruction::Return {
        operand: Some(Operand::Variable {
            name: tmp_struct.into(),
            ty: Type::Class(result_struct_name.to_string()),
        }),
    });

    Function {
        name: fn_name,
        owner_class: None,
        debug_variables: Vec::new(),
        signature: Signature {
            params: vec![(a.into(), ty.clone()), (b.into(), ty.clone())],
            ret: Box::new(Type::Class(result_struct_name.to_string())),
            is_static: true,
        },
        body: CodeBlock {
            basic_blocks: HashMap::from([(
                "entry".to_string(),
                crate::oomir::BasicBlock {
                    label: "entry".to_string(),
                    instructions: instrs,
                },
            )]),
            entry: "entry".to_string(),
        },
    }
}

pub fn emit_all_needed_intrinsics(needed_intrinsics: &[(String, String, String)]) -> DataType {
    let mut intrinsic_methods = HashMap::new();
    for (operation, ty_suffix, result_struct_name) in needed_intrinsics {
        let ty = match ty_suffix.as_str() {
            "i8" => Type::I8,
            "u8" => Type::U8,
            "i16" => Type::I16,
            "u16" => Type::U16,
            "i32" => Type::I32,
            "u32" => Type::U32,
            "i64" => Type::I64,
            "u64" => Type::U64,
            "i128" => Type::Class(crate::lower2::I128_CLASS.to_string()),
            "u128" => Type::Class(crate::lower2::U128_CLASS.to_string()),
            _ => panic!("unsupported checked type suffix: {ty_suffix}"),
        };
        let function =
            emit_checked_arithmetic_intrinsic(operation, &ty, ty_suffix, result_struct_name);
        intrinsic_methods.insert(function.name.clone(), DataTypeMethod::Function(function));
    }
    DataType::Class {
        is_abstract: false,
        super_class: Some("java/lang/Object".to_string()),
        fields: vec![],
        methods: intrinsic_methods,
        interfaces: vec![],
    }
}
