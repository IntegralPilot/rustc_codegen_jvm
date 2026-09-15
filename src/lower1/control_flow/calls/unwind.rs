//! Exceptional calls.
use super::*;

pub(super) fn catch_unwind<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
) {
    let data_ty = oomir_operands[1]
        .get_type()
        .expect("catch_unwind data pointer is typed");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "catchUnwind".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "try_function".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                ),
                ("data".to_string(), data_ty),
                (
                    "catch_function".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                ),
            ],
            ret: Box::new(oomir::Type::Boolean),
            is_static: true,
        },
        args: oomir_operands.clone(),
        dest: effective_dest.clone(),
    });
}
