//! Exact scalar and thin-address conversions need no generated codec or method.
use crate::oomir::{self, Instruction, Operand, Type};

pub(crate) fn emit_direct_transmute(
    source: Operand,
    target: &Type,
    dest: &str,
    instructions: &mut Vec<Instruction>,
) -> Option<Operand> {
    let from = source.get_type()?;
    let integer_size = |ty: &Type| match ty {
        Type::Boolean | Type::I8 | Type::U8 => Some(1),
        Type::I16 | Type::U16 => Some(2),
        Type::I32 | Type::U32 => Some(4),
        Type::I64 | Type::U64 => Some(8),
        _ => None,
    };
    let direct = (from == *target
        && (integer_size(&from).is_some()
            || matches!(from, Type::Unit | Type::F32 | Type::F64 | Type::Pointer(_))))
        || (integer_size(&from).is_some() && integer_size(&from) == integer_size(target))
        || (from.scalar_address_size().is_some() && target.scalar_address_size().is_some());
    if direct {
        if !target.has_jvm_value() {
            return Some(Operand::Constant(oomir::Constant::Unit));
        }
        instructions.push(Instruction::Cast {
            op: source,
            ty: target.clone(),
            dest: dest.into(),
        });
    } else {
        let (owner, method) = match (&from, target) {
            (Type::F32, Type::I32 | Type::U32) => ("java/lang/Float", "floatToRawIntBits"),
            (Type::I32 | Type::U32, Type::F32) => ("java/lang/Float", "intBitsToFloat"),
            (Type::F64, Type::I64 | Type::U64) => ("java/lang/Double", "doubleToRawLongBits"),
            (Type::I64 | Type::U64, Type::F64) => ("java/lang/Double", "longBitsToDouble"),
            _ => return None,
        };
        instructions.push(Instruction::InvokeStatic {
            dest: Some(dest.into()),
            class_name: owner.into(),
            method_name: method.into(),
            method_ty: oomir::Signature {
                params: vec![("value".into(), from)],
                ret: Box::new(target.clone()),
                is_static: true,
            },
            args: vec![source],
        });
    }
    Some(Operand::Variable {
        name: dest.into(),
        ty: target.clone(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pointer_identity_and_float_bits_need_no_codec() {
        let mut code = Vec::new();
        let address = Type::pointer(Type::U64);
        let result = emit_direct_transmute(
            Operand::Variable {
                name: "p".into(),
                ty: address,
            },
            &Type::pointer(Type::F64),
            "q",
            &mut code,
        );
        assert!(result.is_some());
        assert!(matches!(&code[..], [Instruction::Cast { .. }]));
        code.clear();
        assert!(
            emit_direct_transmute(
                Operand::Constant(oomir::Constant::U64(0x7ff8000000001234)),
                &Type::F64,
                "f",
                &mut code
            )
            .is_some()
        );
        assert!(
            matches!(&code[..], [Instruction::InvokeStatic { class_name, method_name, .. }]
            if class_name == "java/lang/Double" && method_name == "longBitsToDouble")
        );
        code.clear();
        assert!(
            emit_direct_transmute(
                Operand::Constant(oomir::Constant::U64(0)),
                &Type::pointer(Type::U64),
                "p",
                &mut code
            )
            .is_none()
        );
        assert!(code.is_empty());
    }
}
