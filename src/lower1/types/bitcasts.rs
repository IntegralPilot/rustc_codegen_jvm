//! Exact scalar and thin-address conversions need no generated codec or method.
use crate::oomir::{self, Instruction, Operand, Type};

pub(super) fn scalar_wrapper_transmute<'tcx>(
    source: super::Ty<'tcx>,
    target: super::Ty<'tcx>,
    tcx: super::TyCtxt<'tcx>,
    definitions: &mut crate::lower1::context::Definitions<'tcx>,
    instance: rustc_middle::ty::Instance<'tcx>,
) -> Option<Vec<Instruction>> {
    use super::*;

    fn path<'tcx>(
        mut ty: Ty<'tcx>,
        tcx: TyCtxt<'tcx>,
        definitions: &mut crate::lower1::context::Definitions<'tcx>,
        instance: rustc_middle::ty::Instance<'tcx>,
    ) -> Option<(Type, Vec<(String, String, Type)>)> {
        let mut fields = Vec::new();
        for _ in 0..16 {
            let inner = match ty.kind() {
                TyKind::Bool
                | TyKind::Char
                | TyKind::Int(_)
                | TyKind::Uint(_)
                | TyKind::Float(_) => {
                    return Some((ty_to_oomir_type(ty, tcx, definitions, instance), fields));
                }
                TyKind::Pat(inner, _) => {
                    ty = *inner;
                    continue;
                }
                TyKind::Adt(def, args)
                    if def.is_struct() && def.non_enum_variant().fields.len() == 1 =>
                {
                    normalize_union_ty(
                        tcx,
                        def.non_enum_variant().fields[FieldIdx::from_usize(0)]
                            .ty(tcx, args)
                            .skip_norm_wip(),
                    )
                    .ok()?
                }
                TyKind::Tuple(elements) if elements.len() == 1 => elements[0],
                _ => return None,
            };
            let layout = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
                .ok()?;
            if layout.fields.offset(0).bytes() != 0
                || layout.size.bytes_usize() != layout_size_bytes(tcx, inner).ok()?
            {
                return None;
            }
            let outer = ty_to_oomir_type(ty, tcx, definitions, instance);
            let nested = ty_to_oomir_type(inner, tcx, definitions, instance);
            if outer != nested {
                let Type::Class(owner) = outer else {
                    return None;
                };
                let Some(DataType::Class {
                    fields: members,
                    kind: oomir::ClassKind::Value,
                    is_abstract: false,
                    ..
                }) = definitions.get(&owner)
                else {
                    return None;
                };
                let [(name, field)] = members.as_slice() else {
                    return None;
                };
                if *field != nested {
                    return None;
                }
                fields.push((owner, name.clone(), nested));
            }
            ty = inner;
        }
        None
    }

    let (source_scalar, source_fields) = path(source, tcx, definitions, instance)?;
    let (target_scalar, target_fields) = path(target, tcx, definitions, instance)?;
    let mut instructions = Vec::new();
    let mut value = Operand::Variable {
        name: "_1".into(),
        ty: source_fields
            .first()
            .map_or(source_scalar, |(owner, _, _)| Type::Class(owner.clone())),
    };
    for (index, (owner, name, ty)) in source_fields.into_iter().enumerate() {
        let dest = format!("_scalar_source_{index}");
        instructions.push(Instruction::GetField {
            dest: dest.clone(),
            object: value,
            field_name: name,
            field_ty: ty.clone(),
            owner_class: owner,
        });
        value = Operand::Variable { name: dest, ty };
    }
    value = emit_direct_transmute(value, &target_scalar, "_scalar_bits", &mut instructions)?;
    for (index, (owner, _, ty)) in target_fields.into_iter().rev().enumerate() {
        let dest = format!("_scalar_target_{index}");
        instructions.push(Instruction::ConstructObject {
            dest: dest.clone(),
            class_name: owner.clone(),
            args: vec![(value, ty)],
        });
        value = Operand::Variable {
            name: dest,
            ty: Type::Class(owner),
        };
    }
    instructions.push(Instruction::Return {
        operand: Some(value),
    });
    Some(instructions)
}

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
