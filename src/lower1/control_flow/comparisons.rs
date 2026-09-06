//! Comparisons.
use super::*;

pub(in crate::lower1) fn emit_raw_eq_pointer<'tcx>(
    operand: oomir::Operand,
    compared_ty: Ty<'tcx>,
    temp_name: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    if matches!(operand.get_type(), Some(oomir::Type::Pointer(_))) {
        return operand;
    }

    let element_ty = match compared_ty.kind() {
        TyKind::Array(element, _) | TyKind::Slice(element) => *element,
        TyKind::Str => tcx.types.u8,
        other => panic!("raw_eq received a non-pointer JVM carrier for {other:?}"),
    };
    assert!(
        matches!(
            operand.get_type(),
            Some(oomir::Type::Slice(_) | oomir::Type::Str)
        ),
        "raw_eq slice conversion received unexpected carrier {:?}",
        operand.get_type()
    );
    let element_oomir_ty =
        crate::lower1::types::ty_to_oomir_type(element_ty, tcx, data_types, instance);
    let pointer_ty = oomir::Type::Pointer(Box::new(element_oomir_ty));
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(temp_name.to_string()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "fromSlice".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "slice".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                ),
                ("element_size".to_string(), oomir::Type::U64),
                ("codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(pointer_ty.clone()),
            is_static: true,
        },
        args: vec![
            operand,
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(
                    crate::lower1::types::layout_size_bytes(tcx, element_ty)
                        .expect("raw_eq element has a concrete layout"),
                )
                .expect("Rust raw_eq element layout exceeds u64"),
            )),
            crate::lower1::types::pointer_view_codec_operand(element_ty, tcx, data_types, instance),
        ],
    });
    oomir::Operand::Variable {
        name: temp_name.to_string(),
        ty: pointer_ty,
    }
}

pub(in crate::lower1) fn comparison_value_type<'tcx>(
    mut ty: oomir::Type,
    mut mir_ty: Ty<'tcx>,
) -> oomir::Type {
    while let TyKind::Ref(_, pointee, _) = mir_ty.kind() {
        let oomir::Type::Pointer(inner) = ty else {
            break;
        };
        ty = *inner;
        mir_ty = *pointee;
    }
    ty
}

pub(in crate::lower1) fn emit_comparison_value<'tcx>(
    mut operand: oomir::Operand,
    mut mir_ty: Ty<'tcx>,
    dest_prefix: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let mut depth = 0;
    while let TyKind::Ref(_, pointee, _) = mir_ty.kind() {
        let Some(oomir::Type::Pointer(inner)) = operand.get_type() else {
            break;
        };
        operand = emit_pointer_read(
            operand,
            inner.as_ref(),
            &format!("{dest_prefix}_{depth}"),
            instructions,
        );
        mir_ty = *pointee;
        depth += 1;
    }
    if matches!(
        mir_ty.kind(),
        TyKind::Adt(adt, _) if crate::lower1::is_non_null_lang_item(tcx, adt.did())
    ) && let Some(oomir::Type::Class(class_name)) = operand.get_type()
        && let Some(oomir::DataType::Class { fields, .. }) = data_types.get(&class_name)
        && let Some((field_name, field_ty)) = fields.iter().find(|(field_name, field_ty)| {
            field_name == "pointer"
                && matches!(
                    field_ty,
                    oomir::Type::Pointer(_) | oomir::Type::Slice(_) | oomir::Type::Str
                )
        })
    {
        let dest = format!("{dest_prefix}_nonnull_pointer");
        instructions.push(oomir::Instruction::GetField {
            dest: dest.clone(),
            object: operand,
            field_name: field_name.clone(),
            field_ty: field_ty.clone(),
            owner_class: class_name,
        });
        operand = oomir::Operand::Variable {
            name: dest,
            ty: field_ty.clone(),
        };
    }
    if matches!(
        mir_ty.kind(),
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)
            if matches!(pointee.kind(), TyKind::Dynamic(..))
    ) && let Some(pointer_ty @ oomir::Type::Pointer(_)) = operand.get_type()
    {
        let normalized = format!("{dest_prefix}_trait_data");
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(normalized.clone()),
            class_name: "org/rustlang/runtime/RuntimeSupport".to_string(),
            method_name: "traitObjectDataPointer".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty.clone()),
                    ("view_size".to_string(), oomir::Type::U64),
                    ("view_codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(pointer_ty.clone()),
                is_static: true,
            },
            args: vec![
                operand,
                oomir::Operand::Constant(oomir::Constant::U64(0)),
                oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string())),
            ],
        });
        operand = oomir::Operand::Variable {
            name: normalized,
            ty: pointer_ty,
        };
    }
    operand
}

pub(in crate::lower1) fn supports_direct_equality(ty: &oomir::Type) -> bool {
    matches!(ty, oomir::Type::Unit)
        || ty.is_jvm_primitive_like()
        || matches!(ty, oomir::Type::Pointer(_))
        || matches!(
            ty,
            oomir::Type::Class(class_name)
                if class_name == crate::lower2::I128_CLASS
                    || class_name == crate::lower2::U128_CLASS
                    || class_name == crate::lower2::F128_CLASS
        )
        || matches!(ty, oomir::Type::Str)
}

pub(in crate::lower1) fn supports_direct_ordering(ty: &oomir::Type) -> bool {
    ty.is_jvm_primitive_like()
        || matches!(ty, oomir::Type::Pointer(_))
        || matches!(
            ty,
            oomir::Type::Class(class_name)
                if class_name == crate::lower2::I128_CLASS
                    || class_name == crate::lower2::U128_CLASS
                    || class_name == crate::lower2::F128_CLASS
        )
}

pub(in crate::lower1) fn is_non_null_comparison_type(tcx: TyCtxt<'_>, mut ty: Ty<'_>) -> bool {
    while let TyKind::Ref(_, pointee, _) = ty.kind() {
        ty = *pointee;
    }
    matches!(
        ty.kind(),
        TyKind::Adt(adt, _) if crate::lower1::is_non_null_lang_item(tcx, adt.did())
    )
}

pub(in crate::lower1) fn non_null_comparison_carrier<'a>(
    tcx: TyCtxt<'_>,
    mir_ty: Ty<'_>,
    oomir_ty: &'a oomir::Type,
    data_types: &'a HashMap<String, oomir::DataType>,
) -> Option<&'a oomir::Type> {
    if !is_non_null_comparison_type(tcx, mir_ty) {
        return None;
    }
    let oomir::Type::Class(class_name) = oomir_ty else {
        return None;
    };
    let oomir::DataType::Class { fields, .. } = data_types.get(class_name)? else {
        return None;
    };
    fields
        .iter()
        .find(|(field_name, _)| field_name == "pointer")
        .map(|(_, field_ty)| field_ty)
}
