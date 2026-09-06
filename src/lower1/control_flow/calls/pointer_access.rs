//! Pointer access.
use super::*;

pub(super) fn as_ref<'tcx>(
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    effective_dest: Option<String>,
    receiver_operand: oomir::Operand,
    dispatch_receiver_ty: oomir::Type,
) {
    if let Some(dest) = effective_dest {
        let option_class = oomir_output_type
            .get_class_name()
            .expect("pointer as_ref/as_mut returns Option")
            .to_string();
        let option_object = format!("{label}_pointer_option_object");
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "asRefOption".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), dispatch_receiver_ty.clone()),
                    ("option_class".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                is_static: true,
            },
            args: vec![
                receiver_operand,
                oomir::Operand::Constant(oomir::Constant::String(option_class)),
            ],
            dest: Some(option_object.clone()),
        });
        instructions.push(oomir::Instruction::Cast {
            op: oomir::Operand::Variable {
                name: option_object,
                ty: oomir::Type::Class("java/lang/Object".to_string()),
            },
            ty: oomir_output_type.clone(),
            dest,
        });
    }
}
pub(super) fn non_null_ref<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    fn_output: Ty<'tcx>,
    oomir_output_type: oomir::Type,
    effective_dest: Option<String>,
    receiver_operand: oomir::Operand,
    dispatch_receiver_ty: oomir::Type,
) {
    // NonNull::as_ref/as_mut return a reference directly, unlike
    // raw-pointer methods with the same names which return Option.
    if let Some(dest) = effective_dest {
        if let oomir::Type::Pointer(non_null_ty) = &dispatch_receiver_ty
            && matches!(non_null_ty.as_ref(), oomir::Type::Pointer(_))
        {
            let pointer = crate::lower1::place::emit_pointer_read(
                receiver_operand,
                non_null_ty,
                &format!("{label}_non_null_pointer"),
                &mut instructions,
            );
            let reference = crate::lower1::value_repr::adapt_operand_to_rust_type(
                pointer,
                fn_output,
                &format!("{label}_non_null_reference"),
                tcx,
                instance,
                data_types,
                &mut instructions,
            );
            instructions.push(oomir::Instruction::Move {
                dest,
                src: reference,
            });
        } else if let oomir::Type::Pointer(non_null_ty) = &dispatch_receiver_ty
            && let oomir::Type::Class(owner_class) = non_null_ty.as_ref()
        {
            // NonNull is a transparent wrapper around its pointer
            // field. Returning the wrapper's storage pointer would
            // make a later dereference observe a NonNull object
            // instead of the pointee, so decode the wrapper and
            // return the actual field for both sized and DST values.
            let wrapper = crate::lower1::place::emit_pointer_read(
                receiver_operand,
                non_null_ty,
                &format!("{label}_non_null_wrapper"),
                &mut instructions,
            );
            let field_ty = match data_types.get(owner_class) {
                Some(oomir::DataType::Class { fields, .. }) => fields
                    .iter()
                    .find(|(name, _)| name == "pointer")
                    .map(|(_, ty)| ty.clone())
                    .expect("NonNull carrier has a pointer field"),
                _ => panic!("NonNull carrier class {owner_class} was not generated"),
            };
            let field_dest = if field_ty == oomir_output_type {
                dest.clone()
            } else {
                format!("{label}_non_null_pointer")
            };
            instructions.push(oomir::Instruction::GetField {
                dest: field_dest.clone(),
                object: wrapper,
                field_name: "pointer".to_string(),
                field_ty: field_ty.clone(),
                owner_class: owner_class.clone(),
            });
            if field_ty != oomir_output_type {
                let adapted = crate::lower1::value_repr::adapt_operand_to_rust_type(
                    oomir::Operand::Variable {
                        name: field_dest,
                        ty: field_ty,
                    },
                    fn_output,
                    &format!("{label}_non_null_reference"),
                    tcx,
                    instance,
                    data_types,
                    &mut instructions,
                );
                instructions.push(oomir::Instruction::Move { dest, src: adapted });
            }
        } else {
            instructions.push(oomir::Instruction::Move {
                dest,
                src: receiver_operand,
            });
        }
    }
}
pub(super) fn is_aligned<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    effective_dest: Option<String>,
    receiver_mir_ty: Ty<'tcx>,
    receiver_operand: oomir::Operand,
    dispatch_receiver_ty: oomir::Type,
) {
    let receiver_ty = EarlyBinder::bind(tcx, receiver_mir_ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let pointee = match receiver_ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
        other => panic!("pointer is_aligned receiver has non-pointer type {other:?}"),
    };
    let alignment = crate::lower1::types::layout_align_bytes(tcx, pointee)
        .unwrap_or_else(|error| panic!("could not determine pointer target alignment: {error}"));
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "is_aligned_to".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), dispatch_receiver_ty.clone()),
                ("alignment".to_string(), oomir::Type::U64),
            ],
            ret: Box::new(oomir::Type::Boolean),
            is_static: true,
        },
        args: vec![
            receiver_operand,
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(alignment).expect("Rust pointer alignment exceeds u64"),
            )),
        ],
        dest: effective_dest,
    });
}
pub(super) fn to_raw_parts<'tcx>(
    data_types: &mut Definitions<'tcx>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    effective_dest: Option<String>,
    receiver_operand: oomir::Operand,
    resolved_receiver_mir_ty: Ty<'tcx>,
    dispatch_receiver_ty: oomir::Type,
) {
    if let Some(dest) = effective_dest {
        let tuple_class = oomir_output_type
            .get_class_name()
            .expect("pointer to_raw_parts returns a tuple")
            .to_string();
        let tuple_fields = match data_types.get(&tuple_class) {
            Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
            other => panic!("to_raw_parts tuple class is unavailable: {other:?}"),
        };
        let data_pointer_ty = tuple_fields
            .first()
            .map(|(_, ty)| ty.clone())
            .expect("to_raw_parts tuple has a data pointer");
        let source_is_trait_object = matches!(
            resolved_receiver_mir_ty.kind(),
            TyKind::RawPtr(pointee, _)
                if matches!(pointee.kind(), TyKind::Dynamic(..))
        );
        let data_pointer_name = format!("{label}_raw_parts_data");
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: if source_is_trait_object {
                "traitObjectDataPointer".to_string()
            } else {
                "retype".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), dispatch_receiver_ty.clone()),
                    ("view_size".to_string(), oomir::Type::U64),
                    ("view_codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(data_pointer_ty.clone()),
                is_static: true,
            },
            args: vec![
                receiver_operand.clone(),
                oomir::Operand::Constant(oomir::Constant::U64(0)),
                oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string())),
            ],
            dest: Some(data_pointer_name.clone()),
        });
        let mut tuple_args = vec![(
            oomir::Operand::Variable {
                name: data_pointer_name,
                ty: data_pointer_ty.clone(),
            },
            data_pointer_ty,
        )];
        for (metadata_index, (_, metadata_ty)) in tuple_fields.into_iter().skip(1).enumerate() {
            if source_is_trait_object && metadata_index == 0 {
                let metadata_name = format!("{label}_raw_parts_metadata");
                emit_trait_object_metadata(
                    receiver_operand.clone(),
                    &metadata_ty,
                    metadata_name.clone(),
                    &metadata_name,
                    data_types,
                    &mut instructions,
                );
                tuple_args.push((
                    oomir::Operand::Variable {
                        name: metadata_name,
                        ty: metadata_ty.clone(),
                    },
                    metadata_ty,
                ));
            } else {
                tuple_args.push((
                    oomir::Operand::Constant(oomir::Constant::Null(metadata_ty.clone())),
                    metadata_ty,
                ));
            }
        }
        instructions.push(oomir::Instruction::ConstructObject {
            dest,
            class_name: tuple_class,
            args: tuple_args,
        });
    }
}
pub(super) fn swap<'tcx>(
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    receiver_operand: oomir::Operand,
    explicit_method_args: &[oomir::Operand],
    pointee_ty: &oomir::Type,
) {
    let left_name = format!("{label}_method_swap_left");
    let right_name = format!("{label}_method_swap_right");
    let left = crate::lower1::place::emit_pointer_read(
        receiver_operand.clone(),
        pointee_ty,
        &left_name,
        &mut instructions,
    );
    let right = crate::lower1::place::emit_pointer_read(
        explicit_method_args[0].clone(),
        pointee_ty,
        &right_name,
        &mut instructions,
    );
    crate::lower1::place::emit_pointer_write(
        receiver_operand,
        pointee_ty,
        right,
        &mut instructions,
    );
    crate::lower1::place::emit_pointer_write(
        explicit_method_args[0].clone(),
        pointee_ty,
        left,
        &mut instructions,
    );
}
