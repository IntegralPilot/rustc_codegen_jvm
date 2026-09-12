//! Atomic.
use super::*;

pub(in crate::lower1) fn atomic_value_as_bits(
    value: oomir::Operand,
    temp_name: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let value_ty = value
        .get_type()
        .expect("Rust atomic value must have a JVM carrier type");
    if matches!(value_ty, oomir::Type::Pointer(_)) {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "address".to_string(),
            method_ty: oomir::Signature {
                params: vec![("pointer".to_string(), value_ty)],
                ret: Box::new(oomir::Type::U64),
                is_static: true,
            },
            args: vec![value],
            dest: Some(temp_name.to_string()),
        });
    } else {
        instructions.push(oomir::Instruction::Cast {
            op: value,
            ty: oomir::Type::U64,
            dest: temp_name.to_string(),
        });
    }
    oomir::Operand::Variable {
        name: temp_name.to_string(),
        ty: oomir::Type::U64,
    }
}

pub(in crate::lower1) fn emit_value_selection(
    condition: oomir::Operand,
    true_value: oomir::Operand,
    false_value: oomir::Operand,
    output_type: &oomir::Type,
    dest: String,
    instructions: &mut Vec<oomir::Instruction>,
) {
    if output_type.is_jvm_reference_type() {
        let selected_object = format!("{dest}_selected_object");
        let object_ty = oomir::Type::Class("java/lang/Object".to_string());
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: "org/rustlang/runtime/Intrinsics".to_string(),
            method_name: "selectUnpredictable".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("condition".to_string(), oomir::Type::Boolean),
                    ("true_value".to_string(), object_ty.clone()),
                    ("false_value".to_string(), object_ty.clone()),
                ],
                ret: Box::new(object_ty.clone()),
                is_static: true,
            },
            args: vec![condition, true_value, false_value],
            dest: Some(selected_object.clone()),
        });
        instructions.push(oomir::Instruction::Cast {
            op: oomir::Operand::Variable {
                name: selected_object,
                ty: object_ty,
            },
            ty: output_type.clone(),
            dest,
        });
    } else {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: "org/rustlang/runtime/Intrinsics".to_string(),
            method_name: "selectUnpredictable".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("condition".to_string(), oomir::Type::Boolean),
                    ("true_value".to_string(), output_type.clone()),
                    ("false_value".to_string(), output_type.clone()),
                ],
                ret: Box::new(output_type.clone()),
                is_static: true,
            },
            args: vec![condition, true_value, false_value],
            dest: Some(dest),
        });
    }
}

pub(in crate::lower1) fn emit_atomic_value_from_bits<'tcx>(
    bits: oomir::Operand,
    atomic_ty: Ty<'tcx>,
    atomic_oomir_ty: &oomir::Type,
    dest: String,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    if let TyKind::RawPtr(pointee, _) = atomic_ty.kind() {
        let view_size =
            crate::lower1::types::layout_size_bytes(tcx, *pointee).unwrap_or_else(|error| {
                panic!("could not determine atomic pointer target size for {pointee:?}: {error}")
            });
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "fromAddress".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("address".to_string(), oomir::Type::U64),
                    ("view_size".to_string(), oomir::Type::U64),
                    ("view_codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(atomic_oomir_ty.clone()),
                is_static: true,
            },
            args: vec![
                bits,
                oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(view_size).expect("Rust atomic pointee layout exceeds u64"),
                )),
                crate::lower1::types::pointer_view_codec_operand(
                    *pointee, tcx, data_types, instance,
                ),
            ],
            dest: Some(dest),
        });
    } else {
        instructions.push(oomir::Instruction::Cast {
            op: bits,
            ty: atomic_oomir_ty.clone(),
            dest,
        });
    }
}

pub(in crate::lower1) fn atomic_ordering_code(instance: Instance<'_>, const_index: usize) -> i32 {
    let ordering = instance.args.const_at(const_index).to_value();
    let discriminant = ordering
        .to_branch()
        .first()
        .expect("Rust atomic ordering must have an enum discriminant")
        .to_leaf()
        .to_i32();
    assert!(
        (0..=4).contains(&discriminant),
        "unknown Rust atomic ordering discriminant {discriminant}"
    );
    discriminant
}

pub(in crate::lower1) fn atomic_ordering_operand(
    instance: Instance<'_>,
    const_index: usize,
) -> oomir::Operand {
    oomir::Operand::Constant(oomir::Constant::I32(atomic_ordering_code(
        instance,
        const_index,
    )))
}

#[allow(clippy::too_many_arguments)]
pub(in crate::lower1) fn emit_atomic_intrinsic<'tcx>(
    intrinsic_name: &str,
    intrinsic_instance: Instance<'tcx>,
    caller_instance: Instance<'tcx>,
    oomir_operands: &[oomir::Operand],
    oomir_output_type: &oomir::Type,
    effective_dest: Option<String>,
    label: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> bool {
    if matches!(intrinsic_name, "atomic_fence" | "atomic_singlethreadfence") {
        let ordering = atomic_ordering_operand(intrinsic_instance, 0);
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "atomicFence".to_string(),
            method_ty: oomir::Signature {
                params: vec![("ordering".to_string(), oomir::Type::I32)],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![ordering],
            dest: None,
        });
        return true;
    }

    let runtime_method = match intrinsic_name {
        "atomic_load" => "atomicLoad",
        "atomic_store" => "atomicStore",
        "atomic_xchg" => "atomicExchange",
        "atomic_xadd" => "atomicAdd",
        "atomic_xsub" => "atomicSubtract",
        "atomic_and" => "atomicAnd",
        "atomic_nand" => "atomicNand",
        "atomic_or" => "atomicOr",
        "atomic_xor" => "atomicXor",
        "atomic_max" => "atomicMax",
        "atomic_min" => "atomicMin",
        "atomic_umax" => "atomicUnsignedMax",
        "atomic_umin" => "atomicUnsignedMin",
        "atomic_cxchg" | "atomic_cxchgweak" => "atomicCompareExchange",
        _ => return false,
    };

    let atomic_ty = intrinsic_instance.args.type_at(0);
    let atomic_oomir_ty =
        crate::lower1::types::ty_to_oomir_type(atomic_ty, tcx, data_types, caller_instance);
    let byte_count =
        crate::lower1::types::layout_size_bytes(tcx, atomic_ty).unwrap_or_else(|error| {
            panic!("could not determine atomic scalar size for {atomic_ty:?}: {error}")
        });
    let byte_count = oomir::Operand::Constant(oomir::Constant::I32(
        i32::try_from(byte_count).expect("Rust atomic scalar width exceeds i32"),
    ));
    let pointer = oomir_operands
        .first()
        .cloned()
        .expect("Rust memory atomic requires a destination pointer");
    let pointer_ty = pointer
        .get_type()
        .expect("Rust atomic destination pointer must be typed");
    let ordering_const_index = if matches!(
        intrinsic_name,
        "atomic_xadd" | "atomic_xsub" | "atomic_and" | "atomic_nand" | "atomic_or" | "atomic_xor"
    ) {
        2
    } else {
        1
    };
    let ordering = atomic_ordering_operand(intrinsic_instance, ordering_const_index);

    if intrinsic_name == "atomic_load" {
        let raw_dest = format!("{label}_atomic_bits");
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: runtime_method.to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty),
                    ("byte_count".to_string(), oomir::Type::I32),
                    ("ordering".to_string(), oomir::Type::I32),
                ],
                ret: Box::new(oomir::Type::U64),
                is_static: true,
            },
            args: vec![pointer, byte_count, ordering],
            dest: Some(raw_dest.clone()),
        });
        emit_atomic_value_from_bits(
            oomir::Operand::Variable {
                name: raw_dest,
                ty: oomir::Type::U64,
            },
            atomic_ty,
            &atomic_oomir_ty,
            effective_dest.expect("atomic_load returns a value"),
            tcx,
            caller_instance,
            data_types,
            instructions,
        );
        return true;
    }

    if intrinsic_name == "atomic_store" {
        let value = atomic_value_as_bits(
            oomir_operands
                .get(1)
                .cloned()
                .expect("atomic_store requires a value"),
            &format!("{label}_atomic_store_bits"),
            instructions,
        );
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: runtime_method.to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty),
                    ("value".to_string(), oomir::Type::U64),
                    ("byte_count".to_string(), oomir::Type::I32),
                    ("ordering".to_string(), oomir::Type::I32),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![pointer, value, byte_count, ordering],
            dest: None,
        });
        return true;
    }

    if matches!(intrinsic_name, "atomic_cxchg" | "atomic_cxchgweak") {
        let expected_value = oomir_operands
            .get(1)
            .cloned()
            .expect("atomic compare/exchange requires an expected value");
        let expected_bits = atomic_value_as_bits(
            expected_value.clone(),
            &format!("{label}_atomic_expected_bits"),
            instructions,
        );
        let replacement_bits = atomic_value_as_bits(
            oomir_operands
                .get(2)
                .cloned()
                .expect("atomic compare/exchange requires a replacement value"),
            &format!("{label}_atomic_replacement_bits"),
            instructions,
        );
        let old_bits_name = format!("{label}_atomic_old_bits");
        let failure_ordering = atomic_ordering_operand(intrinsic_instance, 2);
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: runtime_method.to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty),
                    ("expected".to_string(), oomir::Type::U64),
                    ("value".to_string(), oomir::Type::U64),
                    ("byte_count".to_string(), oomir::Type::I32),
                    ("success_ordering".to_string(), oomir::Type::I32),
                    ("failure_ordering".to_string(), oomir::Type::I32),
                ],
                ret: Box::new(oomir::Type::U64),
                is_static: true,
            },
            args: vec![
                pointer,
                expected_bits.clone(),
                replacement_bits,
                byte_count,
                ordering,
                failure_ordering,
            ],
            dest: Some(old_bits_name.clone()),
        });
        let old_bits = oomir::Operand::Variable {
            name: old_bits_name,
            ty: oomir::Type::U64,
        };
        let old_value_name = format!("{label}_atomic_old_value");
        emit_atomic_value_from_bits(
            old_bits.clone(),
            atomic_ty,
            &atomic_oomir_ty,
            old_value_name.clone(),
            tcx,
            caller_instance,
            data_types,
            instructions,
        );
        let success_name = format!("{label}_atomic_success");
        let compares_pointer_bits = matches!(atomic_ty.kind(), TyKind::RawPtr(..));
        instructions.push(oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Eq,
            dest: success_name.clone(),
            op1: if compares_pointer_bits {
                old_bits
            } else {
                oomir::Operand::Variable {
                    name: old_value_name.clone(),
                    ty: atomic_oomir_ty.clone(),
                }
            },
            op2: if compares_pointer_bits {
                expected_bits
            } else {
                expected_value
            },
        });
        let tuple_class = oomir_output_type
            .get_class_name()
            .expect("atomic compare/exchange result must be a tuple class")
            .to_string();
        instructions.push(oomir::Instruction::ConstructObject {
            dest: effective_dest.expect("atomic compare/exchange returns a value"),
            class_name: tuple_class,
            args: vec![
                (
                    oomir::Operand::Variable {
                        name: old_value_name,
                        ty: atomic_oomir_ty.clone(),
                    },
                    atomic_oomir_ty,
                ),
                (
                    oomir::Operand::Variable {
                        name: success_name,
                        ty: oomir::Type::Boolean,
                    },
                    oomir::Type::Boolean,
                ),
            ],
        });
        return true;
    }

    let value = atomic_value_as_bits(
        oomir_operands
            .get(1)
            .cloned()
            .expect("atomic read/modify/write requires an operand"),
        &format!("{label}_atomic_operand_bits"),
        instructions,
    );
    let raw_dest = format!("{label}_atomic_old_bits");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: runtime_method.to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), pointer_ty),
                ("value".to_string(), oomir::Type::U64),
                ("byte_count".to_string(), oomir::Type::I32),
                ("ordering".to_string(), oomir::Type::I32),
            ],
            ret: Box::new(oomir::Type::U64),
            is_static: true,
        },
        args: vec![pointer, value, byte_count, ordering],
        dest: Some(raw_dest.clone()),
    });
    emit_atomic_value_from_bits(
        oomir::Operand::Variable {
            name: raw_dest,
            ty: oomir::Type::U64,
        },
        atomic_ty,
        &atomic_oomir_ty,
        effective_dest.expect("atomic read/modify/write returns a value"),
        tcx,
        caller_instance,
        data_types,
        instructions,
    );
    true
}
