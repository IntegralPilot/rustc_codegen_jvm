//! Pointer intrinsics.
use super::*;

pub(super) fn ptr_offset_from<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
) {
    let left_ty = oomir_operands[0]
        .get_type()
        .expect("ptr_offset_from left operand is typed");
    let right_ty = oomir_operands[1]
        .get_type()
        .expect("ptr_offset_from right operand is typed");
    if !matches!(left_ty, oomir::Type::Pointer(_)) || !matches!(right_ty, oomir::Type::Pointer(_)) {
        panic!("ptr_offset_from requires pointer operands, found {left_ty:?} and {right_ty:?}");
    }
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: effective_dest.clone(),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "offset_from".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("left".to_string(), left_ty),
                ("right".to_string(), right_ty),
            ],
            ret: Box::new(oomir_output_type.clone()),
            is_static: true,
        },
        args: vec![oomir_operands[0].clone(), oomir_operands[1].clone()],
    });
}
pub(super) fn ptr_drop_in_place<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    _label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
) {
    let pointee_ty = func_instance
        .args
        .types()
        .next()
        .expect("drop_in_place has a pointee type argument");
    emit_drop_in_place(
        pointee_ty,
        oomir_operands[0].clone(),
        tcx,
        instance,
        data_types,
        instructions,
    );
}
pub(super) fn ptr_eq<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    intrinsic_name: String,
) {
    let first_ty = oomir_operands[0]
        .get_type()
        .expect("pointer equality operand is typed");
    let second_ty = oomir_operands[1]
        .get_type()
        .expect("pointer equality operand is typed");
    if matches!(first_ty, oomir::Type::Slice(_) | oomir::Type::Str) {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: if intrinsic_name.as_str() == "addr_eq" {
                "fatPointerSameAddress".to_string()
            } else {
                "fatPointerEquals".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "left".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    (
                        "right".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                ],
                ret: Box::new(oomir::Type::Boolean),
                is_static: true,
            },
            args: oomir_operands[..2].to_vec(),
            dest: effective_dest.clone(),
        });
    } else {
        instructions.push(oomir::Instruction::InvokeVirtual {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: if intrinsic_name.as_str() == "addr_eq" {
                "sameAddress".to_string()
            } else {
                "samePointer".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), first_ty),
                    ("other".to_string(), second_ty),
                ],
                ret: Box::new(oomir::Type::Boolean),
                is_static: false,
            },
            args: vec![oomir_operands[1].clone()],
            dest: effective_dest.clone(),
            operand: oomir_operands[0].clone(),
        });
    }
}
pub(super) fn dangling<'tcx>(
    tcx: TyCtxt<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    fn_output: Ty<'tcx>,
    effective_dest: Option<String>,
    method_signature: oomir::Signature,
) {
    let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
        unreachable!()
    };
    let pointee_size = crate::lower1::types::layout_size_bytes(tcx, *pointee)
        .unwrap_or_else(|error| panic!("could not determine dangling pointer size: {error}"));
    let pointee_alignment = crate::lower1::types::layout_align_bytes(tcx, *pointee)
        .unwrap_or_else(|error| panic!("could not determine dangling pointer alignment: {error}"));
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: effective_dest.clone(),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "withoutProvenance".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("address".to_string(), oomir::Type::U64),
                ("view_size".to_string(), oomir::Type::U64),
            ],
            ret: method_signature.ret.clone(),
            is_static: true,
        },
        args: vec![
            oomir::Operand::Constant(oomir::Constant::U64(pointee_alignment as u64)),
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(pointee_size).expect("Rust pointee layout exceeds u64"),
            )),
        ],
    });
}
pub(super) fn ptr_slice_from_raw_parts<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    fn_output: Ty<'tcx>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
) {
    let data = oomir_operands[0].clone();
    if let Some(dest) = effective_dest.clone() {
        let is_str = matches!(oomir_output_type, oomir::Type::Str);
        let pointee_ty = match fn_output.kind() {
            TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
            other => panic!("raw slice constructor returned unexpected type {other:?}"),
        };
        let element_ty = match pointee_ty.kind() {
            TyKind::Slice(element_ty) => *element_ty,
            TyKind::Str => tcx.types.u8,
            other => panic!("raw slice constructor returned unexpected pointee {other:?}"),
        };
        let element_size =
            crate::lower1::types::layout_size_bytes(tcx, element_ty).unwrap_or_else(|error| {
                panic!("could not determine raw slice element layout: {error}")
            });
        let data = crate::lower1::place::emit_retyped_slice_data_pointer(
            data,
            crate::lower1::types::ty_to_oomir_type(element_ty, tcx, data_types, instance),
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(element_size).expect("Rust slice element layout exceeds u64"),
            )),
            crate::lower1::types::pointer_view_codec_operand(element_ty, tcx, data_types, instance),
            &format!("{label}_raw_slice"),
            instructions,
        );
        let view_class = if is_str {
            oomir::UTF8_VIEW_CLASS
        } else {
            oomir::SLICE_VIEW_CLASS
        };
        let (backing, offset) = crate::lower1::place::emit_pointer_slice_parts(
            data,
            &format!("{label}_raw_slice"),
            instructions,
        );
        let slice_object = format!("{label}_raw_slice_object");
        instructions.push(oomir::Instruction::ConstructObject {
            dest: slice_object.clone(),
            class_name: view_class.to_string(),
            args: vec![
                (backing, oomir::Type::Class("java/lang/Object".to_string())),
                (offset, oomir::Type::I32),
                (oomir_operands[1].clone(), oomir::Type::U64),
            ],
        });
        instructions.push(oomir::Instruction::Cast {
            op: oomir::Operand::Variable {
                name: slice_object,
                ty: oomir::Type::Class(view_class.to_string()),
            },
            ty: oomir_output_type.clone(),
            dest,
        });
    }
}
pub(super) fn ptr_without_provenance<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    fn_output: Ty<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    method_signature: oomir::Signature,
    intrinsic_name: String,
) {
    let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
        unreachable!()
    };
    let pointee_size =
        crate::lower1::types::layout_size_bytes(tcx, *pointee).unwrap_or_else(|error| {
            panic!("could not determine provenance pointer target size: {error}")
        });
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: effective_dest.clone(),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: if intrinsic_name.starts_with("without_") {
            "withoutProvenance".to_string()
        } else {
            "fromAddress".to_string()
        },
        method_ty: oomir::Signature {
            params: vec![
                ("address".to_string(), oomir::Type::U64),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: method_signature.ret.clone(),
            is_static: true,
        },
        args: vec![
            oomir_operands[0].clone(),
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(pointee_size).expect("Rust pointee layout exceeds u64"),
            )),
            crate::lower1::types::pointer_view_codec_operand(*pointee, tcx, data_types, instance),
        ],
    });
}
pub(super) fn ptr_read<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    intrinsic_name: String,
    is_compiler_intrinsic: bool,
    has_diagnostic_item: impl Fn(&str) -> bool,
) {
    if has_diagnostic_item("ptr_read_volatile")
        || (is_compiler_intrinsic
            && matches!(
                intrinsic_name.as_str(),
                "volatile_load" | "unaligned_volatile_load"
            ))
    {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "volatileFence".to_string(),
            method_ty: oomir::Signature {
                params: Vec::new(),
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: Vec::new(),
            dest: None,
        });
    }
    if let Some(dest) = effective_dest.clone()
        && let Some(oomir::Type::Pointer(pointee)) = oomir_operands[0].get_type()
    {
        crate::lower1::place::emit_pointer_read_copy(
            oomir_operands[0].clone(),
            pointee.as_ref(),
            &dest,
            instructions,
        );
    }
}
pub(super) fn ptr_write<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
    intrinsic_name: String,
    is_compiler_intrinsic: bool,
    is_diagnostic_item: impl Fn(Symbol) -> bool,
) {
    if let Some(oomir::Type::Pointer(pointee)) = oomir_operands[0].get_type() {
        crate::lower1::place::emit_pointer_write(
            oomir_operands[0].clone(),
            pointee.as_ref(),
            oomir_operands[1].clone(),
            instructions,
        );
    }
    if is_diagnostic_item(sym::ptr_write_volatile)
        || (is_compiler_intrinsic
            && matches!(
                intrinsic_name.as_str(),
                "volatile_store" | "unaligned_volatile_store"
            ))
    {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "volatileFence".to_string(),
            method_ty: oomir::Signature {
                params: Vec::new(),
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: Vec::new(),
            dest: None,
        });
    }
}
pub(super) fn swap_nonoverlapping_bytes<'tcx>(
    instructions: &mut Vec<oomir::Instruction>,
    oomir_operands: Vec<oomir::Operand>,
) {
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "swapNonOverlappingNonZero".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "left".to_string(),
                    oomir_operands[0]
                        .get_type()
                        .expect("swap left pointer is typed"),
                ),
                (
                    "right".to_string(),
                    oomir_operands[1]
                        .get_type()
                        .expect("swap right pointer is typed"),
                ),
                (
                    "byte_count".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                ),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args: oomir_operands[..3].to_vec(),
        dest: None,
    });
}
pub(super) fn ptr_swap<'tcx>(
    tcx: TyCtxt<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
    is_diagnostic_item: impl Fn(Symbol) -> bool,
) {
    let element_ty = func_instance
        .args
        .types()
        .next()
        .expect("pointer swap has a type argument");
    let byte_count = crate::lower1::types::layout_size_bytes(tcx, element_ty)
        .expect("pointer swap requires a sized pointee");
    let left_ty = oomir_operands[0]
        .get_type()
        .expect("swap left pointer is typed");
    let right_ty = oomir_operands[1]
        .get_type()
        .expect("swap right pointer is typed");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: if is_diagnostic_item(sym::ptr_swap) {
            "swap".to_string()
        } else {
            "swapNonOverlapping".to_string()
        },
        method_ty: oomir::Signature {
            params: vec![
                ("left".to_string(), left_ty),
                ("right".to_string(), right_ty),
                ("byte_count".to_string(), oomir::Type::U64),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args: vec![
            oomir_operands[0].clone(),
            oomir_operands[1].clone(),
            oomir::Operand::Constant(oomir::Constant::U64(byte_count as u64)),
        ],
        dest: None,
    });
}
pub(super) fn ptr_swap_nonoverlapping<'tcx>(
    tcx: TyCtxt<'tcx>,
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
) {
    let element_ty = func_instance
        .args
        .types()
        .next()
        .expect("swap_nonoverlapping has a type argument");
    let (method_name, count) =
        if let Ok(element_size) = crate::lower1::types::layout_size_bytes(tcx, element_ty) {
            let byte_count_name = format!("{label}_swap_nonoverlapping_byte_count");
            instructions.push(oomir::Instruction::Binary {
                op: crate::oomir::BinaryOp::Mul,
                dest: byte_count_name.clone(),
                op1: oomir_operands[2].clone(),
                op2: oomir::Operand::Constant(oomir::Constant::U64(element_size as u64)),
            });
            (
                "swapNonOverlapping".to_string(),
                oomir::Operand::Variable {
                    name: byte_count_name,
                    ty: oomir::Type::U64,
                },
            )
        } else {
            (
                "swapNonOverlappingElements".to_string(),
                oomir_operands[2].clone(),
            )
        };
    let left_ty = oomir_operands[0]
        .get_type()
        .expect("swap_nonoverlapping left pointer is typed");
    let right_ty = oomir_operands[1]
        .get_type()
        .expect("swap_nonoverlapping right pointer is typed");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name,
        method_ty: oomir::Signature {
            params: vec![
                ("left".to_string(), left_ty),
                ("right".to_string(), right_ty),
                ("byte_count".to_string(), oomir::Type::U64),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args: vec![oomir_operands[0].clone(), oomir_operands[1].clone(), count],
        dest: None,
    });
}
pub(super) fn ptr_copy<'tcx>(
    tcx: TyCtxt<'tcx>,
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
    intrinsic_name: String,
    is_compiler_intrinsic: bool,
    is_diagnostic_item: impl Fn(Symbol) -> bool,
) {
    let writes_bytes = is_diagnostic_item(sym::ptr_write_bytes)
        || (is_compiler_intrinsic && intrinsic_name.as_str() == "write_bytes");
    let nonoverlapping = is_diagnostic_item(sym::ptr_copy_nonoverlapping)
        || (is_compiler_intrinsic && intrinsic_name.as_str() == "copy_nonoverlapping");
    let element_ty = func_instance
        .args
        .types()
        .next()
        .expect("pointer memory intrinsic has a type argument");
    let count = oomir_operands
        .last()
        .cloned()
        .expect("pointer memory intrinsic has a count argument");
    let (uses_runtime_element_size, count) =
        if let Ok(element_size) = crate::lower1::types::layout_size_bytes(tcx, element_ty) {
            let byte_count_name = format!("{label}_pointer_memory_byte_count");
            instructions.push(oomir::Instruction::Binary {
                op: crate::oomir::BinaryOp::Mul,
                dest: byte_count_name.clone(),
                op1: count,
                op2: oomir::Operand::Constant(oomir::Constant::U64(element_size as u64)),
            });
            (
                false,
                oomir::Operand::Variable {
                    name: byte_count_name,
                    ty: oomir::Type::U64,
                },
            )
        } else {
            (true, count)
        };
    let first_pointer_ty = oomir_operands
        .first()
        .and_then(oomir::Operand::get_type)
        .expect("pointer memory intrinsic has a destination/source pointer");
    if writes_bytes {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: if uses_runtime_element_size {
                "writeElements".to_string()
            } else {
                "writeBytes".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![
                    ("destination".to_string(), first_pointer_ty),
                    ("value".to_string(), oomir::Type::I32),
                    ("byte_count".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![oomir_operands[0].clone(), oomir_operands[1].clone(), count],
            dest: None,
        });
    } else {
        let second_pointer_ty = oomir_operands[1]
            .get_type()
            .expect("copy intrinsic has a destination pointer");
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: if nonoverlapping && uses_runtime_element_size {
                "copyNonOverlappingElements".to_string()
            } else if nonoverlapping {
                "copyNonOverlapping".to_string()
            } else if uses_runtime_element_size {
                "copyElements".to_string()
            } else {
                "copy".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![
                    ("source".to_string(), first_pointer_ty),
                    ("destination".to_string(), second_pointer_ty),
                    ("byte_count".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![oomir_operands[0].clone(), oomir_operands[1].clone(), count],
            dest: None,
        });
    }
}
