//! Views operations on Rust places.
use super::*;

pub(crate) fn pointer_getter_for_type(ty: &oomir::Type) -> (&'static str, oomir::Type) {
    match ty {
        oomir::Type::Boolean => ("getBoolean", oomir::Type::Boolean),
        oomir::Type::I8 | oomir::Type::U8 => ("getI8", oomir::Type::I8),
        oomir::Type::I16 | oomir::Type::U16 => ("getI16", oomir::Type::I16),
        oomir::Type::F16 => ("getI16", oomir::Type::F16),
        oomir::Type::I32 | oomir::Type::U32 | oomir::Type::Char => ("getI32", oomir::Type::I32),
        oomir::Type::I64 | oomir::Type::U64 => ("getI64", oomir::Type::I64),
        oomir::Type::F32 => ("getF32", oomir::Type::F32),
        oomir::Type::F64 => ("getF64", oomir::Type::F64),
        _ => (
            "getObject",
            oomir::Type::Class("java/lang/Object".to_string()),
        ),
    }
}

/// Loads the pointee while retaining the pointer itself as a first-class JVM
/// value. Reference-valued pointees use Object at the runtime boundary and are
/// cast back to their precise OOMIR type immediately afterwards.
pub(crate) fn emit_pointer_read(
    pointer: Operand,
    pointee_ty: &oomir::Type,
    dest: &str,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    if !pointee_ty.has_jvm_value() {
        return Operand::Constant(oomir::Constant::Unit);
    }
    let pointer_ty = pointer
        .get_type()
        .expect("a pointer read requires a typed operand");
    let (mut method_name, runtime_ret_ty) = pointer_getter_for_type(pointee_ty);
    let requested_class = match pointee_ty {
        oomir::Type::Class(class_name) => Some(class_name.clone()),
        oomir::Type::Slice(_) => Some(oomir::SLICE_VIEW_CLASS.to_string()),
        _ => None,
    };
    if requested_class.is_some() {
        method_name = "getObjectAs";
    }
    let runtime_dest = if runtime_ret_ty == *pointee_ty {
        dest.to_string()
    } else {
        format!("{dest}_object")
    };
    instructions.push(Instruction::InvokeVirtual {
        dest: Some(runtime_dest.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: method_name.to_string(),
        method_ty: oomir::Signature {
            params: vec![("self".to_string(), pointer_ty)]
                .into_iter()
                .chain(
                    requested_class
                        .is_some()
                        .then_some(("target_class".to_string(), oomir::Type::java_string())),
                )
                .collect(),
            ret: Box::new(runtime_ret_ty.clone()),
            is_static: false,
        },
        args: requested_class
            .map(|class_name| vec![Operand::Constant(oomir::Constant::String(class_name))])
            .unwrap_or_default(),
        operand: pointer,
    });
    if runtime_ret_ty != *pointee_ty {
        instructions.push(Instruction::Cast {
            op: Operand::Variable {
                name: runtime_dest,
                ty: runtime_ret_ty,
            },
            ty: pointee_ty.clone(),
            dest: dest.to_string(),
        });
    }
    Operand::Variable {
        name: dest.to_string(),
        ty: pointee_ty.clone(),
    }
}

/// Implements Rust's `ptr::read` family. Unlike an ordinary dereference, these
/// operations produce an independent bitwise copy of the pointee. JVM-backed
/// aggregate carriers therefore need to be detached from the memory view.
pub(crate) fn emit_pointer_read_copy(
    pointer: Operand,
    pointee_ty: &oomir::Type,
    dest: &str,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    if !pointee_ty.is_jvm_reference_type() {
        return emit_pointer_read(pointer, pointee_ty, dest, instructions);
    }

    let loaded_dest = format!("{dest}_loaded");
    let loaded = emit_pointer_read(pointer, pointee_ty, &loaded_dest, instructions);
    let object_dest = format!("{dest}_copy_object");
    let object_ty = oomir::Type::Class("java/lang/Object".to_string());
    instructions.push(Instruction::InvokeStatic {
        dest: Some(object_dest.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "copyManagedValue".to_string(),
        method_ty: oomir::Signature {
            params: vec![("value".to_string(), object_ty.clone())],
            ret: Box::new(object_ty.clone()),
            is_static: true,
        },
        args: vec![loaded],
    });
    instructions.push(Instruction::Cast {
        op: Operand::Variable {
            name: object_dest,
            ty: object_ty,
        },
        ty: pointee_ty.clone(),
        dest: dest.to_string(),
    });
    Operand::Variable {
        name: dest.to_string(),
        ty: pointee_ty.clone(),
    }
}

pub(crate) fn emit_pointer_write(
    pointer: Operand,
    pointee_ty: &oomir::Type,
    value: Operand,
    instructions: &mut Vec<Instruction>,
) {
    if !pointee_ty.has_jvm_value() {
        return;
    }
    let pointer_ty = pointer
        .get_type()
        .expect("a pointer write requires a typed operand");
    let value_ty = if pointee_ty.is_jvm_primitive() {
        pointee_ty.clone()
    } else {
        oomir::Type::Class("java/lang/Object".to_string())
    };
    instructions.push(Instruction::InvokeVirtual {
        dest: None,
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "set".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("self".to_string(), pointer_ty),
                ("value".to_string(), value_ty),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        args: vec![value],
        operand: pointer,
    });
}

pub(crate) fn emit_slice_view(
    source: Operand,
    source_type: &oomir::Type,
    from: u64,
    to: u64,
    from_end: bool,
    dest: &str,
    instructions: &mut Vec<Instruction>,
) -> oomir::Type {
    let element_type = match source_type {
        oomir::Type::Array(element) | oomir::Type::Slice(element) => element.as_ref().clone(),
        other => panic!("Cannot create a slice view over {other:?}"),
    };
    let slice_type = oomir::Type::Slice(Box::new(element_type.clone()));
    let length_name = format!("{dest}_source_length");
    instructions.push(Instruction::Length {
        dest: length_name.clone(),
        array: source.clone(),
    });

    let (backing, base_offset) = if matches!(source_type, oomir::Type::Slice(_)) {
        let backing_object_name = format!("{dest}_backing_object");
        instructions.push(Instruction::GetField {
            dest: backing_object_name.clone(),
            object: source.clone(),
            field_name: "array".to_string(),
            field_ty: oomir::Type::Class("java/lang/Object".to_string()),
            owner_class: oomir::SLICE_VIEW_CLASS.to_string(),
        });
        let offset_name = format!("{dest}_base_offset");
        instructions.push(Instruction::GetField {
            dest: offset_name.clone(),
            object: source,
            field_name: "offset".to_string(),
            field_ty: oomir::Type::I32,
            owner_class: oomir::SLICE_VIEW_CLASS.to_string(),
        });
        (
            Operand::Variable {
                name: backing_object_name,
                ty: oomir::Type::Class("java/lang/Object".to_string()),
            },
            Operand::Variable {
                name: offset_name,
                ty: oomir::Type::I32,
            },
        )
    } else {
        (source, Operand::Constant(oomir::Constant::I32(0)))
    };

    let offset_name = format!("{dest}_offset");
    instructions.push(Instruction::Binary {
        op: crate::oomir::BinaryOp::Add,
        dest: offset_name.clone(),
        op1: base_offset,
        op2: Operand::Constant(oomir::Constant::I32(from as i32)),
    });
    let view_length = if from_end {
        let after_start_name = format!("{dest}_after_start");
        instructions.push(Instruction::Binary {
            op: crate::oomir::BinaryOp::Sub,
            dest: after_start_name.clone(),
            op1: Operand::Variable {
                name: length_name,
                ty: oomir::Type::I32,
            },
            op2: Operand::Constant(oomir::Constant::I32(from as i32)),
        });
        let view_length_name = format!("{dest}_length");
        instructions.push(Instruction::Binary {
            op: crate::oomir::BinaryOp::Sub,
            dest: view_length_name.clone(),
            op1: Operand::Variable {
                name: after_start_name,
                ty: oomir::Type::I32,
            },
            op2: Operand::Constant(oomir::Constant::I32(to as i32)),
        });
        Operand::Variable {
            name: view_length_name,
            ty: oomir::Type::I32,
        }
    } else {
        Operand::Constant(oomir::Constant::I32((to - from) as i32))
    };

    let object_name = format!("{dest}_object");
    instructions.push(Instruction::ConstructObject {
        dest: object_name.clone(),
        class_name: oomir::SLICE_VIEW_CLASS.to_string(),
        args: vec![
            (backing, oomir::Type::Class("java/lang/Object".to_string())),
            (
                Operand::Variable {
                    name: offset_name,
                    ty: oomir::Type::I32,
                },
                oomir::Type::I32,
            ),
            (view_length, oomir::Type::I32),
        ],
    });
    instructions.push(Instruction::Cast {
        dest: dest.to_string(),
        op: Operand::Variable {
            name: object_name,
            ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
        },
        ty: slice_type.clone(),
    });
    slice_type
}

/// Extracts the canonical JVM array and element offset from a Rust data
/// pointer before it is wrapped in a `SliceView` or `Utf8View`.
pub(crate) fn emit_pointer_slice_parts(
    data: Operand,
    dest_prefix: &str,
    instructions: &mut Vec<Instruction>,
) -> (Operand, Operand) {
    let data_ty = data
        .get_type()
        .expect("slice data pointer must have an OOMIR type");
    let backing_name = format!("{dest_prefix}_backing");
    instructions.push(Instruction::InvokeVirtual {
        dest: Some(backing_name.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "sliceBackingArray".to_string(),
        method_ty: oomir::Signature {
            params: vec![("self".to_string(), data_ty.clone())],
            ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
            is_static: false,
        },
        args: Vec::new(),
        operand: data.clone(),
    });
    let offset_name = format!("{dest_prefix}_offset");
    instructions.push(Instruction::InvokeVirtual {
        dest: Some(offset_name.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "sliceElementOffset".to_string(),
        method_ty: oomir::Signature {
            params: vec![("self".to_string(), data_ty)],
            ret: Box::new(oomir::Type::I32),
            is_static: false,
        },
        args: Vec::new(),
        operand: data,
    });
    (
        Operand::Variable {
            name: backing_name,
            ty: oomir::Type::Class("java/lang/Object".to_string()),
        },
        Operand::Variable {
            name: offset_name,
            ty: oomir::Type::I32,
        },
    )
}

/// Gives a raw slice data pointer the element view used by `SliceView` accessors.
/// This matters when MIR constructs a fat pointer from an erased or whole-array
/// view: the backing `Pointer` must advance and load one Rust element at a time.
pub(crate) fn emit_retyped_slice_data_pointer(
    data: Operand,
    element_size: Operand,
    element_codec: Operand,
    dest_prefix: &str,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    let data_ty = data
        .get_type()
        .expect("slice data pointer must have an OOMIR type");
    let dest = format!("{dest_prefix}_element_pointer");
    instructions.push(Instruction::InvokeStatic {
        dest: Some(dest.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "retype".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), data_ty.clone()),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(data_ty.clone()),
            is_static: true,
        },
        args: vec![data, element_size, element_codec],
    });
    Operand::Variable {
        name: dest,
        ty: data_ty,
    }
}
