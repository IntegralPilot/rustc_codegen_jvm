//! Views operations on Rust places.
use super::*;

pub(crate) fn emit_pointer_read(
    pointer: Operand,
    pointee_ty: &oomir::Type,
    dest: &str,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    memory_read(pointer, pointee_ty, dest, false, instructions)
}

/// An owned read must not establish a mutable decoded-view binding.
pub(crate) fn emit_pointer_read_copy(
    pointer: Operand,
    pointee_ty: &oomir::Type,
    dest: &str,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    memory_read(pointer, pointee_ty, dest, true, instructions)
}

fn memory_read(
    pointer: Operand,
    pointee_ty: &oomir::Type,
    dest: &str,
    owned: bool,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    if !pointee_ty.has_jvm_value() {
        return Operand::Constant(oomir::Constant::Unit);
    }
    instructions.push(Instruction::MemoryLoad {
        dest: dest.into(),
        pointer,
        pointee: pointee_ty.clone(),
        owned,
    });
    Operand::Variable {
        name: dest.into(),
        ty: pointee_ty.clone(),
    }
}

/// Only the final read makes an owned copy. Earlier projections need the live owner for writeback.
pub(crate) fn detach_pointer_read(instructions: &mut [Instruction], value_name: &str) -> bool {
    let Some(Instruction::MemoryLoad { dest, owned, .. }) = instructions.last_mut() else {
        return false;
    };
    if dest != value_name {
        return false;
    }
    *owned = true;
    true
}

pub(crate) fn emit_pointer_write(
    pointer: Operand,
    pointee_ty: &oomir::Type,
    value: Operand,
    instructions: &mut Vec<Instruction>,
) {
    if pointee_ty.has_jvm_value() {
        instructions.push(Instruction::MemoryStore {
            pointer,
            pointee: pointee_ty.clone(),
            value,
        });
    }
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

pub(crate) fn emit_pointer_slice_view(
    data: Operand,
    length: Operand,
    dest: &str,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    let Some(oomir::Type::Pointer(element)) = data.get_type() else {
        panic!("slice data must be an element pointer")
    };
    let (backing, start) = if data.get_type().unwrap().scalar_address_size().is_some() {
        emit_pointer_slice_parts(data, dest, instructions)
    } else {
        (data, Operand::Constant(oomir::Constant::I32(0)))
    };
    let object = oomir::Type::Class("java/lang/Object".into());
    let carrier = oomir::Type::Class(oomir::SLICE_VIEW_CLASS.into());
    let name = format!("{dest}_view");
    instructions.push(Instruction::ConstructObject {
        dest: name.clone(),
        class_name: oomir::SLICE_VIEW_CLASS.into(),
        args: vec![
            (backing, object),
            (start, oomir::Type::I32),
            (length, oomir::Type::U64),
        ],
    });
    let ty = oomir::Type::Slice(element.value);
    instructions.push(Instruction::Cast {
        dest: dest.into(),
        op: Operand::Variable { name, ty: carrier },
        ty: ty.clone(),
    });
    Operand::Variable {
        name: dest.into(),
        ty,
    }
}

/// Gives a raw slice data pointer the element view used by `SliceView` accessors.
/// This matters when MIR constructs a fat pointer from an erased or whole-array
/// view: the backing `Pointer` must advance and load one Rust element at a time.
pub(crate) fn emit_retyped_slice_data_pointer(
    data: Operand,
    element_type: oomir::Type,
    element_size: Operand,
    element_codec: Operand,
    dest_prefix: &str,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    let data_ty = data
        .get_type()
        .expect("slice data pointer must have an OOMIR type");
    let result_ty = oomir::Type::pointer(element_type);
    let dest = format!("{dest_prefix}_element_pointer");
    instructions.push(Instruction::AddressRetype {
        dest: Some(dest.clone()),
        source: data,
        layout: Box::new(oomir::AddressLayout {
            pointer_type: result_ty.clone(),
            size: element_size,
            codec: element_codec,
        }),
    });
    Operand::Variable {
        name: dest,
        ty: result_ty,
    }
}
