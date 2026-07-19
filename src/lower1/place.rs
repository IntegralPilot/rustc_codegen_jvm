use super::{
    jvm_names,
    operand::convert_operand,
    types::{
        adapt_simple_enum_operand, generate_adt_jvm_class_name, get_field_name_from_index,
        pointer_view_codec_operand, should_define_named_data_type, ty_to_oomir_type,
        union_getter_method_name, union_setter_method_name,
    },
};
use crate::oomir::{self, DataTypeMethod, Instruction, Operand};
use rustc_middle::{
    mir::{Body, Local, Operand as MirOperand, Place, ProjectionElem, Rvalue, StatementKind},
    ty::{AdtDef, EarlyBinder, GenericArgsRef, Instance, Ty, TyCtxt, TyKind, TypingEnv},
};
use std::collections::HashMap;

pub(crate) fn has_slice_or_str_struct_tail<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> bool {
    if !matches!(ty.kind(), TyKind::Adt(adt_def, _) if adt_def.is_struct()) {
        return false;
    }
    tcx.try_normalize_erasing_regions(
        TypingEnv::fully_monomorphized(),
        rustc_middle::ty::Unnormalized::new_wip(ty),
    )
    .is_ok_and(|normalized| {
        let tail = tcx.struct_tail_for_codegen(normalized, TypingEnv::fully_monomorphized());
        tail.is_slice() || tail.is_str()
    })
}

fn pointer_getter_for_type(ty: &oomir::Type) -> (&'static str, oomir::Type) {
    match ty {
        oomir::Type::Boolean => ("getBoolean", oomir::Type::Boolean),
        oomir::Type::I8 | oomir::Type::U8 => ("getI8", oomir::Type::I8),
        oomir::Type::I16 | oomir::Type::U16 | oomir::Type::F16 => ("getI16", oomir::Type::I16),
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
    instructions.push(Instruction::InvokeVirtual {
        dest: None,
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "set".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("self".to_string(), pointer_ty),
                (
                    "value".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                ),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        args: vec![value],
        operand: pointer,
    });
}

pub fn emit_slice_view(
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
    instructions.push(Instruction::Add {
        dest: offset_name.clone(),
        op1: base_offset,
        op2: Operand::Constant(oomir::Constant::I32(from as i32)),
    });
    let view_length = if from_end {
        let after_start_name = format!("{dest}_after_start");
        instructions.push(Instruction::Sub {
            dest: after_start_name.clone(),
            op1: Operand::Variable {
                name: length_name,
                ty: oomir::Type::I32,
            },
            op2: Operand::Constant(oomir::Constant::I32(from as i32)),
        });
        let view_length_name = format!("{dest}_length");
        instructions.push(Instruction::Sub {
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
pub fn emit_pointer_slice_parts(
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

pub fn place_to_string<'tcx>(place: &Place<'tcx>, _tcx: TyCtxt<'tcx>) -> String {
    // Base variable name (e.g., "_1")
    format!("_{}", place.local.index()) // Start with base local "_N"
}

pub(crate) fn local_cell_name(local: Local) -> String {
    format!("_cell_{}", local.index())
}

/// Returns true when MIR takes the address of the local's storage itself. Such
/// locals live in a canonical Pointer cell so every alias observes subsequent
/// direct and indirect writes without copy-in/copy-out bookkeeping.
pub(crate) fn local_uses_stable_cell(local: Local, mir: &Body<'_>) -> bool {
    if local.index() > 0
        && local.index() <= mir.arg_count
        && matches!(
            mir.local_decls[local].ty.kind(),
            TyKind::Ref(..) | TyKind::RawPtr(..)
        )
    {
        // Incoming references and raw pointers already are stable addresses.
        // Wrapping their local slot in another cell turns an ordinary reborrow
        // into Pointer<Pointer<T>>; JVM instance receivers are especially
        // sensitive because slot 0 contains the pointee object directly.
        return false;
    }
    mir.basic_blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            let StatementKind::Assign(box (_, rvalue)) = &statement.kind else {
                return false;
            };
            let pointed_place = match rvalue {
                Rvalue::Ref(_, _, place) | Rvalue::RawPtr(_, place) => place,
                _ => return false,
            };
            // A projected borrow still carries provenance for the complete
            // enclosing allocation. Keep the root local in one stable cell so
            // field pointers, whole-object casts, and later reborrows share it.
            pointed_place.local == local
                && !matches!(
                    pointed_place.projection.first(),
                    Some(ProjectionElem::Deref)
                )
        })
    })
}

fn union_parts_from_ty<'tcx>(ty: Ty<'tcx>) -> Option<(AdtDef<'tcx>, GenericArgsRef<'tcx>)> {
    match ty.kind() {
        TyKind::Adt(adt_def, substs) if adt_def.is_union() => Some((*adt_def, substs)),
        TyKind::Ref(_, inner_ty, _) => match inner_ty.kind() {
            TyKind::Adt(adt_def, substs) if adt_def.is_union() => Some((*adt_def, substs)),
            _ => None,
        },
        _ => None,
    }
}

fn projection_prefix_place<'tcx>(
    place: &Place<'tcx>,
    proj_index: usize,
    tcx: TyCtxt<'tcx>,
) -> Place<'tcx> {
    Place {
        local: place.local,
        projection: tcx.mk_place_elems(&place.projection[..proj_index]),
    }
}

fn union_field_name<'tcx>(adt_def: AdtDef<'tcx>, field_index: usize, tcx: TyCtxt<'tcx>) -> String {
    adt_def
        .variant(0usize.into())
        .fields
        .get(rustc_abi::FieldIdx::from_usize(field_index))
        .unwrap_or_else(|| {
            panic!(
                "Union field index {} out of bounds for {:?}",
                field_index, adt_def
            )
        })
        .ident(tcx)
        .to_string()
}

struct UnionWriteback {
    class_name: String,
    field_name: String,
    receiver: Operand,
    value_name: String,
    value_ty: oomir::Type,
}

fn collect_union_writebacks<'tcx>(
    base_place: &Place<'tcx>,
    get_instructions: &[Instruction],
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> Vec<UnionWriteback> {
    let mut writebacks = Vec::new();
    let mut instruction_cursor = 0;

    for (projection_index, projection) in base_place.projection.iter().enumerate() {
        let ProjectionElem::Field(field_index, _) = projection else {
            continue;
        };
        let union_base_place = projection_prefix_place(base_place, projection_index, tcx);
        let union_base_ty = EarlyBinder::bind(tcx, union_base_place.ty(&mir.local_decls, tcx).ty)
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        let Some((union_def, _)) = union_parts_from_ty(union_base_ty) else {
            continue;
        };
        let class_name = match ty_to_oomir_type(union_base_ty, tcx, data_types, instance) {
            oomir::Type::Class(name) => name,
            oomir::Type::Reference(inner) => match inner.as_ref() {
                oomir::Type::Class(name) => name.clone(),
                other => panic!("Union field access through non-class reference: {other:?}"),
            },
            other => panic!("Union field access on non-class type: {other:?}"),
        };
        let field_name = union_field_name(union_def, field_index.index(), tcx);
        let getter_name = union_getter_method_name(&field_name);

        let Some((relative_index, getter)) = get_instructions[instruction_cursor..]
            .iter()
            .enumerate()
            .find(|(_, instruction)| {
                matches!(
                    instruction,
                    Instruction::InvokeVirtual {
                        dest: Some(_),
                        class_name: owner,
                        method_name,
                        ..
                    } if owner == &class_name && method_name == &getter_name
                )
            })
        else {
            panic!(
                "Could not locate generated getter for nested union field {}.{}",
                class_name, field_name
            );
        };
        instruction_cursor += relative_index + 1;
        let Instruction::InvokeVirtual {
            dest: Some(value_name),
            method_ty,
            operand,
            ..
        } = getter
        else {
            unreachable!();
        };
        writebacks.push(UnionWriteback {
            class_name,
            field_name,
            receiver: operand.clone(),
            value_name: value_name.clone(),
            value_ty: method_ty.ret.as_ref().clone(),
        });
    }

    writebacks
}

fn emit_union_writebacks(writebacks: &[UnionWriteback], instructions: &mut Vec<Instruction>) {
    for writeback in writebacks.iter().rev() {
        instructions.push(Instruction::InvokeVirtual {
            dest: None,
            class_name: writeback.class_name.clone(),
            method_name: union_setter_method_name(&writeback.field_name),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "self".to_string(),
                        oomir::Type::Class(writeback.class_name.clone()),
                    ),
                    ("value".to_string(), writeback.value_ty.clone()),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: false,
            },
            args: vec![Operand::Variable {
                name: writeback.value_name.clone(),
                ty: writeback.value_ty.clone(),
            }],
            operand: writeback.receiver.clone(),
        });
    }
}

fn collect_memory_view_writebacks(get_instructions: &[Instruction]) -> Vec<Operand> {
    get_instructions
        .iter()
        .filter_map(|instruction| match instruction {
            Instruction::InvokeVirtual {
                class_name,
                method_name,
                operand,
                ..
            } if class_name == oomir::POINTER_CLASS
                && matches!(method_name.as_str(), "getObject" | "getObjectAs")
                && matches!(operand.get_type(), Some(oomir::Type::Pointer(_))) =>
            {
                Some(operand.clone())
            }
            _ => None,
        })
        .collect()
}

fn emit_memory_view_writebacks(writebacks: &[Operand], instructions: &mut Vec<Instruction>) {
    for pointer in writebacks.iter().rev() {
        let pointer_ty = pointer
            .get_type()
            .expect("a memory-view writeback requires a typed pointer");
        instructions.push(Instruction::InvokeVirtual {
            dest: None,
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "commitMemoryView".to_string(),
            method_ty: oomir::Signature {
                params: vec![("self".to_string(), pointer_ty)],
                ret: Box::new(oomir::Type::Void),
                is_static: false,
            },
            args: vec![],
            operand: pointer.clone(),
        });
    }
}

fn field_name_from_rust_ty<'tcx>(
    ty: Ty<'tcx>,
    field_index: usize,
    tcx: TyCtxt<'tcx>,
) -> Option<String> {
    match ty.kind() {
        TyKind::Ref(_, inner_ty, _) => field_name_from_rust_ty(*inner_ty, field_index, tcx),
        TyKind::Tuple(_) => Some(format!("field{}", field_index)),
        TyKind::Adt(adt_def, _) if adt_def.is_struct() => adt_def
            .variant(0usize.into())
            .fields
            .get(rustc_abi::FieldIdx::from_usize(field_index))
            .map(|field| field.ident(tcx).to_string()),
        TyKind::Adt(adt_def, _) if adt_def.is_enum() => Some(format!("field{}", field_index)),
        TyKind::Coroutine(_, _) => Some(format!("arg{}", field_index)),
        _ => None,
    }
}

pub(super) fn coroutine_saved_field_name<'tcx>(
    ty: Ty<'tcx>,
    variant_index: rustc_abi::VariantIdx,
    field_index: usize,
    tcx: TyCtxt<'tcx>,
) -> Option<String> {
    let ty = match ty.kind() {
        TyKind::Ref(_, inner, _) => *inner,
        _ => ty,
    };
    let TyKind::Coroutine(def_id, args) = ty.kind() else {
        return None;
    };
    let layout = tcx.coroutine_layout(*def_id, args).ok()?;
    let saved_local = *layout
        .variant_fields
        .get(variant_index)?
        .get(rustc_abi::FieldIdx::from_usize(field_index))?;
    Some(format!("state{}", saved_local.as_usize()))
}

pub(super) fn field_name_for_projection<'tcx>(
    owner_class_name: &str,
    field_index: usize,
    base_rust_ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &HashMap<String, oomir::DataType>,
) -> Result<String, String> {
    let named_source_field = match base_rust_ty.kind() {
        TyKind::Ref(_, inner_ty, _) => match inner_ty.kind() {
            TyKind::Tuple(_) => field_name_from_rust_ty(*inner_ty, field_index, tcx),
            TyKind::Adt(adt_def, _) if adt_def.is_struct() => {
                field_name_from_rust_ty(*inner_ty, field_index, tcx)
            }
            _ => None,
        },
        TyKind::Tuple(_) => field_name_from_rust_ty(base_rust_ty, field_index, tcx),
        TyKind::Adt(adt_def, _) if adt_def.is_struct() => {
            field_name_from_rust_ty(base_rust_ty, field_index, tcx)
        }
        _ => None,
    };
    if named_source_field.is_some() {
        return Ok(named_source_field.expect("checked above"));
    }
    get_field_name_from_index(owner_class_name, field_index, data_types).or_else(|original_error| {
        field_name_from_rust_ty(base_rust_ty, field_index, tcx).ok_or(original_error)
    })
}

/// Resolves a nested place projection, returning its variable name, emitted
/// instructions, and final OOMIR type.
pub fn emit_instructions_to_get_recursive<'tcx>(
    place: &Place<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> (String, Vec<Instruction>, oomir::Type) {
    // Start with the base local.
    let current_place = Place {
        local: place.local,
        projection: tcx.mk_place_elems(&[]),
    };
    let mut current_var = place_to_string(&current_place, tcx);
    let mut current_type = get_place_type(&current_place, mir, tcx, instance, data_types);
    let mut instructions = vec![];
    let mut coroutine_variant = None;
    if local_uses_stable_cell(place.local, mir) {
        let pointee_type = current_type.clone();
        let value_name = format!("{}_value", local_cell_name(place.local));
        let value = emit_pointer_read(
            Operand::Variable {
                name: local_cell_name(place.local),
                ty: oomir::Type::Pointer(Box::new(pointee_type.clone())),
            },
            &pointee_type,
            &value_name,
            &mut instructions,
        );
        if let Operand::Variable { name, ty } = value {
            current_var = name;
            current_type = ty;
        }
    }
    let base_rust_ty = current_place.ty(&mir.local_decls, tcx).ty;
    if let Some(value) = super::value_repr::materialize_implicit_zst(
        base_rust_ty,
        &format!("{}_implicit", current_var),
        tcx,
        instance,
        data_types,
        &mut instructions,
    ) {
        current_var = value
            .get_name()
            .expect("materialized value must use a temporary")
            .to_string();
        current_type = value
            .get_type()
            .expect("materialized value must have a JVM type");
    }

    // Iterate over each projection element in the order they appear.
    for (proj_index, proj) in place.projection.iter().enumerate() {
        let type_before_proj = current_type.clone();
        match proj {
            ProjectionElem::Field(field_index, field_ty) => {
                let base_place_for_field = projection_prefix_place(place, proj_index, tcx);
                let base_rust_ty =
                    EarlyBinder::bind(tcx, base_place_for_field.ty(&mir.local_decls, tcx).ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();

                let has_slice_tail = matches!(current_type, oomir::Type::Pointer(_))
                    && has_slice_or_str_struct_tail(tcx, base_rust_ty);
                if has_slice_tail {
                    let layout = tcx
                        .layout_of(
                            TypingEnv::fully_monomorphized().as_query_input(base_rust_ty),
                        )
                        .unwrap_or_else(|error| {
                            panic!(
                                "could not determine slice-tailed struct layout for {base_rust_ty:?}: {error:?}"
                            )
                        });
                    let field_offset = layout.fields.offset(field_index.index()).bytes_usize();
                    let field_rust_ty = EarlyBinder::bind(tcx, field_ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let base_pointer_name = current_var.clone();
                    let base_pointer_ty = current_type.clone();
                    let oomir::Type::Pointer(base_pointee_ty) = &base_pointer_ty else {
                        unreachable!();
                    };
                    let oomir::Type::Class(owner_class) = base_pointee_ty.as_ref() else {
                        panic!("slice-tailed Rust struct did not map to a JVM class");
                    };
                    let owner_class = owner_class.clone();
                    let field_name = field_name_for_projection(
                        &owner_class,
                        field_index.index(),
                        base_rust_ty,
                        tcx,
                        data_types,
                    )
                    .unwrap_or_else(|error| panic!("Error getting DST field name: {error}"));
                    let field_offset = Operand::Constant(oomir::Constant::U64(
                        u64::try_from(field_offset).expect("Rust DST field offset exceeds u64"),
                    ));

                    if let TyKind::Slice(element_rust_ty) = field_rust_ty.kind() {
                        let element_oomir_ty =
                            ty_to_oomir_type(*element_rust_ty, tcx, data_types, instance);
                        current_type = oomir::Type::Slice(Box::new(element_oomir_ty));
                        let object_name = format!("{current_var}_slice_object");
                        instructions.push(Instruction::InvokeVirtual {
                            dest: Some(object_name.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "projectStructSliceField".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), base_pointer_ty.clone()),
                                    ("owner_class".to_string(), oomir::Type::java_string()),
                                    ("field_name".to_string(), oomir::Type::java_string()),
                                    ("field_offset".to_string(), oomir::Type::U64),
                                    ("element_size".to_string(), oomir::Type::U64),
                                    ("element_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                                is_static: false,
                            },
                            args: vec![
                                Operand::Constant(oomir::Constant::String(owner_class)),
                                Operand::Constant(oomir::Constant::String(field_name)),
                                field_offset,
                                Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(
                                        super::types::layout_size_bytes(tcx, *element_rust_ty)
                                            .expect("slice tail element must have a layout"),
                                    )
                                    .expect("Rust slice-tail element layout exceeds u64"),
                                )),
                                pointer_view_codec_operand(
                                    *element_rust_ty,
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                            operand: Operand::Variable {
                                name: base_pointer_name,
                                ty: base_pointer_ty,
                            },
                        });
                        let next_var = format!("{current_var}_{}", field_index.index());
                        instructions.push(Instruction::Cast {
                            dest: next_var.clone(),
                            op: Operand::Variable {
                                name: object_name,
                                ty: oomir::Type::Class("java/lang/Object".to_string()),
                            },
                            ty: current_type.clone(),
                        });
                        current_var = next_var;
                    } else {
                        current_type = ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                        let field_pointer_ty = oomir::Type::Pointer(Box::new(current_type.clone()));
                        let typed_pointer_name = format!("{current_var}_typed_field_pointer");
                        instructions.push(Instruction::InvokeVirtual {
                            dest: Some(typed_pointer_name.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "projectStructField".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), base_pointer_ty.clone()),
                                    ("owner_class".to_string(), oomir::Type::java_string()),
                                    ("field_name".to_string(), oomir::Type::java_string()),
                                    ("field_offset".to_string(), oomir::Type::U64),
                                    ("field_size".to_string(), oomir::Type::U64),
                                    ("field_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(field_pointer_ty.clone()),
                                is_static: false,
                            },
                            args: vec![
                                Operand::Constant(oomir::Constant::String(owner_class)),
                                Operand::Constant(oomir::Constant::String(field_name)),
                                field_offset,
                                Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(
                                        super::types::layout_size_bytes(tcx, field_rust_ty)
                                            .expect("sized DST field must have a layout"),
                                    )
                                    .expect("Rust DST field layout exceeds u64"),
                                )),
                                pointer_view_codec_operand(
                                    field_rust_ty,
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                            operand: Operand::Variable {
                                name: base_pointer_name,
                                ty: base_pointer_ty,
                            },
                        });
                        let next_var = format!("{current_var}_{}", field_index.index());
                        let value = emit_pointer_read(
                            Operand::Variable {
                                name: typed_pointer_name,
                                ty: field_pointer_ty,
                            },
                            &current_type,
                            &next_var,
                            &mut instructions,
                        );
                        current_var = value.get_name().unwrap_or(&next_var).to_string();
                    }
                    continue;
                }
                if let Some((adt_def, _substs)) = union_parts_from_ty(base_rust_ty) {
                    let owner_class_name =
                        match ty_to_oomir_type(base_rust_ty, tcx, data_types, instance) {
                            oomir::Type::Class(name) => name,
                            oomir::Type::Reference(inner)
                                if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                            {
                                if let oomir::Type::Class(name) = inner.as_ref() {
                                    name.clone()
                                } else {
                                    unreachable!()
                                }
                            }
                            other => {
                                panic!("Union field access on non-class OOMIR type: {:?}", other)
                            }
                        };
                    let field_name = union_field_name(adt_def, field_index.index(), tcx);
                    let next_var = format!("{}_{}", current_var, field_index.index());
                    let obj_type = current_type.clone();
                    current_type = ty_to_oomir_type(field_ty, tcx, data_types, instance);
                    let is_unit = !current_type.has_jvm_value();
                    instructions.push(oomir::Instruction::InvokeVirtual {
                        dest: (!is_unit).then_some(next_var.clone()),
                        class_name: owner_class_name.clone(),
                        method_name: union_getter_method_name(&field_name),
                        method_ty: oomir::Signature {
                            params: vec![(
                                "self".to_string(),
                                oomir::Type::Class(owner_class_name),
                            )],
                            ret: Box::new(current_type.clone()),
                            is_static: false,
                        },
                        args: vec![],
                        operand: Operand::Variable {
                            name: current_var.clone(),
                            ty: obj_type,
                        },
                    });
                    current_var = next_var;
                    continue;
                }

                // Get the owner class name and field name.
                let owner_class_name = match &current_type {
                    oomir::Type::Class(name) => name.clone(),
                    oomir::Type::Reference(inner)
                        if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                    {
                        if let oomir::Type::Class(name) = inner.as_ref() {
                            name.clone()
                        } else {
                            unreachable!()
                        }
                    }
                    _ => panic!(
                        "Field access on non-class type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };

                let field_name = match coroutine_variant
                    .and_then(|variant| {
                        coroutine_saved_field_name(base_rust_ty, variant, field_index.index(), tcx)
                    })
                    .map(Ok)
                    .unwrap_or_else(|| {
                        field_name_for_projection(
                            &owner_class_name,
                            field_index.index(),
                            base_rust_ty,
                            tcx,
                            data_types,
                        )
                    }) {
                    Ok(name) => name,
                    Err(e) => panic!("Error getting field name: {}", e),
                };
                coroutine_variant = None;

                // Create a temporary name for the result of this field access.
                let next_var = format!("{}_{}", current_var, field_index.index());
                let obj_type = current_type.clone();
                // Update the type to the field’s type.
                current_type = ty_to_oomir_type(field_ty, tcx, data_types, instance);
                instructions.push(oomir::Instruction::GetField {
                    dest: next_var.clone(),
                    object: Operand::Variable {
                        name: current_var.clone(),
                        ty: obj_type,
                    },
                    field_name,
                    field_ty: current_type.clone(),
                    owner_class: owner_class_name,
                });
                // Update current variable.
                current_var = next_var;
            }
            ProjectionElem::Index(index_local) => {
                let type_before_proj = current_type.clone();
                // Convert the MIR index operand.
                let index_operand = convert_operand(
                    &MirOperand::Copy(Place::from(index_local)),
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );
                // Create a temporary name for the array element.
                let next_var = format!("{}_elem", current_var);
                // Determine element type from the current type (which should be an array or reference-to-array).
                current_type = match &current_type {
                    oomir::Type::Array(inner) | oomir::Type::Slice(inner) => inner.as_ref().clone(),
                    oomir::Type::Reference(inner)
                        if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                    {
                        if let oomir::Type::Array(element_type) = inner.as_ref() {
                            element_type.as_ref().clone()
                        } else {
                            unreachable!()
                        }
                    }
                    _ => panic!(
                        "Index access on non-array type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };
                instructions.push(oomir::Instruction::ArrayGet {
                    dest: next_var.clone(),
                    array: Operand::Variable {
                        name: current_var.clone(),
                        ty: type_before_proj,
                    },
                    index: index_operand,
                });
                current_var = next_var;
            }
            ProjectionElem::ConstantIndex {
                offset,
                min_length: _,
                from_end,
            } => {
                let type_before_proj = current_type.clone();
                let next_var = format!("{}_elem", current_var);
                // Determine element type based on current_type being an array or reference-to-array.
                current_type = match &current_type {
                    oomir::Type::Array(inner) | oomir::Type::Slice(inner) => inner.as_ref().clone(),
                    oomir::Type::Reference(inner)
                        if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                    {
                        if let oomir::Type::Array(element_type) = inner.as_ref() {
                            element_type.as_ref().clone()
                        } else {
                            unreachable!()
                        }
                    }
                    _ => panic!(
                        "Constant index access on non-array type: current var '{}' has type: {:?}",
                        current_var, current_type
                    ),
                };

                if !from_end {
                    // Simple constant index: array[offset]
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: next_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                        index: Operand::Constant(oomir::Constant::I32(offset as i32)),
                    });
                } else {
                    // For access from the end: calculate length - offset.
                    let len_var = format!("{}_len", current_var);
                    instructions.push(oomir::Instruction::Length {
                        dest: len_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                    });
                    let calc_idx_var = format!("{}_calc_idx", current_var);
                    instructions.push(oomir::Instruction::Sub {
                        dest: calc_idx_var.clone(),
                        op1: Operand::Variable {
                            name: len_var,
                            ty: oomir::Type::I32,
                        },
                        op2: Operand::Constant(oomir::Constant::I32(offset as i32)),
                    });
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: next_var.clone(),
                        array: Operand::Variable {
                            name: current_var.clone(),
                            ty: type_before_proj.clone(),
                        },
                        index: Operand::Variable {
                            name: calc_idx_var,
                            ty: oomir::Type::I32,
                        },
                    });
                }
                current_var = next_var;
            }
            ProjectionElem::Subslice { from, to, from_end } => {
                let next_var = format!("{}_slice_{}", current_var, proj_index);
                current_type = emit_slice_view(
                    Operand::Variable {
                        name: current_var,
                        ty: type_before_proj.clone(),
                    },
                    &type_before_proj,
                    from,
                    to,
                    from_end,
                    &next_var,
                    &mut instructions,
                );
                current_var = next_var;
            }
            ProjectionElem::Deref => {
                let base_place_for_deref = projection_prefix_place(place, proj_index, tcx);
                let base_rust_ty =
                    EarlyBinder::bind(tcx, base_place_for_deref.ty(&mir.local_decls, tcx).ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                let pointer_pointee = match base_rust_ty.kind() {
                    TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => Some(*pointee),
                    _ => None,
                };
                let preserve_slice_tailed_pointer = pointer_pointee
                    .is_some_and(|pointee| has_slice_or_str_struct_tail(tcx, pointee));
                if preserve_slice_tailed_pointer {
                    continue;
                }
                match type_before_proj {
                    oomir::Type::Pointer(element_type) => {
                        let next_var = format!("{}_deref", current_var);
                        let result = emit_pointer_read(
                            Operand::Variable {
                                name: current_var.clone(),
                                ty: oomir::Type::Pointer(element_type.clone()),
                            },
                            element_type.as_ref(),
                            &next_var,
                            &mut instructions,
                        );
                        current_var = result.get_name().unwrap_or(&next_var).to_string();
                        current_type = element_type.as_ref().clone();
                    }
                    oomir::Type::MutableReference(_) => {
                        let type_before_deref = current_type.clone();

                        match type_before_deref.clone() {
                            oomir::Type::MutableReference(element_type) => {
                                // Create a temporary variable name for the dereferenced value
                                let next_var = format!("{}_deref", current_var);

                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Info,
                                    "place-lowering",
                                    format!(
                                        "Info: Handling Deref: Var '{}' ({:?}) -> Temp Var '{}' (Type: {:?})",
                                        current_var,
                                        type_before_deref,
                                        next_var,
                                        element_type.as_ref()
                                    )
                                );

                                instructions.push(oomir::Instruction::ArrayGet {
                                    dest: next_var.clone(),
                                    array: Operand::Variable {
                                        name: current_var.clone(),
                                        ty: type_before_deref, // The type is Array(T)
                                    },
                                    // Index is always 0 for our reference representation
                                    index: Operand::Constant(oomir::Constant::I32(0)),
                                });

                                // Update current_var and current_type for subsequent projections
                                current_var = next_var;
                                current_type = element_type.as_ref().clone(); // Type becomes T
                            }
                            _ => {
                                panic!(
                                    "Attempted to Deref a non-reference (non-array) type: \
                             Variable '{}' has type {:?}. Place: {:?}",
                                    current_var,
                                    current_type, // Use the original current_type for the error
                                    place
                                );
                            }
                        }
                    }
                    _ => {
                        // no op
                    }
                }
            }
            ProjectionElem::Downcast(_, variant_idx) => {
                // A downcast changes the *effective type* for subsequent projections.

                // 1. Get the AdtDef of the enum to find the variant's actual name.
                //    We need the Rust Ty of the base enum *before* the downcast.
                let base_place_proj_slice = &place.projection[..proj_index]; // Projections *before* this downcast
                let base_place_for_downcast = Place {
                    local: place.local,
                    projection: tcx.mk_place_elems(base_place_proj_slice),
                };
                let base_rust_ty = base_place_for_downcast.ty(&mir.local_decls, tcx).ty;
                let base_rust_ty = EarlyBinder::bind(tcx, base_rust_ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();

                let coroutine_ty = match base_rust_ty.kind() {
                    TyKind::Coroutine(..) => Some(base_rust_ty),
                    TyKind::Ref(_, inner, _) if matches!(inner.kind(), TyKind::Coroutine(..)) => {
                        Some(*inner)
                    }
                    _ => None,
                };
                if coroutine_ty.is_some() {
                    // Coroutine variants occupy one mutable state-machine
                    // object. The downcast only selects how the following
                    // field index is interpreted; no JVM cast is required.
                    coroutine_variant = Some(variant_idx);
                    continue;
                }

                let (adt_def, substs) = match base_rust_ty.kind() {
                    TyKind::Adt(adt, s) => (*adt, s),
                    TyKind::Ref(_, ty, _) => match ty.kind() {
                        // Handle reference to enum
                        TyKind::Adt(adt, s) => (*adt, s),
                        _ => panic!(
                            "Downcast base is Ref to non-ADT: {:?} ({:?})",
                            base_rust_ty, place
                        ),
                    },
                    _ => panic!(
                        "Downcast base is not an ADT: {:?} ({:?})",
                        base_rust_ty, place
                    ),
                };

                if !adt_def.is_enum() {
                    panic!(
                        "Downcast applied to non-enum ADT: {:?} ({:?})",
                        adt_def, place
                    );
                }

                // Derive the base from the canonical Rust ADT rather than the
                // current lowered carrier. The latter may already be refined
                // to a variant by an earlier projection, which would otherwise
                // manufacture invalid names such as `$Break$Break`.
                let base_enum_oomir_name =
                    generate_adt_jvm_class_name(&adt_def, substs, tcx, data_types, instance);

                // 2. Get the specific variant definition using the index.
                let variant_def = adt_def.variant(variant_idx);

                // 3. Construct the OOMIR variant class name
                let variant_class_name = format!(
                    "{}${}",
                    base_enum_oomir_name, // Use OOMIR name already derived
                    jvm_names::member_name(&variant_def.name.to_string())
                );

                // 4. Update current_type directly to the OOMIR Class representing the variant.
                current_type = oomir::Type::Class(variant_class_name.clone());

                // Verify this class exists in data_types
                if should_define_named_data_type(tcx, adt_def.did())
                    && !data_types.contains_key(&variant_class_name)
                {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "place-lowering",
                        format!(
                            "Info: Downcast resulted in variant class name '{}' which is not yet in data_types! Will insert it.",
                            variant_class_name
                        )
                    );
                    let mut fields = vec![];
                    for field in variant_def.fields.iter() {
                        let field_type = ty_to_oomir_type(
                            field.ty(tcx, substs).skip_norm_wip(),
                            tcx,
                            data_types,
                            instance,
                        );
                        if field_type.has_jvm_value() {
                            let field_name = format!("field{}", fields.len());
                            fields.push((field_name, field_type));
                        }
                    }

                    let mut methods = HashMap::new();
                    methods.insert(
                        "getVariantIdx".to_string(),
                        DataTypeMethod::SimpleConstantReturn(
                            oomir::Type::I32,
                            Some(oomir::Constant::I32(variant_idx.as_u32() as i32)),
                        ),
                    );

                    data_types.insert(
                        variant_class_name.clone(),
                        oomir::DataType::Class {
                            fields,
                            is_abstract: false,
                            methods,
                            super_class: Some(base_enum_oomir_name.clone()),
                            interfaces: vec![],
                        },
                    );
                }

                // insert a Cast instruction to convert the base enum to the variant class
                instructions.push(oomir::Instruction::Cast {
                    dest: current_var.clone(),
                    op: Operand::Variable {
                        name: current_var.clone(),
                        ty: type_before_proj,
                    },
                    ty: oomir::Type::Class(variant_class_name),
                });

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "place-lowering",
                    format!(
                        "Info: Handled Downcast: Variant {}({}), BaseEnum='{}', New Type (Variant Class): {:?}, Var: {}",
                        variant_def.name,
                        variant_idx.index(),
                        base_enum_oomir_name,
                        current_type,
                        current_var
                    )
                );
            }
            // Will add more projection kinds when needed.
            _ => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "place-lowering",
                    format!(
                        "Warning: Unhandled projection element in nested access: {:?}. Skipping.",
                        proj
                    )
                );
            }
        }
    }

    (current_var, instructions, current_type)
}

/// Helper to get the OOMIR type for a Place.
pub fn get_place_type<'tcx>(
    place: &Place<'tcx>,
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Type {
    if let Some(ProjectionElem::Downcast(_, variant_idx)) = place.projection.last() {
        let prefix = &place.projection[..place.projection.len() - 1];
        let base_place = Place {
            local: place.local,
            projection: tcx.mk_place_elems(prefix),
        };
        let base_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        if matches!(base_ty.kind(), TyKind::Coroutine(..)) {
            return ty_to_oomir_type(base_ty, tcx, data_types, instance);
        }
        let (adt_def, substs) = match base_ty.kind() {
            TyKind::Adt(adt_def, substs) => (*adt_def, *substs),
            TyKind::Ref(_, inner, _) => match inner.kind() {
                TyKind::Adt(adt_def, substs) => (*adt_def, *substs),
                _ => return ty_to_oomir_type(base_ty, tcx, data_types, instance),
            },
            _ => return ty_to_oomir_type(base_ty, tcx, data_types, instance),
        };
        if adt_def.is_enum() {
            let base_class =
                generate_adt_jvm_class_name(&adt_def, substs, tcx, data_types, instance);
            let variant = adt_def.variant(*variant_idx);
            return oomir::Type::Class(format!(
                "{}${}",
                base_class,
                jvm_names::member_name(&variant.name.to_string())
            ));
        }
    }

    let place_ty = place.ty(&mir.local_decls, tcx);
    // Instantiate the type with the instance's generic arguments to get concrete types
    let instantiated_ty = rustc_middle::ty::EarlyBinder::bind(tcx, place_ty.ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    ty_to_oomir_type(instantiated_ty, tcx, data_types, instance)
}

/// Generates OOMIR instructions to "get" the value from a Place.
/// This function now supports nested projections by calling
/// `emit_instructions_to_get_recursive`.
pub fn emit_instructions_to_get_on_own<'tcx>(
    place: &Place<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> (String, Vec<Instruction>, oomir::Type) {
    // Delegate the recursive handling.
    emit_instructions_to_get_recursive(place, tcx, instance, mir, data_types)
}

/// Generates OOMIR instructions to store the `source_operand` value into the `dest_place`.
///
/// This function handles assignments recursively. It first generates instructions
/// to get the object or array that contains the final field/element, and then
/// generates the appropriate SetField or ArrayStore instruction.
pub fn emit_instructions_to_set_value<'tcx>(
    dest_place: &Place<'tcx>,
    source_operand: Operand, // The OOMIR value to store
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    let target_rust_ty = dest_place.ty(&mir.local_decls, tcx).ty;
    let source_operand = super::value_repr::adapt_operand_to_rust_type(
        source_operand,
        target_rust_ty,
        &format!("{}_assignment", place_to_string(dest_place, tcx)),
        tcx,
        instance,
        data_types,
        &mut instructions,
    );

    if dest_place.projection.is_empty() {
        if local_uses_stable_cell(dest_place.local, mir) {
            let pointee_type = get_place_type(dest_place, mir, tcx, instance, data_types);
            emit_pointer_write(
                Operand::Variable {
                    name: local_cell_name(dest_place.local),
                    ty: oomir::Type::Pointer(Box::new(pointee_type.clone())),
                },
                &pointee_type,
                source_operand,
                &mut instructions,
            );
        } else {
            // e.g., _1 = source_operand
            let dest_var_name = place_to_string(dest_place, tcx);
            instructions.push(Instruction::Move {
                dest: dest_var_name,
                src: source_operand,
            });
        }
    } else {
        // 1. Separate the destination into the base and the last projection element.
        let (last_projection, base_projection_elems) = dest_place.projection.split_last().unwrap(); // Safe because we checked is_empty()
        let base_place = Place {
            local: dest_place.local,
            projection: tcx.mk_place_elems(base_projection_elems),
        };

        // 2. Generate instructions to get the value of the *base* place.
        //    This base value is the object we'll call SetField on, or the array
        //    we'll call ArrayStore on.
        //    We use `get_on_own` which internally handles recursion if base_place itself is nested.
        let (base_var_name, get_base_instructions, base_oomir_type) =
            emit_instructions_to_get_on_own(&base_place, tcx, instance, mir, data_types);
        let union_writebacks = collect_union_writebacks(
            &base_place,
            &get_base_instructions,
            tcx,
            instance,
            mir,
            data_types,
        );
        let memory_view_writebacks = collect_memory_view_writebacks(&get_base_instructions);
        instructions.extend(get_base_instructions); // Add instructions to get the base

        // 3. Generate the final store instruction based on the *last* projection.
        match last_projection {
            ProjectionElem::Field(field_index, field_mir_ty) => {
                let base_rust_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                if matches!(base_oomir_type, oomir::Type::Pointer(_))
                    && has_slice_or_str_struct_tail(tcx, base_rust_ty)
                {
                    let layout = tcx
                        .layout_of(TypingEnv::fully_monomorphized().as_query_input(base_rust_ty))
                        .unwrap_or_else(|error| {
                            panic!(
                                "could not determine slice-tailed struct layout for field assignment to {base_rust_ty:?}: {error:?}"
                            )
                        });
                    let field_offset = layout.fields.offset(field_index.index()).bytes_usize();
                    let field_rust_ty = EarlyBinder::bind(tcx, *field_mir_ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let field_oomir_ty = ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                    let field_pointer_ty = oomir::Type::Pointer(Box::new(field_oomir_ty.clone()));
                    let oomir::Type::Pointer(base_pointee_ty) = &base_oomir_type else {
                        unreachable!();
                    };
                    let oomir::Type::Class(owner_class) = base_pointee_ty.as_ref() else {
                        panic!("slice-tailed Rust struct did not map to a JVM class");
                    };
                    let owner_class = owner_class.clone();
                    let field_name = field_name_for_projection(
                        &owner_class,
                        field_index.index(),
                        base_rust_ty,
                        tcx,
                        data_types,
                    )
                    .unwrap_or_else(|error| panic!("Error getting DST field name: {error}"));
                    let typed_pointer_name = format!("{base_var_name}_typed_field_pointer");
                    instructions.push(Instruction::InvokeVirtual {
                        dest: Some(typed_pointer_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "projectStructField".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("self".to_string(), base_oomir_type.clone()),
                                ("owner_class".to_string(), oomir::Type::java_string()),
                                ("field_name".to_string(), oomir::Type::java_string()),
                                ("field_offset".to_string(), oomir::Type::U64),
                                ("field_size".to_string(), oomir::Type::U64),
                                ("field_codec".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(field_pointer_ty.clone()),
                            is_static: false,
                        },
                        args: vec![
                            Operand::Constant(oomir::Constant::String(owner_class)),
                            Operand::Constant(oomir::Constant::String(field_name)),
                            Operand::Constant(oomir::Constant::U64(
                                u64::try_from(field_offset)
                                    .expect("Rust DST field offset exceeds u64"),
                            )),
                            Operand::Constant(oomir::Constant::U64(
                                u64::try_from(
                                    super::types::layout_size_bytes(tcx, field_rust_ty)
                                        .expect("sized DST field must have a layout"),
                                )
                                .expect("Rust DST field layout exceeds u64"),
                            )),
                            pointer_view_codec_operand(field_rust_ty, tcx, data_types, instance),
                        ],
                        operand: Operand::Variable {
                            name: base_var_name,
                            ty: base_oomir_type,
                        },
                    });
                    emit_pointer_write(
                        Operand::Variable {
                            name: typed_pointer_name,
                            ty: field_pointer_ty,
                        },
                        &field_oomir_ty,
                        source_operand,
                        &mut instructions,
                    );
                    emit_union_writebacks(&union_writebacks, &mut instructions);
                    emit_memory_view_writebacks(&memory_view_writebacks, &mut instructions);
                    return instructions;
                }
                if let Some((adt_def, _substs)) = union_parts_from_ty(base_rust_ty) {
                    let owner_class_name =
                        match ty_to_oomir_type(base_rust_ty, tcx, data_types, instance) {
                            oomir::Type::Class(name) => name,
                            oomir::Type::Reference(inner)
                                if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                            {
                                if let oomir::Type::Class(name) = inner.as_ref() {
                                    name.clone()
                                } else {
                                    unreachable!()
                                }
                            }
                            other => panic!(
                                "Union field assignment on non-class OOMIR type: {:?}",
                                other
                            ),
                        };
                    let field_name = union_field_name(adt_def, field_index.index(), tcx);
                    let field_ty = ty_to_oomir_type(*field_mir_ty, tcx, data_types, instance);
                    let is_unit = !field_ty.has_jvm_value();
                    let source_operand = adapt_simple_enum_operand(
                        source_operand,
                        &field_ty,
                        &format!("{}_{}_union", base_var_name, field_name),
                        data_types,
                        &mut instructions,
                    );
                    instructions.push(Instruction::InvokeVirtual {
                        dest: None,
                        class_name: owner_class_name.clone(),
                        method_name: union_setter_method_name(&field_name),
                        method_ty: oomir::Signature {
                            params: vec![(
                                "self".to_string(),
                                oomir::Type::Class(owner_class_name),
                            )]
                            .into_iter()
                            .chain((!is_unit).then_some(("value".to_string(), field_ty)))
                            .collect(),
                            ret: Box::new(oomir::Type::Void),
                            is_static: false,
                        },
                        args: if is_unit {
                            vec![]
                        } else {
                            vec![source_operand]
                        },
                        operand: Operand::Variable {
                            name: base_var_name,
                            ty: base_oomir_type,
                        },
                    });
                    emit_union_writebacks(&union_writebacks, &mut instructions);
                    emit_memory_view_writebacks(&memory_view_writebacks, &mut instructions);
                    return instructions;
                }

                // Target is a field: base_var_name.field = source_operand
                let owner_class_name = match &base_oomir_type {
                    oomir::Type::Class(name) => name.clone(),
                    oomir::Type::Reference(inner)
                        if matches!(inner.as_ref(), oomir::Type::Class(_)) =>
                    {
                        if let oomir::Type::Class(name) = inner.as_ref() {
                            name.clone()
                        } else {
                            unreachable!()
                        }
                    }
                    _ => panic!(
                        "SetField target base '{}' (Place: {:?}) is not a class or reference-to-class type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                };

                let coroutine_variant =
                    base_place
                        .projection
                        .iter()
                        .rev()
                        .find_map(|projection| match projection {
                            ProjectionElem::Downcast(_, variant) => Some(variant),
                            _ => None,
                        });
                let field_name = match coroutine_variant
                    .and_then(|variant| {
                        coroutine_saved_field_name(base_rust_ty, variant, field_index.index(), tcx)
                    })
                    .map(Ok)
                    .unwrap_or_else(|| {
                        field_name_for_projection(
                            &owner_class_name,
                            field_index.index(),
                            base_rust_ty,
                            tcx,
                            data_types,
                        )
                    }) {
                    Ok(name) => name,
                    Err(e) => panic!("Error getting field name for SetField: {}", e),
                };
                let field_ty = ty_to_oomir_type(*field_mir_ty, tcx, data_types, instance);

                instructions.push(Instruction::SetField {
                    object: base_var_name.clone(), // The object/struct retrieved in step 2
                    field_name,
                    field_ty,
                    value: source_operand, // The value we want to store
                    owner_class: owner_class_name,
                });
            }

            ProjectionElem::Index(index_local) => {
                // Target is an array element: base_var_name[index] = source_operand
                // Ensure the base is actually an array or ref-to-array
                match &base_oomir_type {
                    oomir::Type::Array(_) | oomir::Type::Slice(_) => {}
                    oomir::Type::Reference(t) if matches!(t.as_ref(), oomir::Type::Array(_)) => {}
                    _ => panic!(
                        "ArrayStore target base '{}' (Place: {:?}) is not an array or reference-to-array type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                }

                // Convert the MIR index operand (_local) to an OOMIR operand
                let mir_index_operand = MirOperand::Copy(Place::from(*index_local)); // Or Move? Copy usually safer.
                let oomir_index_operand = convert_operand(
                    &mir_index_operand,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );

                instructions.push(Instruction::ArrayStore {
                    array: base_var_name.clone(),
                    index: oomir_index_operand, // The index operand
                    value: source_operand,      // The value to store
                    copy_value: false,
                });
            }

            ProjectionElem::ConstantIndex {
                offset,
                min_length: _,
                from_end,
            } => {
                // Target is array element with constant index: base_var_name[const_idx] = source_operand
                // Ensure the base is actually an array or ref-to-array
                match &base_oomir_type {
                    oomir::Type::Array(_) | oomir::Type::Slice(_) => {}
                    oomir::Type::Reference(t) if matches!(t.as_ref(), oomir::Type::Array(_)) => {}
                    _ => panic!(
                        "ArrayStore target base '{}' (Place: {:?}) is not an array or reference-to-array type: {:?}",
                        base_var_name, base_place, base_oomir_type
                    ),
                }

                let index_operand: Operand;

                if !from_end {
                    // Simple constant index from the start
                    index_operand = Operand::Constant(oomir::Constant::I32(*offset as i32));
                    // No extra instructions needed for the index itself
                } else {
                    // Index is calculated as length - offset
                    // We need to insert Length and Sub *before* the ArrayStore

                    // Temp name for length result (avoid collision)
                    let len_var_name = format!("{}_len_set", base_var_name);
                    instructions.push(Instruction::Length {
                        dest: len_var_name.clone(),
                        array: Operand::Variable {
                            name: base_var_name.clone(),
                            ty: base_oomir_type.clone(),
                        },
                    });

                    // Temp name for calculated index (avoid collision)
                    let index_var_name = format!("{}_calc_idx_set", base_var_name);
                    let offset_op = Operand::Constant(oomir::Constant::I32(*offset as i32));
                    instructions.push(Instruction::Sub {
                        dest: index_var_name.clone(),
                        op1: Operand::Variable {
                            name: len_var_name,
                            ty: oomir::Type::I32,
                        },
                        op2: offset_op,
                    });

                    // Use the calculated index variable
                    index_operand = Operand::Variable {
                        name: index_var_name,
                        ty: oomir::Type::I32,
                    };
                }

                instructions.push(Instruction::ArrayStore {
                    array: base_var_name.clone(), // The array retrieved in step 2
                    index: index_operand,         // The constant or calculated index
                    value: source_operand,        // The value to store
                    copy_value: false,
                });
            }

            ProjectionElem::Deref => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "place-lowering",
                    format!(
                        "Info: Handling Set via Deref: Target Base Var '{}' ({:?}), Source: {:?}",
                        base_var_name, base_oomir_type, source_operand
                    )
                );

                match &base_oomir_type {
                    oomir::Type::Pointer(element_type) => {
                        emit_pointer_write(
                            Operand::Variable {
                                name: base_var_name,
                                ty: base_oomir_type.clone(),
                            },
                            element_type,
                            source_operand,
                            &mut instructions,
                        );
                    }
                    oomir::Type::MutableReference(_element_type) => {
                        instructions.push(Instruction::ArrayStore {
                            array: base_var_name.clone(), // The variable holding the array reference
                            // Index is always 0 for our reference representation
                            index: Operand::Constant(oomir::Constant::I32(0)),
                            value: source_operand, // The value being assigned
                            copy_value: false,
                        });
                    }
                    _ => {
                        // no-op - non-mutable reference
                    }
                }
            }
            ProjectionElem::Downcast(..) => {
                // Downcast should not be the last element for an assignment.
                // You assign to a field/index within the downcast variant.
                panic!(
                    "Downcast cannot be the final projection element for an assignment. Place: {:?}",
                    dest_place
                );
            }
            _ => {
                panic!(
                    "Unsupported projection element type {:?} found at the end of destination Place during assignment: {:?}",
                    last_projection, dest_place
                );
            }
        }
        emit_union_writebacks(&union_writebacks, &mut instructions);
        emit_memory_view_writebacks(&memory_view_writebacks, &mut instructions);
    }

    instructions
}
