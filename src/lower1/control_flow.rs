use super::{
    jvm_names,
    operand::convert_operand,
    place::{
        emit_instructions_to_get_on_own, emit_instructions_to_set_value, emit_pointer_read,
        place_to_string,
    },
    types::mir_int_to_oomir_const,
};
use crate::oomir;

use rustc_middle::{
    mir::{
        BasicBlock, BasicBlockData, Body, Local, NonDivergingIntrinsic, Operand as MirOperand,
        Place, SourceInfo, StatementKind, TerminatorKind,
    },
    ty::{EarlyBinder, Instance, InstanceKind, ShimKind, Ty, TyCtxt, TyKind, TypingEnv},
};
use rustc_span::{Symbol, sym};
use std::collections::{HashMap, HashSet};

mod checked_intrinsic_registry;
pub mod checked_intrinsics;
mod checked_ops;
pub(crate) mod rvalue;
mod trait_objects;

pub use checked_intrinsic_registry::take_needed_intrinsics;

fn caller_location_operand<'tcx>(
    source_info: SourceInfo,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_prefix: &str,
) -> oomir::Operand {
    let location_ty = tcx.caller_location_ty();
    let location_oomir_ty = super::types::ty_to_oomir_type(location_ty, tcx, data_types, instance);
    let inherited_location =
        instance
            .def
            .requires_caller_location(tcx)
            .then(|| oomir::Operand::Variable {
                name: oomir::CALLER_LOCATION_PARAM_NAME.to_string(),
                ty: location_oomir_ty,
            });

    mir.caller_location_span(source_info, inherited_location, tcx, |span| {
        let raw_location = super::operand::handle_const_value(
            None,
            tcx.span_as_caller_location(span),
            &location_ty,
            tcx,
            data_types,
            instance,
        );
        super::value_repr::adapt_operand_to_rust_type(
            raw_location,
            location_ty,
            temp_prefix,
            tcx,
            instance,
            data_types,
            instructions,
        )
    })
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct PointerOrigin<'tcx> {
    pub original_place: Place<'tcx>,
    pub carrier_name: String,
    pub pointee_type: oomir::Type,
    pub writable: bool,
}

pub(super) type MutableBorrowMap<'tcx> = HashMap<Local, PointerOrigin<'tcx>>;

fn emit_trait_object_metadata(
    pointer: oomir::Operand,
    output_type: &oomir::Type,
    dest: String,
    temp_prefix: &str,
    data_types: &HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    let metadata_class = output_type
        .get_class_name()
        .expect("trait-object metadata must be represented by a JVM class")
        .to_string();
    let metadata_fields = match data_types.get(&metadata_class) {
        Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
        other => panic!("trait-object metadata class is unavailable: {other:?}"),
    };
    let [(_, vtable_type), (_, phantom_type)] = metadata_fields.as_slice() else {
        panic!("trait-object metadata must contain vtable and phantom fields")
    };
    let vtable_class = vtable_type
        .get_class_name()
        .expect("trait-object vtable pointer must be represented by a JVM class")
        .to_string();
    let vtable_fields = match data_types.get(&vtable_class) {
        Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
        other => panic!("trait-object vtable pointer class is unavailable: {other:?}"),
    };
    let [(_, marker_type)] = vtable_fields.as_slice() else {
        panic!("trait-object vtable pointer must contain exactly one pointer field")
    };
    let phantom_class = phantom_type
        .get_class_name()
        .expect("trait-object metadata phantom field must be a JVM class")
        .to_string();

    let marker_name = format!("{temp_prefix}_vtable_marker");
    let pointer_type = pointer
        .get_type()
        .expect("trait-object metadata source must be typed");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(marker_name.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "traitMetadataMarker".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), pointer_type),
                ("metadata_class".to_string(), oomir::Type::String),
            ],
            ret: Box::new(marker_type.clone()),
            is_static: true,
        },
        args: vec![
            pointer,
            oomir::Operand::Constant(oomir::Constant::String(metadata_class.clone())),
        ],
    });

    let vtable_name = format!("{temp_prefix}_vtable");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: vtable_name.clone(),
        class_name: vtable_class,
        args: vec![(
            oomir::Operand::Variable {
                name: marker_name,
                ty: marker_type.clone(),
            },
            marker_type.clone(),
        )],
    });
    let phantom_name = format!("{temp_prefix}_phantom");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: phantom_name.clone(),
        class_name: phantom_class,
        args: Vec::new(),
    });
    instructions.push(oomir::Instruction::ConstructObject {
        dest,
        class_name: metadata_class,
        args: vec![
            (
                oomir::Operand::Variable {
                    name: vtable_name,
                    ty: vtable_type.clone(),
                },
                vtable_type.clone(),
            ),
            (
                oomir::Operand::Variable {
                    name: phantom_name,
                    ty: phantom_type.clone(),
                },
                phantom_type.clone(),
            ),
        ],
    });
}

pub(super) fn emit_rust_drop_value<'tcx>(
    rust_ty: Ty<'tcx>,
    mut value: oomir::Operand,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    let rust_ty = EarlyBinder::bind(tcx, rust_ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let oomir_ty = super::types::ty_to_oomir_type(rust_ty, tcx, data_types, instance);
    if !oomir_ty.has_jvm_value() {
        let Some(materialized) = super::value_repr::materialize_implicit_zst(
            rust_ty,
            &format!("{temp_prefix}_zst"),
            tcx,
            instance,
            data_types,
            instructions,
        ) else {
            return;
        };
        value = materialized;
    }

    match rust_ty.kind() {
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            if !adt_def.did().is_local() {
                // Standard-library/JVM-backed containers are represented by
                // managed carriers (for example Rust String -> JVM String).
                // Their native Rust field layout is not the JVM object layout.
                return;
            }
            if adt_def.destructor(tcx).is_some() {
                let class_name = oomir_ty
                    .get_class_name()
                    .expect("a Rust Drop ADT has a JVM class")
                    .to_string();
                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name,
                    method_name: "drop".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![value.clone()],
                    dest: None,
                });
            }

            for (field_index, field) in adt_def
                .variant(rustc_abi::VariantIdx::from_usize(0))
                .fields
                .iter()
                .enumerate()
            {
                let field_rust_ty = field.ty(tcx, substs).skip_norm_wip();
                if !field_rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let field_oomir_ty =
                    super::types::ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                let field_value = if field_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_field_{field_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: field.ident(tcx).to_string(),
                        field_ty: field_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a struct field owner has a JVM class")
                            .to_string(),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: field_oomir_ty,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    field_rust_ty,
                    field_value,
                    &format!("{temp_prefix}_field_{field_index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Tuple(fields) => {
            for (field_index, field_rust_ty) in fields.iter().enumerate() {
                if !field_rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let field_oomir_ty =
                    super::types::ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                let field_value = if field_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_tuple_{field_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: format!("field{field_index}"),
                        field_ty: field_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a non-empty tuple has a JVM class")
                            .to_string(),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: field_oomir_ty,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    field_rust_ty,
                    field_value,
                    &format!("{temp_prefix}_tuple_{field_index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Array(element_ty, length) => {
            let Some(length) = length.try_to_target_usize(tcx) else {
                return;
            };
            if !element_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                return;
            }
            let element_oomir_ty =
                super::types::ty_to_oomir_type(*element_ty, tcx, data_types, instance);
            for index in 0..length {
                let element_value = if element_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_array_{index}");
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: dest.clone(),
                        array: value.clone(),
                        index: oomir::Operand::Constant(oomir::Constant::I32(index as i32)),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: element_oomir_ty.clone(),
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    *element_ty,
                    element_value,
                    &format!("{temp_prefix}_array_{index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        // Rust unions require ManuallyDrop fields. Enums use a virtual helper
        // generated on every variant so the active payload is destroyed without
        // trying to reinterpret the JVM subclass as an inactive variant.
        TyKind::Adt(adt_def, _) if adt_def.is_enum() => {
            let class_name = oomir_ty
                .get_class_name()
                .expect("a Rust enum has a JVM class")
                .to_string();
            if adt_def.destructor(tcx).is_some() {
                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name: class_name.clone(),
                    method_name: "drop".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![value.clone()],
                    dest: None,
                });
            }
            if super::types::should_define_named_data_type(tcx, adt_def.did()) {
                instructions.push(oomir::Instruction::InvokeVirtual {
                    class_name,
                    method_name: "__rust_drop_fields".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty)],
                        ret: Box::new(oomir::Type::Void),
                        is_static: false,
                    },
                    args: Vec::new(),
                    dest: None,
                    operand: value,
                });
            }
        }
        _ => {}
    }
}

pub(super) fn collect_pointer_origins<'tcx>(
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> MutableBorrowMap<'tcx> {
    let mut origins = MutableBorrowMap::new();
    let assignment_count = mir
        .basic_blocks
        .iter()
        .map(|block| block.statements.len())
        .sum::<usize>();

    for _ in 0..=assignment_count {
        let mut changed = false;
        for block in mir.basic_blocks.iter() {
            for statement in &block.statements {
                let StatementKind::Assign(box (destination, rvalue)) = &statement.kind else {
                    continue;
                };
                if !destination.projection.is_empty() {
                    continue;
                }
                let destination_ty =
                    super::place::get_place_type(destination, mir, tcx, instance, data_types);
                let oomir::Type::Pointer(pointee_ty) = destination_ty else {
                    continue;
                };

                let inherit_deref_origin = |place: &Place<'tcx>| {
                    place
                        .projection
                        .last()
                        .filter(|projection| {
                            matches!(projection, rustc_middle::mir::ProjectionElem::Deref)
                        })
                        .and_then(|_| origins.get(&place.local))
                        .map(|origin| origin.original_place.clone())
                        .unwrap_or_else(|| place.clone())
                };
                let origin = match rvalue {
                    rustc_middle::mir::Rvalue::Ref(_, borrow_kind, place) => Some((
                        inherit_deref_origin(place),
                        pointee_ty.as_ref().clone(),
                        matches!(borrow_kind, rustc_middle::mir::BorrowKind::Mut { .. }),
                    )),
                    rustc_middle::mir::Rvalue::RawPtr(pointer_kind, place) => Some((
                        inherit_deref_origin(place),
                        pointee_ty.as_ref().clone(),
                        matches!(pointer_kind, rustc_middle::mir::RawPtrKind::Mut),
                    )),
                    rustc_middle::mir::Rvalue::Use(
                        MirOperand::Copy(place) | MirOperand::Move(place),
                        _,
                    ) => origins.get(&place.local).map(|entry| {
                        (
                            entry.original_place.clone(),
                            entry.pointee_type.clone(),
                            entry.writable,
                        )
                    }),
                    rustc_middle::mir::Rvalue::Cast(
                        _,
                        MirOperand::Copy(place) | MirOperand::Move(place),
                        _,
                    ) => origins.get(&place.local).map(|entry| {
                        (
                            entry.original_place.clone(),
                            entry.pointee_type.clone(),
                            entry.writable,
                        )
                    }),
                    _ => None,
                };
                let Some((origin, storage_ty, writable)) = origin else {
                    continue;
                };
                let entry = PointerOrigin {
                    original_place: origin,
                    carrier_name: super::place::place_to_string(destination, tcx),
                    pointee_type: storage_ty,
                    writable,
                };
                if origins.get(&destination.local) != Some(&entry) {
                    origins.insert(destination.local, entry);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    origins
}

fn emit_mutable_borrow_writeback<'tcx>(
    borrow_local: Local,
    mutable_borrows: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    visited: &mut HashSet<Local>,
) -> Vec<oomir::Instruction> {
    if !visited.insert(borrow_local) {
        return Vec::new();
    }
    let mut instructions = Vec::new();
    let Some(origin) = mutable_borrows.get(&borrow_local).cloned() else {
        return instructions;
    };
    if !origin.writable {
        return instructions;
    }
    let original_place = origin.original_place;
    let carrier_name = origin.carrier_name;
    let element_ty = origin.pointee_type;
    if super::place::local_uses_stable_cell(original_place.local, mir) {
        // The pointer and all projected field views already share the root
        // allocation. Copying a derived/address-only view back would both be
        // redundant and could overwrite the field with the enclosing object.
        return instructions;
    }
    if !element_ty.has_jvm_value() {
        return instructions;
    }
    let value_name = format!(
        "_writeback_{}_{}",
        borrow_local.index(),
        original_place.local.index()
    );
    let carrier = mutable_borrows
        .get(&borrow_local)
        .map(|origin| oomir::Type::Pointer(Box::new(origin.pointee_type.clone())))
        .unwrap_or_else(|| oomir::Type::Pointer(Box::new(element_ty.clone())));
    emit_pointer_read(
        oomir::Operand::Variable {
            name: carrier_name,
            ty: carrier,
        },
        &element_ty,
        &value_name,
        &mut instructions,
    );
    instructions.extend(emit_instructions_to_set_value(
        &original_place,
        oomir::Operand::Variable {
            name: value_name,
            ty: element_ty,
        },
        tcx,
        instance,
        mir,
        data_types,
    ));
    if mutable_borrows.contains_key(&original_place.local) {
        instructions.extend(emit_mutable_borrow_writeback(
            original_place.local,
            mutable_borrows,
            tcx,
            instance,
            mir,
            data_types,
            visited,
        ));
    }
    instructions
}

fn emit_pointer_origin_refreshes<'tcx>(
    updated_place: &Place<'tcx>,
    available_pointer_locals: &HashSet<Local>,
    pointer_origins: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> Vec<oomir::Instruction> {
    let matching_origins = available_pointer_locals
        .iter()
        .filter_map(|local| pointer_origins.get(local))
        .filter(|origin| origin.original_place == *updated_place)
        .filter(|origin| !super::place::local_uses_stable_cell(origin.original_place.local, mir))
        .cloned()
        .collect::<Vec<_>>();
    if matching_origins.is_empty() {
        return Vec::new();
    }

    let (value_name, mut instructions, value_type) =
        emit_instructions_to_get_on_own(updated_place, tcx, instance, mir, data_types);
    for origin in matching_origins {
        super::place::emit_pointer_write(
            oomir::Operand::Variable {
                name: origin.carrier_name,
                ty: oomir::Type::Pointer(Box::new(origin.pointee_type.clone())),
            },
            &origin.pointee_type,
            oomir::Operand::Variable {
                name: value_name.clone(),
                ty: value_type.clone(),
            },
            &mut instructions,
        );
    }
    instructions
}

fn emit_selected_mutable_borrow_writebacks<'tcx>(
    borrow_locals: impl IntoIterator<Item = Local>,
    mutable_borrows: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> Vec<oomir::Instruction> {
    let mut instructions = Vec::new();
    let mut visited = HashSet::new();
    for borrow_local in borrow_locals {
        instructions.extend(emit_mutable_borrow_writeback(
            borrow_local,
            mutable_borrows,
            tcx,
            instance,
            mir,
            data_types,
            &mut visited,
        ));
    }
    instructions
}

fn requires_compiled_static_dispatch(ty: &oomir::Type) -> bool {
    if let oomir::Type::MutableReference(inner)
    | oomir::Type::Reference(inner)
    | oomir::Type::Pointer(inner) = ty
    {
        return requires_compiled_static_dispatch(inner);
    }
    matches!(ty, oomir::Type::Unit)
        || ty.is_jvm_primitive_like()
        || matches!(
            ty,
            oomir::Type::Class(class_name)
                if class_name == crate::lower2::I128_CLASS
                    || class_name == crate::lower2::U128_CLASS
                    || class_name == crate::lower2::F128_CLASS
        )
        || matches!(
            ty,
            oomir::Type::Array(_) | oomir::Type::Slice(_) | oomir::Type::Str | oomir::Type::String
        )
}

fn comparison_value_type<'tcx>(mut ty: oomir::Type, mut mir_ty: Ty<'tcx>) -> oomir::Type {
    while let TyKind::Ref(_, pointee, _) = mir_ty.kind() {
        let oomir::Type::Pointer(inner) = ty else {
            break;
        };
        ty = *inner;
        mir_ty = *pointee;
    }
    ty
}

fn emit_comparison_value<'tcx>(
    mut operand: oomir::Operand,
    mut mir_ty: Ty<'tcx>,
    dest_prefix: &str,
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
    operand
}

fn supports_direct_equality(ty: &oomir::Type) -> bool {
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
        || matches!(ty, oomir::Type::Str | oomir::Type::String)
}

fn supports_direct_ordering(ty: &oomir::Type) -> bool {
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

/// Convert a single MIR basic block into an OOMIR basic block.
pub(super) fn convert_basic_block<'tcx>(
    bb: BasicBlock,
    bb_data: &BasicBlockData<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    return_oomir_type: &oomir::Type, // Pass function return type
    basic_blocks: &mut HashMap<String, oomir::BasicBlock>,
    data_types: &mut HashMap<String, oomir::DataType>,
    mutable_borrow_arrays: &mut MutableBorrowMap<'tcx>,
    initially_available_pointer_locals: HashSet<Local>,
) -> oomir::BasicBlock {
    // Use the basic block index as its label.
    let label = format!("bb{}", bb.index());
    let mut instructions = Vec::new();
    let mut initialized_borrows = initially_available_pointer_locals;
    // Convert each MIR statement in the block.
    for stmt in &bb_data.statements {
        match &stmt.kind {
            StatementKind::Assign(box (place, rvalue)) => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!("Assign statement: place={:?}, rvalue={:?}", place, rvalue)
                );
                // 1. Evaluate the Rvalue to get the source operand and temp instructions
                let (rvalue_instructions, source_operand) = rvalue::convert_rvalue_to_operand(
                    // Call the refactored function
                    rvalue,
                    place, // Pass original destination for temp naming hints
                    mir,
                    tcx,
                    instance,
                    data_types,
                    mutable_borrow_arrays,
                    &initialized_borrows,
                );

                // Add instructions needed to calculate the Rvalue
                instructions.extend(rvalue_instructions);

                if let rustc_middle::mir::Rvalue::Ref(_, _, borrowed_place) = rvalue {
                    let dest_ty = place.ty(&mir.local_decls, tcx).ty;
                    let borrowed_is_trait_object = matches!(dest_ty.kind(), TyKind::Ref(..))
                        && matches!(source_operand.get_type(), Some(oomir::Type::Interface(_)));
                    if borrowed_is_trait_object {
                        // Trait objects do not use the MutableReference array wrapper
                    } else {
                        // Check if the destination is a simple local (most common case for &mut assignment)
                        if place.projection.is_empty() {
                            if let oomir::Operand::Variable {
                                name: array_var_name,
                                ty: array_ty,
                            } = &source_operand
                            {
                                match array_ty {
                                    oomir::Type::Pointer(element_ty) => {
                                        breadcrumbs::log!(
                                            breadcrumbs::LogLevel::Info,
                                            "mir-lowering",
                                            format!(
                                                "Info: Tracking mutable borrow array for place {:?} stored in local {:?}. Original: {:?}, ArrayVar: {}, ElementTy: {:?}",
                                                place,
                                                place.local,
                                                borrowed_place,
                                                array_var_name,
                                                element_ty
                                            )
                                        );
                                        mutable_borrow_arrays.insert(
                                            place.local,
                                            PointerOrigin {
                                                original_place: borrowed_place.clone(),
                                                carrier_name: place_to_string(place, tcx),
                                                pointee_type: *element_ty.clone(),
                                                writable: matches!(
                                                    rvalue,
                                                    rustc_middle::mir::Rvalue::Ref(
                                                        _,
                                                        rustc_middle::mir::BorrowKind::Mut { .. },
                                                        _
                                                    )
                                                ),
                                            },
                                        );
                                        initialized_borrows.insert(place.local);
                                    }
                                    oomir::Type::Slice(_) | oomir::Type::Str => {
                                        // Slice views already alias their backing array, so writes
                                        // are visible directly and need no copy-out bookkeeping.
                                        // Utf8View has the same canonical aliasing behaviour for
                                        // `str` references.
                                    }
                                    _ => {
                                        breadcrumbs::log!(
                                            breadcrumbs::LogLevel::Warn,
                                            "mir-lowering",
                                            format!(
                                                "Warning: Expected mutable-reference or slice representation, found {:?}",
                                                array_ty
                                            )
                                        );
                                    }
                                }
                            } else {
                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Warn,
                                    "mir-lowering",
                                    format!(
                                        "Warning: Expected variable operand for mutable borrow ref assignment result, found {:?}",
                                        source_operand
                                    )
                                );
                            }
                        } else {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "mir-lowering",
                                format!(
                                    "Warning: Mutable borrow assigned to complex place {:?}, write-back might not work correctly.",
                                    place
                                )
                            );
                        }
                    }
                }

                if let rustc_middle::mir::Rvalue::RawPtr(
                    rustc_middle::mir::RawPtrKind::Const | rustc_middle::mir::RawPtrKind::Mut,
                    pointed_place,
                ) = rvalue
                    && place.projection.is_empty()
                    && let oomir::Operand::Variable {
                        name: _,
                        ty: oomir::Type::Pointer(element_ty),
                    } = &source_operand
                {
                    let inherited_origin = pointed_place
                        .projection
                        .last()
                        .filter(|projection| {
                            matches!(projection, rustc_middle::mir::ProjectionElem::Deref)
                        })
                        .and_then(|_| mutable_borrow_arrays.get(&pointed_place.local))
                        .map(|origin| origin.original_place.clone());
                    mutable_borrow_arrays.insert(
                        place.local,
                        PointerOrigin {
                            original_place: inherited_origin
                                .unwrap_or_else(|| pointed_place.clone()),
                            // The assigned local is the stable carrier across
                            // block boundaries, not the rvalue temporary.
                            carrier_name: place_to_string(place, tcx),
                            pointee_type: element_ty.as_ref().clone(),
                            writable: matches!(
                                rvalue,
                                rustc_middle::mir::Rvalue::RawPtr(
                                    rustc_middle::mir::RawPtrKind::Mut,
                                    _
                                )
                            ),
                        },
                    );
                    initialized_borrows.insert(place.local);
                }

                // 2. Generate instructions to store the computed value into the destination place
                let assignment_instructions = emit_instructions_to_set_value(
                    place,          // The actual destination Place
                    source_operand, // The OOMIR operand holding the value from the Rvalue
                    tcx,
                    instance,
                    mir,
                    data_types,
                );

                // Add the final assignment instructions (Move, SetField, ArrayStore)
                instructions.extend(assignment_instructions);
                instructions.extend(emit_pointer_origin_refreshes(
                    place,
                    &initialized_borrows,
                    mutable_borrow_arrays,
                    tcx,
                    instance,
                    mir,
                    data_types,
                ));
                if place.projection.iter().any(|projection| {
                    matches!(projection, rustc_middle::mir::ProjectionElem::Deref)
                }) {
                    instructions.extend(emit_selected_mutable_borrow_writebacks(
                        initialized_borrows.iter().copied(),
                        mutable_borrow_arrays,
                        tcx,
                        instance,
                        mir,
                        data_types,
                    ));
                }
            }
            StatementKind::StorageLive(_) | StatementKind::StorageDead(_) => {
                // no-op, currently
            }
            StatementKind::Nop | StatementKind::ConstEvalCounter => {
                // Literally a no-op
            }
            StatementKind::Intrinsic(box NonDivergingIntrinsic::Assume(operand)) => {
                // `assume(false)` is UB, so a valid program never needs a JVM-side
                // branch here. Still lower the operand so any place projection is
                // evaluated consistently with other MIR operands.
                let _ = convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);
            }
            StatementKind::Intrinsic(box NonDivergingIntrinsic::CopyNonOverlapping(copy)) => {
                let source =
                    convert_operand(&copy.src, tcx, instance, mir, data_types, &mut instructions);
                let destination =
                    convert_operand(&copy.dst, tcx, instance, mir, data_types, &mut instructions);
                let count = convert_operand(
                    &copy.count,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );
                let source_rust_ty = copy.src.ty(&mir.local_decls, tcx);
                let pointee = match source_rust_ty.kind() {
                    TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
                    other => {
                        panic!("copy_nonoverlapping MIR statement has non-pointer source {other:?}")
                    }
                };
                let (method_name, count) = if let Ok(element_size) =
                    super::types::layout_size_bytes(tcx, pointee)
                {
                    let byte_count_name = format!("{label}_copy_nonoverlapping_bytes");
                    instructions.push(oomir::Instruction::Mul {
                        dest: byte_count_name.clone(),
                        op1: count,
                        op2: oomir::Operand::Constant(oomir::Constant::U64(element_size as u64)),
                    });
                    (
                        "copyNonOverlapping".to_string(),
                        oomir::Operand::Variable {
                            name: byte_count_name,
                            ty: oomir::Type::U64,
                        },
                    )
                } else {
                    // Generic `core` bodies can retain `T` here. Pointer values carry
                    // their concrete view size, so defer the sizeof(T) multiplication
                    // to the runtime in that case.
                    ("copyNonOverlappingElements".to_string(), count)
                };
                let source_ty = source
                    .get_type()
                    .expect("copy_nonoverlapping source is typed");
                let destination_ty = destination
                    .get_type()
                    .expect("copy_nonoverlapping destination is typed");
                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name: oomir::POINTER_CLASS.to_string(),
                    method_name,
                    method_ty: oomir::Signature {
                        params: vec![
                            ("source".to_string(), source_ty),
                            ("destination".to_string(), destination_ty),
                            ("byte_count".to_string(), oomir::Type::U64),
                        ],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![source, destination, count],
                    dest: None,
                });
            }
            StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                let enum_ty = place.ty(&mir.local_decls, tcx).ty;
                if let Some(value) = super::value_repr::construct_fieldless_enum_variant(
                    enum_ty,
                    *variant_index,
                    &format!("{}_variant", super::place::place_to_string(place, tcx)),
                    tcx,
                    instance,
                    data_types,
                    &mut instructions,
                ) {
                    instructions.extend(emit_instructions_to_set_value(
                        place, value, tcx, instance, mir, data_types,
                    ));
                } else {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!(
                            "Warning: SetDiscriminant for data-carrying or non-enum place is unsupported. Place: {:?}, Index: {:?}",
                            place, variant_index
                        )
                    );
                }
            }
            // Handle other StatementKind variants if necessary
            _ => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!("Warning: Unhandled StatementKind: {:?}", stmt.kind)
                );
            }
        }
    }

    // Convert the MIR terminator into corresponding OOMIR instructions.
    if let Some(terminator) = &bb_data.terminator {
        match &terminator.kind {
            TerminatorKind::Return => {
                // Handle Return without operand
                if *return_oomir_type == oomir::Type::Void {
                    instructions.push(oomir::Instruction::Return { operand: None });
                } else {
                    let return_operand = convert_operand(
                        &MirOperand::Move(Place::return_place()),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
                    let return_operand = super::value_repr::adapt_operand_to_rust_type(
                        return_operand,
                        Place::return_place().ty(&mir.local_decls, tcx).ty,
                        &format!("{}_return", label),
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    );
                    instructions.push(oomir::Instruction::Return {
                        operand: Some(return_operand),
                    });
                }
            }
            TerminatorKind::Goto { target } => {
                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump {
                    target: target_label,
                });
            }
            TerminatorKind::SwitchInt { discr, targets, .. } => {
                let discr_operand =
                    convert_operand(discr, tcx, instance, mir, data_types, &mut instructions);
                let discr_ty = discr.ty(&mir.local_decls, tcx);

                let oomir_targets: Vec<(oomir::Constant, String)> = targets
                    .iter()
                    .map(|(value, target_bb)| {
                        let oomir_const = mir_int_to_oomir_const(value, discr_ty, tcx);
                        if !oomir_const.is_integer_like() {
                            breadcrumbs::log!(breadcrumbs::LogLevel::Warn, "mir-lowering", format!("Warning: SwitchInt target value {:?} for type {:?} cannot be directly used in JVM switch. Block: {}", oomir_const, discr_ty, label));
                        }
                        let target_label = format!("bb{}", target_bb.index());
                        (oomir_const, target_label)
                    })
                    .collect();

                let otherwise_label = format!("bb{}", targets.otherwise().index());

                // Add the single OOMIR Switch instruction
                instructions.push(oomir::Instruction::Switch {
                    discr: discr_operand,
                    targets: oomir_targets,
                    otherwise: otherwise_label,
                });
                // This Switch instruction terminates the current OOMIR basic block.
            }
            TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                ..
            } => {
                let mut pre_call_instructions = Vec::new();
                let mut oomir_operands: Vec<oomir::Operand> = args
                    .iter()
                    .map(|arg| {
                        convert_operand(
                            &arg.node,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut pre_call_instructions,
                        )
                    })
                    .collect();
                instructions.extend(pre_call_instructions);
                for (index, arg) in args.iter().enumerate() {
                    let source = oomir_operands[index].clone();
                    oomir_operands[index] = super::value_repr::adapt_operand_to_rust_type(
                        source,
                        arg.node.ty(mir, tcx),
                        &format!("{}_call_operand_{}", label, index),
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    );
                }

                let destination_oomir_type =
                    super::place::get_place_type(destination, mir, tcx, instance, data_types);
                let deferred_destination_store = !destination.projection.is_empty()
                    || super::place::local_uses_stable_cell(destination.local, mir);
                let dest_var_name = destination_oomir_type.has_jvm_value().then(|| {
                    if deferred_destination_store {
                        format!("{}_call_result", label)
                    } else {
                        format!("_{}", destination.local.index())
                    }
                });

                let typing_env = TypingEnv::post_analysis(tcx, mir.source.def_id());
                let func_ty = EarlyBinder::bind(tcx, func.ty(mir, tcx))
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                if let rustc_middle::ty::TyKind::FnDef(def_id, substs) = func_ty.kind() {
                    // Resolve the instance
                    let func_instance = rustc_middle::ty::Instance::expect_resolve(
                        tcx,
                        typing_env,
                        *def_id,
                        substs.no_bound_vars().unwrap(),
                        terminator.source_info.span,
                    );

                    let instance_ty = func_instance.ty(tcx, typing_env);
                    let (fn_inputs, fn_output) = match instance_ty.kind() {
                        TyKind::Closure(_, args) => {
                            let sig = args.as_closure().sig();
                            (
                                sig.inputs().skip_binder().to_vec(),
                                sig.output().skip_binder(),
                            )
                        }
                        _ => {
                            let sig = instance_ty.fn_sig(tcx).skip_binder();
                            (sig.inputs().to_vec(), sig.output())
                        }
                    };

                    if !matches!(instance_ty.kind(), TyKind::Closure(..)) {
                        for (index, target_ty) in fn_inputs.iter().enumerate() {
                            let Some(source) = oomir_operands.get(index).cloned() else {
                                break;
                            };
                            oomir_operands[index] = super::value_repr::adapt_operand_to_rust_type(
                                source,
                                *target_ty,
                                &format!("{}_call_arg_{}", label, index),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                        }
                    }

                    let oomir_output_type =
                        super::types::ty_to_oomir_type(fn_output, tcx, data_types, instance);

                    let effective_dest = if !oomir_output_type.has_jvm_value() {
                        None
                    } else {
                        dest_var_name.clone()
                    };

                    let oomir_input_types: Vec<oomir::Type> = fn_inputs
                        .iter()
                        .map(|ty| super::types::ty_to_oomir_type(*ty, tcx, data_types, instance))
                        .collect();

                    let oomir_params: Vec<(String, oomir::Type)> = oomir_input_types
                        .into_iter()
                        .enumerate()
                        .map(|(i, ty)| (format!("arg{}", i), ty))
                        .collect();

                    let mut method_signature = oomir::Signature {
                        params: oomir_params,
                        ret: Box::new(oomir_output_type.clone()),
                        is_static: false,
                    };

                    if func_instance.def.requires_caller_location(tcx) {
                        let location = caller_location_operand(
                            terminator.source_info,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                            &format!("{label}_caller_location"),
                        );
                        let location_ty = location
                            .get_type()
                            .expect("caller location operand must have a JVM type");
                        method_signature
                            .params
                            .push((oomir::CALLER_LOCATION_PARAM_NAME.to_string(), location_ty));
                        oomir_operands.push(location);
                    }

                    let assoc_item = tcx.opt_associated_item(func_instance.def_id());

                    if let InstanceKind::Shim(ShimKind::DropGlue(_, drop_ty)) = func_instance.def {
                        if let Some(drop_ty) = drop_ty {
                            let drop_oomir_ty =
                                super::types::ty_to_oomir_type(drop_ty, tcx, data_types, instance);
                            let value_name = format!("{label}_drop_glue_value");
                            let value = super::place::emit_pointer_read(
                                oomir_operands[0].clone(),
                                &drop_oomir_ty,
                                &value_name,
                                &mut instructions,
                            );
                            emit_rust_drop_value(
                                drop_ty,
                                value,
                                &format!("{label}_drop_glue"),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                        }
                    } else if let Some(item) = assoc_item {
                        if item.is_method() {
                            let receiver_mir_ty = args[0].node.ty(mir, tcx);
                            let receiver_operand = oomir_operands[0].clone();

                            // Keep the self parameter in the signature. Signature::to_string()
                            // is responsible for omitting the implicit JVM receiver.
                            method_signature.is_static = false;

                            // Separate args for InvokeInterface/InvokeVirtual (receiver handled via 'operand')
                            let method_args = oomir_operands[1..].to_vec();
                            let method_name = super::naming::associated_method_name_from_instance(
                                tcx,
                                func_instance,
                            );

                            if let rustc_middle::ty::TyKind::Dynamic(preds, ..) =
                                receiver_mir_ty.kind()
                            {
                                let principal = preds.principal().unwrap().skip_binder();
                                instructions.push(oomir::Instruction::InvokeInterface {
                                    class_name: jvm_names::class_for_def_id(tcx, principal.def_id),
                                    method_name,
                                    method_ty: method_signature,
                                    args: method_args,
                                    dest: effective_dest,
                                    operand: receiver_operand,
                                });
                            } else {
                                // Check if this method is declared in a trait (interface)
                                let trait_container = item.trait_container(tcx);

                                // Check if the receiver operand is an interface type (after any casts)
                                let receiver_oomir_ty = receiver_operand.get_type();

                                // Use InvokeInterface if:
                                // 1. The receiver type is explicitly an Interface type, OR
                                // 2. The method is declared in a trait (which maps to an interface)
                                let use_interface =
                                    if let Some(oomir::Type::Interface(interface_name)) =
                                        &receiver_oomir_ty
                                    {
                                        Some(interface_name.clone())
                                    } else if let Some(trait_def_id) = trait_container {
                                        // Get the trait name and convert to interface name
                                        Some(jvm_names::class_for_def_id(tcx, trait_def_id))
                                    } else {
                                        None
                                    };
                                let dispatch_receiver_ty = super::types::ty_to_oomir_type(
                                    receiver_mir_ty,
                                    tcx,
                                    data_types,
                                    instance,
                                );
                                let resolved_receiver_mir_ty =
                                    EarlyBinder::bind(tcx, receiver_mir_ty)
                                        .instantiate(tcx, instance.args)
                                        .skip_norm_wip();
                                let pointer_api_receiver = {
                                    let receiver_value_ty = match resolved_receiver_mir_ty.kind() {
                                        TyKind::Ref(_, pointee, _) => *pointee,
                                        _ => resolved_receiver_mir_ty,
                                    };
                                    matches!(receiver_value_ty.kind(), TyKind::RawPtr(..))
                                        || matches!(
                                            receiver_value_ty.kind(),
                                            TyKind::Adt(adt_def, _)
                                                if tcx.def_path_str(adt_def.did()).ends_with("::NonNull")
                                        )
                                };
                                let declared_method_name = item.name().as_str().to_string();
                                let is_pointer_cast_method = [
                                    sym::const_ptr_cast,
                                    sym::ptr_cast,
                                    sym::ptr_cast_const,
                                    sym::ptr_cast_mut,
                                ]
                                .into_iter()
                                .any(|diagnostic| {
                                    tcx.is_diagnostic_item(diagnostic, func_instance.def_id())
                                });
                                let is_pointer_null_method = tcx.is_diagnostic_item(
                                    sym::ptr_const_is_null,
                                    func_instance.def_id(),
                                ) || tcx
                                    .is_diagnostic_item(sym::ptr_is_null, func_instance.def_id());
                                let comparison_value_ty = comparison_value_type(
                                    dispatch_receiver_ty.clone(),
                                    resolved_receiver_mir_ty,
                                );
                                let direct_equality =
                                    matches!(declared_method_name.as_str(), "eq" | "ne")
                                        && supports_direct_equality(&comparison_value_ty)
                                        && oomir_operands.len() == 2;
                                let direct_ordering = matches!(
                                    declared_method_name.as_str(),
                                    "lt" | "le" | "gt" | "ge"
                                ) && supports_direct_ordering(
                                    &comparison_value_ty,
                                ) && oomir_operands.len() == 2;

                                let direct_wrapping_integer_op = matches!(
                                    declared_method_name.as_str(),
                                    "wrapping_add" | "wrapping_sub" | "wrapping_mul"
                                ) && (matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::I8
                                        | oomir::Type::U8
                                        | oomir::Type::I16
                                        | oomir::Type::U16
                                        | oomir::Type::I32
                                        | oomir::Type::U32
                                        | oomir::Type::I64
                                        | oomir::Type::U64
                                ) || matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::Class(class_name)
                                        if class_name == crate::lower2::I128_CLASS
                                            || class_name == crate::lower2::U128_CLASS
                                )) && oomir_operands.len() == 2;

                                let direct_overflowing_integer_op = matches!(
                                    declared_method_name.as_str(),
                                    "overflowing_add" | "overflowing_sub" | "overflowing_mul"
                                ) && (matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::I8
                                        | oomir::Type::U8
                                        | oomir::Type::I16
                                        | oomir::Type::U16
                                        | oomir::Type::I32
                                        | oomir::Type::U32
                                        | oomir::Type::I64
                                        | oomir::Type::U64
                                ) || matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::Class(class_name)
                                        if class_name == crate::lower2::I128_CLASS
                                            || class_name == crate::lower2::U128_CLASS
                                )) && oomir_operands.len() == 2;

                                let direct_f16_to_bits = dispatch_receiver_ty == oomir::Type::F16
                                    && declared_method_name == "to_bits"
                                    && oomir_operands.len() == 1;

                                if direct_f16_to_bits {
                                    if let Some(dest) = effective_dest {
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            dest: Some(dest),
                                            class_name: "org/rustlang/runtime/Numbers".to_string(),
                                            method_name: "f16ToBits".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![(
                                                    "value".to_string(),
                                                    oomir::Type::F16,
                                                )],
                                                ret: Box::new(oomir::Type::U16),
                                                is_static: true,
                                            },
                                            args: vec![oomir_operands[0].clone()],
                                        });
                                    }
                                } else if direct_overflowing_integer_op {
                                    if let Some(dest) = effective_dest {
                                        let tuple_class = oomir_output_type
                                            .get_class_name()
                                            .expect(
                                                "overflowing integer result must be a tuple class",
                                            )
                                            .to_string();
                                        let operation = declared_method_name
                                            .strip_prefix("overflowing_")
                                            .expect("overflowing operation prefix");
                                        let (generated, pair, _, _) =
                                            checked_ops::emit_checked_arithmetic_oomir_instructions(
                                                &dest,
                                                &oomir_operands[0],
                                                &oomir_operands[1],
                                                &dispatch_receiver_ty,
                                                operation,
                                                instructions.len(),
                                                &tuple_class,
                                            );
                                        instructions.extend(generated);
                                        instructions.push(oomir::Instruction::Move {
                                            dest,
                                            src: oomir::Operand::Variable {
                                                name: pair,
                                                ty: oomir::Type::Class(tuple_class),
                                            },
                                        });
                                    }
                                } else if direct_wrapping_integer_op {
                                    if let Some(dest) = effective_dest {
                                        let op1 = oomir_operands[0].clone();
                                        let op2 = oomir_operands[1].clone();
                                        let instruction = match declared_method_name.as_str() {
                                            "wrapping_add" => {
                                                oomir::Instruction::Add { dest, op1, op2 }
                                            }
                                            "wrapping_sub" => {
                                                oomir::Instruction::Sub { dest, op1, op2 }
                                            }
                                            "wrapping_mul" => {
                                                oomir::Instruction::Mul { dest, op1, op2 }
                                            }
                                            _ => unreachable!(),
                                        };
                                        instructions.push(instruction);
                                    }
                                } else if direct_equality || direct_ordering {
                                    if let Some(dest) = effective_dest {
                                        let left = emit_comparison_value(
                                            oomir_operands[0].clone(),
                                            fn_inputs[0],
                                            &format!("{label}_comparison_left"),
                                            &mut instructions,
                                        );
                                        let right = emit_comparison_value(
                                            oomir_operands[1].clone(),
                                            fn_inputs[1],
                                            &format!("{label}_comparison_right"),
                                            &mut instructions,
                                        );
                                        if matches!(left.get_type(), Some(oomir::Type::Pointer(_)))
                                        {
                                            let pointer_result = if declared_method_name == "ne" {
                                                format!("{dest}_pointer_eq")
                                            } else {
                                                dest.clone()
                                            };
                                            let method_name = match declared_method_name.as_str() {
                                                "eq" | "ne" => "sameAddress",
                                                "lt" => "lessThan",
                                                "le" => "lessOrEqual",
                                                "gt" => "greaterThan",
                                                "ge" => "greaterOrEqual",
                                                _ => unreachable!(),
                                            };
                                            instructions.push(oomir::Instruction::InvokeVirtual {
                                                dest: Some(pointer_result.clone()),
                                                class_name: oomir::POINTER_CLASS.to_string(),
                                                method_name: method_name.to_string(),
                                                method_ty: oomir::Signature {
                                                    params: vec![
                                                        (
                                                            "self".to_string(),
                                                            left.get_type().unwrap(),
                                                        ),
                                                        (
                                                            "other".to_string(),
                                                            right.get_type().unwrap(),
                                                        ),
                                                    ],
                                                    ret: Box::new(oomir::Type::Boolean),
                                                    is_static: false,
                                                },
                                                args: vec![right],
                                                operand: left,
                                            });
                                            if declared_method_name == "ne" {
                                                instructions.push(oomir::Instruction::Not {
                                                    dest,
                                                    src: oomir::Operand::Variable {
                                                        name: pointer_result,
                                                        ty: oomir::Type::Boolean,
                                                    },
                                                });
                                            }
                                        } else {
                                            let instruction = match declared_method_name.as_str() {
                                                "eq" => oomir::Instruction::Eq {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "ne" => oomir::Instruction::Ne {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "lt" => oomir::Instruction::Lt {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "le" => oomir::Instruction::Le {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "gt" => oomir::Instruction::Gt {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "ge" => oomir::Instruction::Ge {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                _ => unreachable!(),
                                            };
                                            instructions.push(instruction);
                                        }
                                    }
                                } else if matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::Slice(_) | oomir::Type::Str
                                ) && is_pointer_cast_method
                                    && matches!(oomir_output_type, oomir::Type::Pointer(_))
                                {
                                    let source_pointee = match receiver_mir_ty.kind() {
                                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                            if pointee.is_slice() {
                                                pointee.sequence_element_type(tcx)
                                            } else {
                                                tcx.types.u8
                                            }
                                        }
                                        other => panic!(
                                            "fat pointer cast receiver has unexpected type {other:?}"
                                        ),
                                    };
                                    let target_pointee = match fn_output.kind() {
                                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                            *pointee
                                        }
                                        other => panic!(
                                            "fat pointer cast returns unexpected type {other:?}"
                                        ),
                                    };
                                    let source_element_size =
                                        super::types::layout_size_bytes(tcx, source_pointee)
                                            .expect("fat pointer element has a concrete layout");
                                    let data_pointer = format!("{label}_fat_cast_data");
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        dest: Some(data_pointer.clone()),
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "fromSlice".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![
                                                (
                                                    "slice".to_string(),
                                                    oomir::Type::Class(
                                                        "java/lang/Object".to_string(),
                                                    ),
                                                ),
                                                ("element_size".to_string(), oomir::Type::I32),
                                                ("codec".to_string(), oomir::Type::String),
                                            ],
                                            ret: Box::new(oomir_output_type.clone()),
                                            is_static: true,
                                        },
                                        args: vec![
                                            receiver_operand,
                                            oomir::Operand::Constant(oomir::Constant::I32(
                                                i32::try_from(source_element_size).expect(
                                                    "fat pointer element exceeds JVM address space",
                                                ),
                                            )),
                                            super::types::pointer_memory_codec_operand(
                                                source_pointee,
                                                tcx,
                                                data_types,
                                                instance,
                                            ),
                                        ],
                                    });
                                    instructions.push(oomir::Instruction::InvokeVirtual {
                                        dest: effective_dest,
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "retype".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![
                                                (
                                                    "self".to_string(),
                                                    oomir_output_type.clone(),
                                                ),
                                                ("view_size".to_string(), oomir::Type::I32),
                                                ("view_codec".to_string(), oomir::Type::String),
                                            ],
                                            ret: Box::new(oomir_output_type.clone()),
                                            is_static: false,
                                        },
                                        args: vec![
                                            oomir::Operand::Constant(oomir::Constant::I32(
                                                i32::try_from(
                                                    super::types::layout_size_bytes(
                                                        tcx,
                                                        target_pointee,
                                                    )
                                                    .expect(
                                                        "fat pointer cast target has a concrete layout",
                                                    ),
                                                )
                                                .expect(
                                                    "fat pointer cast target exceeds JVM address space",
                                                ),
                                            )),
                                            super::types::pointer_view_codec_operand(
                                                target_pointee,
                                                tcx,
                                                data_types,
                                                instance,
                                            ),
                                        ],
                                        operand: oomir::Operand::Variable {
                                            name: data_pointer,
                                            ty: oomir_output_type.clone(),
                                        },
                                    });
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && matches!(declared_method_name.as_str(), "as_ref" | "as_mut")
                                    && matches!(
                                        fn_output.kind(),
                                        TyKind::Adt(adt_def, _)
                                            if tcx.is_lang_item(
                                                adt_def.did(),
                                                rustc_hir::LangItem::Option,
                                            )
                                    )
                                {
                                    if let Some(dest) = effective_dest {
                                        let option_class = oomir_output_type
                                            .get_class_name()
                                            .expect("pointer as_ref/as_mut returns Option")
                                            .to_string();
                                        let option_object =
                                            format!("{label}_pointer_option_object");
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: "asRefOption".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![
                                                    (
                                                        "pointer".to_string(),
                                                        dispatch_receiver_ty.clone(),
                                                    ),
                                                    (
                                                        "option_class".to_string(),
                                                        oomir::Type::String,
                                                    ),
                                                ],
                                                ret: Box::new(oomir::Type::Class(
                                                    "java/lang/Object".to_string(),
                                                )),
                                                is_static: true,
                                            },
                                            args: vec![
                                                receiver_operand,
                                                oomir::Operand::Constant(oomir::Constant::String(
                                                    option_class,
                                                )),
                                            ],
                                            dest: Some(option_object.clone()),
                                        });
                                        instructions.push(oomir::Instruction::Cast {
                                            op: oomir::Operand::Variable {
                                                name: option_object,
                                                ty: oomir::Type::Class(
                                                    "java/lang/Object".to_string(),
                                                ),
                                            },
                                            ty: oomir_output_type.clone(),
                                            dest,
                                        });
                                    }
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && matches!(declared_method_name.as_str(), "as_ref" | "as_mut")
                                {
                                    // NonNull::as_ref/as_mut return a reference directly, unlike
                                    // raw-pointer methods with the same names which return Option.
                                    if let Some(dest) = effective_dest {
                                        instructions.push(oomir::Instruction::Move {
                                            dest,
                                            src: receiver_operand,
                                        });
                                    }
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && matches!(
                                        declared_method_name.as_str(),
                                        "as_ref_unchecked" | "as_mut_unchecked"
                                    )
                                {
                                    if let Some(dest) = effective_dest {
                                        instructions.push(oomir::Instruction::Move {
                                            dest,
                                            src: receiver_operand,
                                        });
                                    }
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && declared_method_name == "is_aligned"
                                {
                                    let receiver_ty = EarlyBinder::bind(tcx, receiver_mir_ty)
                                        .instantiate(tcx, instance.args)
                                        .skip_norm_wip();
                                    let pointee = match receiver_ty.kind() {
                                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                            *pointee
                                        }
                                        other => panic!(
                                            "pointer is_aligned receiver has non-pointer type {other:?}"
                                        ),
                                    };
                                    let alignment = super::types::layout_align_bytes(tcx, pointee)
                                        .unwrap_or_else(|error| {
                                            panic!(
                                                "could not determine pointer target alignment: {error}"
                                            )
                                        });
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "is_aligned_to".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![
                                                (
                                                    "pointer".to_string(),
                                                    dispatch_receiver_ty.clone(),
                                                ),
                                                ("alignment".to_string(), oomir::Type::I32),
                                            ],
                                            ret: Box::new(oomir::Type::Boolean),
                                            is_static: true,
                                        },
                                        args: vec![
                                            receiver_operand,
                                            oomir::Operand::Constant(oomir::Constant::I32(
                                                i32::try_from(alignment).expect(
                                                    "pointer target alignment exceeds JVM integer",
                                                ),
                                            )),
                                        ],
                                        dest: effective_dest,
                                    });
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && declared_method_name == "to_raw_parts"
                                {
                                    if let Some(dest) = effective_dest {
                                        let tuple_class = oomir_output_type
                                            .get_class_name()
                                            .expect("pointer to_raw_parts returns a tuple")
                                            .to_string();
                                        let tuple_fields = match data_types.get(&tuple_class) {
                                            Some(oomir::DataType::Class { fields, .. }) => {
                                                fields.clone()
                                            }
                                            other => panic!(
                                                "to_raw_parts tuple class is unavailable: {other:?}"
                                            ),
                                        };
                                        let data_pointer_ty = tuple_fields
                                            .first()
                                            .map(|(_, ty)| ty.clone())
                                            .expect("to_raw_parts tuple has a data pointer");
                                        let data_pointer_name = format!("{label}_raw_parts_data");
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: "retype".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![
                                                    (
                                                        "pointer".to_string(),
                                                        dispatch_receiver_ty.clone(),
                                                    ),
                                                    ("view_size".to_string(), oomir::Type::I32),
                                                    ("view_codec".to_string(), oomir::Type::String),
                                                ],
                                                ret: Box::new(data_pointer_ty.clone()),
                                                is_static: true,
                                            },
                                            args: vec![
                                                receiver_operand,
                                                oomir::Operand::Constant(oomir::Constant::I32(0)),
                                                oomir::Operand::Constant(oomir::Constant::Null(
                                                    oomir::Type::String,
                                                )),
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
                                        for (_, metadata_ty) in tuple_fields.into_iter().skip(1) {
                                            tuple_args.push((
                                                oomir::Operand::Constant(oomir::Constant::Null(
                                                    metadata_ty.clone(),
                                                )),
                                                metadata_ty,
                                            ));
                                        }
                                        instructions.push(oomir::Instruction::ConstructObject {
                                            dest,
                                            class_name: tuple_class,
                                            args: tuple_args,
                                        });
                                    }
                                } else if let oomir::Type::Pointer(pointee_ty) =
                                    &dispatch_receiver_ty
                                    && pointer_api_receiver
                                    && matches!(
                                        declared_method_name.as_str(),
                                        "read" | "read_unaligned" | "read_volatile"
                                    )
                                {
                                    if let Some(dest) = effective_dest {
                                        if declared_method_name == "read_volatile" {
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
                                        super::place::emit_pointer_read(
                                            receiver_operand,
                                            pointee_ty,
                                            &dest,
                                            &mut instructions,
                                        );
                                    }
                                } else if let oomir::Type::Pointer(pointee_ty) =
                                    &dispatch_receiver_ty
                                    && pointer_api_receiver
                                    && matches!(
                                        declared_method_name.as_str(),
                                        "write" | "write_unaligned" | "write_volatile"
                                    )
                                    && method_args.len() == 1
                                {
                                    super::place::emit_pointer_write(
                                        receiver_operand,
                                        pointee_ty,
                                        method_args[0].clone(),
                                        &mut instructions,
                                    );
                                    if declared_method_name == "write_volatile" {
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
                                } else if let oomir::Type::Pointer(pointee_ty) =
                                    &dispatch_receiver_ty
                                    && pointer_api_receiver
                                    && declared_method_name == "replace"
                                    && method_args.len() == 1
                                {
                                    if let Some(dest) = effective_dest {
                                        super::place::emit_pointer_read(
                                            receiver_operand.clone(),
                                            pointee_ty,
                                            &dest,
                                            &mut instructions,
                                        );
                                    }
                                    super::place::emit_pointer_write(
                                        receiver_operand,
                                        pointee_ty,
                                        method_args[0].clone(),
                                        &mut instructions,
                                    );
                                } else if let oomir::Type::Pointer(pointee_ty) =
                                    &dispatch_receiver_ty
                                    && pointer_api_receiver
                                    && declared_method_name == "swap"
                                    && method_args.len() == 1
                                {
                                    let left_name = format!("{label}_method_swap_left");
                                    let right_name = format!("{label}_method_swap_right");
                                    let left = super::place::emit_pointer_read(
                                        receiver_operand.clone(),
                                        pointee_ty,
                                        &left_name,
                                        &mut instructions,
                                    );
                                    let right = super::place::emit_pointer_read(
                                        method_args[0].clone(),
                                        pointee_ty,
                                        &right_name,
                                        &mut instructions,
                                    );
                                    super::place::emit_pointer_write(
                                        receiver_operand,
                                        pointee_ty,
                                        right,
                                        &mut instructions,
                                    );
                                    super::place::emit_pointer_write(
                                        method_args[0].clone(),
                                        pointee_ty,
                                        left,
                                        &mut instructions,
                                    );
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && matches!(
                                        declared_method_name.as_str(),
                                        "copy_to"
                                            | "copy_to_nonoverlapping"
                                            | "copy_from"
                                            | "copy_from_nonoverlapping"
                                            | "write_bytes"
                                    )
                                    && method_args.len() == 2
                                {
                                    let receiver_ty = EarlyBinder::bind(tcx, receiver_mir_ty)
                                        .instantiate(tcx, instance.args)
                                        .skip_norm_wip();
                                    let pointee = match receiver_ty.kind() {
                                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                            *pointee
                                        }
                                        other => panic!(
                                            "pointer memory method receiver has non-pointer type {other:?}"
                                        ),
                                    };
                                    let element_size = super::types::layout_size_bytes(tcx, pointee)
                                        .unwrap_or_else(|error| {
                                            panic!(
                                                "could not determine pointer memory method element size: {error}"
                                            )
                                        });
                                    let byte_count_name =
                                        format!("{label}_{declared_method_name}_byte_count");
                                    instructions.push(oomir::Instruction::Mul {
                                        dest: byte_count_name.clone(),
                                        op1: method_args[1].clone(),
                                        op2: oomir::Operand::Constant(oomir::Constant::U64(
                                            element_size as u64,
                                        )),
                                    });
                                    let byte_count = oomir::Operand::Variable {
                                        name: byte_count_name,
                                        ty: oomir::Type::U64,
                                    };
                                    if declared_method_name == "write_bytes" {
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: "writeBytes".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![
                                                    (
                                                        "destination".to_string(),
                                                        dispatch_receiver_ty.clone(),
                                                    ),
                                                    ("value".to_string(), oomir::Type::I32),
                                                    ("byte_count".to_string(), oomir::Type::U64),
                                                ],
                                                ret: Box::new(oomir::Type::Void),
                                                is_static: true,
                                            },
                                            args: vec![
                                                receiver_operand,
                                                method_args[0].clone(),
                                                byte_count,
                                            ],
                                            dest: None,
                                        });
                                    } else {
                                        let from_receiver =
                                            declared_method_name.starts_with("copy_to");
                                        let source = if from_receiver {
                                            receiver_operand.clone()
                                        } else {
                                            method_args[0].clone()
                                        };
                                        let destination = if from_receiver {
                                            method_args[0].clone()
                                        } else {
                                            receiver_operand
                                        };
                                        let source_ty = source
                                            .get_type()
                                            .expect("pointer copy source is typed");
                                        let destination_ty = destination
                                            .get_type()
                                            .expect("pointer copy destination is typed");
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: if declared_method_name
                                                .ends_with("nonoverlapping")
                                            {
                                                "copyNonOverlapping".to_string()
                                            } else {
                                                "copy".to_string()
                                            },
                                            method_ty: oomir::Signature {
                                                params: vec![
                                                    ("source".to_string(), source_ty),
                                                    ("destination".to_string(), destination_ty),
                                                    ("byte_count".to_string(), oomir::Type::U64),
                                                ],
                                                ret: Box::new(oomir::Type::Void),
                                                is_static: true,
                                            },
                                            args: vec![source, destination, byte_count],
                                            dest: None,
                                        });
                                    }
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && declared_method_name == "map_addr"
                                    && method_args.len() == 1
                                {
                                    // `map_addr` is generic over a Rust `FnOnce`. A closure value is
                                    // only its captured environment on the JVM; its body is emitted as
                                    // a static method on the crate module class. Invoke that body here
                                    // and then apply the returned address while retaining provenance.
                                    let mapper_ty =
                                        EarlyBinder::bind(tcx, args[1].node.ty(mir, tcx))
                                            .instantiate(tcx, instance.args)
                                            .skip_norm_wip();
                                    let address_dest = format!("{label}_map_addr_input");
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "addr".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![(
                                                "pointer".to_string(),
                                                dispatch_receiver_ty.clone(),
                                            )],
                                            ret: Box::new(oomir::Type::U64),
                                            is_static: true,
                                        },
                                        args: vec![receiver_operand.clone()],
                                        dest: Some(address_dest.clone()),
                                    });
                                    let mapped_address_dest = format!("{label}_map_addr_output");
                                    let address_operand = oomir::Operand::Variable {
                                        name: address_dest,
                                        ty: oomir::Type::U64,
                                    };
                                    match mapper_ty.kind() {
                                        TyKind::Closure(closure_def_id, closure_args) => {
                                            let closure_instance =
                                                Instance::new_raw(*closure_def_id, *closure_args);
                                            let closure_signature = closure_args.as_closure().sig();
                                            let closure_inputs =
                                                closure_signature.inputs().skip_binder().to_vec();
                                            let closure_arg_ty = *closure_inputs.first().expect(
                                                "FnOnce closure signature has an argument tuple",
                                            );
                                            let closure_arg_oomir_ty =
                                                super::types::ty_to_oomir_type(
                                                    closure_arg_ty,
                                                    tcx,
                                                    data_types,
                                                    instance,
                                                );
                                            let closure_tuple_dest =
                                                format!("{label}_map_addr_closure_tuple");
                                            let closure_tuple_class = closure_arg_oomir_ty
                                                .get_class_name()
                                                .expect(
                                                    "FnOnce closure argument tuple is a JVM class",
                                                )
                                                .to_string();
                                            instructions.push(
                                                oomir::Instruction::ConstructObject {
                                                    dest: closure_tuple_dest.clone(),
                                                    class_name: closure_tuple_class,
                                                    args: vec![(
                                                        address_operand.clone(),
                                                        oomir::Type::U64,
                                                    )],
                                                },
                                            );

                                            let captures = closure_args
                                                .as_closure()
                                                .upvar_tys()
                                                .iter()
                                                .next()
                                                .is_some();
                                            let mut closure_params = closure_inputs
                                                .iter()
                                                .enumerate()
                                                .map(|(index, input)| {
                                                    (
                                                        format!("arg{index}"),
                                                        super::types::ty_to_oomir_type(
                                                            *input, tcx, data_types, instance,
                                                        ),
                                                    )
                                                })
                                                .collect::<Vec<_>>();
                                            let mut closure_call_args =
                                                vec![oomir::Operand::Variable {
                                                    name: closure_tuple_dest,
                                                    ty: closure_arg_oomir_ty,
                                                }];
                                            if captures {
                                                let environment_ty = method_args[0]
                                                    .get_type()
                                                    .expect(
                                                        "capturing map_addr closure has an environment",
                                                    );
                                                closure_params.insert(
                                                    0,
                                                    ("closure_env".to_string(), environment_ty),
                                                );
                                                closure_call_args.insert(0, method_args[0].clone());
                                            }
                                            instructions.push(oomir::Instruction::InvokeStatic {
                                                class_name: super::jvm_names::crate_module_class(
                                                    tcx,
                                                    closure_instance.def_id().krate,
                                                ),
                                                method_name: super::generate_closure_function_name(
                                                    tcx,
                                                    closure_instance,
                                                ),
                                                method_ty: oomir::Signature {
                                                    params: closure_params,
                                                    ret: Box::new(oomir::Type::U64),
                                                    is_static: true,
                                                },
                                                args: closure_call_args,
                                                dest: Some(mapped_address_dest.clone()),
                                            });
                                        }
                                        TyKind::FnDef(def_id, generic_args) => {
                                            let mapper_instance = Instance::resolve_for_fn_ptr(
                                                tcx,
                                                typing_env,
                                                *def_id,
                                                generic_args.no_bound_vars().unwrap(),
                                            )
                                            .expect("map_addr function item resolves");
                                            let target = super::naming::mono_fn_name_from_instance(
                                                tcx,
                                                mapper_instance,
                                            );
                                            instructions.push(oomir::Instruction::InvokeStatic {
                                                class_name: target.class_to_call_on.expect(
                                                    "map_addr function item has a JVM owner",
                                                ),
                                                method_name: target.method_name,
                                                method_ty: oomir::Signature {
                                                    params: vec![(
                                                        "address".to_string(),
                                                        oomir::Type::U64,
                                                    )],
                                                    ret: Box::new(oomir::Type::U64),
                                                    is_static: true,
                                                },
                                                args: vec![address_operand],
                                                dest: Some(mapped_address_dest.clone()),
                                            });
                                        }
                                        TyKind::FnPtr(_, _) => {
                                            let signature = super::types::fn_ptr_signature_from_ty(
                                                mapper_ty, tcx, data_types, instance,
                                            );
                                            super::types::ensure_fn_ptr_interface(
                                                &signature, data_types, tcx, instance,
                                            );
                                            instructions.push(oomir::Instruction::CallIndirect {
                                                dest: Some(mapped_address_dest.clone()),
                                                function_ptr: Box::new(method_args[0].clone()),
                                                args: vec![address_operand],
                                                signature,
                                            });
                                        }
                                        other => panic!(
                                            "raw pointer map_addr requires a monomorphized callable, got {other:?}"
                                        ),
                                    }
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "with_addr".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![
                                                (
                                                    "pointer".to_string(),
                                                    dispatch_receiver_ty.clone(),
                                                ),
                                                ("address".to_string(), oomir::Type::U64),
                                            ],
                                            ret: Box::new(dispatch_receiver_ty.clone()),
                                            is_static: true,
                                        },
                                        args: vec![
                                            receiver_operand,
                                            oomir::Operand::Variable {
                                                name: mapped_address_dest,
                                                ty: oomir::Type::U64,
                                            },
                                        ],
                                        dest: effective_dest,
                                    });
                                } else if let Some(interface_name) = use_interface {
                                    if requires_compiled_static_dispatch(&dispatch_receiver_ty) {
                                        let target = super::naming::mono_fn_name_from_instance(
                                            tcx,
                                            func_instance,
                                        );
                                        let mut static_signature = method_signature;
                                        static_signature.is_static = true;
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: target
                                                .class_to_call_on
                                                .expect("monomorphized functions have JVM owners"),
                                            method_name: target.method_name,
                                            method_ty: static_signature,
                                            args: oomir_operands.clone(),
                                            dest: effective_dest,
                                        });
                                    } else {
                                        // The method is from an interface - use InvokeInterface
                                        instructions.push(oomir::Instruction::InvokeInterface {
                                            class_name: interface_name,
                                            method_name,
                                            method_ty: method_signature,
                                            args: method_args,
                                            dest: effective_dest,
                                            operand: receiver_operand,
                                        });
                                    }
                                } else {
                                    // The receiver is a concrete class - use InvokeVirtual
                                    let class_type = super::types::ty_to_oomir_type(
                                        receiver_mir_ty,
                                        tcx,
                                        data_types,
                                        instance,
                                    );
                                    let runtime_static_target = match &class_type {
                                        oomir::Type::Str if method_name == "as_bytes" => Some((
                                            oomir::UTF8_VIEW_CLASS.to_string(),
                                            "asSlice".to_string(),
                                        )),
                                        oomir::Type::Str
                                            if method_name == "starts_with"
                                                && oomir_operands.get(1).is_some_and(
                                                    |operand| {
                                                        matches!(
                                                            operand.get_type(),
                                                            Some(oomir::Type::I32)
                                                        )
                                                    },
                                                ) =>
                                        {
                                            Some((
                                                oomir::UTF8_VIEW_CLASS.to_string(),
                                                "startsWithChar".to_string(),
                                            ))
                                        }
                                        oomir::Type::Str if method_name == "starts_with" => Some((
                                            oomir::UTF8_VIEW_CLASS.to_string(),
                                            "startsWith".to_string(),
                                        )),
                                        oomir::Type::Str if method_name == "eq" => Some((
                                            oomir::UTF8_VIEW_CLASS.to_string(),
                                            "equals".to_string(),
                                        )),
                                        oomir::Type::Str if method_name == "len" => Some((
                                            oomir::UTF8_VIEW_CLASS.to_string(),
                                            "len".to_string(),
                                        )),
                                        oomir::Type::String if method_name == "as_bytes" => Some((
                                            oomir::SLICE_VIEW_CLASS.to_string(),
                                            "fromString".to_string(),
                                        )),
                                        oomir::Type::String => Some((
                                            "org/rustlang/primitives/RustString".to_string(),
                                            method_name.clone(),
                                        )),
                                        oomir::Type::Slice(_) if method_name == "starts_with" => {
                                            Some((
                                                oomir::SLICE_VIEW_CLASS.to_string(),
                                                "startsWith".to_string(),
                                            ))
                                        }
                                        oomir::Type::Slice(_)
                                            if matches!(
                                                method_name.as_str(),
                                                "as_ptr" | "as_mut_ptr"
                                            ) =>
                                        {
                                            Some((
                                                oomir::POINTER_CLASS.to_string(),
                                                "fromSlice".to_string(),
                                            ))
                                        }
                                        oomir::Type::Pointer(_)
                                            if pointer_api_receiver
                                                && (is_pointer_cast_method
                                                    || is_pointer_null_method
                                                    || matches!(
                                                        method_name.as_str(),
                                                        "add"
                                                            | "sub"
                                                            | "offset"
                                                            | "offset_from"
                                                            | "offsetFrom"
                                                            | "offset_from_unsigned"
                                                            | "byte_offset_from"
                                                            | "byte_offset_from_unsigned"
                                                            | "wrapping_add"
                                                            | "wrapping_sub"
                                                            | "wrapping_offset"
                                                            | "byte_add"
                                                            | "byte_sub"
                                                            | "byte_offset"
                                                            | "wrapping_byte_add"
                                                            | "wrapping_byte_sub"
                                                            | "wrapping_byte_offset"
                                                            | "align_offset"
                                                            | "is_aligned_to"
                                                            | "addr"
                                                            | "expose_provenance"
                                                            | "with_addr"
                                                            | "map_addr"
                                                            | "with_metadata_of"
                                                    )) =>
                                        {
                                            Some((
                                                oomir::POINTER_CLASS.to_string(),
                                                if is_pointer_cast_method
                                                    || method_name == "with_metadata_of"
                                                {
                                                    "retype".to_string()
                                                } else {
                                                    method_name.clone()
                                                },
                                            ))
                                        }
                                        oomir::Type::Class(class_name)
                                            if class_name == crate::lower2::F128_CLASS
                                                && item.name().as_str() == "to_bits" =>
                                        {
                                            Some((
                                                crate::lower2::F128_CLASS.to_string(),
                                                "to_bits".to_string(),
                                            ))
                                        }
                                        oomir::Type::Array(inner) | oomir::Type::Slice(inner)
                                            if matches!(inner.as_ref(), oomir::Type::I16)
                                                && method_name == "starts_with" =>
                                        {
                                            Some((
                                                "org/rustlang/core/Core".to_string(),
                                                method_name.clone(),
                                            ))
                                        }
                                        _ => None,
                                    };
                                    let static_target = runtime_static_target.or_else(|| {
                                        requires_compiled_static_dispatch(&class_type).then(|| {
                                            let target = super::naming::mono_fn_name_from_instance(
                                                tcx,
                                                func_instance,
                                            );
                                            (
                                                target.class_to_call_on.expect(
                                                    "monomorphized functions have JVM owners",
                                                ),
                                                target.method_name,
                                            )
                                        })
                                    });

                                    if let Some((class_name, static_method_name)) = static_target {
                                        let mut static_signature = method_signature;
                                        static_signature.is_static = true;
                                        let mut static_args = oomir_operands.clone();
                                        if class_name == oomir::POINTER_CLASS
                                            && static_method_name == "fromSlice"
                                        {
                                            static_signature.params = vec![
                                                (
                                                    "slice".to_string(),
                                                    oomir::Type::Class(
                                                        "java/lang/Object".to_string(),
                                                    ),
                                                ),
                                                ("element_size".to_string(), oomir::Type::I32),
                                                ("codec".to_string(), oomir::Type::String),
                                            ];
                                            let receiver_value_ty = match receiver_mir_ty.kind() {
                                                TyKind::Ref(_, pointee, _)
                                                | TyKind::RawPtr(pointee, _) => *pointee,
                                                _ => receiver_mir_ty,
                                            };
                                            let element_ty =
                                                receiver_value_ty.sequence_element_type(tcx);
                                            let element_ty = EarlyBinder::bind(tcx, element_ty)
                                                .instantiate(tcx, instance.args)
                                                .skip_norm_wip();
                                            let element_size = super::types::layout_size_bytes(
                                                tcx,
                                                element_ty,
                                            )
                                            .unwrap_or_else(|error| {
                                                panic!(
                                                    "could not determine slice pointer element size: {error}"
                                                )
                                            });
                                            static_args.push(oomir::Operand::Constant(
                                                oomir::Constant::I32(
                                                    i32::try_from(element_size).expect(
                                                        "slice element exceeds JVM address space",
                                                    ),
                                                ),
                                            ));
                                            static_args.push(
                                                super::types::pointer_memory_codec_operand(
                                                    element_ty, tcx, data_types, instance,
                                                ),
                                            );
                                        } else if class_name == oomir::POINTER_CLASS
                                            && static_method_name == "retype"
                                        {
                                            if method_name == "with_metadata_of" {
                                                static_signature.params.truncate(1);
                                                static_args.truncate(1);
                                            }
                                            static_signature
                                                .params
                                                .push(("view_size".to_string(), oomir::Type::I32));
                                            static_signature.params.push((
                                                "view_codec".to_string(),
                                                oomir::Type::String,
                                            ));
                                            let target_pointee = match fn_output.kind() {
                                                TyKind::RawPtr(pointee, _)
                                                | TyKind::Ref(_, pointee, _) => *pointee,
                                                other => panic!(
                                                    "pointer cast returned non-pointer type {other:?}"
                                                ),
                                            };
                                            let target_pointee =
                                                EarlyBinder::bind(tcx, target_pointee)
                                                    .instantiate(tcx, instance.args)
                                                    .skip_norm_wip();
                                            let target_size = super::types::layout_size_bytes(
                                                tcx,
                                                target_pointee,
                                            )
                                            .unwrap_or_else(|error| {
                                                panic!(
                                                    "could not determine pointer cast target size: {error}"
                                                )
                                            });
                                            static_args
                                                .push(oomir::Operand::Constant(oomir::Constant::I32(
                                                i32::try_from(target_size).expect(
                                                    "pointer cast target exceeds JVM address space",
                                                ),
                                            )));
                                            static_args.push(
                                                super::types::pointer_view_codec_operand(
                                                    target_pointee,
                                                    tcx,
                                                    data_types,
                                                    instance,
                                                ),
                                            );
                                        } else if class_name == oomir::POINTER_CLASS
                                            && static_method_name == "map_addr"
                                            && static_signature.params.len() >= 2
                                        {
                                            static_signature.params[1].1 =
                                                oomir::Type::Class("java/lang/Object".to_string());
                                        }
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name,
                                            method_name: static_method_name,
                                            method_ty: static_signature,
                                            args: static_args,
                                            dest: effective_dest,
                                        });
                                    } else {
                                        let class_name_opt = class_type
                                            .get_class_name()
                                            .map(|s| s.to_string())
                                            .or_else(|| {
                                                Some(format!(
                                                    "org/rustlang/primitives/{}",
                                                    jvm_names::path_segment(&format!(
                                                        "{:?}",
                                                        class_type
                                                    ))
                                                ))
                                            });

                                        let class_name = class_name_opt.unwrap();
                                        let mut virtual_method_signature = method_signature;
                                        if method_name == "eq" {
                                            if let Some((_name, self_ty)) =
                                                virtual_method_signature.params.get_mut(0)
                                            {
                                                *self_ty = oomir::Type::Class(class_name.clone());
                                            }
                                            if let Some((_name, other_ty)) =
                                                virtual_method_signature.params.get_mut(1)
                                            {
                                                *other_ty = oomir::Type::Class(class_name.clone());
                                            }
                                        }

                                        instructions.push(oomir::Instruction::InvokeVirtual {
                                            class_name,
                                            method_name,
                                            method_ty: virtual_method_signature,
                                            args: method_args,
                                            dest: effective_dest,
                                            operand: receiver_operand,
                                        });
                                    }
                                }
                            }
                        } else {
                            let method_name = super::naming::associated_method_name_from_instance(
                                tcx,
                                func_instance,
                            );
                            method_signature.is_static = true;

                            let self_ty_opt = if let Some(impl_def_id) = item.impl_container(tcx) {
                                Some(
                                    tcx.type_of(impl_def_id)
                                        .instantiate(tcx, func_instance.args)
                                        .skip_norm_wip(),
                                )
                            } else {
                                func_instance.args.types().next()
                            };

                            let mut generated = false;
                            if item.trait_container(tcx).is_none()
                                && let Some(self_ty) = self_ty_opt
                            {
                                let class_type = super::types::ty_to_oomir_type(
                                    self_ty,
                                    tcx,
                                    data_types,
                                    func_instance,
                                );

                                if class_type == oomir::Type::F16
                                    && method_name == "from_bits"
                                    && oomir_operands.len() == 1
                                {
                                    if let Some(dest) = effective_dest.clone() {
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            dest: Some(dest),
                                            class_name: "org/rustlang/runtime/Numbers".to_string(),
                                            method_name: "f16FromBits".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![(
                                                    "bits".to_string(),
                                                    oomir::Type::U16,
                                                )],
                                                ret: Box::new(oomir::Type::F16),
                                                is_static: true,
                                            },
                                            args: vec![oomir_operands[0].clone()],
                                        });
                                    }
                                    generated = true;
                                }

                                if !generated && let Some(class_name) = class_type.get_class_name()
                                {
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        class_name: class_name.to_string(),
                                        method_name: method_name.clone(),
                                        method_ty: method_signature.clone(),
                                        args: oomir_operands.clone(),
                                        dest: effective_dest.clone(), // use effective_dest
                                    });
                                    generated = true;
                                }
                            }

                            if !generated {
                                let fn_name_data =
                                    super::naming::mono_fn_name_from_instance(tcx, func_instance);
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: fn_name_data
                                        .class_to_call_on
                                        .expect("monomorphized functions have JVM owners"),
                                    method_name: fn_name_data.method_name,
                                    method_ty: method_signature.clone(),
                                    args: oomir_operands.clone(),
                                    dest: effective_dest, // use effective_dest
                                });
                            }
                        }
                    } else {
                        let called_def_id = func_instance.def_id();
                        let intrinsic_name = tcx
                            .opt_item_name(called_def_id)
                            .map(|name| name.as_str().to_string())
                            .unwrap_or_default();
                        let is_compiler_intrinsic =
                            matches!(func_instance.def, InstanceKind::Intrinsic(_));
                        let is_diagnostic_item =
                            |diagnostic| tcx.is_diagnostic_item(diagnostic, called_def_id);
                        let has_diagnostic_item = |diagnostic: &str| {
                            tcx.is_diagnostic_item(Symbol::intern(diagnostic), called_def_id)
                        };
                        let is_external_intrinsic = !called_def_id.is_local();
                        let is_core_ptr = is_external_intrinsic
                            && oomir_operands.first().is_some_and(|operand| {
                                matches!(
                                    operand.get_type(),
                                    Some(
                                        oomir::Type::Pointer(_)
                                            | oomir::Type::Slice(_)
                                            | oomir::Type::Str
                                    )
                                )
                            });
                        let is_size_of = (is_compiler_intrinsic
                            && intrinsic_name.as_str() == "size_of")
                            || has_diagnostic_item("mem_size_of");
                        let is_align_of = (is_compiler_intrinsic
                            && intrinsic_name.as_str() == "align_of")
                            || has_diagnostic_item("mem_align_of");
                        let is_size_of_val = (is_compiler_intrinsic
                            && intrinsic_name.as_str() == "size_of_val")
                            || has_diagnostic_item("mem_size_of_val");
                        if intrinsic_name.as_str() == "caller_location"
                            && is_compiler_intrinsic
                            && let Some(dest) = effective_dest.clone()
                        {
                            let location = caller_location_operand(
                                terminator.source_info,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                                &format!("{label}_caller_location_intrinsic"),
                            );
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: location,
                            });
                        } else if (is_size_of || is_align_of)
                            && let Some(dest) = effective_dest.clone()
                        {
                            let measured_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("size/alignment intrinsic has a type argument");
                            let layout = tcx
                                .layout_of(
                                    TypingEnv::fully_monomorphized()
                                        .as_query_input(measured_ty),
                                )
                                .unwrap_or_else(|error| {
                                    panic!(
                                        "could not determine layout for {intrinsic_name}::<{measured_ty:?}>: {error:?}"
                                    )
                                });
                            let value = if is_size_of {
                                layout.size.bytes()
                            } else {
                                layout.align.abi.bytes()
                            };
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: oomir::Operand::Constant(oomir::Constant::I32(
                                    i32::try_from(value)
                                        .expect("Rust layout value exceeds the JVM usize carrier"),
                                )),
                            });
                        } else if is_size_of_val && let Some(dest) = effective_dest.clone() {
                            let measured_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("size_of_val intrinsic has a type argument");
                            match measured_ty.kind() {
                                TyKind::Slice(element_ty) => {
                                    let element_size =
                                        super::types::layout_size_bytes(tcx, *element_ty)
                                            .unwrap_or_else(|error| {
                                                panic!(
                                                    "could not determine size_of_val slice element layout: {error}"
                                                )
                                            });
                                    let length_name = format!("{label}_size_of_val_length");
                                    instructions.push(oomir::Instruction::Length {
                                        dest: length_name.clone(),
                                        array: oomir_operands[0].clone(),
                                    });
                                    instructions.push(oomir::Instruction::Mul {
                                        dest,
                                        op1: oomir::Operand::Variable {
                                            name: length_name,
                                            ty: oomir::Type::I32,
                                        },
                                        op2: oomir::Operand::Constant(oomir::Constant::I32(
                                            i32::try_from(element_size).expect(
                                                "slice element exceeds the JVM address space",
                                            ),
                                        )),
                                    });
                                }
                                TyKind::Str => {
                                    instructions.push(oomir::Instruction::Length {
                                        dest,
                                        array: oomir_operands[0].clone(),
                                    });
                                }
                                _ => {
                                    let size = super::types::layout_size_bytes(tcx, measured_ty)
                                        .unwrap_or_else(|error| {
                                            panic!(
                                                "could not determine size_of_val layout: {error}"
                                            )
                                        });
                                    instructions.push(oomir::Instruction::Move {
                                        dest,
                                        src: oomir::Operand::Constant(oomir::Constant::I32(
                                            i32::try_from(size).expect(
                                                "size_of_val result exceeds the JVM address space",
                                            ),
                                        )),
                                    });
                                }
                            }
                        } else if intrinsic_name.as_str() == "type_id_eq"
                            && is_compiler_intrinsic
                            && oomir_operands.len() == 2
                            && let Some(dest) = effective_dest.clone()
                        {
                            let type_id_ty = oomir_operands[0]
                                .get_type()
                                .expect("TypeId equality operands are typed");
                            let oomir::Type::Class(type_id_class) = &type_id_ty else {
                                panic!("type_id_eq received non-class operand {type_id_ty:?}");
                            };
                            let data_ty = match data_types.get(type_id_class) {
                                Some(oomir::DataType::Class { fields, .. }) => fields
                                    .iter()
                                    .find(|(name, _)| name == "data")
                                    .map(|(_, ty)| ty.clone())
                                    .expect("TypeId class has a data field"),
                                _ => panic!("TypeId class {type_id_class} is not defined"),
                            };
                            let oomir::Type::Array(pointer_ty) = &data_ty else {
                                panic!("TypeId data field is not an array: {data_ty:?}");
                            };
                            let left_data = format!("{label}_type_id_left_data");
                            let right_data = format!("{label}_type_id_right_data");
                            instructions.push(oomir::Instruction::GetField {
                                dest: left_data.clone(),
                                object: oomir_operands[0].clone(),
                                field_name: "data".to_string(),
                                field_ty: data_ty.clone(),
                                owner_class: type_id_class.clone(),
                            });
                            instructions.push(oomir::Instruction::GetField {
                                dest: right_data.clone(),
                                object: oomir_operands[1].clone(),
                                field_name: "data".to_string(),
                                field_ty: data_ty.clone(),
                                owner_class: type_id_class.clone(),
                            });

                            let limb_count = 16usize
                                / usize::try_from(tcx.data_layout.pointer_size().bytes())
                                    .expect("pointer size fits usize");
                            let mut equality = None;
                            for limb in 0..limb_count {
                                let left_limb = format!("{label}_type_id_left_{limb}");
                                let right_limb = format!("{label}_type_id_right_{limb}");
                                let limb_equal = format!("{label}_type_id_equal_{limb}");
                                let index =
                                    oomir::Operand::Constant(oomir::Constant::I32(limb as i32));
                                instructions.push(oomir::Instruction::ArrayGet {
                                    dest: left_limb.clone(),
                                    array: oomir::Operand::Variable {
                                        name: left_data.clone(),
                                        ty: data_ty.clone(),
                                    },
                                    index: index.clone(),
                                });
                                instructions.push(oomir::Instruction::ArrayGet {
                                    dest: right_limb.clone(),
                                    array: oomir::Operand::Variable {
                                        name: right_data.clone(),
                                        ty: data_ty.clone(),
                                    },
                                    index,
                                });
                                instructions.push(oomir::Instruction::InvokeVirtual {
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "sameAddress".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("self".to_string(), pointer_ty.as_ref().clone()),
                                            ("other".to_string(), pointer_ty.as_ref().clone()),
                                        ],
                                        ret: Box::new(oomir::Type::Boolean),
                                        is_static: false,
                                    },
                                    args: vec![oomir::Operand::Variable {
                                        name: right_limb,
                                        ty: pointer_ty.as_ref().clone(),
                                    }],
                                    dest: Some(limb_equal.clone()),
                                    operand: oomir::Operand::Variable {
                                        name: left_limb,
                                        ty: pointer_ty.as_ref().clone(),
                                    },
                                });
                                let limb_equal = oomir::Operand::Variable {
                                    name: limb_equal,
                                    ty: oomir::Type::Boolean,
                                };
                                equality = Some(if let Some(previous) = equality {
                                    let combined = format!("{label}_type_id_combined_{limb}");
                                    instructions.push(oomir::Instruction::BitAnd {
                                        dest: combined.clone(),
                                        op1: previous,
                                        op2: limb_equal,
                                    });
                                    oomir::Operand::Variable {
                                        name: combined,
                                        ty: oomir::Type::Boolean,
                                    }
                                } else {
                                    limb_equal
                                });
                            }
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: equality.unwrap_or(oomir::Operand::Constant(
                                    oomir::Constant::Boolean(true),
                                )),
                            });
                        } else if intrinsic_name.as_str() == "forget" && is_external_intrinsic {
                            // `mem::forget` intentionally consumes its argument without
                            // running drop glue. The value is already represented by the
                            // evaluated call operand, so there is no JVM instruction to emit.
                        } else if has_diagnostic_item("ptr_drop_in_place") && is_core_ptr {
                            let pointee_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("drop_in_place has a pointee type argument");
                            let pointee_oomir_ty = super::types::ty_to_oomir_type(
                                pointee_ty, tcx, data_types, instance,
                            );
                            let value_name = format!("{label}_drop_in_place_value");
                            let value = super::place::emit_pointer_read(
                                oomir_operands[0].clone(),
                                &pointee_oomir_ty,
                                &value_name,
                                &mut instructions,
                            );
                            emit_rust_drop_value(
                                pointee_ty,
                                value,
                                &format!("{label}_drop_in_place"),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                        } else if (is_diagnostic_item(sym::ptr_from_ref)
                            || intrinsic_name.as_str() == "from_mut")
                            && is_core_ptr
                        {
                            if let Some(dest) = effective_dest.clone() {
                                instructions.push(oomir::Instruction::Move {
                                    dest,
                                    src: oomir_operands[0].clone(),
                                });
                            }
                        } else if (has_diagnostic_item("ptr_eq")
                            || intrinsic_name.as_str() == "addr_eq")
                            && is_core_ptr
                            && oomir_operands.len() == 2
                        {
                            let first_ty = oomir_operands[0]
                                .get_type()
                                .expect("pointer equality operand is typed");
                            let second_ty = oomir_operands[1]
                                .get_type()
                                .expect("pointer equality operand is typed");
                            instructions.push(oomir::Instruction::InvokeVirtual {
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "sameAddress".to_string(),
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
                        } else if matches!(intrinsic_name.as_str(), "dangling" | "dangling_mut")
                            && is_external_intrinsic
                            && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
                        {
                            let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
                                unreachable!()
                            };
                            let pointee_size = super::types::layout_size_bytes(tcx, *pointee)
                                .unwrap_or_else(|error| {
                                    panic!("could not determine dangling pointer size: {error}")
                                });
                            let pointee_alignment = super::types::layout_align_bytes(tcx, *pointee)
                                .unwrap_or_else(|error| {
                                    panic!(
                                        "could not determine dangling pointer alignment: {error}"
                                    )
                                });
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: effective_dest.clone(),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "withoutProvenance".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("address".to_string(), oomir::Type::U64),
                                        ("view_size".to_string(), oomir::Type::I32),
                                    ],
                                    ret: method_signature.ret.clone(),
                                    is_static: true,
                                },
                                args: vec![
                                    oomir::Operand::Constant(oomir::Constant::U64(
                                        pointee_alignment as u64,
                                    )),
                                    oomir::Operand::Constant(oomir::Constant::I32(
                                        i32::try_from(pointee_size)
                                            .expect("dangling pointer size exceeds JVM integer"),
                                    )),
                                ],
                            });
                        } else if (has_diagnostic_item("ptr_slice_from_raw_parts")
                            || has_diagnostic_item("ptr_slice_from_raw_parts_mut")
                            || matches!(
                                intrinsic_name.as_str(),
                                "from_raw_parts" | "from_raw_parts_mut"
                            ))
                            && is_core_ptr
                            && matches!(oomir_output_type, oomir::Type::Slice(_) | oomir::Type::Str)
                        {
                            let data = oomir_operands[0].clone();
                            if let Some(dest) = effective_dest.clone() {
                                let is_str = matches!(oomir_output_type, oomir::Type::Str);
                                let view_class = if is_str {
                                    oomir::UTF8_VIEW_CLASS
                                } else {
                                    oomir::SLICE_VIEW_CLASS
                                };
                                let (backing, offset) = if is_str {
                                    let data_ty =
                                        data.get_type().expect("raw str data pointer is typed");
                                    let backing_name = format!("{label}_raw_str_backing");
                                    instructions.push(oomir::Instruction::InvokeVirtual {
                                        dest: Some(backing_name.clone()),
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "sliceBackingArray".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![("self".to_string(), data_ty.clone())],
                                            ret: Box::new(oomir::Type::Class(
                                                "java/lang/Object".to_string(),
                                            )),
                                            is_static: false,
                                        },
                                        args: Vec::new(),
                                        operand: data.clone(),
                                    });
                                    let offset_name = format!("{label}_raw_str_offset");
                                    instructions.push(oomir::Instruction::InvokeVirtual {
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
                                        oomir::Operand::Variable {
                                            name: backing_name,
                                            ty: oomir::Type::Class("java/lang/Object".to_string()),
                                        },
                                        oomir::Operand::Variable {
                                            name: offset_name,
                                            ty: oomir::Type::I32,
                                        },
                                    )
                                } else {
                                    (data, oomir::Operand::Constant(oomir::Constant::I32(0)))
                                };
                                let slice_object = format!("{label}_raw_slice_object");
                                instructions.push(oomir::Instruction::ConstructObject {
                                    dest: slice_object.clone(),
                                    class_name: view_class.to_string(),
                                    args: vec![
                                        (
                                            backing,
                                            oomir::Type::Class("java/lang/Object".to_string()),
                                        ),
                                        (offset, oomir::Type::I32),
                                        (oomir_operands[1].clone(), oomir::Type::I32),
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
                        } else if matches!(intrinsic_name.as_str(), "metadata" | "ptr_metadata")
                            && is_core_ptr
                            && (matches!(
                                oomir_operands[0].get_type(),
                                Some(oomir::Type::Slice(_) | oomir::Type::Str)
                            ) || fn_inputs.first().is_some_and(|input| {
                                matches!(
                                    input.kind(),
                                    TyKind::RawPtr(pointee, _)
                                        if pointee.is_slice() || pointee.is_str()
                                )
                            }))
                        {
                            if let Some(dest) = effective_dest.clone() {
                                let length_dest = if oomir_output_type == oomir::Type::I32 {
                                    dest.clone()
                                } else {
                                    format!("{dest}_slice_metadata_i32")
                                };
                                instructions.push(oomir::Instruction::GetField {
                                    dest: length_dest.clone(),
                                    object: oomir_operands[0].clone(),
                                    field_name: "length".to_string(),
                                    field_ty: oomir::Type::I32,
                                    owner_class: oomir::SLICE_VIEW_CLASS.to_string(),
                                });
                                if oomir_output_type != oomir::Type::I32 {
                                    instructions.push(oomir::Instruction::Cast {
                                        op: oomir::Operand::Variable {
                                            name: length_dest,
                                            ty: oomir::Type::I32,
                                        },
                                        ty: oomir_output_type.clone(),
                                        dest,
                                    });
                                }
                            }
                        } else if matches!(intrinsic_name.as_str(), "metadata" | "ptr_metadata")
                            && is_core_ptr
                            && matches!(oomir_operands[0].get_type(), Some(oomir::Type::Pointer(_)))
                        {
                            if let Some(dest) = effective_dest.clone() {
                                let pointee = fn_inputs.first().and_then(|input| {
                                    let (TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)) =
                                        input.kind()
                                    else {
                                        return None;
                                    };
                                    Some(*pointee)
                                });
                                let slice_tailed_pointee = pointee.and_then(|pointee| {
                                    let tail = tcx.struct_tail_for_codegen(
                                        pointee,
                                        TypingEnv::fully_monomorphized(),
                                    );
                                    (tail.is_slice() || tail.is_str()).then_some(pointee)
                                });
                                if slice_tailed_pointee.is_some() {
                                    let pointer_ty = oomir_operands[0]
                                        .get_type()
                                        .expect("DST metadata operand must be typed");
                                    instructions.push(oomir::Instruction::InvokeVirtual {
                                        dest: Some(dest),
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "metadata".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![("self".to_string(), pointer_ty)],
                                            ret: Box::new(oomir_output_type.clone()),
                                            is_static: false,
                                        },
                                        args: Vec::new(),
                                        operand: oomir_operands[0].clone(),
                                    });
                                } else if pointee.is_some_and(|pointee| {
                                    matches!(pointee.kind(), TyKind::Dynamic(..))
                                }) {
                                    emit_trait_object_metadata(
                                        oomir_operands[0].clone(),
                                        &oomir_output_type,
                                        dest,
                                        &format!("{label}_trait_metadata"),
                                        data_types,
                                        &mut instructions,
                                    );
                                } else {
                                    instructions.push(oomir::Instruction::Move {
                                        dest,
                                        src: oomir::Operand::Constant(oomir::Constant::Null(
                                            oomir_output_type.clone(),
                                        )),
                                    });
                                }
                            }
                        } else if matches!(
                            intrinsic_name.as_str(),
                            "from_raw_parts" | "from_raw_parts_mut"
                        ) && is_core_ptr
                            && matches!(oomir_output_type, oomir::Type::Pointer(_))
                        {
                            let pointee = match fn_output.kind() {
                                TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
                                other => {
                                    panic!("from_raw_parts returned non-pointer type {other:?}")
                                }
                            };
                            let source_ty = oomir_operands[0]
                                .get_type()
                                .expect("from_raw_parts data pointer is typed");
                            if matches!(pointee.kind(), TyKind::Dynamic(_, _)) {
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: effective_dest.clone(),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "restoreAllocationView".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![("pointer".to_string(), source_ty)],
                                        ret: method_signature.ret.clone(),
                                        is_static: true,
                                    },
                                    args: vec![oomir_operands[0].clone()],
                                });
                            } else {
                                let pointee_size = super::types::layout_size_bytes(tcx, pointee)
                                    .unwrap_or_else(|error| {
                                        panic!(
                                            "could not determine from_raw_parts pointee size: {error}"
                                        )
                                    });
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: effective_dest.clone(),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "retype".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("pointer".to_string(), source_ty),
                                            ("view_size".to_string(), oomir::Type::I32),
                                            ("view_codec".to_string(), oomir::Type::String),
                                        ],
                                        ret: method_signature.ret.clone(),
                                        is_static: true,
                                    },
                                    args: vec![
                                        oomir_operands[0].clone(),
                                        oomir::Operand::Constant(oomir::Constant::I32(
                                            i32::try_from(pointee_size).expect(
                                                "from_raw_parts pointee exceeds JVM address space",
                                            ),
                                        )),
                                        super::types::pointer_view_codec_operand(
                                            pointee, tcx, data_types, instance,
                                        ),
                                    ],
                                });
                            }
                        } else if (is_diagnostic_item(sym::ptr_null)
                            || is_diagnostic_item(sym::ptr_null_mut))
                            && is_external_intrinsic
                            && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
                        {
                            let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
                                unreachable!()
                            };
                            let pointee_size = super::types::layout_size_bytes(tcx, *pointee)
                                .unwrap_or_else(|error| {
                                    panic!("could not determine null pointer target size: {error}")
                                });
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: effective_dest.clone(),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "nullPointer".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![("view_size".to_string(), oomir::Type::I32)],
                                    ret: method_signature.ret.clone(),
                                    is_static: true,
                                },
                                args: vec![oomir::Operand::Constant(oomir::Constant::I32(
                                    i32::try_from(pointee_size)
                                        .expect("null pointer target exceeds JVM address space"),
                                ))],
                            });
                        } else if (has_diagnostic_item("ptr_without_provenance")
                            || has_diagnostic_item("ptr_without_provenance_mut")
                            || matches!(
                                intrinsic_name.as_str(),
                                "with_exposed_provenance" | "with_exposed_provenance_mut"
                            ))
                            && is_external_intrinsic
                            && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
                        {
                            let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
                                unreachable!()
                            };
                            let pointee_size = super::types::layout_size_bytes(tcx, *pointee)
                                .unwrap_or_else(|error| {
                                    panic!(
                                        "could not determine provenance pointer target size: {error}"
                                    )
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
                                        ("view_size".to_string(), oomir::Type::I32),
                                        ("view_codec".to_string(), oomir::Type::String),
                                    ],
                                    ret: method_signature.ret.clone(),
                                    is_static: true,
                                },
                                args: vec![
                                    oomir_operands[0].clone(),
                                    oomir::Operand::Constant(oomir::Constant::I32(
                                        i32::try_from(pointee_size).expect(
                                            "provenance pointer target exceeds JVM address space",
                                        ),
                                    )),
                                    super::types::pointer_view_codec_operand(
                                        *pointee, tcx, data_types, instance,
                                    ),
                                ],
                            });
                        } else if (is_diagnostic_item(sym::ptr_read)
                            || is_diagnostic_item(sym::ptr_read_unaligned)
                            || has_diagnostic_item("ptr_read_volatile")
                            || (is_compiler_intrinsic
                                && matches!(
                                    intrinsic_name.as_str(),
                                    "volatile_load" | "unaligned_volatile_load"
                                )))
                            && is_core_ptr
                        {
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
                                && let Some(oomir::Type::Pointer(pointee)) =
                                    oomir_operands[0].get_type()
                            {
                                super::place::emit_pointer_read(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    &dest,
                                    &mut instructions,
                                );
                            }
                        } else if (is_diagnostic_item(sym::ptr_write)
                            || is_diagnostic_item(sym::ptr_write_unaligned)
                            || is_diagnostic_item(sym::ptr_write_volatile)
                            || (is_compiler_intrinsic
                                && matches!(
                                    intrinsic_name.as_str(),
                                    "volatile_store" | "unaligned_volatile_store"
                                )))
                            && is_core_ptr
                        {
                            if let Some(oomir::Type::Pointer(pointee)) =
                                oomir_operands[0].get_type()
                            {
                                super::place::emit_pointer_write(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    oomir_operands[1].clone(),
                                    &mut instructions,
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
                        } else if is_diagnostic_item(sym::ptr_replace) && is_core_ptr {
                            if let Some(oomir::Type::Pointer(pointee)) =
                                oomir_operands[0].get_type()
                            {
                                if let Some(dest) = effective_dest.clone() {
                                    super::place::emit_pointer_read(
                                        oomir_operands[0].clone(),
                                        pointee.as_ref(),
                                        &dest,
                                        &mut instructions,
                                    );
                                }
                                super::place::emit_pointer_write(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    oomir_operands[1].clone(),
                                    &mut instructions,
                                );
                            }
                        // This private `core` helper has no diagnostic item. It is reached by
                        // optimized `ptr::swap_nonoverlapping` for dynamically sized byte counts.
                        } else if intrinsic_name.as_str() == "swap_nonoverlapping_bytes"
                            && is_core_ptr
                            && oomir_operands.len() == 3
                        {
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
                        } else if is_diagnostic_item(sym::ptr_swap) && is_core_ptr {
                            if let Some(oomir::Type::Pointer(pointee)) =
                                oomir_operands[0].get_type()
                            {
                                let left_name = format!("{}_swap_left", label);
                                let right_name = format!("{}_swap_right", label);
                                let left = super::place::emit_pointer_read(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    &left_name,
                                    &mut instructions,
                                );
                                let right = super::place::emit_pointer_read(
                                    oomir_operands[1].clone(),
                                    pointee.as_ref(),
                                    &right_name,
                                    &mut instructions,
                                );
                                super::place::emit_pointer_write(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    right,
                                    &mut instructions,
                                );
                                super::place::emit_pointer_write(
                                    oomir_operands[1].clone(),
                                    pointee.as_ref(),
                                    left,
                                    &mut instructions,
                                );
                            }
                        } else if is_diagnostic_item(sym::ptr_swap_nonoverlapping) && is_core_ptr {
                            let element_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("swap_nonoverlapping has a type argument");
                            let (method_name, count) = if let Ok(element_size) =
                                super::types::layout_size_bytes(tcx, element_ty)
                            {
                                let byte_count_name =
                                    format!("{label}_swap_nonoverlapping_byte_count");
                                instructions.push(oomir::Instruction::Mul {
                                    dest: byte_count_name.clone(),
                                    op1: oomir_operands[2].clone(),
                                    op2: oomir::Operand::Constant(oomir::Constant::U64(
                                        element_size as u64,
                                    )),
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
                                args: vec![
                                    oomir_operands[0].clone(),
                                    oomir_operands[1].clone(),
                                    count,
                                ],
                                dest: None,
                            });
                        } else if (is_diagnostic_item(sym::ptr_copy)
                            || is_diagnostic_item(sym::ptr_copy_nonoverlapping)
                            || is_diagnostic_item(sym::ptr_write_bytes)
                            || (is_compiler_intrinsic
                                && matches!(
                                    intrinsic_name.as_str(),
                                    "copy" | "copy_nonoverlapping" | "write_bytes"
                                )))
                            && is_core_ptr
                        {
                            let writes_bytes = is_diagnostic_item(sym::ptr_write_bytes)
                                || (is_compiler_intrinsic
                                    && intrinsic_name.as_str() == "write_bytes");
                            let nonoverlapping = is_diagnostic_item(sym::ptr_copy_nonoverlapping)
                                || (is_compiler_intrinsic
                                    && intrinsic_name.as_str() == "copy_nonoverlapping");
                            let element_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("pointer memory intrinsic has a type argument");
                            let count = oomir_operands
                                .last()
                                .cloned()
                                .expect("pointer memory intrinsic has a count argument");
                            let (uses_runtime_element_size, count) = if let Ok(element_size) =
                                super::types::layout_size_bytes(tcx, element_ty)
                            {
                                let byte_count_name = format!("{label}_pointer_memory_byte_count");
                                instructions.push(oomir::Instruction::Mul {
                                    dest: byte_count_name.clone(),
                                    op1: count,
                                    op2: oomir::Operand::Constant(oomir::Constant::U64(
                                        element_size as u64,
                                    )),
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
                                .expect(
                                    "pointer memory intrinsic has a destination/source pointer",
                                );
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
                                    args: vec![
                                        oomir_operands[0].clone(),
                                        oomir_operands[1].clone(),
                                        count,
                                    ],
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
                                    args: vec![
                                        oomir_operands[0].clone(),
                                        oomir_operands[1].clone(),
                                        count,
                                    ],
                                    dest: None,
                                });
                            }
                        } else {
                            let is_closure_call = matches!(instance_ty.kind(), TyKind::Closure(..));
                            let closure_has_captures = matches!(
                                instance_ty.kind(),
                                TyKind::Closure(_, args)
                                    if args.as_closure().upvar_tys().iter().next().is_some()
                            );
                            let (class_name, function) = if is_closure_call {
                                (
                                    super::jvm_names::crate_module_class(
                                        tcx,
                                        func_instance.def_id().krate,
                                    ),
                                    super::generate_closure_function_name(tcx, func_instance),
                                )
                            } else {
                                let fn_name_data =
                                    super::naming::mono_fn_name_from_instance(tcx, func_instance);
                                (
                                    fn_name_data
                                        .class_to_call_on
                                        .expect("monomorphized functions have JVM owners"),
                                    fn_name_data.method_name,
                                )
                            };
                            let call_args = if is_closure_call && !oomir_operands.is_empty() {
                                if closure_has_captures {
                                    oomir_operands.clone()
                                } else {
                                    oomir_operands[1..].to_vec()
                                }
                            } else {
                                oomir_operands.clone()
                            };
                            if closure_has_captures {
                                let closure_env_ty = oomir_operands
                                    .first()
                                    .and_then(oomir::Operand::get_type)
                                    .expect("capturing closure calls have an environment operand");
                                method_signature
                                    .params
                                    .insert(0, ("closure_env".to_string(), closure_env_ty));
                            }
                            method_signature.is_static = true;

                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name,
                                method_name: function,
                                method_ty: method_signature,
                                args: call_args,
                                dest: effective_dest, // use effective_dest
                            });
                        }
                    }
                } else {
                    let indirect_sig = func_ty.fn_sig(tcx).skip_binder();
                    for (index, target_ty) in indirect_sig.inputs().iter().enumerate() {
                        let Some(source) = oomir_operands.get(index).cloned() else {
                            break;
                        };
                        oomir_operands[index] = super::value_repr::adapt_operand_to_rust_type(
                            source,
                            *target_ty,
                            &format!("{}_indirect_arg_{}", label, index),
                            tcx,
                            instance,
                            data_types,
                            &mut instructions,
                        );
                    }
                    let func_oomir_operand =
                        convert_operand(&func, tcx, instance, mir, data_types, &mut instructions);

                    let oomir_sig =
                        super::types::fn_ptr_signature_from_ty(func_ty, tcx, data_types, instance);
                    super::types::ensure_fn_ptr_interface(&oomir_sig, data_types, tcx, instance);

                    let effective_dest = if !oomir_sig.ret.has_jvm_value() {
                        None
                    } else {
                        dest_var_name.clone()
                    };

                    instructions.push(oomir::Instruction::CallIndirect {
                        dest: effective_dest,
                        function_ptr: Box::new(func_oomir_operand),
                        args: oomir_operands.clone(),
                        signature: oomir_sig,
                    });
                }

                if deferred_destination_store && let Some(result_name) = dest_var_name {
                    instructions.extend(emit_instructions_to_set_value(
                        destination,
                        oomir::Operand::Variable {
                            name: result_name,
                            ty: destination_oomir_type,
                        },
                        tcx,
                        instance,
                        mir,
                        data_types,
                    ));
                }

                let destination_ty = destination.ty(&mir.local_decls, tcx).ty;
                if destination.projection.is_empty()
                    && matches!(
                        destination_ty.kind(),
                        TyKind::Ref(_, _, mutability) if mutability.is_mut()
                    )
                {
                    let aliases = args
                        .iter()
                        .filter_map(|arg| match &arg.node {
                            MirOperand::Move(place) | MirOperand::Copy(place)
                                if place.projection.is_empty() =>
                            {
                                mutable_borrow_arrays.get(&place.local).cloned()
                            }
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    if let [alias] = aliases.as_slice() {
                        mutable_borrow_arrays.insert(
                            destination.local,
                            PointerOrigin {
                                original_place: alias.original_place.clone(),
                                carrier_name: super::place::place_to_string(destination, tcx),
                                pointee_type: alias.pointee_type.clone(),
                                writable: alias.writable,
                            },
                        );
                        initialized_borrows.insert(destination.local);
                    }
                }

                let mut writeback_locals = initialized_borrows.clone();
                for arg in args {
                    if let MirOperand::Move(place) | MirOperand::Copy(place) = &arg.node
                        && place.projection.is_empty()
                        && mutable_borrow_arrays.contains_key(&place.local)
                    {
                        writeback_locals.insert(place.local);
                    }
                }
                let mut writeback_locals = writeback_locals.into_iter().collect::<Vec<_>>();
                writeback_locals.sort_by_key(|local| local.index());
                instructions.extend(emit_selected_mutable_borrow_writebacks(
                    writeback_locals,
                    mutable_borrow_arrays,
                    tcx,
                    instance,
                    mir,
                    data_types,
                ));

                if let Some(target_bb) = target {
                    let target_label = format!("bb{}", target_bb.index());
                    instructions.push(oomir::Instruction::Jump {
                        target: target_label,
                    });
                } else {
                    instructions.push(oomir::Instruction::ThrowNewWithMessage {
                        exception_class: "java/lang/AssertionError".to_string(),
                        message: "Diverging Rust call returned unexpectedly".to_string(),
                    });
                }
            }
            TerminatorKind::Assert {
                target,
                cond,
                expected,
                msg,
                unwind: _,
            } => {
                let condition_operand: oomir::Operand;

                // Check if the condition operand is a direct use of a place (Copy or Move)
                let condition_place_opt = match cond {
                    MirOperand::Copy(place) | MirOperand::Move(place) => Some(place),
                    _ => None, // If it's a constant, handle directly
                };

                if let Some(place) = condition_place_opt {
                    // Now, check if this place has a field projection
                    let (temp_dest, instrs, field_oomir_type) =
                        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                    instructions.extend(instrs);
                    // Use the temporary variable as the condition operand
                    condition_operand = oomir::Operand::Variable {
                        name: temp_dest.clone(),
                        ty: field_oomir_type,
                    };
                } else {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!("Info: Assert condition uses constant operand {:?}", cond)
                    );
                    // Condition is likely a constant itself
                    condition_operand =
                        convert_operand(cond, tcx, instance, mir, data_types, &mut instructions);
                }

                // The MIR assert checks `!cond == expected`. Rust asserts check `cond == expected`.
                // Standard Rust `assert!(expr)` lowers to MIR `assert(expr, expected: true, ...)`
                // Standard Rust `assert_eq!(a,b)` might lower differently, but `assert!(a==b)` lowers like above.
                // The `checked_add` MIR uses `assert(!move (_7.1: bool), expected: true, ...)` effectively meaning "panic if _7.1 is true".
                // So, we need to check if `condition_operand == *expected`.

                // Generate a comparison instruction to check if the *actual condition value*
                // matches the expected boolean value.
                let comparison_dest = format!("assert_cmp_{}", bb.index()); // e.g., assert_cmp_3

                // Handle potential negation: MIR `assert(!cond)` means panic if `cond` is true.
                // MIR `assert(cond)` means panic if `cond` is false.
                // The `expected` field tells us what the non-panic value should be.
                // We want to branch to the failure block if `condition_operand != expected`.

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!(
                        "Info: Generating Assert comparison: '{}' = ({:?}) == {:?}",
                        comparison_dest, condition_operand, *expected
                    )
                );

                instructions.push(oomir::Instruction::Eq {
                    dest: comparison_dest.clone(),
                    op1: condition_operand, // Use the potentially GetField'd value
                    op2: oomir::Operand::Constant(oomir::Constant::Boolean(*expected)),
                });

                // Generate a branch based on the comparison result
                let success_block = format!("bb{}", target.index()); // Success path
                let failure_block = format!("assert_fail_{}", bb.index()); // Failure path label

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!(
                        "Info: Generating Assert branch: if '{}' == true goto {} else goto {}",
                        comparison_dest, success_block, failure_block
                    )
                );

                instructions.push(oomir::Instruction::Branch {
                    condition: oomir::Operand::Variable {
                        name: comparison_dest, // Use the result of the Eq comparison
                        ty: oomir::Type::Boolean,
                    },
                    true_block: success_block, // Jump here if condition == expected (assertion holds)
                    false_block: failure_block.clone(), // Jump here if assertion fails
                });

                // Extract the message. msg is an AssertMessage.
                // We need to handle different kinds of AssertMessage.
                let panic_message = match &**msg {
                    rustc_middle::mir::AssertKind::BoundsCheck { len, index } => {
                        // TODO: More sophisticated message generation using len/index operands later
                        format!("BoundsCheck failed (len: {:?}, index: {:?})", len, index)
                    }
                    rustc_middle::mir::AssertKind::Overflow(op, l, r) => {
                        // TODO: Convert l and r operands to strings if possible later
                        format!("Overflow({:?}, {:?}, {:?})", op, l, r)
                    }
                    rustc_middle::mir::AssertKind::OverflowNeg(op) => {
                        format!("OverflowNeg({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::DivisionByZero(op) => {
                        format!("DivisionByZero({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::RemainderByZero(op) => {
                        format!("RemainderByZero({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::ResumedAfterReturn(_) => {
                        "ResumedAfterReturn".to_string()
                    }
                    rustc_middle::mir::AssertKind::ResumedAfterPanic(_) => {
                        "ResumedAfterPanic".to_string()
                    }
                    rustc_middle::mir::AssertKind::MisalignedPointerDereference {
                        required,
                        found,
                    } => {
                        format!(
                            "MisalignedPointerDereference (required: {:?}, found: {:?})",
                            required, found
                        )
                    }
                    rustc_middle::mir::AssertKind::NullPointerDereference => {
                        "NullPointerDereference".to_string()
                    }
                    rustc_middle::mir::AssertKind::NullReferenceConstructed => {
                        "NullReferenceConstructed".to_string()
                    }
                    rustc_middle::mir::AssertKind::ResumedAfterDrop(_) => {
                        "ResumedAfterDrop".to_string()
                    }
                    rustc_middle::mir::AssertKind::InvalidEnumConstruction(_) => {
                        "InvalidEnumConstruction".to_string()
                    }
                };

                let fail_instructions = vec![oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/RuntimeException".to_string(), // Or ArithmeticException for overflows?
                    message: panic_message,
                }];
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!("Info: Creating failure block '{}'", failure_block)
                );
                basic_blocks.insert(
                    // Ensure 'basic_blocks' map is mutable and passed in
                    failure_block.clone(),
                    oomir::BasicBlock {
                        label: failure_block,
                        instructions: fail_instructions,
                    },
                );
            }
            TerminatorKind::Drop {
                place,
                target,
                unwind: _,
                replace: _,
                drop: _,
            } => {
                let rust_ty = place.ty(&mir.local_decls, tcx).ty;
                if rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    let (value_name, value_instructions, value_ty) =
                        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                    instructions.extend(value_instructions);
                    emit_rust_drop_value(
                        rust_ty,
                        oomir::Operand::Variable {
                            name: value_name,
                            ty: value_ty,
                        },
                        &format!("{label}_drop"),
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    );
                }

                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump {
                    target: target_label,
                });
            }
            TerminatorKind::Unreachable => {
                instructions.push(oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/RuntimeException".to_string(),
                    message: "Unreachable code reached".to_string(),
                });
            }
            TerminatorKind::UnwindResume => {
                // "Resume" implies we are in a cleanup block, we finished cleanup,
                // and now we must continue unwinding (rethrow the exception).
                instructions.push(oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/RuntimeException".to_string(),
                    message: "Panic unwinding resumed.".to_string(),
                });
            }
            // Other terminator kinds will be added as needed.
            _ => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!("Warning: Unhandled terminator {:?}", terminator.kind)
                );
            }
        }
    }

    oomir::BasicBlock {
        label,
        instructions,
    }
}
