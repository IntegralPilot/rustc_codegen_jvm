//! Borrows.
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub(in crate::lower1) struct PointerOrigin<'tcx> {
    pub original_place: Place<'tcx>,
    pub carrier_name: String,
    pub pointee_type: oomir::Type,
    pub writable: bool,
}

pub(in crate::lower1) type MutableBorrowMap<'tcx> = HashMap<Local, PointerOrigin<'tcx>>;

pub(in crate::lower1) fn collect_pointer_origins<'tcx>(
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> MutableBorrowMap<'tcx> {
    let mut origins = MutableBorrowMap::default();
    let assignment_count = mir
        .basic_blocks
        .iter()
        .map(|block| block.statements.len())
        .sum::<usize>();

    for _ in 0..=assignment_count {
        let mut changed = false;
        for block in mir.basic_blocks.iter() {
            for statement in &block.statements {
                let StatementKind::Assign(assignment) = &statement.kind else {
                    continue;
                };
                let (destination, rvalue) = assignment.as_ref();
                if !destination.projection.is_empty() {
                    continue;
                }
                let destination_rust_ty =
                    EarlyBinder::bind(tcx, destination.ty(&mir.local_decls, tcx).ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                if !matches!(
                    destination_rust_ty.kind(),
                    TyKind::Ref(..) | TyKind::RawPtr(..)
                ) {
                    continue;
                }
                let destination_ty = crate::lower1::place::get_place_type(
                    destination,
                    mir,
                    tcx,
                    instance,
                    data_types,
                );
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
                            pointee_ty.as_ref().clone(),
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
                            pointee_ty.as_ref().clone(),
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
                    carrier_name: crate::lower1::place::place_to_string(destination, tcx),
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

pub(in crate::lower1) fn emit_mutable_borrow_writeback<'tcx>(
    borrow_local: Local,
    mutable_borrows: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
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
    let carrier_pointee_ty = origin.pointee_type;
    if data_types.local_uses_stable_cell(original_place.local) {
        // The pointer and all projected field views already share the root
        // allocation. Copying a derived/address-only view back would both be
        // redundant and could overwrite the field with the enclosing object.
        return instructions;
    }
    if let Some((rustc_middle::mir::ProjectionElem::Field(_, _), base_projection)) =
        original_place.projection.split_last()
    {
        let base_place = Place {
            local: original_place.local,
            projection: tcx.mk_place_elems(base_projection),
        };
        let base_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        if matches!(base_ty.kind(), TyKind::Tuple(_))
            || matches!(
                base_ty.kind(),
                TyKind::Adt(adt_def, _) if adt_def.is_struct() || adt_def.is_enum()
            )
        {
            return instructions;
        }
    }
    let storage_rust_ty = EarlyBinder::bind(tcx, original_place.ty(&mir.local_decls, tcx).ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let storage_ty =
        crate::lower1::types::ty_to_oomir_type(storage_rust_ty, tcx, data_types, instance);
    if !storage_ty.has_jvm_value() {
        return instructions;
    }
    let value_name = format!(
        "_writeback_{}_{}",
        borrow_local.index(),
        original_place.local.index()
    );
    let carrier_ty = mutable_borrows
        .get(&borrow_local)
        .map(|origin| oomir::Type::Pointer(Box::new(origin.pointee_type.clone())))
        .unwrap_or_else(|| oomir::Type::Pointer(Box::new(carrier_pointee_ty.clone())));
    let mut carrier = oomir::Operand::Variable {
        name: carrier_name,
        ty: carrier_ty,
    };
    if carrier_pointee_ty != storage_ty {
        let storage_pointer_ty = oomir::Type::Pointer(Box::new(storage_ty.clone()));
        let retyped_name = format!("{value_name}_storage_pointer");
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: Some(retyped_name.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "retype".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), carrier.get_type().unwrap()),
                    ("size".to_string(), oomir::Type::U64),
                    ("codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(storage_pointer_ty.clone()),
                is_static: false,
            },
            args: vec![
                oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(
                        crate::lower1::types::layout_size_bytes(tcx, storage_rust_ty)
                            .unwrap_or_else(|error| {
                                panic!("could not determine pointer write-back layout: {error}")
                            }),
                    )
                    .expect("Rust pointer write-back layout exceeds u64"),
                )),
                crate::lower1::types::pointer_view_codec_operand(
                    storage_rust_ty,
                    tcx,
                    data_types,
                    instance,
                ),
            ],
            operand: carrier,
        });
        carrier = oomir::Operand::Variable {
            name: retyped_name,
            ty: storage_pointer_ty,
        };
    }
    emit_pointer_read(carrier, &storage_ty, &value_name, &mut instructions);
    instructions.extend(emit_instructions_to_set_value(
        &original_place,
        oomir::Operand::Variable {
            name: value_name,
            ty: storage_ty,
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

pub(in crate::lower1) fn emit_pointer_origin_refreshes<'tcx>(
    updated_place: &Place<'tcx>,
    available_pointer_locals: &HashSet<Local>,
    pointer_origins: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> Vec<oomir::Instruction> {
    let matching_origins = available_pointer_locals
        .iter()
        .filter_map(|local| pointer_origins.get(local))
        .filter(|origin| origin.original_place == *updated_place)
        .filter(|origin| !data_types.local_uses_stable_cell(origin.original_place.local))
        .cloned()
        .collect::<Vec<_>>();
    if matching_origins.is_empty() {
        return Vec::new();
    }

    let (value_name, mut instructions, value_type) =
        emit_instructions_to_get_on_own(updated_place, tcx, instance, mir, data_types);
    let updated_rust_ty = EarlyBinder::bind(tcx, updated_place.ty(&mir.local_decls, tcx).ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    for origin in matching_origins {
        let mut carrier = oomir::Operand::Variable {
            name: origin.carrier_name,
            ty: oomir::Type::Pointer(Box::new(origin.pointee_type.clone())),
        };
        if origin.pointee_type != value_type {
            let storage_pointer_ty = oomir::Type::Pointer(Box::new(value_type.clone()));
            let retyped_name = format!("{value_name}_refresh_pointer");
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: Some(retyped_name.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "retype".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("self".to_string(), carrier.get_type().unwrap()),
                        ("size".to_string(), oomir::Type::U64),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(storage_pointer_ty.clone()),
                    is_static: false,
                },
                args: vec![
                    oomir::Operand::Constant(oomir::Constant::U64(
                        u64::try_from(
                            crate::lower1::types::layout_size_bytes(tcx, updated_rust_ty)
                                .unwrap_or_else(|error| {
                                    panic!("could not determine pointer refresh layout: {error}")
                                }),
                        )
                        .expect("Rust pointer refresh layout exceeds u64"),
                    )),
                    crate::lower1::types::pointer_view_codec_operand(
                        updated_rust_ty,
                        tcx,
                        data_types,
                        instance,
                    ),
                ],
                operand: carrier,
            });
            carrier = oomir::Operand::Variable {
                name: retyped_name,
                ty: storage_pointer_ty,
            };
        }
        crate::lower1::place::emit_pointer_write(
            carrier,
            &value_type,
            oomir::Operand::Variable {
                name: value_name.clone(),
                ty: value_type.clone(),
            },
            &mut instructions,
        );
    }
    instructions
}

pub(in crate::lower1) fn emit_selected_mutable_borrow_writebacks<'tcx>(
    borrow_locals: impl IntoIterator<Item = Local>,
    mutable_borrows: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> Vec<oomir::Instruction> {
    let mut instructions = Vec::new();
    let mut visited = HashSet::default();
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
