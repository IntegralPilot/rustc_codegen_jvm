use super::debug::DebugLocalCollector;
use super::{
    jvm_names,
    operand::convert_operand,
    place::{
        emit_instructions_to_get_on_own, emit_instructions_to_set_value, emit_pointer_read,
        emit_pointer_slice_parts, emit_retyped_slice_data_pointer, place_to_string,
    },
    types::{enum_scoped_method_name, mir_int_to_oomir_const},
};
use crate::lower1::context::Definitions;
use crate::oomir;

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::{attrs::lang_items::LangItem, def::DefKind};
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlockData, Body, Local, Location, NonDivergingIntrinsic,
        Operand as MirOperand, Place, SourceInfo, StatementKind, TerminatorKind, UnwindAction,
        visit::Visitor,
    },
    ty::{EarlyBinder, Instance, InstanceKind, ShimKind, Ty, TyCtxt, TyKind, TypingEnv, VtblEntry},
};
use rustc_span::{Symbol, sym};

mod calls;
pub mod checked_intrinsics;
mod checked_ops;
mod comparisons;
pub(super) use comparisons::*;
mod diagnostics;
pub(super) use diagnostics::*;
mod borrows;
pub(super) use borrows::*;
mod drop_lowering;
pub(super) use drop_lowering::*;
mod atomic;
pub(super) use atomic::*;

pub(crate) mod rvalue;
pub(crate) mod trait_objects;

fn is_core_ptr_metadata_api(tcx: TyCtxt<'_>, def_id: rustc_span::def_id::DefId) -> bool {
    let metadata = Symbol::intern("metadata");
    let Some(metadata_module) = tcx.opt_parent(def_id) else {
        return false;
    };
    let Some(ptr_module) = tcx.opt_parent(metadata_module) else {
        return false;
    };
    tcx.crate_name(def_id.krate) == sym::core
        && tcx.opt_item_name(def_id) == Some(metadata)
        && tcx.opt_item_name(metadata_module) == Some(metadata)
        && tcx.opt_item_name(ptr_module) == Some(sym::ptr)
}

fn is_core_ptr_free_function(
    tcx: TyCtxt<'_>,
    def_id: rustc_span::def_id::DefId,
    name: Symbol,
) -> bool {
    tcx.crate_name(def_id.krate) == sym::core
        && tcx.opt_item_name(def_id) == Some(name)
        && tcx
            .opt_parent(def_id)
            .is_some_and(|parent| tcx.opt_item_name(parent) == Some(sym::ptr))
}

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
                ("metadata_class".to_string(), oomir::Type::java_string()),
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

fn requires_compiled_static_dispatch(ty: &oomir::Type) -> bool {
    if let oomir::Type::MutableReference(inner)
    | oomir::Type::Reference(inner)
    | oomir::Type::Pointer(inner) = ty
    {
        return requires_compiled_static_dispatch(inner);
    }
    matches!(ty, oomir::Type::Unit | oomir::Type::Void)
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
            oomir::Type::Array(_) | oomir::Type::Slice(_) | oomir::Type::Str
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
    data_types: &mut Definitions<'tcx>,
    external_interfaces: &mut HashSet<String>,
    mutable_borrow_arrays: &mut MutableBorrowMap<'tcx>,
    debug_variables: &[oomir::DebugVariable],
    debug_scope_cache: &super::DebugScopeCache,
    initially_available_pointer_locals: HashSet<Local>,
) -> oomir::BasicBlock {
    // Use the basic block index as its label.
    let label = format!("bb{}", bb.index());
    let mut instructions = Vec::new();
    let mut initialized_borrows = initially_available_pointer_locals;
    // Convert each MIR statement in the block.
    for (statement_index, stmt) in bb_data.statements.iter().enumerate() {
        let statement_location = Location {
            block: bb,
            statement_index,
        };
        let mut debug_local_collector = DebugLocalCollector::default();
        if !debug_variables.is_empty() {
            debug_local_collector.visit_statement(stmt, statement_location);
        }
        let instruction_start = instructions.len();
        match &stmt.kind {
            StatementKind::Assign(assignment) => {
                let (place, rvalue) = assignment.as_ref();
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
                    external_interfaces,
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
                        [place.local],
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
            StatementKind::Intrinsic(intrinsic) => match intrinsic.as_ref() {
                NonDivergingIntrinsic::Assume(operand) => {
                    // `assume(false)` is UB, so a valid program never needs a JVM-side
                    // branch here. Still lower the operand so any place projection is
                    // evaluated consistently with other MIR operands.
                    let _ =
                        convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);
                }
                NonDivergingIntrinsic::CopyNonOverlapping(copy) => {
                    let source = convert_operand(
                        &copy.src,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
                    let destination = convert_operand(
                        &copy.dst,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
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
                        other => panic!(
                            "copy_nonoverlapping MIR statement has non-pointer source {other:?}"
                        ),
                    };
                    let (method_name, count) =
                        if let Ok(element_size) = super::types::layout_size_bytes(tcx, pointee) {
                            let byte_count_name = format!("{label}_copy_nonoverlapping_bytes");
                            instructions.push(oomir::Instruction::Binary {
                                op: crate::oomir::BinaryOp::Mul,
                                dest: byte_count_name.clone(),
                                op1: count,
                                op2: oomir::Operand::Constant(oomir::Constant::U64(
                                    element_size as u64,
                                )),
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
            },
            StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                let enum_ty = EarlyBinder::bind(tcx, place.ty(&mir.local_decls, tcx).ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                if matches!(enum_ty.kind(), TyKind::Coroutine(..)) {
                    let (object_name, object_instructions, object_ty) =
                        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                    instructions.extend(object_instructions);
                    let oomir::Type::Class(owner_class) = object_ty else {
                        panic!("coroutine discriminant target is not a JVM class");
                    };
                    instructions.push(oomir::Instruction::SetField {
                        object: object_name,
                        field_name: "__state".to_string(),
                        field_ty: oomir::Type::I32,
                        value: oomir::Operand::Constant(oomir::Constant::I32(
                            variant_index.as_u32() as i32,
                        )),
                        owner_class,
                    });
                } else if let Some(value) = super::value_repr::construct_fieldless_enum_variant(
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
        if instructions.len() > instruction_start {
            let mut metadata = Vec::new();
            if let Some(location) = super::source_location(tcx, mir.span, stmt.source_info.span) {
                metadata.push(oomir::Instruction::SourceLocation(location));
            }
            if !debug_variables.is_empty() {
                metadata.push(super::local_variable_scope(
                    debug_scope_cache,
                    stmt.source_info.scope,
                    &debug_local_collector.locals,
                    debug_variables,
                ));
            }
            instructions.splice(instruction_start..instruction_start, metadata);
        }
    }

    // Convert the MIR terminator into corresponding OOMIR instructions.
    if let Some(terminator) = &bb_data.terminator {
        let terminator_location = Location {
            block: bb,
            statement_index: bb_data.statements.len(),
        };
        let mut debug_local_collector = DebugLocalCollector::default();
        if !debug_variables.is_empty() {
            debug_local_collector.visit_terminator(terminator, terminator_location);
        }
        let instruction_start = instructions.len();
        let unwind_target = match &terminator.kind {
            TerminatorKind::Call {
                unwind: UnwindAction::Cleanup(target),
                ..
            }
            | TerminatorKind::Assert {
                unwind: UnwindAction::Cleanup(target),
                ..
            }
            | TerminatorKind::Drop {
                unwind: UnwindAction::Cleanup(target),
                ..
            } => Some(format!("bb{}", target.index())),
            _ => None,
        };
        if let Some(target) = &unwind_target {
            instructions.push(oomir::Instruction::UnwindStart {
                target: target.clone(),
            });
        }
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
                calls::emit(
                    tcx,
                    instance,
                    mir,
                    data_types,
                    external_interfaces,
                    &label,
                    &mut instructions,
                    args,
                    terminator,
                    func,
                    destination,
                    target,
                    mutable_borrow_arrays,
                    &mut initialized_borrows,
                );
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

                instructions.push(oomir::Instruction::Binary {
                    op: crate::oomir::BinaryOp::Eq,
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

                let mut fail_instructions = Vec::new();
                if let Some(location) =
                    super::source_location(tcx, mir.span, terminator.source_info.span)
                {
                    fail_instructions.push(oomir::Instruction::SourceLocation(location));
                }
                if !debug_variables.is_empty() {
                    fail_instructions.push(super::local_variable_scope(
                        debug_scope_cache,
                        terminator.source_info.scope,
                        &debug_local_collector.locals,
                        debug_variables,
                    ));
                }
                let panic = assert_panic_lang_item(msg).map(|lang_item| {
                    let args =
                        if let rustc_middle::mir::AssertKind::BoundsCheck { len, index } = &**msg {
                            let index = convert_operand(
                                index,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut fail_instructions,
                            );
                            let len = convert_operand(
                                len,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut fail_instructions,
                            );
                            vec![index, len]
                        } else {
                            Vec::new()
                        };
                    (lang_item, args)
                });
                if let Some((lang_item, args)) = panic {
                    emit_panic_lang_item(
                        lang_item,
                        args,
                        terminator.source_info,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut fail_instructions,
                        &format!("{failure_block}_caller_location"),
                    );
                } else {
                    fail_instructions.push(oomir::Instruction::ThrowNewWithMessage {
                        exception_class: "java/lang/RuntimeException".to_string(),
                        message: panic_message,
                    });
                }
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
                let rust_ty = EarlyBinder::bind(tcx, place.ty(&mir.local_decls, tcx).ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
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
                instructions.push(oomir::Instruction::Rethrow);
            }
            TerminatorKind::UnwindTerminate(_) => {
                instructions.push(oomir::Instruction::InvokeStatic {
                    dest: None,
                    class_name: "org/rustlang/runtime/PanicSupport".to_string(),
                    method_name: "abort".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![(
                            "failure".to_string(),
                            oomir::Type::Class("java/lang/Throwable".to_string()),
                        )],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![oomir::Operand::Variable {
                        name: "__rust_unwind_exception".to_string(),
                        ty: oomir::Type::Class("java/lang/Throwable".to_string()),
                    }],
                });
                instructions.push(oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/AssertionError".to_string(),
                    message: "Rust abort unexpectedly returned".to_string(),
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
        if unwind_target.is_some() {
            instructions.push(oomir::Instruction::UnwindEnd);
        }
        if instructions.len() > instruction_start {
            let mut metadata = Vec::new();
            if let Some(location) =
                super::source_location(tcx, mir.span, terminator.source_info.span)
            {
                metadata.push(oomir::Instruction::SourceLocation(location));
            }
            if !debug_variables.is_empty() {
                metadata.push(super::local_variable_scope(
                    debug_scope_cache,
                    terminator.source_info.scope,
                    &debug_local_collector.locals,
                    debug_variables,
                ));
            }
            instructions.splice(instruction_start..instruction_start, metadata);
        }
    }

    oomir::BasicBlock {
        label,
        instructions,
    }
}
