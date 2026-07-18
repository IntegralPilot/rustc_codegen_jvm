//! This is the stage 1 lowering pass of the compiler.
//! It is responsible for coverting the MIR into a lower-level IR, called OOMIR (see src/oomir.rs).
//! It is a simple pass that converts the MIR into a more object-oriented representation.

// lower1.rs
//! This module converts Rust MIR into an object-oriented MIR (OOMIR)
//! that sits between MIR and JVM bytecode. It supports a subset of Rust constructs
//! (arithmetic, branching, returns) and can be extended to support more of Rust.

use crate::oomir;
use control_flow::convert_basic_block;
use rustc_middle::{
    mir::{
        BasicBlock, Body, Local, OUTERMOST_SOURCE_SCOPE, Place, ProjectionElem, SourceScope,
        StatementKind, TerminatorKind, VarDebugInfoContents,
    },
    ty::{EarlyBinder, Instance, TyCtxt},
};
use rustc_span::{Span, hygiene};
use std::collections::{HashMap, HashSet, VecDeque};
use types::ty_to_oomir_type;

mod closures;
pub mod control_flow;
pub mod jvm_names;
pub mod naming;
pub mod operand;
pub mod place;
pub mod statics;
pub mod types;
mod value_repr;

pub use closures::generate_closure_function_name;

pub(crate) fn source_location(
    tcx: TyCtxt<'_>,
    function_span: Span,
    span: Span,
) -> Option<oomir::SourceLocation> {
    if span.is_dummy() {
        return None;
    }

    let span = hygiene::walk_chain_collapsed(span, function_span);
    let location = tcx.sess.source_map().lookup_char_pos(span.lo());
    Some(oomir::SourceLocation {
        file_name: location.file.name.short().to_string(),
        line: u32::try_from(location.line).ok()?,
    })
}

fn source_scope_contains<'tcx>(
    mir: &Body<'tcx>,
    ancestor: SourceScope,
    mut scope: SourceScope,
) -> bool {
    loop {
        if scope == ancestor {
            return true;
        }
        let Some(parent) = mir.source_scopes[scope].parent_scope else {
            return false;
        };
        scope = parent;
    }
}

pub(crate) fn local_variable_scope(
    mir: &Body<'_>,
    scope: SourceScope,
    referenced_locals: &HashSet<Local>,
    debug_variables: &[oomir::DebugVariable],
    debug_variable_scopes: &[SourceScope],
) -> oomir::Instruction {
    let mut visible_by_name = HashMap::<String, (usize, SourceScope)>::new();
    for (index, variable_scope) in debug_variable_scopes.iter().copied().enumerate() {
        if !source_scope_contains(mir, variable_scope, scope) {
            continue;
        }
        let Some(variable) = debug_variables.get(index) else {
            continue;
        };
        match visible_by_name.get(&variable.name).copied() {
            Some((_, visible_scope))
                if source_scope_contains(mir, visible_scope, variable_scope) =>
            {
                // A binding in the more deeply nested Rust scope shadows the
                // outer binding with the same source name.
                visible_by_name.insert(variable.name.clone(), (index, variable_scope));
            }
            None => {
                visible_by_name.insert(variable.name.clone(), (index, variable_scope));
            }
            _ => {}
        }
    }

    for local in referenced_locals {
        let plain_name = format!("_{}", local.index());
        let cell_name = place::local_cell_name(*local);
        for (index, variable) in debug_variables.iter().enumerate() {
            if variable.oomir_name != plain_name && variable.oomir_name != cell_name {
                continue;
            }
            let variable_scope = debug_variable_scopes
                .get(index)
                .copied()
                .unwrap_or(OUTERMOST_SOURCE_SCOPE);
            // MIR optimizations can move a binding's definition or use into
            // its parent source scope. A referenced debug local is still the
            // visible binding and shadows an outer binding by source name.
            visible_by_name.insert(variable.name.clone(), (index, variable_scope));
        }
    }

    let mut visible = visible_by_name
        .into_values()
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    visible.sort_unstable();
    oomir::Instruction::LocalVariableScope(visible)
}

fn available_pointer_locals_at_block_entries<'tcx>(
    mir: &Body<'tcx>,
    pointer_origins: &control_flow::MutableBorrowMap<'tcx>,
) -> HashMap<BasicBlock, HashSet<Local>> {
    let all_pointer_locals = pointer_origins.keys().copied().collect::<HashSet<_>>();
    let mut generated = HashMap::<BasicBlock, HashSet<Local>>::new();
    let mut predecessors = HashMap::<BasicBlock, Vec<BasicBlock>>::new();
    let mut reachable = HashSet::new();
    let mut queue = VecDeque::from([BasicBlock::from_usize(0)]);

    for (block, data) in mir.basic_blocks.iter_enumerated() {
        let generated_here = data
            .statements
            .iter()
            .filter_map(|statement| {
                let StatementKind::Assign(box (place, _)) = &statement.kind else {
                    return None;
                };
                (place.projection.is_empty() && pointer_origins.contains_key(&place.local))
                    .then_some(place.local)
            })
            .collect();
        generated.insert(block, generated_here);
        for successor in data.terminator().successors() {
            predecessors.entry(successor).or_default().push(block);
        }
    }

    while let Some(block) = queue.pop_front() {
        if !reachable.insert(block) {
            continue;
        }
        queue.extend(mir.basic_blocks[block].terminator().successors());
    }

    let entry = BasicBlock::from_usize(0);
    let mut available = mir
        .basic_blocks
        .indices()
        .map(|block| {
            let initial = if block == entry || !reachable.contains(&block) {
                HashSet::new()
            } else {
                all_pointer_locals.clone()
            };
            (block, initial)
        })
        .collect::<HashMap<_, _>>();

    loop {
        let mut changed = false;
        for block in mir.basic_blocks.indices().filter(|block| *block != entry) {
            if !reachable.contains(&block) {
                continue;
            }
            let incoming = predecessors
                .get(&block)
                .into_iter()
                .flatten()
                .filter(|predecessor| reachable.contains(predecessor))
                .map(|predecessor| {
                    let mut outgoing = available[predecessor].clone();
                    outgoing.extend(generated[predecessor].iter().copied());
                    outgoing
                })
                .reduce(|left, right| left.intersection(&right).copied().collect())
                .unwrap_or_default();
            if available.get(&block) != Some(&incoming) {
                available.insert(block, incoming);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    available
}

fn update_class_carrier_state(
    place: &Place<'_>,
    initialized: &mut HashSet<Local>,
    needs_initial_carrier: Option<&mut HashSet<Local>>,
) {
    if place.projection.is_empty() {
        initialized.insert(place.local);
        return;
    }

    if matches!(place.projection.first(), Some(ProjectionElem::Field(..))) {
        if let Some(needs_initial_carrier) = needs_initial_carrier
            && !initialized.contains(&place.local)
        {
            needs_initial_carrier.insert(place.local);
        }
        initialized.insert(place.local);
    }
}

fn transfer_class_carrier_state(mir: &Body<'_>, block: BasicBlock, state: &mut HashSet<Local>) {
    let block_data = &mir.basic_blocks[block];
    for statement in &block_data.statements {
        match &statement.kind {
            StatementKind::Assign(box (place, _)) => {
                update_class_carrier_state(place, state, None);
            }
            StatementKind::StorageLive(local) | StatementKind::StorageDead(local) => {
                state.remove(local);
            }
            _ => {}
        }
    }
    if let TerminatorKind::Call { destination, .. } = &block_data.terminator().kind {
        update_class_carrier_state(destination, state, None);
    }
}

fn class_locals_needing_initial_carriers(mir: &Body<'_>) -> HashSet<Local> {
    let entry = BasicBlock::from_usize(0);
    let all_locals = mir.local_decls.indices().collect::<HashSet<_>>();
    let argument_locals = (1..=mir.arg_count)
        .map(Local::from_usize)
        .collect::<HashSet<_>>();
    let mut predecessors = HashMap::<BasicBlock, Vec<BasicBlock>>::new();
    let mut reachable = HashSet::new();
    let mut queue = VecDeque::from([entry]);

    for (block, data) in mir.basic_blocks.iter_enumerated() {
        for successor in data.terminator().successors() {
            predecessors.entry(successor).or_default().push(block);
        }
    }
    while let Some(block) = queue.pop_front() {
        if !reachable.insert(block) {
            continue;
        }
        queue.extend(mir.basic_blocks[block].terminator().successors());
    }

    let mut available = mir
        .basic_blocks
        .indices()
        .map(|block| {
            let initial = if block == entry {
                argument_locals.clone()
            } else if reachable.contains(&block) {
                all_locals.clone()
            } else {
                HashSet::new()
            };
            (block, initial)
        })
        .collect::<HashMap<_, _>>();

    loop {
        let mut changed = false;
        for block in mir.basic_blocks.indices().filter(|block| *block != entry) {
            if !reachable.contains(&block) {
                continue;
            }
            let incoming = predecessors
                .get(&block)
                .into_iter()
                .flatten()
                .filter(|predecessor| reachable.contains(predecessor))
                .map(|predecessor| {
                    let mut outgoing = available[predecessor].clone();
                    transfer_class_carrier_state(mir, *predecessor, &mut outgoing);
                    outgoing
                })
                .reduce(|left, right| left.intersection(&right).copied().collect())
                .unwrap_or_default();
            if available.get(&block) != Some(&incoming) {
                available.insert(block, incoming);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let mut needs_initial_carrier = HashSet::new();
    for block in mir.basic_blocks.indices() {
        if !reachable.contains(&block) {
            continue;
        }
        let mut state = available[&block].clone();
        let block_data = &mir.basic_blocks[block];
        for statement in &block_data.statements {
            match &statement.kind {
                StatementKind::Assign(box (place, _)) => {
                    update_class_carrier_state(place, &mut state, Some(&mut needs_initial_carrier))
                }
                StatementKind::StorageLive(local) | StatementKind::StorageDead(local) => {
                    state.remove(local);
                }
                _ => {}
            }
        }
        if let TerminatorKind::Call { destination, .. } = &block_data.terminator().kind {
            update_class_carrier_state(destination, &mut state, Some(&mut needs_initial_carrier));
        }
    }
    needs_initial_carrier
}

fn jvm_default_operand(ty: &oomir::Type) -> oomir::Operand {
    use oomir::{Constant, Operand, Type};

    let constant = match ty {
        Type::Unit | Type::Void => Constant::Unit,
        Type::Boolean => Constant::Boolean(false),
        Type::Char => Constant::Char('\0'),
        Type::I8 => Constant::I8(0),
        Type::U8 => Constant::U8(0),
        Type::I16 => Constant::I16(0),
        Type::U16 => Constant::U16(0),
        Type::I32 => Constant::I32(0),
        Type::U32 => Constant::U32(0),
        Type::I64 => Constant::I64(0),
        Type::U64 => Constant::U64(0),
        Type::F16 => Constant::F16(0),
        Type::F32 => Constant::F32(0.0),
        Type::F64 => Constant::F64(0.0),
        Type::Pointer(_)
        | Type::MutableReference(_)
        | Type::Reference(_)
        | Type::Array(_)
        | Type::Slice(_)
        | Type::Str
        | Type::Class(_)
        | Type::Interface(_) => Constant::Null(ty.clone()),
    };
    Operand::Constant(constant)
}

/// Converts a MIR body into an OOMIR function and control-flow graph.
/// `fn_name_override` supplies names for closures, which lack normal rustc item names.
pub fn mir_to_oomir<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &mut Body<'tcx>,
    fn_name_override: Option<naming::FnNameData>,
    is_static: bool,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Function {
    use rustc_middle::ty::TyKind;

    // Get a function name from the instance or use the provided override.
    // Prefer monomorphized naming to disambiguate generic instantiations.
    let fn_name_data =
        fn_name_override.unwrap_or_else(|| naming::mono_fn_name_from_instance(tcx, instance));
    let fn_name = fn_name_data.method_name.clone();
    let instrumented_fn_name = fn_name_data
        .class_to_call_on
        .as_deref()
        .map(|owner| format!("{owner}::{fn_name}"))
        .unwrap_or_else(|| fn_name.clone());
    let _timer = crate::instrumentation::Timer::function("lower1", None, &instrumented_fn_name);

    // Extract function signature
    // Closures require special handling - we must use as_closure().sig() instead of fn_sig()
    // Instantiate the function's item type with this instance's generic args, so
    // generic functions get concrete param/return types.
    let instance_ty = tcx
        .type_of(instance.def_id())
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let (params_ty, return_ty): (Vec<_>, _) = match instance_ty.kind() {
        TyKind::Closure(_def_id, args) => {
            let sig = args.as_closure().sig();
            (
                sig.inputs().skip_binder().iter().copied().collect(),
                sig.output().skip_binder(),
            )
        }
        TyKind::FnDef(_def_id, _args) => {
            // For FnDef, compute the signature from the instantiated item type
            let sig = instance_ty.fn_sig(tcx);
            (
                sig.inputs().skip_binder().iter().copied().collect(),
                sig.output().skip_binder(),
            )
        }
        _ => {
            // Compiler-generated callable bodies such as coroutines have no
            // `FnSig`; their MIR argument locals and return place define the ABI.
            let params = (1..=mir.arg_count)
                .map(|index| mir.local_decls[Local::from_usize(index)].ty)
                .collect();
            (params, mir.local_decls[Local::from_usize(0)].ty)
        }
    };

    let closure_has_captures = matches!(
        instance_ty.kind(),
        TyKind::Closure(_, args) if !args.as_closure().upvar_tys().is_empty()
    );

    let mut params_oomir: Vec<(String, oomir::Type)> = params_ty
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            // Arguments start at MIR local 1. The index `i` starts at 0.
            let local_index = rustc_middle::mir::Local::from_usize(i + 1);

            // Try to find the parameter name from var_debug_info
            let param_name = mir
                .var_debug_info
                .iter()
                .find_map(|var_info| {
                    // Check if this debug info entry is for our parameter
                    if let rustc_middle::mir::VarDebugInfoContents::Place(place) = &var_info.value {
                        if place.local == local_index && place.projection.is_empty() {
                            return Some(var_info.name.to_string());
                        }
                    }
                    None
                })
                .unwrap_or_else(|| format!("arg{}", i));

            let oomir_type = ty_to_oomir_type(*ty, tcx, data_types, instance);

            // Return the (name, type) tuple
            (param_name, oomir_type)
        })
        .collect();

    if closure_has_captures {
        let closure_env_mir_ty = EarlyBinder::bind(tcx, mir.local_decls[Local::from_usize(1)].ty)
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        let closure_env_ty = ty_to_oomir_type(closure_env_mir_ty, tcx, data_types, instance);
        params_oomir.insert(0, ("closure_env".to_string(), closure_env_ty));
    }

    if instance.def.requires_caller_location(tcx) {
        params_oomir.push((
            oomir::CALLER_LOCATION_PARAM_NAME.to_string(),
            ty_to_oomir_type(tcx.caller_location_ty(), tcx, data_types, instance),
        ));
    }

    let return_oomir_ty: oomir::Type = ty_to_oomir_type(return_ty, tcx, data_types, instance);

    let mut signature = oomir::Signature {
        params: params_oomir,
        ret: Box::new(return_oomir_ty.clone()), // Clone here to pass to convert_basic_block
        is_static,
    };

    // check if txc.entry_fn() matches the DefId of the function
    // note: libraries exist and don't have an entry function, handle that case
    if let Some(entry_fn) = tcx.entry_fn(()) {
        if entry_fn.0 == instance.def_id() {
            // see if the name is "main"
            if fn_name == "main" {
                // manually override the signature to match the JVM main method
                signature = oomir::Signature {
                    params: vec![(
                        "args".to_string(),
                        oomir::Type::Array(Box::new(oomir::Type::Class(
                            "java/lang/String".to_string(),
                        ))),
                    )],
                    ret: Box::new(oomir::Type::Void),
                    is_static: true,
                };
            }
        }
    }

    let mut debug_variables = Vec::new();
    let mut debug_variable_scopes = Vec::new();
    let mut seen_debug_variables = HashSet::new();
    for variable in &mir.var_debug_info {
        let VarDebugInfoContents::Place(debug_place) = &variable.value else {
            continue;
        };
        if variable.composite.is_some() || !debug_place.projection.is_empty() {
            continue;
        }

        let source_name = variable.name.to_string();
        if source_name.is_empty() {
            continue;
        }
        let local = debug_place.local;
        let value_type = place::get_place_type(
            &rustc_middle::mir::Place::from(local),
            mir,
            tcx,
            instance,
            data_types,
        );
        let (oomir_name, debug_type) = if place::local_uses_stable_cell(local, mir) {
            (
                place::local_cell_name(local),
                oomir::Type::Pointer(Box::new(value_type)),
            )
        } else {
            (format!("_{}", local.index()), value_type)
        };
        if !debug_type.has_jvm_value()
            || !seen_debug_variables.insert((
                source_name.clone(),
                oomir_name.clone(),
                variable.source_info.scope,
            ))
        {
            continue;
        }
        debug_variables.push(oomir::DebugVariable {
            name: source_name,
            oomir_name,
            ty: debug_type,
        });
        debug_variable_scopes.push(variable.source_info.scope);
    }

    // Preserve descriptor parameters even when rustc did not create a
    // VarDebugInfo entry (notably the JVM `String[] args` main parameter).
    for (index, (param_name, param_type)) in signature.params.iter().enumerate() {
        if param_name == oomir::CALLER_LOCATION_PARAM_NAME || !param_type.has_jvm_value() {
            continue;
        }
        let oomir_name = if signature.is_static && fn_name == "main" && index == 0 {
            "param_0".to_string()
        } else {
            format!("_{}", index + 1)
        };
        if debug_variables
            .iter()
            .any(|variable| variable.oomir_name == oomir_name)
        {
            continue;
        }
        debug_variables.push(oomir::DebugVariable {
            name: param_name.clone(),
            oomir_name,
            ty: param_type.clone(),
        });
        debug_variable_scopes.push(OUTERMOST_SOURCE_SCOPE);
    }

    // Build a CodeBlock from the MIR basic blocks.
    let mut basic_blocks = HashMap::new();
    // MIR guarantees that the start block is BasicBlock 0.
    let entry_label = "bb0".to_string();

    let mir_cloned = mir.clone();
    let mut mutable_borrows =
        control_flow::collect_pointer_origins(&mir_cloned, tcx, instance, data_types);
    let available_pointer_locals =
        available_pointer_locals_at_block_entries(&mir_cloned, &mutable_borrows);

    // Need read-only access to mir for local_decls inside the loop
    for (bb, bb_data) in mir.basic_blocks_mut().iter_enumerated() {
        let bb_ir = convert_basic_block(
            bb,
            bb_data,
            tcx,
            instance,
            &mir_cloned,
            &return_oomir_ty,
            &mut basic_blocks,
            data_types,
            &mut mutable_borrows,
            &debug_variables,
            &debug_variable_scopes,
            available_pointer_locals
                .get(&bb)
                .cloned()
                .unwrap_or_default(),
        ); // Pass return type here
        basic_blocks.insert(bb_ir.label.clone(), bb_ir);
    }

    // For closures, we need to unpack the tuple argument into local variables
    // Closures take a single tuple parameter, but MIR expects individual arguments in separate locals
    let mut instrs = vec![];

    if matches!(instance_ty.kind(), TyKind::Closure(..)) && mir_cloned.arg_count > 0 {
        // For closures: local 0 = return place, local 1 = tuple argument
        // MIR expects: local 0 = return, local 1 = first arg, local 2 = second arg, etc.
        // But we receive: local 1 = tuple containing all args

        // Get the tuple parameter type (should be the first parameter in the signature)
        let tuple_param_index = if closure_has_captures { 1 } else { 0 };
        let tuple_param_local = if closure_has_captures { "_2" } else { "_1" };
        if let Some((_tuple_param_name, tuple_param_ty)) = signature.params.get(tuple_param_index) {
            // Check if it's a tuple/struct type that we need to unpack
            if let oomir::Type::Class(class_name) = tuple_param_ty {
                // Get the data type definition to see its fields
                if let Some(oomir::DataType::Class { fields, .. }) = data_types.get(class_name) {
                    // Unpack each field from the tuple into the expected local variables
                    // Local 1 contains the tuple, we need to extract fields to locals 2, 3, 4...
                    for (field_idx, (field_name, field_ty)) in fields.iter().enumerate() {
                        let local_var_index = field_idx + 2; // Start from local 2 (local 1 is the tuple)

                        // Get the field from the tuple object (local 1)
                        instrs.push(oomir::Instruction::GetField {
                            dest: format!("_{}", local_var_index),
                            object: oomir::Operand::Variable {
                                name: tuple_param_local.to_string(),
                                ty: tuple_param_ty.clone(),
                            },
                            field_name: field_name.clone(),
                            field_ty: field_ty.clone(),
                            owner_class: class_name.clone(),
                        });
                    }
                }
            }
        }
    }

    let mut carrier_locals = class_locals_needing_initial_carriers(&mir_cloned)
        .into_iter()
        .collect::<Vec<_>>();
    carrier_locals.sort_by_key(|local| local.index());
    for local in carrier_locals {
        if place::local_uses_stable_cell(local, &mir_cloned) {
            continue;
        }
        let oomir::Type::Class(class_name) = place::get_place_type(
            &rustc_middle::mir::Place::from(local),
            &mir_cloned,
            tcx,
            instance,
            data_types,
        ) else {
            continue;
        };
        let Some(oomir::DataType::Class {
            fields,
            is_abstract: false,
            ..
        }) = data_types.get(&class_name)
        else {
            continue;
        };
        let constructor_args = fields
            .iter()
            .filter(|(_, field_ty)| field_ty.has_jvm_value())
            .map(|(_, field_ty)| (jvm_default_operand(field_ty), field_ty.clone()))
            .collect();
        instrs.push(oomir::Instruction::ConstructObject {
            dest: format!("_{}", local.index()),
            class_name,
            args: constructor_args,
        });
    }

    for (local, _) in mir_cloned.local_decls.iter_enumerated() {
        if !place::local_uses_stable_cell(local, &mir_cloned) {
            continue;
        }
        let value_type = place::get_place_type(
            &rustc_middle::mir::Place::from(local),
            &mir_cloned,
            tcx,
            instance,
            data_types,
        );
        let implicit_zst = value_repr::materialize_implicit_zst(
            mir_cloned.local_decls[local].ty,
            &format!("{}_initial", place::local_cell_name(local)),
            tcx,
            instance,
            data_types,
            &mut instrs,
        );
        let initial_value = if local.index() > 0
            && local.index() <= mir_cloned.arg_count
            && value_type.has_jvm_value()
        {
            oomir::Operand::Variable {
                name: format!("_{}", local.index()),
                ty: value_type.clone(),
            }
        } else if let Some(value) = implicit_zst {
            value
        } else {
            oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::Class(
                "java/lang/Object".to_string(),
            )))
        };
        instrs.push(oomir::Instruction::InvokeStatic {
            dest: Some(place::local_cell_name(local)),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "cellAligned".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "value".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    ("size".to_string(), oomir::Type::I32),
                    ("codec".to_string(), oomir::Type::java_string()),
                    ("alignment".to_string(), oomir::Type::I32),
                ],
                ret: Box::new(oomir::Type::Pointer(Box::new(value_type))),
                is_static: true,
            },
            args: vec![
                initial_value,
                oomir::Operand::Constant(oomir::Constant::I32(
                    i32::try_from(
                        types::layout_size_bytes(
                            tcx,
                            EarlyBinder::bind(tcx, mir_cloned.local_decls[local].ty)
                                .instantiate(tcx, instance.args)
                                .skip_norm_wip(),
                        )
                        .unwrap_or_else(|error| {
                            panic!("could not determine stable local layout: {error}")
                        }),
                    )
                    .expect("stable local layout exceeds the JVM runtime address space"),
                )),
                types::pointer_memory_codec_operand(
                    EarlyBinder::bind(tcx, mir_cloned.local_decls[local].ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip(),
                    tcx,
                    data_types,
                    instance,
                ),
                oomir::Operand::Constant(oomir::Constant::I32(
                    i32::try_from(
                        types::layout_align_bytes(
                            tcx,
                            EarlyBinder::bind(tcx, mir_cloned.local_decls[local].ty)
                                .instantiate(tcx, instance.args)
                                .skip_norm_wip(),
                        )
                        .unwrap_or_else(|error| {
                            panic!("could not determine stable local alignment: {error}")
                        }),
                    )
                    .expect("stable local alignment exceeds the JVM runtime address space"),
                )),
            ],
        });
    }

    if let Some(location) = source_location(tcx, mir_cloned.span, mir_cloned.span) {
        instrs.insert(0, oomir::Instruction::SourceLocation(location));
    }
    let no_referenced_debug_locals = HashSet::new();
    instrs.insert(
        usize::from(matches!(
            instrs.first(),
            Some(oomir::Instruction::SourceLocation(_))
        )),
        local_variable_scope(
            &mir_cloned,
            OUTERMOST_SOURCE_SCOPE,
            &no_referenced_debug_locals,
            &debug_variables,
            &debug_variable_scopes,
        ),
    );

    // add instrs to the start of the entry block
    if !instrs.is_empty() {
        let entry_block = basic_blocks.get_mut(&entry_label).unwrap();
        entry_block.instructions.splice(0..0, instrs);
    }

    let codeblock = oomir::CodeBlock {
        basic_blocks,
        entry: entry_label,
    };

    // Return the OOMIR representation of the function.
    oomir::Function {
        name: fn_name,
        owner_class: fn_name_data.class_to_call_on,
        signature,
        debug_variables,
        body: codeblock,
    }
}
