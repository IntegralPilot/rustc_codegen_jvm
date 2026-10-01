//! Lower monomorphized Rust MIR into typed JVM operations for SSA construction.

use crate::lower1::context::Definitions;
use crate::oomir;
use control_flow::convert_basic_block;
use rustc_attr_ir::lang_items::LangItem;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_middle::{
    mir::{Body, Local, OUTERMOST_SOURCE_SCOPE, VarDebugInfoContents},
    ty::{EarlyBinder, Instance, InstanceKind, ShimKind, TyCtxt},
};
use rustc_span::def_id::DefId;
use types::ty_to_oomir_type;

mod initialization;
use initialization::{MirControlFlow, class_locals_needing_initial_carriers};

mod debug;
pub(crate) use debug::source_location;
use debug::{DebugScopeCache, local_variable_scope};
pub(crate) mod context;
pub mod control_flow;
pub mod jvm_names;
pub mod naming;
pub mod operand;
pub mod place;
pub mod statics;
pub mod types;
mod value_repr;

pub(crate) fn is_non_null_lang_item(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    tcx.is_lang_item(def_id, LangItem::NonNull)
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
        | Type::Array(_)
        | Type::Slice(_)
        | Type::TaggedI64
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
    mir: &Body<'tcx>,
    fn_name_override: Option<naming::FnNameData>,
    is_static: bool,
    data_types: &mut Definitions<'tcx>,
    external_interfaces: &mut HashSet<String>,
) -> oomir::Function {
    data_types.with_body(mir, |data_types| {
        lower_body(
            tcx,
            instance,
            mir,
            fn_name_override,
            is_static,
            data_types,
            external_interfaces,
        )
    })
}

fn lower_body<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    fn_name_override: Option<naming::FnNameData>,
    is_static: bool,
    data_types: &mut Definitions<'tcx>,
    external_interfaces: &mut HashSet<String>,
) -> oomir::Function {
    use rustc_middle::ty::TyKind;

    // Get a function name from the instance or use the provided override.
    // Prefer monomorphized naming to disambiguate generic instantiations.
    let fn_name_data = fn_name_override.unwrap_or_else(|| data_types.function_name(tcx, instance));
    let fn_name = fn_name_data.method_name.clone();

    // Extract function signature
    // Closures require special handling - we must use as_closure().sig() instead of fn_sig()
    // Instantiate the function's item type with this instance's generic args, so
    // generic functions get concrete param/return types.
    // Instantiate its bound lifetimes before lowering parameters and results,
    // including callbacks that refer to the enclosing signature's lifetimes.
    let instance_ty = tcx
        .type_of(instance.def_id())
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let (params_ty, return_ty): (Vec<_>, _) = match instance_ty.kind() {
        TyKind::Closure(_def_id, args) => {
            let sig = tcx.instantiate_bound_regions_with_erased(args.as_closure().sig());
            (sig.inputs().to_vec(), sig.output())
        }
        TyKind::FnDef(_def_id, _args)
            if !matches!(instance.def, InstanceKind::Shim(ShimKind::VTable(_))) =>
        {
            // For FnDef, compute the signature from the instantiated item type
            let sig = tcx.instantiate_bound_regions_with_erased(instance_ty.fn_sig(tcx));
            (sig.inputs().to_vec(), sig.output())
        }
        _ => {
            // Coroutines have no `FnSig`, and vtable shims replace by-value
            // self with *mut Self. Their MIR locals define the actual ABI.
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

    if instance.def_id().is_local()
        && crate::java_exports::is_lowerable_java_public_function(tcx, instance.def_id())
        && tcx.visibility(instance.def_id()).is_public()
        && crate::java_exports::is_exported(tcx, instance.def_id())
    {
        control_flow::rvalue::ensure_exported_closure_calls(return_ty, tcx, data_types, instance);
    }

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
    let is_jvm_main = signature.is_static
        && fn_name == "main"
        && matches!(
            signature.params.as_slice(),
            [(name, oomir::Type::Array(element))]
                if name == "args"
                    && matches!(element.as_ref(), oomir::Type::Class(class_name) if class_name == "java/lang/String")
        );

    let mut debug_variables = Vec::new();
    let mut debug_variable_scopes = Vec::new();
    if crate::lower2::debug_info_options(tcx).local_variables {
        let mut seen_debug_variables = HashSet::default();
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
            let (oomir_name, debug_type) = if data_types.local_uses_stable_cell(local) {
                (
                    place::local_cell_name(local),
                    oomir::Type::pointer(value_type),
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
    }
    let debug_scope_cache = DebugScopeCache::new(mir, &debug_variables, &debug_variable_scopes);

    // Build a CodeBlock from the MIR basic blocks.
    let mut basic_blocks = HashMap::default();
    // MIR guarantees that the start block is BasicBlock 0.
    let entry_label = "bb0".to_string();

    let mir_control_flow = MirControlFlow::new(mir);
    for (bb, bb_data) in mir.basic_blocks.iter_enumerated() {
        let bb_ir = convert_basic_block(
            bb,
            bb_data,
            tcx,
            instance,
            mir,
            &return_oomir_ty,
            &mut basic_blocks,
            data_types,
            external_interfaces,
            &debug_variables,
            &debug_scope_cache,
        ); // Pass return type here
        basic_blocks.insert(bb_ir.label.clone(), bb_ir);
    }

    // For closures, we need to unpack the tuple argument into local variables
    // Closures take a single tuple parameter, but MIR expects individual arguments in separate locals
    let mut instrs = vec![];

    if is_jvm_main {
        let args_ty = signature.params[0].1.clone();
        instrs.push(oomir::Instruction::InvokeStatic {
            dest: None,
            class_name: "org/rustlang/runtime/RuntimeSupport".to_string(),
            method_name: "initializeArgs".to_string(),
            method_ty: oomir::Signature {
                params: vec![("args".to_string(), args_ty.clone())],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![oomir::Operand::Variable {
                name: "param_0".to_string(),
                ty: args_ty,
            }],
        });
    }

    if matches!(instance_ty.kind(), TyKind::Closure(..)) && mir.arg_count > 0 {
        // For closures: local 0 = return place, local 1 = tuple argument
        // MIR expects: local 0 = return, local 1 = first arg, local 2 = second arg, etc.
        // But we receive: local 1 = tuple containing all args

        // Get the tuple parameter type (should be the first parameter in the signature)
        let tuple_param_index = if closure_has_captures { 1 } else { 0 };
        let tuple_param_local = format!("param_{tuple_param_index}");
        if let Some((_tuple_param_name, tuple_param_ty)) = signature.params.get(tuple_param_index) {
            let values = types::tuple_fields(
                params_ty[0],
                oomir::Operand::Variable {
                    name: tuple_param_local,
                    ty: tuple_param_ty.clone(),
                },
                "closure_arg",
                tcx,
                data_types,
                instance,
                &mut instrs,
            );
            for (index, src) in values.into_iter().enumerate() {
                instrs.push(oomir::Instruction::Move {
                    dest: format!("_{}", index + 2),
                    src,
                });
            }
        }
    }

    let carrier_locals = class_locals_needing_initial_carriers(mir, &mir_control_flow);
    let mut initialized_cells = HashSet::default();
    for local in carrier_locals {
        let rust_ty = data_types.normalize(tcx, mir.local_decls[local].ty, instance);
        if let Some(word) = types::packed_word(rust_ty, tcx) {
            instrs.push(oomir::Instruction::Move {
                dest: format!("_{}", local.index()),
                src: word.zero(),
            });
            if data_types.local_uses_stable_cell(local) {
                initialized_cells.insert(local);
            }
            continue;
        }
        let oomir::Type::Class(class_name) = place::get_place_type(
            &rustc_middle::mir::Place::from(local),
            mir,
            tcx,
            instance,
            data_types,
        ) else {
            continue;
        };
        let Some(oomir::DataType::Class {
            fields,
            kind: crate::oomir::ClassKind::Value | crate::oomir::ClassKind::JavaValue,
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
        if data_types.local_uses_stable_cell(local) {
            initialized_cells.insert(local);
        }
    }

    for (local, _) in mir.local_decls.iter_enumerated() {
        if !data_types.local_uses_stable_cell(local) {
            continue;
        }
        let value_type = place::get_place_type(
            &rustc_middle::mir::Place::from(local),
            mir,
            tcx,
            instance,
            data_types,
        );
        let implicit_zst = value_repr::materialize_implicit_zst(
            mir.local_decls[local].ty,
            &format!("{}_initial", place::local_cell_name(local)),
            tcx,
            instance,
            data_types,
            &mut instrs,
        );
        let initial_value = if ((local.index() > 0 && local.index() <= mir.arg_count)
            || initialized_cells.contains(&local))
            && value_type.has_jvm_value()
        {
            // An address does not initialize its value. Inlined clone shims can assign fields
            // before they assign the complete aggregate.
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
                    ("layout".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(oomir::Type::pointer(value_type)),
                is_static: true,
            },
            args: vec![
                initial_value,
                oomir::Operand::Constant(oomir::Constant::I32(
                    i32::try_from(
                        types::layout_size_bytes(
                            tcx,
                            EarlyBinder::bind(tcx, mir.local_decls[local].ty)
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
                    EarlyBinder::bind(tcx, mir.local_decls[local].ty)
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
                            EarlyBinder::bind(tcx, mir.local_decls[local].ty)
                                .instantiate(tcx, instance.args)
                                .skip_norm_wip(),
                        )
                        .unwrap_or_else(|error| {
                            panic!("could not determine stable local alignment: {error}")
                        }),
                    )
                    .expect("stable local alignment exceeds the JVM runtime address space"),
                )),
                types::scalar_storage_layout(mir.local_decls[local].ty, tcx, data_types, instance),
            ],
        });
    }

    if let Some(location) = source_location(tcx, mir.span, mir.span) {
        instrs.insert(0, oomir::Instruction::SourceLocation(location));
    }
    if !debug_variables.is_empty() {
        let no_referenced_debug_locals = HashSet::default();
        instrs.insert(
            usize::from(matches!(
                instrs.first(),
                Some(oomir::Instruction::SourceLocation(_))
            )),
            local_variable_scope(
                &debug_scope_cache,
                OUTERMOST_SOURCE_SCOPE,
                &no_referenced_debug_locals,
                &debug_variables,
            ),
        );
    }

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
        body: codeblock.into(),
    }
}
