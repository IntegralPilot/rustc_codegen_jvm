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
    mir::Body,
    ty::{Instance, TyCtxt},
};
use std::collections::HashMap;
use types::ty_to_oomir_type;

mod closures;
pub mod control_flow;
pub mod naming;
pub mod operand;
pub mod place;
pub mod types;

pub use closures::{ClosureCallInfo, extract_closure_info, generate_closure_function_name};

/// Converts a MIR Body into an OOMIR Function.
/// This function extracts a function's signature (currently minimal) and builds
/// a control flow graph of basic blocks.
///
/// If `fn_name_override` is provided, it will be used as the function name instead of
/// querying rustc for the item name. This is necessary for closures, which don't have
/// proper item names in rustc's internal representation.
pub fn mir_to_oomir<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &mut Body<'tcx>,
    fn_name_override: Option<String>,
) -> (oomir::Function, HashMap<String, oomir::DataType>) {
    use rustc_middle::ty::TyKind;

    // Get a function name from the instance or use the provided override.
    // Prefer monomorphized naming to disambiguate generic instantiations.
    let fn_name =
        fn_name_override.unwrap_or_else(|| naming::mono_fn_name_from_instance(tcx, instance));

    // Extract function signature
    // Closures require special handling - we must use as_closure().sig() instead of fn_sig()
    // Instantiate the function's item type with this instance's generic args, so
    // generic functions get concrete param/return types.
    let instance_ty = tcx
        .type_of(instance.def_id())
        .instantiate(tcx, instance.args);
    let (params_ty, return_ty) = match instance_ty.kind() {
        TyKind::Closure(_def_id, args) => {
            let sig = args.as_closure().sig();
            (sig.inputs(), sig.output())
        }
        TyKind::FnDef(def_id, _args) => {
            // For FnDef, compute the signature from the instantiated item type
            let mir_sig = instance_ty.fn_sig(tcx);
            (mir_sig.inputs(), mir_sig.output())
        }
        _ => {
            // Regular function pointer or other callable types
            let mir_sig = instance_ty.fn_sig(tcx);
            (mir_sig.inputs(), mir_sig.output())
        }
    };

    let data_types = &mut HashMap::new();

    let params_oomir: Vec<(String, oomir::Type)> = params_ty
        .skip_binder()
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
    let return_oomir_ty: oomir::Type =
        ty_to_oomir_type(return_ty.skip_binder(), tcx, data_types, instance);

    let mut signature = oomir::Signature {
        params: params_oomir,
        ret: Box::new(return_oomir_ty.clone()), // Clone here to pass to convert_basic_block
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
                };
            }
        }
    }

    // Build a CodeBlock from the MIR basic blocks.
    let mut basic_blocks = HashMap::new();
    // MIR guarantees that the start block is BasicBlock 0.
    let entry_label = "bb0".to_string();

    let mir_cloned = mir.clone();

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
        if let Some((_tuple_param_name, tuple_param_ty)) = signature.params.first() {
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
                                name: "_1".to_string(),
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
    (
        oomir::Function {
            name: fn_name,
            signature,
            body: codeblock,
            is_static: false,
        },
        data_types.clone(),
    )
}
