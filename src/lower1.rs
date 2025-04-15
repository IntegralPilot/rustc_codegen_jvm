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

mod control_flow;
mod operand;
mod place;
mod types;

/// Converts a MIR Body into an OOMIR Function.
/// This function extracts a function’s signature (currently minimal) and builds
/// a control flow graph of basic blocks.
pub fn mir_to_oomir<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &mut Body<'tcx>,
) -> (oomir::Function, HashMap<String, oomir::DataType>) {
    // Get a function name from the instance.
    let fn_name = tcx.item_name(instance.def_id()).to_string();

    // Extract function signature
    let mir_sig = tcx.type_of(instance.def_id()).skip_binder().fn_sig(tcx);
    let params_ty = mir_sig.inputs();
    let return_ty = mir_sig.output();

    let data_types = &mut HashMap::new();

    let params_oomir_ty: Vec<oomir::Type> = params_ty
        .skip_binder()
        .iter()
        .map(|ty| ty_to_oomir_type(*ty, tcx, data_types))
        .collect();
    let return_oomir_ty: oomir::Type = ty_to_oomir_type(return_ty.skip_binder(), tcx, data_types);

    let mut signature = oomir::Signature {
        params: params_oomir_ty,
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
                    params: vec![oomir::Type::Array(Box::new(oomir::Type::Class(
                        "java/lang/String".to_string(),
                    )))],
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
            &mir_cloned,
            &return_oomir_ty,
            &mut basic_blocks,
            data_types,
        ); // Pass return type here
        basic_blocks.insert(bb_ir.label.clone(), bb_ir);
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
        },
        data_types.clone(),
    )
}
