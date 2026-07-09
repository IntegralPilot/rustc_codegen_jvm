use super::{
    jvm_names,
    operand::convert_operand,
    place::{emit_instructions_to_get_on_own, emit_instructions_to_set_value, place_to_string},
    types::mir_int_to_oomir_const,
};
use crate::oomir;

use rustc_middle::{
    mir::{
        BasicBlock, BasicBlockData, Body, Local, Operand as MirOperand, Place, StatementKind,
        TerminatorKind,
    },
    ty::{Instance, TyCtxt, TyKind, TypingEnv},
};
use std::collections::HashMap;

mod checked_intrinsic_registry;
pub mod checked_intrinsics;
mod checked_ops;
mod rvalue;

pub use checked_intrinsic_registry::take_needed_intrinsics;

/// Convert a single MIR basic block into an OOMIR basic block.
pub fn convert_basic_block<'tcx>(
    bb: BasicBlock,
    bb_data: &BasicBlockData<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    return_oomir_type: &oomir::Type, // Pass function return type
    basic_blocks: &mut HashMap<String, oomir::BasicBlock>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::BasicBlock {
    // Use the basic block index as its label.
    let label = format!("bb{}", bb.index());
    let mut instructions = Vec::new();
    let mut mutable_borrow_arrays: HashMap<Local, (Place<'tcx>, String, oomir::Type)> =
        HashMap::new();

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
                    rvalue, place, // Pass original destination for temp naming hints
                    mir, tcx, instance, data_types,
                );

                // Add instructions needed to calculate the Rvalue
                instructions.extend(rvalue_instructions);

                if let rustc_middle::mir::Rvalue::Ref(
                    _,
                    rustc_middle::mir::BorrowKind::Mut { .. },
                    borrowed_place,
                ) = rvalue
                {
                    let dest_ty = place.ty(&mir.local_decls, tcx).ty;
                    let borrowed_is_trait_object = match dest_ty.kind() {
                        rustc_middle::ty::TyKind::Ref(_, pointee_ty, _) => {
                            matches!(pointee_ty.kind(), rustc_middle::ty::TyKind::Dynamic(_, _))
                        }
                        _ => false,
                    };
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
                                // Extract element type from array type
                                if let oomir::Type::MutableReference(element_ty) = array_ty {
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
                                        place.local, // The local holding the array reference (e.g., _3)
                                        (
                                            borrowed_place.clone(), // The original place borrowed (e.g., _1)
                                            array_var_name.clone(), // The OOMIR name of the array var (e.g., "3_tmp0")
                                            *element_ty.clone(), // The type of the element in the array
                                        ),
                                    );
                                } else {
                                    breadcrumbs::log!(
                                        breadcrumbs::LogLevel::Warn,
                                        "mir-lowering",
                                        format!(
                                            "Warning: Expected type for mutable borrow ref, found {:?}",
                                            array_ty
                                        )
                                    );
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
            }
            StatementKind::StorageLive(_) | StatementKind::StorageDead(_) => {
                // no-op, currently
            }
            StatementKind::Nop => {
                // Literally a no-op
            }
            StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!(
                        "Warning: StatementKind::SetDiscriminant NYI. Place: {:?}, Index: {:?}",
                        place, variant_index
                    )
                );
                // TODO: Need logic similar to emit_instructions_to_set_value but for discriminants
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
                // --- GENERAL SwitchInt Handling ---
                let discr_operand =
                    convert_operand(discr, tcx, instance, mir, data_types, &mut instructions);
                // Get the actual type of the discriminant from MIR local declarations
                let discr_ty = discr.ty(&mir.local_decls, tcx);

                // Convert the MIR targets into OOMIR (Constant, Label) pairs
                let oomir_targets: Vec<(oomir::Constant, String)> = targets
                    .iter()
                    .map(|(value, target_bb)| {
                        // Convert MIR value (u128) to appropriate OOMIR constant based on discr_ty
                        let oomir_const = mir_int_to_oomir_const(value, discr_ty, tcx); // Use helper
                        // Check if the constant type is suitable for a JVM switch
                    if !oomir_const.is_integer_like() {
                        breadcrumbs::log!(breadcrumbs::LogLevel::Warn, "mir-lowering", format!("Warning: SwitchInt target value {:?} for type {:?} cannot be directly used in JVM switch. Block: {}", oomir_const, discr_ty, label));
                             // Decide on fallback: error, skip target, default value?
                             // For now, let's potentially create an invalid switch target for lower2 to handle/error on.
                        }
                        let target_label = format!("bb{}", target_bb.index());
                        (oomir_const, target_label)
                    })
                    .collect();

                // Get the label for the 'otherwise' block
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
                // --- Argument Processing ---
                let mut pre_call_instructions = Vec::new();
                let oomir_operands: Vec<oomir::Operand> = args
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

                let dest_var_name = destination
                    .projection
                    .is_empty()
                    .then(|| format!("_{}", destination.local.index()));

                // --- Call Type Dispatch ---
                let func_ty = func.ty(mir, tcx);
                if let rustc_middle::ty::TyKind::FnDef(def_id, substs) = func_ty.kind() {
                    // Resolve the instance
                    let func_instance = rustc_middle::ty::Instance::resolve_for_fn_ptr(
                        tcx,
                        TypingEnv::post_analysis(tcx, mir.source.def_id()),
                        *def_id,
                        substs,
                    )
                    .unwrap();

                    let instance_ty = tcx
                        .type_of(func_instance.def_id())
                        .instantiate(tcx, func_instance.args)
                        .skip_norm_wip();
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

                    let oomir_output_type =
                        super::types::ty_to_oomir_type(fn_output, tcx, data_types, instance);

                    let effective_dest = if matches!(oomir_output_type, oomir::Type::Void) {
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
                        ret: Box::new(oomir_output_type),
                        is_static: false,
                    };

                    let assoc_item = tcx.opt_associated_item(func_instance.def_id());

                    if let Some(item) = assoc_item {
                        if item.is_method() {
                            // --- Instance Method (has 'self') ---
                            let receiver_mir_ty = args[0].node.ty(mir, tcx);
                            let receiver_operand = oomir_operands[0].clone();

                            // Keep the self parameter in the signature. Signature::to_string()
                            // is responsible for omitting the implicit JVM receiver.
                            method_signature.is_static = false;

                            // Separate args for InvokeInterface/InvokeVirtual (receiver handled via 'operand')
                            let method_args = oomir_operands[1..].to_vec();
                            let method_name = tcx.item_name(func_instance.def_id()).to_string();

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
                                let container_id = item.container_id(tcx);
                                let is_trait_method = matches!(
                                    tcx.def_kind(container_id),
                                    rustc_hir::def::DefKind::Trait
                                );

                                // Check if the receiver operand is an interface type (after any casts)
                                let receiver_oomir_ty = receiver_operand.get_type();

                                // Use InvokeInterface if:
                                // 1. The receiver type is explicitly an Interface type, OR
                                // 2. The method is declared in a trait (which maps to an interface)
                                let use_interface =
                                    if let Some(oomir::Type::Interface(interface_name)) =
                                        receiver_oomir_ty
                                    {
                                        Some(interface_name.clone())
                                    } else if is_trait_method {
                                        // Get the trait name and convert to interface name
                                        Some(jvm_names::class_for_def_id(tcx, container_id))
                                    } else {
                                        None
                                    };

                                if let Some(interface_name) = use_interface {
                                    // The method is from an interface - use InvokeInterface
                                    instructions.push(oomir::Instruction::InvokeInterface {
                                        class_name: interface_name,
                                        method_name,
                                        method_ty: method_signature,
                                        args: method_args,
                                        dest: effective_dest,
                                        operand: receiver_operand,
                                    });
                                } else {
                                    // The receiver is a concrete class - use InvokeVirtual
                                    let class_type = super::types::ty_to_oomir_type(
                                        receiver_mir_ty,
                                        tcx,
                                        data_types,
                                        instance,
                                    );
                                    let primitive_shim_class = match &class_type {
                                        oomir::Type::String => {
                                            Some("org/rustlang/primitives/RustString".to_string())
                                        }
                                        oomir::Type::F32 => {
                                            Some("org/rustlang/primitives/F32".to_string())
                                        }
                                        oomir::Type::F64 => {
                                            Some("org/rustlang/primitives/F64".to_string())
                                        }
                                        oomir::Type::Array(inner)
                                            if matches!(inner.as_ref(), oomir::Type::I16)
                                                && method_name == "starts_with" =>
                                        {
                                            Some("org/rustlang/core/Core".to_string())
                                        }
                                        _ => None,
                                    };

                                    if let Some(class_name) = primitive_shim_class {
                                        let mut static_signature = method_signature;
                                        static_signature.is_static = true;
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name,
                                            method_name,
                                            method_ty: static_signature,
                                            args: oomir_operands.clone(),
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

                                        instructions.push(oomir::Instruction::InvokeVirtual {
                                            class_name: class_name_opt.unwrap(),
                                            method_name,
                                            method_ty: method_signature,
                                            args: method_args,
                                            dest: effective_dest,
                                            operand: receiver_operand,
                                        });
                                    }
                                }
                            }
                        } else {
                            // --- Associated Static Function (NO 'self') ---
                            let method_name = tcx.item_name(func_instance.def_id()).to_string();
                            method_signature.is_static = true;

                            let container_id = item.container_id(tcx);
                            let self_ty_opt = if matches!(
                                tcx.def_kind(container_id),
                                rustc_hir::def::DefKind::Impl { .. }
                            ) {
                                Some(
                                    tcx.type_of(container_id)
                                        .instantiate(tcx, func_instance.args)
                                        .skip_norm_wip(),
                                )
                            } else {
                                func_instance.args.types().next()
                            };

                            let mut generated = false;
                            if let Some(self_ty) = self_ty_opt {
                                let class_type = super::types::ty_to_oomir_type(
                                    self_ty, tcx, data_types, instance,
                                );

                                if let Some(class_name) = class_type.get_class_name() {
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
                                instructions.push(oomir::Instruction::Call {
                                    class_name: fn_name_data.class_to_call_on,
                                    function: fn_name_data.method_name,
                                    args: oomir_operands.clone(),
                                    dest: effective_dest, // use effective_dest
                                });
                            }
                        }
                    } else {
                        // --- Free Function ---
                        let is_closure_call = matches!(instance_ty.kind(), TyKind::Closure(..));
                        let (class_name, function) = if is_closure_call {
                            (
                                None,
                                super::generate_closure_function_name(tcx, func_instance.def_id()),
                            )
                        } else {
                            let fn_name_data =
                                super::naming::mono_fn_name_from_instance(tcx, func_instance);
                            (fn_name_data.class_to_call_on, fn_name_data.method_name)
                        };
                        let call_args = if is_closure_call && !oomir_operands.is_empty() {
                            let closure_has_captures = match instance_ty.kind() {
                                TyKind::Closure(_, args) => {
                                    args.as_closure().upvar_tys().iter().next().is_some()
                                }
                                _ => false,
                            };

                            if closure_has_captures {
                                oomir_operands.clone()
                            } else {
                                oomir_operands[1..].to_vec()
                            }
                        } else {
                            oomir_operands.clone()
                        };
                        method_signature.is_static = true;

                        instructions.push(oomir::Instruction::Call {
                            class_name,
                            function,
                            args: call_args,
                            dest: effective_dest, // use effective_dest
                        });
                    }
                } else {
                    let func_oomir_operand =
                        convert_operand(&func, tcx, instance, mir, data_types, &mut instructions);

                    let oomir_sig =
                        super::types::fn_ptr_signature_from_ty(func_ty, tcx, data_types, instance);
                    super::types::ensure_fn_ptr_interface(&oomir_sig, data_types, tcx, instance);

                    let effective_dest = if matches!(oomir_sig.ret.as_ref(), oomir::Type::Void) {
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

                // --- Post-call Logic (Unchanged) ---
                let mut write_back_instrs = Vec::new();

                let mut operand_to_place_map = HashMap::new();
                for (mir_arg, oomir_op) in args.iter().zip(oomir_operands.iter()) {
                    if let MirOperand::Move(p) | MirOperand::Copy(p) = &mir_arg.node {
                        if p.projection.is_empty() {
                            operand_to_place_map.insert(oomir_op.clone(), p.clone());
                        }
                    }
                }

                for (mir_arg, oomir_arg_operand) in args.iter().zip(oomir_operands.iter()) {
                    let maybe_arg_place: Option<Place<'tcx>> = match &mir_arg.node {
                        MirOperand::Move(p) | MirOperand::Copy(p) => Some(p.clone()),
                        _ => None,
                    };

                    if let Some(arg_place) = maybe_arg_place {
                        if let Some((original_place, array_var_name, element_ty)) =
                            mutable_borrow_arrays.get(&arg_place.local)
                        {
                            if let oomir::Operand::Variable { .. } = oomir_arg_operand {
                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Info,
                                    "mir-lowering",
                                    format!(
                                        "Info: Emitting write-back for mutable borrow. Arg Place: {:?}, Original Place: {:?}, Array Var: {}",
                                        arg_place, original_place, array_var_name
                                    )
                                );

                                let temp_writeback_var =
                                    format!("_writeback_{}", original_place.local.index());

                                let array_operand = oomir::Operand::Variable {
                                    name: array_var_name.clone(),
                                    ty: oomir::Type::Array(Box::new(element_ty.clone())),
                                };
                                write_back_instrs.push(oomir::Instruction::ArrayGet {
                                    dest: temp_writeback_var.clone(),
                                    array: array_operand,
                                    index: oomir::Operand::Constant(oomir::Constant::I32(0)),
                                });

                                let value_to_set = oomir::Operand::Variable {
                                    name: temp_writeback_var,
                                    ty: element_ty.clone(),
                                };
                                let set_instrs = emit_instructions_to_set_value(
                                    original_place,
                                    value_to_set,
                                    tcx,
                                    instance,
                                    mir,
                                    data_types,
                                );
                                write_back_instrs.extend(set_instrs);
                            }
                        }
                    }
                }
                instructions.extend(write_back_instrs);

                if let Some(target_bb) = target {
                    let target_label = format!("bb{}", target_bb.index());
                    instructions.push(oomir::Instruction::Jump {
                        target: target_label,
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
                // --- End of condition operand handling ---

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

                // --- Add the failure block ---
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
                place: dropped_place,
                target,
                unwind: _,
                replace: _,
                drop: _,
            } => {
                // In simple cases (no custom Drop trait), a MIR drop often just signifies
                // the end of a scope before control flow continues.
                // We need to emit the jump to the target block.
                // We ignore unwind paths for now.
                // We also don't emit an explicit OOMIR 'drop' instruction yet,
                // as standard GC handles memory. If explicit resource cleanup (like file.close())
                // were needed, this would require much more complex handling (e.g., try-finally).

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!(
                        "Info: Handling Drop terminator for place {:?}. Jumping to target bb{}.",
                        place_to_string(dropped_place, tcx),
                        target.index()
                    )
                );

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
