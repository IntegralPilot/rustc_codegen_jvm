use super::place::{get_place_type, place_to_string};
use crate::oomir;

use super::place::emit_instructions_to_get_on_own;
use rustc_middle::{
    mir::{
        Body, Const, ConstOperand, ConstValue, Operand as MirOperand, Place,
        interpret::{ErrorHandled, Scalar},
    },
    ty::{ConstKind, EarlyBinder, Instance, Ty, TyCtxt, TypeVisitableExt, TypingEnv},
};
use std::collections::HashMap;

pub(super) mod const_eval;

/// Convert a MIR operand to an OOMIR operand.
pub fn convert_operand<'tcx>(
    mir_op: &MirOperand<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    match mir_op {
        MirOperand::Constant(box constant) => {
            match constant.const_ {
                Const::Val(const_val, ty) => {
                    handle_const_value(Some(constant), const_val, &ty, tcx, data_types, instance)
                }
                Const::Ty(const_ty, ty_const) => {
                    let ty_const = EarlyBinder::bind(tcx, ty_const)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let kind = ty_const.kind();
                    match kind {
                        ConstKind::Value(val) => {
                            let constval = tcx.valtree_to_const_val(val);
                            handle_const_value(None, constval, &const_ty, tcx, data_types, instance)
                        }
                        _ => {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "const-eval",
                                format!(
                                    "Warning: unhandled constant kind for a Ty const: {:?}",
                                    kind
                                )
                            );
                            oomir::Operand::Constant(oomir::Constant::I32(-1))
                        }
                    }
                }
                Const::Unevaluated(uv, ty) => {
                    let uv = EarlyBinder::bind(tcx, uv)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let ty = EarlyBinder::bind(tcx, ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let ty = tcx
                        .try_normalize_erasing_regions(
                            TypingEnv::fully_monomorphized(),
                            rustc_middle::ty::Unnormalized::new_wip(ty),
                        )
                        .unwrap_or(ty);
                    // If the constant depends on generic parameters (e.g. Self::Assoc),
                    // we cannot evaluate it at compile time (and trying to causes an ICE).
                    if uv.args.has_param() {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "const-eval",
                            format!("Skipping evaluation of generic constant {:?}", uv)
                        );
                        return oomir::Operand::Constant(oomir::Constant::I32(-2));
                    }

                    // Create the parameter environment. reveal_all is usually okay for codegen.
                    let typing_env = TypingEnv::post_analysis(tcx, uv.def);

                    // Try to evaluate the unevaluated constant using the correct function
                    // Use uv (the UnevaluatedConst) directly.
                    // Pass Some(span) for better error location if evaluation fails.
                    let span = tcx.def_span(uv.def); // Define `span` using the definition span of `uv`
                    match tcx.const_eval_resolve(typing_env, uv, span) {
                        Ok(const_val) => {
                            // Evaluation succeeded!
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "const-eval",
                                format!(
                                    "Successfully evaluated Unevaluated constant ({:?} at {:?}): {:?}",
                                    uv, span, const_val
                                )
                            );
                            // Now handle the resulting ConstValue using the existing function
                            handle_const_value(
                                Some(constant),
                                const_val,
                                &ty,
                                tcx,
                                data_types,
                                instance,
                            )
                        }
                        Err(ErrorHandled::Reported(error_reported, ..)) => {
                            // An error occurred during evaluation and rustc has already reported it.
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Error,
                                "const-eval",
                                format!(
                                    "ERROR: Const evaluation failed for {:?} (already reported) at {:?}. Error: {:?}",
                                    uv,
                                    span,
                                    error_reported // error_reported is a DiagnosticBuilder emission guarantee token
                                )
                            );
                            // You might want to propagate this error or panic.
                            oomir::Operand::Constant(oomir::Constant::I32(-1)) // Error placeholder
                        }
                        Err(ErrorHandled::TooGeneric(..)) => {
                            // The constant couldn't be evaluated because it depends on generic
                            // parameters that weren't fully specified. This is often an error
                            // for final codegen.
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "const-eval",
                                format!(
                                    "Warning: Could not evaluate Unevaluated constant {:?} at {:?} due to generics.",
                                    uv, span
                                )
                            );
                            oomir::Operand::Constant(oomir::Constant::I32(-2)) // Placeholder for generic error
                        }
                    }
                }
            }
        }
        MirOperand::Copy(place) | MirOperand::Move(place) => {
            let (final_var_name, get_instructions, final_type) =
                emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);

            instructions.extend(get_instructions);

            oomir::Operand::Variable {
                name: final_var_name,
                ty: final_type,
            }
        }
        MirOperand::RuntimeChecks(checks) => {
            oomir::Operand::Constant(oomir::Constant::Boolean(checks.value(tcx.sess)))
        }
    }
}

pub fn handle_const_value<'tcx>(
    _constant: Option<&ConstOperand>,
    const_val: ConstValue,
    ty: &Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> oomir::Operand {
    match const_val {
        ConstValue::Scalar(scalar) => match scalar {
            Scalar::Int(scalar_int) => match const_eval::read_scalar_int_constant(
                tcx, scalar_int, *ty, data_types, instance,
            ) {
                Ok(oomir_const) => oomir::Operand::Constant(oomir_const),
                Err(e) => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Error,
                        "const-eval",
                        format!(
                            "Error: Failed to convert scalar constant of type {:?}: {}",
                            ty, e
                        )
                    );
                    oomir::Operand::Constant(oomir::Constant::I32(-1))
                }
            },
            Scalar::Ptr(pointer, _) => {
                match const_eval::read_pointer_constant(tcx, pointer, *ty, data_types, instance) {
                    Ok(oomir_const) => oomir::Operand::Constant(oomir_const),
                    Err(e) => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "const-eval",
                            format!(
                                "Warning: Failed to read pointer constant of type {:?}: {}",
                                ty, e
                            )
                        );
                        oomir::Operand::Constant(oomir::Constant::I32(0))
                    }
                }
            }
        },
        ConstValue::Slice { alloc_id, meta } => {
            let pointee_ty = ty.builtin_deref(false).unwrap_or(*ty);
            let tail_ty = tcx.struct_tail_for_codegen(pointee_ty, TypingEnv::fully_monomorphized());

            if !tail_ty.is_str() && !tail_ty.is_slice() {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "const-eval",
                    format!(
                        "Warning: ConstValue::Slice for type {:?} has unsupported unsized tail {:?}",
                        ty, tail_ty
                    )
                );
                return oomir::Operand::Constant(oomir::Constant::String(
                    "UnsupportedSliceTail".to_string(),
                ));
            }

            match const_eval::read_slice_constant(
                tcx, alloc_id, meta, pointee_ty, data_types, instance,
            ) {
                Ok(constant) => oomir::Operand::Constant(constant),
                Err(error) => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "const-eval",
                        format!(
                            "Warning: Failed to decode slice-backed constant of type {:?}: {}",
                            ty, error
                        )
                    );
                    oomir::Operand::Constant(oomir::Constant::String("SliceReadError".to_string()))
                }
            }
        }
        ConstValue::ZeroSized => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Info: Encountered ZeroSized constant for type {:?}", ty)
            );
            match const_eval::read_zero_sized_constant(tcx, *ty, data_types, instance) {
                Ok(constant) => oomir::Operand::Constant(constant),
                Err(error) => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Error,
                        "const-eval",
                        format!("Failed to materialize ZST constant {ty:?}: {error}")
                    );
                    oomir::Operand::Constant(oomir::Constant::Unit)
                }
            }
        }
        ConstValue::Indirect { alloc_id, offset } => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Info: Handling Indirect constant (AllocId: {:?}, Offset: {:?}, Type: {:?})",
                    alloc_id, offset, ty
                )
            );
            // Get the allocation where the constant data resides
            match tcx.global_alloc(alloc_id) {
                rustc_middle::mir::interpret::GlobalAlloc::Memory(const_allocation) => {
                    let allocation = const_allocation.inner();
                    // Use the dedicated function to read the value from memory
                    match const_eval::read_constant_value_from_memory(
                        tcx, allocation,
                        offset, // The offset provided by ConstValue::Indirect
                        *ty,    // The type of the constant we are reading
                        data_types, instance,
                    ) {
                        Ok(oomir_const) => {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "const-eval",
                                format!(
                                    "Info: Successfully extracted indirect constant: {:?}",
                                    oomir_const
                                )
                            );
                            oomir::Operand::Constant(oomir_const)
                        }
                        Err(e) => {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Error,
                                "const-eval",
                                format!(
                                    "Error: Failed to read indirect constant of type {:?} from allocation {:?}: {}",
                                    ty, alloc_id, e
                                )
                            );
                            // Return an error placeholder, maybe distinct from other errors
                            oomir::Operand::Constant(oomir::Constant::I64(-60))
                        }
                    }
                }
                other_alloc => {
                    // This case should be rare for constants defined in code, but handle it defensively.
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "const-eval",
                        format!(
                            "Warning: Indirect constant points to unexpected GlobalAlloc kind: {:?}. AllocId: {:?}, Type: {:?}",
                            other_alloc, alloc_id, ty
                        )
                    );
                    oomir::Operand::Constant(oomir::Constant::I64(-61))
                }
            }
        }
    }
}

pub fn get_placeholder_operand<'tcx>(
    dest_place: &Place<'tcx>,
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Operand {
    let dest_oomir_type = get_place_type(dest_place, mir, tcx, instance, data_types);
    if dest_oomir_type.is_jvm_reference_type() {
        // Destination needs a reference, use Null placeholder
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "const-eval",
            format!(
                "Info: Generating Object placeholder for unhandled assignment to reference type var '{}' ({:?})",
                place_to_string(dest_place, tcx),
                dest_oomir_type
            )
        );
        oomir::Operand::Constant(oomir::Constant::Class(
            "java/lang/Object".to_string(), // Use Object as a placeholder
        ))
    } else {
        // Destination is likely a primitive, use I32(0) as placeholder
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "const-eval",
            format!(
                "Info: Generating I32(0) placeholder for unhandled assignment to primitive type var '{}' ({:?})",
                place_to_string(dest_place, tcx),
                dest_oomir_type
            )
        );
        oomir::Operand::Constant(oomir::Constant::I32(0))
    }
}

// For when you have an OOMIR Operand but just want the inner number it holds (only works for Consts)
// I8, I16, I32, I64, Char: Returns inner value
// F32, F64: Returns rounded inner value
// Boolean: Returns 1 for true, 0 for false
// Others: Returns None
pub fn extract_number_from_operand(operand: oomir::Operand) -> Option<i64> {
    match operand {
        oomir::Operand::Constant(constant) => match constant {
            oomir::Constant::I8(val) => Some(val as i64),
            oomir::Constant::I16(val) => Some(val as i64),
            oomir::Constant::I32(val) => Some(val as i64),
            oomir::Constant::I64(val) => Some(val),
            oomir::Constant::Boolean(val) => Some(if val { 1 } else { 0 }),
            oomir::Constant::Char(val) => Some(val as i64),
            oomir::Constant::F32(val) => Some(val.round() as i64),
            oomir::Constant::F64(val) => Some(val.round() as i64),
            _ => None,
        },
        oomir::Operand::Variable { .. } => None, // can't be known at compiletime
    }
}
