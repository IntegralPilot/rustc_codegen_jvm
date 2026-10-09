//! Calls.
use super::*;

pub(super) fn emit<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
    external_interfaces: &mut HashSet<String>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    args: &[rustc_span::Spanned<MirOperand<'tcx>>],
    terminator: &rustc_middle::mir::Terminator<'tcx>,
    func: &MirOperand<'tcx>,
    destination: &Place<'tcx>,
    target: &Option<BasicBlock>,
) {
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
        oomir_operands[index] = crate::lower1::value_repr::adapt_operand_to_rust_type(
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
        crate::lower1::place::get_place_type(destination, mir, tcx, instance, data_types);
    let deferred_destination_store =
        !destination.projection.is_empty() || data_types.local_uses_stable_cell(destination.local);
    let dest_var_name = destination_oomir_type.has_jvm_value().then(|| {
        if deferred_destination_store {
            format!("{}_call_result", label)
        } else {
            format!("_{}", destination.local.index())
        }
    });

    // These calls are already instantiated for codegen. Generic predicates
    // from the source MIR can hide concrete impls behind its parameter bounds.
    let typing_env = TypingEnv::fully_monomorphized();
    let instantiated_func_ty =
        EarlyBinder::bind(tcx, func.ty(mir, tcx)).instantiate(tcx, instance.args);
    let func_ty = tcx
        .try_normalize_erasing_regions(typing_env, instantiated_func_ty)
        .unwrap_or_else(|_| instantiated_func_ty.skip_norm_wip());
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
                let sig = tcx.instantiate_bound_regions_with_erased(args.as_closure().sig());
                (sig.inputs().to_vec(), sig.output())
            }
            TyKind::FnDef(..) | TyKind::FnPtr(..) => {
                let sig = tcx.instantiate_bound_regions_with_erased(instance_ty.fn_sig(tcx));
                (sig.inputs().to_vec(), sig.output())
            }
            _ => {
                // Compiler-generated callable bodies have no `FnSig`;
                // their MIR argument locals and return place define the ABI.
                let body = tcx.instance_mir(func_instance.def);
                let inputs = (1..=body.arg_count)
                    .map(|index| {
                        EarlyBinder::bind(tcx, body.local_decls[Local::from_usize(index)].ty)
                            .instantiate(tcx, func_instance.args)
                            .skip_norm_wip()
                    })
                    .collect();
                let output = EarlyBinder::bind(tcx, body.local_decls[Local::from_usize(0)].ty)
                    .instantiate(tcx, func_instance.args)
                    .skip_norm_wip();
                (inputs, output)
            }
        };

        if !matches!(instance_ty.kind(), TyKind::Closure(..)) {
            for (index, target_ty) in fn_inputs.iter().enumerate() {
                let Some(source) = oomir_operands.get(index).cloned() else {
                    break;
                };
                oomir_operands[index] = crate::lower1::value_repr::adapt_operand_to_rust_type(
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
        if fn_inputs.len() > oomir_operands.len()
            && let Some(tuple_operand) = oomir_operands.last().cloned()
            && let Some(arg) = args.last()
        {
            let tuple_ty = data_types.normalize(tcx, arg.node.ty(mir, tcx), instance);
            let prefix = oomir_operands.len() - 1;
            if matches!(tuple_ty.kind(), TyKind::Tuple(fields) if fields.len() == fn_inputs.len() - prefix)
            {
                oomir_operands.pop();
                oomir_operands.extend(crate::lower1::types::tuple_fields(
                    tuple_ty,
                    tuple_operand,
                    &format!("{label}_call_tuple_arg"),
                    tcx,
                    data_types,
                    instance,
                    instructions,
                ));
            }
        }

        let oomir_output_type =
            crate::lower1::types::ty_to_oomir_type(fn_output, tcx, data_types, instance);

        let effective_dest = if !oomir_output_type.has_jvm_value() {
            None
        } else {
            dest_var_name.clone()
        };

        let oomir_input_types: Vec<oomir::Type> = fn_inputs
            .iter()
            .map(|ty| crate::lower1::types::ty_to_oomir_type(*ty, tcx, data_types, instance))
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

        let jvm_import = crate::lower1::naming::jvm_import_from_instance(tcx, func_instance)
            .unwrap_or_else(|message| tcx.dcx().span_fatal(terminator.source_info.span, message));
        let assoc_item = tcx.opt_associated_item(func_instance.def_id());

        if let Some(jvm_import) = jvm_import {
            imports::emit(
                tcx,
                data_types,
                &mut instructions,
                terminator,
                func_instance,
                oomir_operands,
                effective_dest,
                method_signature,
                jvm_import,
            );
        } else if let InstanceKind::Shim(ShimKind::DropGlue(_, drop_ty)) = func_instance.def {
            if let Some(drop_ty) = drop_ty {
                emit_drop_in_place(
                    drop_ty,
                    oomir_operands[0].clone(),
                    tcx,
                    instance,
                    data_types,
                    &mut instructions,
                );
            }
        } else if matches!(tcx.def_kind(func_instance.def_id()), DefKind::Ctor(..)) {
            let TyKind::Adt(adt_def, _) = fn_output.kind() else {
                panic!("constructor returned non-ADT type {fn_output:?}")
            };
            if let Some(payload) = crate::lower1::types::transparent_payload(fn_output, tcx) {
                if let Some(dest) = effective_dest {
                    instructions.push(oomir::Instruction::Move {
                        dest,
                        src: oomir_operands[payload.field].clone(),
                    });
                }
            } else if let Some(word) = crate::lower1::types::packed_word(fn_output, tcx) {
                if let Some(dest) = effective_dest {
                    let mut value = word.zero();
                    for (index, field) in oomir_operands.into_iter().enumerate() {
                        value = word.insert(
                            value,
                            index,
                            field,
                            &format!("{dest}_field_{index}"),
                            &mut instructions,
                        );
                    }
                    instructions.push(oomir::Instruction::Move { dest, src: value });
                }
            } else if matches!(oomir_output_type, oomir::Type::TaggedI64) {
                if let Some(dest) = effective_dest {
                    instructions.push(crate::lower1::types::tagged_value(
                        dest,
                        oomir_operands
                            .into_iter()
                            .next()
                            .expect("Some has one payload"),
                        1,
                    ));
                }
            } else if let Some(carrier) = crate::lower1::types::enum_carrier(fn_output, tcx) {
                let variant = tcx.parent(func_instance.def_id());
                if adt_def.variant(carrier.variant).def_id != variant {
                    instructions.push(oomir::Instruction::Unreachable);
                } else if let Some(dest) = effective_dest {
                    instructions.push(oomir::Instruction::Move {
                        dest,
                        src: oomir_operands
                            .into_iter()
                            .next()
                            .expect("direct enum has one field"),
                    });
                }
            } else if let Some(dest) = effective_dest {
                let base_class = oomir_output_type
                    .get_class_name()
                    .expect("constructor result has a JVM class");
                let class_name = if adt_def.is_enum() {
                    let variant_def_id = tcx.parent(func_instance.def_id());
                    format!(
                        "{}${}",
                        base_class,
                        crate::lower1::types::enum_variant_name(
                            adt_def.variant(adt_def.variant_index_with_id(variant_def_id)),
                            tcx,
                        )
                    )
                } else {
                    base_class.to_string()
                };
                let constructor_args = oomir_operands
                    .into_iter()
                    .filter_map(|operand| {
                        let operand_ty = operand.get_type()?;
                        operand_ty.has_jvm_value().then_some((operand, operand_ty))
                    })
                    .collect();
                instructions.push(oomir::Instruction::ConstructObject {
                    dest,
                    class_name,
                    args: constructor_args,
                });
            }
        } else if let Some(item) = assoc_item {
            methods::emit(
                tcx,
                instance,
                mir,
                data_types,
                &label,
                &mut instructions,
                args,
                func_instance,
                typing_env,
                fn_inputs,
                fn_output,
                oomir_output_type,
                oomir_operands,
                effective_dest,
                method_signature,
                item,
            );
        } else {
            intrinsics::emit(
                tcx,
                instance,
                mir,
                data_types,
                external_interfaces,
                &label,
                &mut instructions,
                terminator,
                func_instance,
                instance_ty,
                fn_inputs,
                fn_output,
                oomir_output_type,
                oomir_operands,
                effective_dest,
                method_signature,
            );
        }
    } else {
        let indirect_sig = tcx.instantiate_bound_regions_with_erased(func_ty.fn_sig(tcx));
        for (index, target_ty) in indirect_sig.inputs().iter().enumerate() {
            let Some(source) = oomir_operands.get(index).cloned() else {
                break;
            };
            oomir_operands[index] = crate::lower1::value_repr::adapt_operand_to_rust_type(
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
            crate::lower1::types::fn_ptr_signature_from_ty(func_ty, tcx, data_types, instance);
        crate::lower1::types::ensure_fn_ptr_interface(&oomir_sig, data_types, tcx, instance);

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

    if let Some(target_bb) = target {
        let target_label = format!("bb{}", target_bb.index());
        instructions.push(oomir::Instruction::Jump {
            target: target_label,
        });
    } else {
        instructions.push(oomir::Instruction::Unreachable);
    }
}

mod callables;
mod imports;
mod intrinsics;
mod memory_intrinsics;
mod methods;
mod numeric_intrinsics;
mod numeric_methods;
mod pointer_access;
mod pointer_address;
mod pointer_intrinsics;
mod pointer_metadata;
mod runtime_methods;
mod type_intrinsics;
mod unwind;
