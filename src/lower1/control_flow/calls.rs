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
    mutable_borrow_arrays: &mut MutableBorrowMap<'tcx>,
    initialized_borrows: &mut HashSet<Local>,
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
    let destination_pointer_pointee = match &destination_oomir_type {
        oomir::Type::Pointer(pointee) => Some(pointee.as_ref().clone()),
        _ => None,
    };
    let deferred_destination_store =
        !destination.projection.is_empty() || data_types.local_uses_stable_cell(destination.local);
    let dest_var_name = destination_oomir_type.has_jvm_value().then(|| {
        if deferred_destination_store {
            format!("{}_call_result", label)
        } else {
            format!("_{}", destination.local.index())
        }
    });

    let typing_env = TypingEnv::post_analysis(tcx, mir.source.def_id());
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
                let sig = args.as_closure().sig();
                (
                    sig.inputs().skip_binder().to_vec(),
                    sig.output().skip_binder(),
                )
            }
            TyKind::FnDef(..) | TyKind::FnPtr(..) => {
                let sig = instance_ty.fn_sig(tcx).skip_binder();
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
            && let Some(tuple_class) = tuple_operand
                .get_type()
                .and_then(|ty| ty.get_class_name().map(str::to_string))
        {
            let prefix_len = oomir_operands.len() - 1;
            let tuple_fields = match data_types.get(&tuple_class) {
                Some(oomir::DataType::Class { fields, .. })
                    if fields.len() == fn_inputs.len() - prefix_len =>
                {
                    Some(fields.clone())
                }
                _ => None,
            };
            if let Some(tuple_fields) = tuple_fields {
                oomir_operands.pop();
                for (field_index, (field_name, field_ty)) in tuple_fields.into_iter().enumerate() {
                    let dest = format!("{label}_call_tuple_arg_{field_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: tuple_operand.clone(),
                        field_name,
                        field_ty: field_ty.clone(),
                        owner_class: tuple_class.clone(),
                    });
                    oomir_operands.push(oomir::Operand::Variable {
                        name: dest,
                        ty: field_ty,
                    });
                }
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
                &mut instructions,
                terminator,
                func_instance,
                oomir_operands,
                effective_dest,
                method_signature,
                jvm_import,
            );
        } else if let InstanceKind::Shim(ShimKind::DropGlue(_, drop_ty)) = func_instance.def {
            if let Some(drop_ty) = drop_ty
                && drop_ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
            {
                let drop_oomir_ty =
                    crate::lower1::types::ty_to_oomir_type(drop_ty, tcx, data_types, instance);
                let pointer = oomir_operands[0].clone();
                let value = if matches!(drop_ty.kind(), TyKind::Slice(_)) {
                    match pointer {
                        oomir::Operand::Variable { name, .. } => oomir::Operand::Variable {
                            name,
                            ty: drop_oomir_ty.clone(),
                        },
                        other => other,
                    }
                } else if matches!(pointer.get_type(), Some(oomir::Type::Pointer(_))) {
                    let value_name = format!("{label}_drop_glue_value");
                    crate::lower1::place::emit_pointer_read(
                        pointer,
                        &drop_oomir_ty,
                        &value_name,
                        &mut instructions,
                    )
                } else {
                    pointer
                };
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
        } else if matches!(tcx.def_kind(func_instance.def_id()), DefKind::Ctor(..)) {
            let TyKind::Adt(adt_def, _) = fn_output.kind() else {
                panic!("constructor returned non-ADT type {fn_output:?}")
            };
            if let Some(dest) = effective_dest {
                let base_class = oomir_output_type
                    .get_class_name()
                    .expect("constructor result has a JVM class");
                let class_name = if adt_def.is_enum() {
                    let variant_def_id = tcx.parent(func_instance.def_id());
                    format!(
                        "{}${}",
                        base_class,
                        jvm_names::member_name(tcx.item_name(variant_def_id).as_str())
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
        let indirect_sig = func_ty.fn_sig(tcx).skip_binder();
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
        // A returned `&mut U` is only a plain reborrow of an input
        // `&mut T` when U and T are the same representation. Methods
        // such as `ManuallyDrop::deref_mut` return a pointer derived
        // from a field of `T`; that Pointer already carries the field
        // address and must not be copied back as though it were T.
        if let [alias] = aliases.as_slice()
            && destination_pointer_pointee.as_ref() == Some(&alias.pointee_type)
        {
            mutable_borrow_arrays.insert(
                destination.local,
                PointerOrigin {
                    original_place: alias.original_place.clone(),
                    carrier_name: crate::lower1::place::place_to_string(destination, tcx),
                    pointee_type: alias.pointee_type.clone(),
                    writable: alias.writable,
                },
            );
            initialized_borrows.insert(destination.local);
        }
    }

    let mut writeback_locals = HashSet::default();
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

mod callables;
mod imports;
mod intrinsics;
mod memory_intrinsics;
mod method_dispatch;
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
