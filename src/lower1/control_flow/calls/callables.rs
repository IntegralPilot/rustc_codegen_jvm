//! Callables.
use super::*;

pub(super) fn function_item<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    typing_env: TypingEnv<'tcx>,
    effective_dest: Option<String>,
    explicit_method_args: &[oomir::Operand],
    function_item: Instance<'tcx>,
) {
    let tuple_operand = explicit_method_args[0].clone();
    let function_ty = function_item.ty(tcx, typing_env);
    let function_sig = tcx.instantiate_bound_regions_with_erased(function_ty.fn_sig(tcx));
    let tuple_ty = Ty::new_tup(tcx, function_sig.inputs());
    let fields = crate::lower1::types::tuple_fields(
        tuple_ty,
        tuple_operand,
        &format!("{label}_fn_def_arg"),
        tcx,
        data_types,
        function_item,
        instructions,
    );
    let function_args = fields
        .into_iter()
        .zip(function_sig.inputs())
        .enumerate()
        .map(|(index, (field, input_ty))| {
            crate::lower1::value_repr::adapt_operand_to_rust_type(
                field,
                *input_ty,
                &format!("{label}_fn_def_arg_{index}_adapted"),
                tcx,
                instance,
                data_types,
                instructions,
            )
        })
        .collect();
    let target = data_types.function_name(tcx, function_item);
    instructions.push(oomir::Instruction::InvokeRustStatic {
        class_name: target
            .class_to_call_on
            .expect("function item has a JVM owner"),
        method_name: target.method_name,
        method_ty: oomir::Signature {
            params: function_sig
                .inputs()
                .iter()
                .enumerate()
                .map(|(index, ty)| {
                    (
                        format!("arg{index}"),
                        crate::lower1::types::ty_to_oomir_type(*ty, tcx, data_types, function_item),
                    )
                })
                .collect(),
            ret: Box::new(crate::lower1::types::ty_to_oomir_type(
                function_sig.output(),
                tcx,
                data_types,
                function_item,
            )),
            is_static: true,
        },
        args: function_args,
        dest: effective_dest,
    });
}
pub(super) fn ordinary_call<'tcx>(
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    external_interfaces: &mut HashSet<String>,
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    instance_ty: Ty<'tcx>,
    fn_inputs: Vec<Ty<'tcx>>,
    mut oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    mut method_signature: oomir::Signature,
) {
    let is_closure_call = matches!(instance_ty.kind(), TyKind::Closure(..));
    let closure_has_captures = matches!(
        instance_ty.kind(),
        TyKind::Closure(_, args)
            if args.as_closure().upvar_tys().iter().next().is_some()
    );
    let target = data_types.function_name(tcx, func_instance);
    let class_name = target
        .class_to_call_on
        .expect("monomorphized functions have JVM owners");
    let function = target.method_name;
    if crate::lower1::naming::is_global_link_symbol_class(&class_name) {
        for (index, input_ty) in fn_inputs.iter().enumerate() {
            let TyKind::Adt(adt_def, _) = input_ty.kind() else {
                continue;
            };
            if !crate::lower1::is_non_null_lang_item(tcx, adt_def.did()) {
                continue;
            }
            let Some(operand) = oomir_operands.get(index).cloned() else {
                continue;
            };
            let Some(oomir::Type::Class(wrapper_class)) = operand.get_type() else {
                continue;
            };
            let pointer_ty = match data_types.get(&wrapper_class) {
                Some(oomir::DataType::Class { fields, .. }) => fields
                    .iter()
                    .find(|(field_name, _)| field_name == "pointer")
                    .map(|(_, field_ty)| field_ty.clone())
                    .expect("NonNull carrier has a pointer field"),
                _ => panic!("NonNull carrier class {wrapper_class} was not generated"),
            };
            let unwrapped = format!("{label}_global_abi_arg_{index}");
            instructions.push(oomir::Instruction::GetField {
                dest: unwrapped.clone(),
                object: operand,
                field_name: "pointer".to_string(),
                field_ty: pointer_ty.clone(),
                owner_class: wrapper_class,
            });
            oomir_operands[index] = oomir::Operand::Variable {
                name: unwrapped,
                ty: pointer_ty.clone(),
            };
            method_signature.params[index].1 = pointer_ty;
        }
    }
    let flattened_closure_args =
        if is_closure_call && oomir_operands.len() == 2 && method_signature.params.len() > 1 {
            let tuple_ty = Ty::new_tup(tcx, &fn_inputs);
            Some(crate::lower1::types::tuple_fields(
                tuple_ty,
                oomir_operands[1].clone(),
                &format!("{label}_closure_arg"),
                tcx,
                data_types,
                func_instance,
                instructions,
            ))
        } else {
            None
        };
    let call_args = if let Some(flattened) = flattened_closure_args {
        if closure_has_captures {
            std::iter::once(oomir_operands[0].clone())
                .chain(flattened)
                .collect()
        } else {
            flattened
        }
    } else if is_closure_call && !oomir_operands.is_empty() {
        if closure_has_captures {
            oomir_operands.clone()
        } else {
            oomir_operands[1..].to_vec()
        }
    } else {
        oomir_operands.clone()
    };
    if closure_has_captures {
        let closure_env_ty = oomir_operands
            .first()
            .and_then(oomir::Operand::get_type)
            .expect("capturing closure calls have an environment operand");
        method_signature
            .params
            .insert(0, ("closure_env".to_string(), closure_env_ty));
    }
    method_signature.is_static = true;

    if crate::lower1::naming::instance_is_trait_interface_owned(tcx, func_instance, &class_name) {
        external_interfaces.insert(class_name.clone());
    }
    instructions.push(oomir::Instruction::InvokeRustStatic {
        class_name,
        method_name: function,
        method_ty: method_signature,
        args: call_args,
        dest: effective_dest, // use effective_dest
    });
}
