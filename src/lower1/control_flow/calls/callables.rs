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
    let function_sig = function_ty.fn_sig(tcx).skip_binder();
    let value_input_count = function_sig
        .inputs()
        .iter()
        .filter(|ty| {
            crate::lower1::types::ty_to_oomir_type(**ty, tcx, data_types, function_item)
                .has_jvm_value()
        })
        .count();
    let (tuple_class, tuple_fields) = if value_input_count == 0 {
        (None, Vec::new())
    } else {
        let tuple_class = tuple_operand
            .get_type()
            .and_then(|ty| ty.get_class_name().map(str::to_string))
            .expect("non-empty Fn argument tuple has a JVM class");
        let fields = match data_types.get(&tuple_class) {
            Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
            _ => panic!("Fn argument tuple class {tuple_class} was not defined"),
        };
        (Some(tuple_class), fields)
    };
    let mut function_args = Vec::new();
    let mut tuple_fields = tuple_fields.into_iter();
    for (index, input_ty) in function_sig.inputs().iter().enumerate() {
        let input_oomir_ty =
            crate::lower1::types::ty_to_oomir_type(*input_ty, tcx, data_types, function_item);
        if !input_oomir_ty.has_jvm_value() {
            function_args.push(oomir::Operand::Constant(oomir::Constant::Unit));
            continue;
        }
        let (field_name, field_ty) = tuple_fields
            .next()
            .expect("Fn argument tuple has one field per JVM argument");
        let field_dest = format!("{label}_fn_def_arg_{index}");
        instructions.push(oomir::Instruction::GetField {
            dest: field_dest.clone(),
            object: tuple_operand.clone(),
            field_name,
            field_ty: field_ty.clone(),
            owner_class: tuple_class
                .clone()
                .expect("value-bearing Fn tuple has a class"),
        });
        function_args.push(crate::lower1::value_repr::adapt_operand_to_rust_type(
            oomir::Operand::Variable {
                name: field_dest,
                ty: field_ty,
            },
            *input_ty,
            &format!("{label}_fn_def_arg_{index}_adapted"),
            tcx,
            instance,
            data_types,
            &mut instructions,
        ));
    }
    let target = crate::lower1::naming::mono_fn_name_from_instance(tcx, function_item);
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
    let (class_name, function) = if is_closure_call {
        (
            crate::lower1::naming::mono_owner_class(tcx, func_instance),
            crate::lower1::generate_closure_function_name(tcx, func_instance),
        )
    } else {
        let fn_name_data = crate::lower1::naming::mono_fn_name_from_instance(tcx, func_instance);
        (
            fn_name_data
                .class_to_call_on
                .expect("monomorphized functions have JVM owners"),
            fn_name_data.method_name,
        )
    };
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
            oomir_operands[1]
                .get_type()
                .and_then(|ty| ty.get_class_name().map(str::to_string))
                .and_then(|tuple_class| {
                    let fields = match data_types.get(&tuple_class) {
                        Some(oomir::DataType::Class { fields, .. })
                            if fields.len() == method_signature.params.len() =>
                        {
                            fields.clone()
                        }
                        _ => return None,
                    };
                    let mut flattened = Vec::new();
                    for (field_index, (field_name, field_ty)) in fields.into_iter().enumerate() {
                        let dest = format!("{label}_closure_arg_{field_index}");
                        instructions.push(oomir::Instruction::GetField {
                            dest: dest.clone(),
                            object: oomir_operands[1].clone(),
                            field_name,
                            field_ty: field_ty.clone(),
                            owner_class: tuple_class.clone(),
                        });
                        flattened.push(oomir::Operand::Variable {
                            name: dest,
                            ty: field_ty,
                        });
                    }
                    Some(flattened)
                })
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
