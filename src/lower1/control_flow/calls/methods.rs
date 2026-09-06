//! Methods.
use super::*;

pub(super) fn emit<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    args: &[rustc_span::Spanned<MirOperand<'tcx>>],
    func_instance: Instance<'tcx>,
    typing_env: TypingEnv<'tcx>,
    fn_inputs: Vec<Ty<'tcx>>,
    fn_output: Ty<'tcx>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    mut method_signature: oomir::Signature,
    item: rustc_middle::ty::AssocItem,
) {
    if item.is_method() {
        let receiver_mir_ty = args[0].node.ty(mir, tcx);
        let receiver_operand = oomir_operands[0].clone();

        // Keep the self parameter in the signature. Signature::to_string()
        // is responsible for omitting the implicit JVM receiver.
        method_signature.is_static = false;

        // Separate args for InvokeInterface/InvokeVirtual (receiver handled via 'operand')
        let method_args = oomir_operands[1..].to_vec();
        let explicit_method_args = if method_signature
            .params
            .last()
            .is_some_and(|(name, _)| name == oomir::CALLER_LOCATION_PARAM_NAME)
        {
            &method_args[..method_args.len() - 1]
        } else {
            &method_args[..]
        };
        let method_name = crate::lower1::naming::associated_method_name_from_instance(
            tcx,
            func_instance,
            &method_signature,
        );
        let declared_method_name = item.name().as_str().to_string();
        let is_provided_trait_method = item.trait_container(tcx).is_some_and(|trait_def_id| {
            tcx.provided_trait_methods(trait_def_id).any(|provided| {
                provided.def_id == item.def_id || item.trait_item_def_id() == Some(provided.def_id)
            })
        });
        let method_requires_sized_self = tcx.generics_require_sized_self(item.def_id);
        let transparent_virtual_receiver = if matches!(func_instance.def, InstanceKind::Virtual(..))
            && !method_requires_sized_self
        {
            match receiver_operand.get_type() {
                Some(oomir::Type::Class(receiver_class)) => match data_types.get(&receiver_class) {
                    Some(oomir::DataType::Class { fields, .. })
                        if fields.len() == 1
                            && matches!(fields[0].1, oomir::Type::Interface(_)) =>
                    {
                        let (field_name, field_ty) = fields[0].clone();
                        let dest = format!("{label}_virtual_trait_receiver");
                        instructions.push(oomir::Instruction::GetField {
                            dest: dest.clone(),
                            object: receiver_operand.clone(),
                            field_name,
                            field_ty: field_ty.clone(),
                            owner_class: receiver_class,
                        });
                        Some(oomir::Operand::Variable {
                            name: dest,
                            ty: field_ty,
                        })
                    }
                    _ => None,
                },
                _ => None,
            }
        } else {
            None
        };

        let callable_trait_method = item.trait_container(tcx).is_some_and(|trait_def_id| {
            let lang_items = tcx.lang_items();
            [
                lang_items.fn_trait(),
                lang_items.fn_mut_trait(),
                lang_items.fn_once_trait(),
            ]
            .contains(&Some(trait_def_id))
        });
        if let Some(callable_abi) = callable_trait_method
            .then(|| {
                crate::lower1::types::callable_trait_object_abi(
                    receiver_mir_ty,
                    tcx,
                    data_types,
                    instance,
                )
            })
            .flatten()
        {
            let mut flattened_args = Vec::new();
            if !callable_abi.signature.params.is_empty() {
                let tuple_operand = method_args
                    .first()
                    .cloned()
                    .expect("Fn trait calls carry an argument tuple");
                let tuple_oomir_ty = tuple_operand
                    .get_type()
                    .expect("Fn trait argument tuple is typed");
                let tuple_class = tuple_oomir_ty
                    .get_class_name()
                    .expect("non-unit Fn argument tuple is a JVM class")
                    .to_string();
                let fields = match data_types.get(&tuple_class) {
                    Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
                    _ => panic!("Fn argument tuple class {tuple_class} was not defined"),
                };
                for (field_index, (field_name, field_ty)) in fields.into_iter().enumerate() {
                    if !field_ty.has_jvm_value() {
                        continue;
                    }
                    let field_dest = format!("{label}_callable_arg_{field_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: field_dest.clone(),
                        object: tuple_operand.clone(),
                        field_name,
                        field_ty: field_ty.clone(),
                        owner_class: tuple_class.clone(),
                    });
                    flattened_args.push(oomir::Operand::Variable {
                        name: field_dest,
                        ty: field_ty,
                    });
                }
            }
            instructions.push(oomir::Instruction::InvokeInterface {
                class_name: callable_abi.interface_name,
                method_name: "call".to_string(),
                // This descriptor contains only the flattened Java
                // arguments; the receiver is carried separately.
                method_ty: callable_abi.signature,
                args: flattened_args,
                dest: effective_dest,
                operand: receiver_operand,
            });
        } else if let Some(interface_receiver) = transparent_virtual_receiver {
            let Some(oomir::Type::Interface(interface_name)) = interface_receiver.get_type() else {
                unreachable!("validated transparent trait receiver")
            };
            instructions.push(oomir::Instruction::InvokeInterface {
                class_name: interface_name,
                method_name: declared_method_name,
                method_ty: method_signature,
                args: method_args,
                dest: effective_dest,
                operand: interface_receiver,
            });
        } else if matches!(func_instance.def, InstanceKind::Virtual(..))
            && let rustc_middle::ty::TyKind::Dynamic(predicates, ..) = receiver_mir_ty.kind()
        {
            let principal = predicates.principal().unwrap().skip_binder();
            let interface_name = match receiver_operand.get_type() {
                Some(oomir::Type::Interface(interface_name)) => interface_name,
                _ => jvm_names::class_for_def_id(tcx, principal.def_id),
            };
            instructions.push(oomir::Instruction::InvokeInterface {
                class_name: interface_name,
                method_name: declared_method_name,
                method_ty: method_signature,
                args: method_args,
                dest: effective_dest,
                operand: receiver_operand,
            });
        } else {
            // Check if this method is declared in a trait (interface)
            let trait_container = item.trait_container(tcx);

            // Check if the receiver operand is an interface type (after any casts)
            let receiver_oomir_ty = receiver_operand.get_type();
            let has_concrete_receiver_method = match &receiver_oomir_ty {
                Some(oomir::Type::Class(class_name)) => matches!(
                    data_types.get(class_name),
                    Some(oomir::DataType::Class { methods, .. })
                        if methods.contains_key(&method_name)
                ),
                _ => false,
            };
            let uses_concrete_trait_default =
                is_provided_trait_method && !has_concrete_receiver_method;

            // Use InvokeInterface if:
            // 1. The receiver type is explicitly an Interface type, OR
            // 2. The method is declared in a trait (which maps to an interface)
            let use_interface =
                if let Some(oomir::Type::Interface(interface_name)) = &receiver_oomir_ty {
                    Some(interface_name.clone())
                } else if let Some(trait_def_id) = trait_container
                    && item.impl_container(tcx).is_none()
                    && !has_concrete_receiver_method
                {
                    // Get the trait name and convert to interface name
                    let interface_name = jvm_names::class_for_def_id(tcx, trait_def_id);
                    matches!(
                        data_types.get(&interface_name),
                        Some(oomir::DataType::Interface { methods, .. })
                            if methods.contains_key(&declared_method_name)
                    )
                    .then_some(interface_name)
                } else {
                    None
                };
            let dispatch_receiver_ty =
                crate::lower1::types::ty_to_oomir_type(receiver_mir_ty, tcx, data_types, instance);
            let resolved_receiver_mir_ty = EarlyBinder::bind(tcx, receiver_mir_ty)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let receiver_self_mir_ty = match resolved_receiver_mir_ty.kind() {
                TyKind::Ref(_, pointee, _) => *pointee,
                _ => resolved_receiver_mir_ty,
            };
            let impl_container_mir_ty = item.impl_container(tcx).map(|impl_def_id| {
                tcx.type_of(impl_def_id)
                    .instantiate(tcx, func_instance.args)
                    .skip_norm_wip()
            });
            let inherent_container_mir_ty = item
                .impl_container(tcx)
                .filter(|impl_def_id| tcx.impl_opt_trait_ref(*impl_def_id).is_none())
                .and(impl_container_mir_ty);
            let trait_impl_self_requires_static_dispatch = item
                .impl_container(tcx)
                .is_some_and(|impl_def_id| tcx.impl_opt_trait_ref(impl_def_id).is_some())
                && !has_concrete_receiver_method;
            let has_arbitrary_self_receiver = inherent_container_mir_ty
                .is_some_and(|container_ty| container_ty != receiver_self_mir_ty);
            let mut receiver_value_mir_ty = receiver_self_mir_ty;
            while let TyKind::Ref(_, pointee, _) = receiver_value_mir_ty.kind() {
                receiver_value_mir_ty = *pointee;
            }
            let trait_object_method_requires_static_dispatch = if let TyKind::Dynamic(predicates, _) =
                receiver_value_mir_ty.kind()
                && let Some(principal) = predicates.principal()
                && item.trait_container(tcx).is_some()
            {
                let trait_ref = tcx.instantiate_bound_regions_with_erased(
                    principal.with_self_ty(tcx, receiver_value_mir_ty),
                );
                !tcx.vtable_entries(trait_ref).iter().any(|entry| {
                    let VtblEntry::Method(method) = entry else {
                        return false;
                    };
                    method.def_id() == item.def_id
                        || tcx
                            .opt_associated_item(method.def_id())
                            .and_then(|method_item| method_item.trait_item_def_id())
                            == Some(item.def_id)
                })
            } else {
                false
            };
            let has_enum_reference_receiver = matches!(
                resolved_receiver_mir_ty.kind(),
                TyKind::Ref(_, pointee, _)
                    if matches!(pointee.kind(), TyKind::Adt(adt_def, _)
                        if adt_def.is_enum())
            );
            let method_has_own_generic_params = !tcx
                .generics_of(func_instance.def_id())
                .own_params
                .is_empty();
            // The class behind a reference implements
            // methods for its pointee, not blanket Rust
            // impls whose `Self` is the reference itself
            // when that pointee has no generated instance
            // methods (notably closures and function items).
            let receiver_self_requires_static_dispatch = inherent_container_mir_ty.is_some()
                || has_enum_reference_receiver
                || has_arbitrary_self_receiver
                || trait_impl_self_requires_static_dispatch
                || trait_object_method_requires_static_dispatch
                || method_requires_sized_self
                || matches!(
                    receiver_value_mir_ty.kind(),
                    TyKind::Closure(..) | TyKind::FnDef(..) | TyKind::FnPtr(..)
                )
                || matches!(
                    receiver_self_mir_ty.kind(),
                    TyKind::Ref(..) if method_has_own_generic_params
                )
                || matches!(
                    receiver_self_mir_ty.kind(),
                    TyKind::RawPtr(..) | TyKind::FnPtr(..)
                );
            let pointer_api_receiver = {
                let receiver_value_ty = match resolved_receiver_mir_ty.kind() {
                    TyKind::Ref(_, pointee, _) => *pointee,
                    _ => resolved_receiver_mir_ty,
                };
                matches!(receiver_value_ty.kind(), TyKind::RawPtr(..))
                    || matches!(
                        receiver_value_ty.kind(),
                        TyKind::Adt(adt_def, _)
                        if crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
                    )
            };
            let zero_sized_raw_pointer_offset = matches!(
                declared_method_name.as_str(),
                "add" | "sub" | "offset" | "wrapping_add" | "wrapping_sub" | "wrapping_offset"
            ) && {
                let receiver_value_ty = match resolved_receiver_mir_ty.kind() {
                    TyKind::Ref(_, pointee, _) => *pointee,
                    _ => resolved_receiver_mir_ty,
                };
                match receiver_value_ty.kind() {
                    TyKind::RawPtr(pointee, _) => {
                        crate::lower1::types::layout_size_bytes(tcx, *pointee)
                            .is_ok_and(|size| size == 0)
                    }
                    _ => false,
                }
            };
            let direct_fn_def_instance = match resolved_receiver_mir_ty.kind() {
                TyKind::FnDef(def_id, args)
                    if matches!(
                        declared_method_name.as_str(),
                        "call" | "call_mut" | "call_once"
                    ) && explicit_method_args.len() == 1 =>
                {
                    Instance::resolve_for_fn_ptr(
                        tcx,
                        typing_env,
                        *def_id,
                        args.no_bound_vars().unwrap(),
                    )
                    .filter(|target| tcx.is_mir_available(target.def_id()))
                }
                _ => None,
            };
            let is_pointer_cast_method = [
                sym::const_ptr_cast,
                sym::ptr_cast,
                sym::ptr_cast_const,
                sym::ptr_cast_mut,
            ]
            .into_iter()
            .any(|diagnostic| tcx.is_diagnostic_item(diagnostic, func_instance.def_id()));
            let is_pointer_null_method = tcx
                .is_diagnostic_item(sym::ptr_const_is_null, func_instance.def_id())
                || tcx.is_diagnostic_item(sym::ptr_is_null, func_instance.def_id());
            let comparison_value_ty =
                comparison_value_type(dispatch_receiver_ty.clone(), resolved_receiver_mir_ty);
            let comparison_rhs_ty = oomir_operands
                .get(1)
                .and_then(oomir::Operand::get_type)
                .zip(fn_inputs.get(1).copied())
                .map(|(ty, mir_ty)| comparison_value_type(ty, mir_ty));
            let non_null_carrier = non_null_comparison_carrier(
                tcx,
                resolved_receiver_mir_ty,
                &comparison_value_ty,
                data_types,
            );
            let direct_non_null_fat_equality = non_null_carrier
                .is_some_and(|ty| matches!(ty, oomir::Type::Slice(_) | oomir::Type::Str));
            let direct_equality = matches!(declared_method_name.as_str(), "eq" | "ne")
                && (supports_direct_equality(&comparison_value_ty)
                    || non_null_carrier.is_some_and(|ty| {
                        supports_direct_equality(ty) || direct_non_null_fat_equality
                    }))
                && comparison_rhs_ty.as_ref() == Some(&comparison_value_ty)
                && oomir_operands.len() == 2;
            let direct_ordering =
                matches!(declared_method_name.as_str(), "lt" | "le" | "gt" | "ge")
                    && (supports_direct_ordering(&comparison_value_ty)
                        || non_null_carrier.is_some_and(supports_direct_ordering))
                    && comparison_rhs_ty.as_ref() == Some(&comparison_value_ty)
                    && oomir_operands.len() == 2;
            let direct_unit_comparison = comparison_value_ty == oomir::Type::Unit
                && comparison_rhs_ty.as_ref() == Some(&oomir::Type::Unit)
                && oomir_operands.len() == 2
                && matches!(
                    declared_method_name.as_str(),
                    "eq" | "ne" | "lt" | "le" | "gt" | "ge"
                );

            let direct_wrapping_integer_op = matches!(
                declared_method_name.as_str(),
                "wrapping_add" | "wrapping_sub" | "wrapping_mul"
            ) && (matches!(
                &dispatch_receiver_ty,
                oomir::Type::I8
                    | oomir::Type::U8
                    | oomir::Type::I16
                    | oomir::Type::U16
                    | oomir::Type::I32
                    | oomir::Type::U32
                    | oomir::Type::I64
                    | oomir::Type::U64
            ) || matches!(
                &dispatch_receiver_ty,
                oomir::Type::Class(class_name)
                    if class_name == crate::lower2::I128_CLASS
                        || class_name == crate::lower2::U128_CLASS
            )) && oomir_operands.len() == 2;

            let direct_overflowing_integer_op = matches!(
                declared_method_name.as_str(),
                "overflowing_add" | "overflowing_sub" | "overflowing_mul"
            ) && (matches!(
                &dispatch_receiver_ty,
                oomir::Type::I8
                    | oomir::Type::U8
                    | oomir::Type::I16
                    | oomir::Type::U16
                    | oomir::Type::I32
                    | oomir::Type::U32
                    | oomir::Type::I64
                    | oomir::Type::U64
            ) || matches!(
                &dispatch_receiver_ty,
                oomir::Type::Class(class_name)
                    if class_name == crate::lower2::I128_CLASS
                        || class_name == crate::lower2::U128_CLASS
            )) && oomir_operands.len() == 2;

            let direct_f16_to_bits = dispatch_receiver_ty == oomir::Type::F16
                && declared_method_name == "to_bits"
                && oomir_operands.len() == 1;

            if direct_f16_to_bits {
                if let Some(dest) = effective_dest {
                    instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(dest),
                        class_name: "org/rustlang/runtime/Numbers".to_string(),
                        method_name: "f16ToBits".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![("value".to_string(), oomir::Type::F16)],
                            ret: Box::new(oomir::Type::U16),
                            is_static: true,
                        },
                        args: vec![oomir_operands[0].clone()],
                    });
                }
            } else if direct_unit_comparison {
                if let Some(dest) = effective_dest {
                    instructions.push(oomir::Instruction::Move {
                        dest,
                        src: oomir::Operand::Constant(oomir::Constant::Boolean(matches!(
                            declared_method_name.as_str(),
                            "eq" | "le" | "ge"
                        ))),
                    });
                }
            } else if zero_sized_raw_pointer_offset {
                if let Some(dest) = effective_dest {
                    instructions.push(oomir::Instruction::Move {
                        dest,
                        src: oomir_operands[0].clone(),
                    });
                }
            } else if direct_overflowing_integer_op {
                numeric_methods::overflowing(
                    data_types,
                    &mut instructions,
                    oomir_output_type,
                    oomir_operands,
                    effective_dest,
                    declared_method_name,
                    dispatch_receiver_ty,
                );
            } else if direct_wrapping_integer_op {
                numeric_methods::wrapping(
                    &mut instructions,
                    oomir_operands,
                    effective_dest,
                    declared_method_name,
                );
            } else if direct_equality || direct_ordering {
                numeric_methods::comparison(
                    tcx,
                    data_types,
                    &label,
                    &mut instructions,
                    fn_inputs,
                    oomir_operands,
                    effective_dest,
                    declared_method_name,
                    direct_non_null_fat_equality,
                );
            } else if let Some(function_item) = direct_fn_def_instance {
                callables::function_item(
                    tcx,
                    instance,
                    data_types,
                    &label,
                    &mut instructions,
                    typing_env,
                    effective_dest,
                    explicit_method_args,
                    function_item,
                );
            } else if declared_method_name == "with_metadata_of"
                && matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                && matches!(&oomir_output_type, oomir::Type::Slice(_) | oomir::Type::Str)
                && oomir_operands.get(1).is_some_and(|metadata| {
                    matches!(
                        metadata.get_type(),
                        Some(oomir::Type::Slice(_) | oomir::Type::Str)
                    )
                })
            {
                pointer_metadata::with_metadata(
                    tcx,
                    instance,
                    data_types,
                    &label,
                    &mut instructions,
                    fn_output,
                    oomir_operands,
                    effective_dest,
                );
            } else if matches!(
                &dispatch_receiver_ty,
                oomir::Type::Slice(_) | oomir::Type::Str
            ) && is_pointer_cast_method
                && matches!(oomir_output_type, oomir::Type::Pointer(_))
            {
                pointer_metadata::cast(
                    tcx,
                    instance,
                    data_types,
                    &label,
                    &mut instructions,
                    fn_output,
                    oomir_output_type,
                    effective_dest,
                    receiver_operand,
                    resolved_receiver_mir_ty,
                );
            } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                && pointer_api_receiver
                && matches!(declared_method_name.as_str(), "as_ref" | "as_mut")
                && matches!(
                    fn_output.kind(),
                    TyKind::Adt(adt_def, _)
                        if tcx.is_lang_item(
                            adt_def.did(),
                            rustc_hir::attrs::lang_items::LangItem::Option,
                        )
                )
            {
                pointer_access::as_ref(
                    &label,
                    &mut instructions,
                    oomir_output_type,
                    effective_dest,
                    receiver_operand,
                    dispatch_receiver_ty,
                );
            } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                && pointer_api_receiver
                && matches!(declared_method_name.as_str(), "as_ref" | "as_mut")
            {
                pointer_access::non_null_ref(
                    tcx,
                    instance,
                    data_types,
                    &label,
                    &mut instructions,
                    fn_output,
                    oomir_output_type,
                    effective_dest,
                    receiver_operand,
                    dispatch_receiver_ty,
                );
            } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                && pointer_api_receiver
                && matches!(
                    declared_method_name.as_str(),
                    "as_ref_unchecked" | "as_mut_unchecked"
                )
            {
                if let Some(dest) = effective_dest {
                    instructions.push(oomir::Instruction::Move {
                        dest,
                        src: receiver_operand,
                    });
                }
            } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                && pointer_api_receiver
                && declared_method_name == "is_aligned"
            {
                pointer_access::is_aligned(
                    tcx,
                    instance,
                    &mut instructions,
                    effective_dest,
                    receiver_mir_ty,
                    receiver_operand,
                    dispatch_receiver_ty,
                );
            } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                && pointer_api_receiver
                && declared_method_name == "to_raw_parts"
            {
                pointer_access::to_raw_parts(
                    data_types,
                    &label,
                    &mut instructions,
                    oomir_output_type,
                    effective_dest,
                    receiver_operand,
                    resolved_receiver_mir_ty,
                    dispatch_receiver_ty,
                );
            } else if let oomir::Type::Pointer(pointee_ty) = &dispatch_receiver_ty
                && pointer_api_receiver
                && matches!(
                    declared_method_name.as_str(),
                    "read" | "read_unaligned" | "read_volatile"
                )
            {
                if let Some(dest) = effective_dest {
                    if declared_method_name == "read_volatile" {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "volatileFence".to_string(),
                            method_ty: oomir::Signature {
                                params: Vec::new(),
                                ret: Box::new(oomir::Type::Void),
                                is_static: true,
                            },
                            args: Vec::new(),
                            dest: None,
                        });
                    }
                    crate::lower1::place::emit_pointer_read_copy(
                        receiver_operand,
                        pointee_ty,
                        &dest,
                        &mut instructions,
                    );
                }
            } else if let oomir::Type::Pointer(pointee_ty) = &dispatch_receiver_ty
                && pointer_api_receiver
                && matches!(
                    declared_method_name.as_str(),
                    "write" | "write_unaligned" | "write_volatile"
                )
                && explicit_method_args.len() == 1
            {
                crate::lower1::place::emit_pointer_write(
                    receiver_operand,
                    pointee_ty,
                    explicit_method_args[0].clone(),
                    &mut instructions,
                );
                if declared_method_name == "write_volatile" {
                    instructions.push(oomir::Instruction::InvokeStatic {
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "volatileFence".to_string(),
                        method_ty: oomir::Signature {
                            params: Vec::new(),
                            ret: Box::new(oomir::Type::Void),
                            is_static: true,
                        },
                        args: Vec::new(),
                        dest: None,
                    });
                }
            } else if let oomir::Type::Pointer(pointee_ty) = &dispatch_receiver_ty
                && pointer_api_receiver
                && declared_method_name == "replace"
                && explicit_method_args.len() == 1
            {
                if let Some(dest) = effective_dest {
                    crate::lower1::place::emit_pointer_read(
                        receiver_operand.clone(),
                        pointee_ty,
                        &dest,
                        &mut instructions,
                    );
                }
                crate::lower1::place::emit_pointer_write(
                    receiver_operand,
                    pointee_ty,
                    explicit_method_args[0].clone(),
                    &mut instructions,
                );
            } else if let oomir::Type::Pointer(pointee_ty) = &dispatch_receiver_ty
                && pointer_api_receiver
                && declared_method_name == "swap"
                && explicit_method_args.len() == 1
            {
                pointer_access::swap(
                    &label,
                    &mut instructions,
                    receiver_operand,
                    explicit_method_args,
                    pointee_ty,
                );
            } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                && pointer_api_receiver
                && matches!(
                    declared_method_name.as_str(),
                    "copy_to"
                        | "copy_to_nonoverlapping"
                        | "copy_from"
                        | "copy_from_nonoverlapping"
                        | "write_bytes"
                )
                && explicit_method_args.len() == 2
            {
                pointer_address::copy_or_fill(
                    tcx,
                    instance,
                    &label,
                    &mut instructions,
                    receiver_mir_ty,
                    receiver_operand,
                    explicit_method_args,
                    declared_method_name,
                    dispatch_receiver_ty,
                );
            } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                && pointer_api_receiver
                && declared_method_name == "map_addr"
                && explicit_method_args.len() == 1
            {
                pointer_address::map_address(
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &label,
                    &mut instructions,
                    args,
                    typing_env,
                    effective_dest,
                    receiver_operand,
                    explicit_method_args,
                    dispatch_receiver_ty,
                );
            } else if let Some(interface_name) = use_interface {
                method_dispatch::interface(
                    tcx,
                    &mut instructions,
                    func_instance,
                    oomir_operands,
                    effective_dest,
                    method_signature,
                    receiver_operand,
                    method_args,
                    declared_method_name,
                    dispatch_receiver_ty,
                    receiver_self_requires_static_dispatch,
                    uses_concrete_trait_default,
                    interface_name,
                );
            } else {
                runtime_methods::concrete(
                    tcx,
                    instance,
                    data_types,
                    &mut instructions,
                    func_instance,
                    fn_output,
                    oomir_operands,
                    effective_dest,
                    method_signature,
                    item,
                    receiver_mir_ty,
                    receiver_operand,
                    method_args,
                    method_name,
                    declared_method_name,
                    resolved_receiver_mir_ty,
                    is_pointer_null_method,
                    is_pointer_cast_method,
                    receiver_self_requires_static_dispatch,
                    uses_concrete_trait_default,
                    comparison_rhs_ty,
                    pointer_api_receiver,
                );
            }
        }
    } else {
        method_signature.is_static = true;
        let method_name = crate::lower1::naming::associated_method_name_from_instance(
            tcx,
            func_instance,
            &method_signature,
        );

        let self_ty_opt = if let Some(impl_def_id) = item.impl_container(tcx) {
            Some(
                tcx.type_of(impl_def_id)
                    .instantiate(tcx, func_instance.args)
                    .skip_norm_wip(),
            )
        } else {
            func_instance.args.types().next()
        };

        let mut generated = false;
        if item.trait_container(tcx).is_none()
            && item.trait_item_def_id().is_none()
            && let Some(self_ty) = self_ty_opt
        {
            let class_type =
                crate::lower1::types::ty_to_oomir_type(self_ty, tcx, data_types, func_instance);

            if class_type == oomir::Type::F16
                && method_name == "from_bits"
                && oomir_operands.len() == 1
            {
                if let Some(dest) = effective_dest.clone() {
                    instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(dest),
                        class_name: "org/rustlang/runtime/Numbers".to_string(),
                        method_name: "f16FromBits".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![("bits".to_string(), oomir::Type::U16)],
                            ret: Box::new(oomir::Type::F16),
                            is_static: true,
                        },
                        args: vec![oomir_operands[0].clone()],
                    });
                }
                generated = true;
            }

            if !generated
                && let Some(class_name) = class_type.get_class_name()
                && matches!(
                    data_types.get(class_name),
                    Some(oomir::DataType::Class { methods, .. })
                        if methods.contains_key(&method_name)
                )
            {
                instructions.push(oomir::Instruction::InvokeRustStatic {
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
            let fn_name_data = data_types.function_name(tcx, func_instance);
            instructions.push(oomir::Instruction::InvokeRustStatic {
                class_name: fn_name_data
                    .class_to_call_on
                    .expect("monomorphized functions have JVM owners"),
                method_name: fn_name_data.method_name,
                method_ty: method_signature.clone(),
                args: oomir_operands.clone(),
                dest: effective_dest, // use effective_dest
            });
        }
    }
}
