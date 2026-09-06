use super::*;
mod representations;

impl<'tcx> RvalueContext<'_, 'tcx> {
    pub(super) fn lower_casts(
        self,
        rvalue: &Rvalue<'tcx>,
    ) -> (Vec<oomir::Instruction>, oomir::Operand) {
        let Self {
            original_dest_place,
            mir,
            tcx,
            instance,
            data_types,
            external_interfaces,
            ..
        } = self;
        let mut instructions = Vec::new();
        let result_operand;
        let base_temp_name = place_to_string(original_dest_place, tcx);
        match rvalue {
            Rvalue::Cast(cast_kind, operand, target_mir_ty) => {
                let temp_cast_var = generate_temp_var_name(data_types, &base_temp_name);
                let oomir_target_type = ty_to_oomir_type(*target_mir_ty, tcx, data_types, instance);
                let source_mir_ty = EarlyBinder::bind(tcx, operand.ty(&mir.local_decls, tcx))
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                let resolved_target_mir_ty = normalize_unsize_ty(*target_mir_ty, tcx, instance);

                if matches!(
                    cast_kind,
                    CastKind::PointerCoercion(PointerCoercion::ClosureFnPointer(_), _)
                ) {
                    if let TyKind::Closure(def_id, closure_args) = source_mir_ty.kind() {
                        let closure_instance = Instance::new_raw(*def_id, closure_args);
                        let signature =
                            fn_ptr_signature_from_ty(*target_mir_ty, tcx, data_types, instance);
                        let interface_name =
                            ensure_fn_ptr_interface(&signature, data_types, tcx, instance);
                        let (target_class_name, target_method_name) =
                            ensure_non_capturing_closure_fn_pointer_bridge(
                                data_types,
                                external_interfaces,
                                closure_instance,
                                &signature,
                                tcx,
                                instance,
                            );
                        instructions.push(oomir::Instruction::CreateFunctionPointer {
                            dest: temp_cast_var.clone(),
                            interface_name: interface_name.clone(),
                            signature,
                            target_class_name,
                            target_method_name,
                        });
                        result_operand = oomir::Operand::Variable {
                            name: temp_cast_var,
                            ty: oomir::Type::Interface(interface_name),
                        };
                    } else {
                        panic!(
                            "ClosureFnPointer cast has non-closure source type {:?}",
                            source_mir_ty
                        );
                    }
                } else if matches!(
                    cast_kind,
                    CastKind::PointerCoercion(PointerCoercion::ReifyFnPointer(_), _)
                ) {
                    if let TyKind::FnDef(def_id, substs) = source_mir_ty.kind() {
                        let func_instance = Instance::resolve_for_fn_ptr(
                            tcx,
                            TypingEnv::post_analysis(tcx, mir.source.def_id()),
                            *def_id,
                            substs.no_bound_vars().unwrap(),
                        )
                        .unwrap();
                        let fn_name =
                            crate::lower1::naming::mono_fn_name_from_instance(tcx, func_instance);
                        let signature =
                            fn_ptr_signature_from_ty(*target_mir_ty, tcx, data_types, instance);
                        let interface_name =
                            ensure_fn_ptr_interface(&signature, data_types, tcx, instance);
                        let callable_target = fn_pointer_target(tcx, func_instance, &signature);
                        if callable_target.is_none() {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "mir-lowering",
                                format!(
                                    "Warning: Reified non-local function pointer '{}' will use an UnsupportedOperationException stub if invoked.",
                                    fn_name.method_name
                                )
                            );
                        }
                        if let Some(FnPointerTarget::Static(target)) = &callable_target
                            && let Some(target_class_name) = &target.class_to_call_on
                        {
                            instructions.push(oomir::Instruction::CreateFunctionPointer {
                                dest: temp_cast_var.clone(),
                                interface_name: interface_name.clone(),
                                signature,
                                target_class_name: target_class_name.clone(),
                                target_method_name: target.method_name.clone(),
                            });
                        } else {
                            let adapter_class = ensure_fn_pointer_adapter_class(
                                data_types,
                                callable_target.as_ref(),
                                &signature,
                                &interface_name,
                                tcx,
                                instance,
                            );
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mir-lowering",
                                format!(
                                    "Info: Reifying FnDef to FnPtr: '{}' -> '{}' as '{}'",
                                    source_mir_ty, fn_name.method_name, adapter_class
                                )
                            );
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest: temp_cast_var.clone(),
                                class_name: adapter_class,
                                args: Vec::new(),
                            });
                        }
                        result_operand = oomir::Operand::Variable {
                            name: temp_cast_var,
                            ty: oomir::Type::Interface(interface_name),
                        };
                    } else {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "mir-lowering",
                            format!(
                                "Warning: ReifyFnPointer cast with non-FnDef source type: {:?}",
                                source_mir_ty
                            )
                        );
                        result_operand =
                            oomir::Operand::Constant(oomir::Constant::Null(oomir_target_type));
                    }
                } else {
                    if matches!(source_mir_ty.kind(), TyKind::FnPtr(..))
                        && matches!(target_mir_ty.kind(), TyKind::FnPtr(..))
                    {
                        let source_signature =
                            fn_ptr_signature_from_ty(source_mir_ty, tcx, data_types, instance);
                        let target_signature =
                            fn_ptr_signature_from_ty(*target_mir_ty, tcx, data_types, instance);
                        let source_interface =
                            ensure_fn_ptr_interface(&source_signature, data_types, tcx, instance);
                        let target_interface =
                            ensure_fn_ptr_interface(&target_signature, data_types, tcx, instance);
                        let oomir_operand = convert_operand(
                            operand,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );

                        if let Ok(bridge_class) = ensure_erased_receiver_fn_pointer_bridge(
                            data_types,
                            &source_signature,
                            &source_interface,
                            &target_signature,
                            &target_interface,
                            tcx,
                            instance,
                        ) {
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest: temp_cast_var.clone(),
                                class_name: bridge_class,
                                args: vec![(
                                    oomir_operand,
                                    oomir::Type::Interface(source_interface),
                                )],
                            });
                        } else if source_signature.to_jvm_descriptor_with_explicit_params()
                            == target_signature.to_jvm_descriptor_with_explicit_params()
                        {
                            instructions.push(oomir::Instruction::Move {
                                dest: temp_cast_var.clone(),
                                src: oomir_operand,
                            });
                        } else {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "mir-lowering",
                                format!(
                                    "Warning: Function pointer cast from '{}' to '{}' changes the JVM descriptor; generating an UnsupportedOperationException stub.",
                                    source_signature.to_jvm_descriptor_with_explicit_params(),
                                    target_signature.to_jvm_descriptor_with_explicit_params()
                                )
                            );
                            let adapter_class = ensure_fn_pointer_adapter_class(
                                data_types,
                                None,
                                &target_signature,
                                &target_interface,
                                tcx,
                                instance,
                            );
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest: temp_cast_var.clone(),
                                class_name: adapter_class,
                                args: Vec::new(),
                            });
                        }

                        result_operand = oomir::Operand::Variable {
                            name: temp_cast_var,
                            ty: oomir::Type::Interface(target_interface),
                        };
                    } else {
                        let oomir_source_type =
                            ty_to_oomir_type(source_mir_ty, tcx, data_types, instance);
                        let raw_oomir_operand = convert_operand(
                            operand,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        if matches!(source_mir_ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..))
                            && matches!(
                                pointer_pointee_ty(source_mir_ty).kind(),
                                TyKind::Dynamic(..)
                            )
                            && matches!(
                                resolved_target_mir_ty.kind(),
                                TyKind::RawPtr(..) | TyKind::Ref(..)
                            )
                            && !matches!(
                                pointer_pointee_ty(resolved_target_mir_ty).kind(),
                                TyKind::Dynamic(..)
                            )
                            && crate::lower1::types::is_codegen_sized(
                                pointer_pointee_ty(resolved_target_mir_ty),
                                tcx,
                            )
                            && matches!(oomir_target_type, oomir::Type::Pointer(_))
                        {
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_cast_var.clone()),
                                class_name: "org/rustlang/runtime/RuntimeSupport".to_string(),
                                method_name: "traitObjectDataPointer".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("pointer".to_string(), oomir_source_type),
                                        ("view_size".to_string(), oomir::Type::U64),
                                        ("view_codec".to_string(), oomir::Type::java_string()),
                                    ],
                                    ret: Box::new(oomir_target_type.clone()),
                                    is_static: true,
                                },
                                args: vec![
                                    raw_oomir_operand,
                                    pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                    crate::lower1::types::pointer_view_codec_operand(
                                        pointer_pointee_ty(*target_mir_ty),
                                        tcx,
                                        data_types,
                                        instance,
                                    ),
                                ],
                            });
                            return (
                                instructions,
                                oomir::Operand::Variable {
                                    name: temp_cast_var,
                                    ty: oomir_target_type,
                                },
                            );
                        }

                        // Promoted reference constants are exposed by rustc as the
                        // pointee scalar/object. Re-materialize their canonical
                        // Pointer carrier before interpreting any pointer cast.
                        let oomir_operand = crate::lower1::value_repr::adapt_operand_to_rust_type(
                            raw_oomir_operand,
                            source_mir_ty,
                            &format!("{}_cast_source", base_temp_name),
                            tcx,
                            instance,
                            data_types,
                            &mut instructions,
                        );
                        if matches!(source_mir_ty.kind(), TyKind::Ref(_, pointee, _)
                            if matches!(pointee.kind(), TyKind::Dynamic(..)))
                            && matches!(resolved_target_mir_ty.kind(), TyKind::RawPtr(pointee, _)
                            if matches!(pointee.kind(), TyKind::Dynamic(..)))
                            && matches!(oomir_target_type, oomir::Type::Pointer(_))
                        {
                            let pointer = crate::lower1::value_repr::adapt_operand_to_rust_type(
                                oomir_operand,
                                resolved_target_mir_ty,
                                &format!("{}_trait_object_pointer", base_temp_name),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                            return (instructions, pointer);
                        }
                        if matches!(source_mir_ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..))
                            && matches!(target_mir_ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..))
                            && let Some(result) = emit_trait_object_to_struct_tail_cast(
                                source_mir_ty,
                                resolved_target_mir_ty,
                                oomir_operand.clone(),
                                &temp_cast_var,
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            )
                        {
                            return (instructions, result);
                        }
                        if matches!(source_mir_ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..))
                            && matches!(target_mir_ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..))
                            && let Some(result) = emit_struct_tail_pointer_cast(
                                source_mir_ty,
                                resolved_target_mir_ty,
                                oomir_operand.clone(),
                                &temp_cast_var,
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            )
                        {
                            return (instructions, result);
                        }

                        if matches!(
                            cast_kind,
                            CastKind::PointerCoercion(PointerCoercion::Unsize, _)
                        ) && let Some(result) = emit_raw_array_pointer_unsize(
                            source_mir_ty,
                            resolved_target_mir_ty,
                            oomir_operand.clone(),
                            &temp_cast_var,
                            tcx,
                            instance,
                            data_types,
                            &mut instructions,
                        ) {
                            return (instructions, result);
                        }

                        if matches!(source_mir_ty.kind(), TyKind::FnPtr(..))
                            && matches!(target_mir_ty.kind(), TyKind::Int(..) | TyKind::Uint(..))
                            && matches!(
                                cast_kind,
                                CastKind::Transmute | CastKind::PointerExposeProvenance
                            )
                        {
                            let address_dest = if oomir_target_type == oomir::Type::U64 {
                                temp_cast_var.clone()
                            } else {
                                format!("{temp_cast_var}_function_address")
                            };
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(address_dest.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "functionPointerAddress".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![(
                                        "function".to_string(),
                                        oomir::Type::Class("java/lang/Object".to_string()),
                                    )],
                                    ret: Box::new(oomir::Type::U64),
                                    is_static: true,
                                },
                                args: vec![oomir_operand],
                            });
                            if oomir_target_type != oomir::Type::U64 {
                                instructions.push(oomir::Instruction::Cast {
                                    op: oomir::Operand::Variable {
                                        name: address_dest,
                                        ty: oomir::Type::U64,
                                    },
                                    ty: oomir_target_type.clone(),
                                    dest: temp_cast_var.clone(),
                                });
                            }
                            return (
                                instructions,
                                oomir::Operand::Variable {
                                    name: temp_cast_var,
                                    ty: oomir_target_type,
                                },
                            );
                        }

                        if matches!(source_mir_ty.kind(), TyKind::FnPtr(..))
                            && matches!(target_mir_ty.kind(), TyKind::RawPtr(..))
                        {
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_cast_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "fromFunctionPointer".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        (
                                            "value".to_string(),
                                            oomir::Type::Class("java/lang/Object".to_string()),
                                        ),
                                        ("view_size".to_string(), oomir::Type::U64),
                                        ("codec".to_string(), oomir::Type::java_string()),
                                    ],
                                    ret: Box::new(oomir_target_type.clone()),
                                    is_static: true,
                                },
                                args: vec![
                                    oomir_operand,
                                    pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                    crate::lower1::types::pointer_view_codec_operand(
                                        pointer_pointee_ty(*target_mir_ty),
                                        tcx,
                                        data_types,
                                        instance,
                                    ),
                                ],
                            });
                            return (
                                instructions,
                                oomir::Operand::Variable {
                                    name: temp_cast_var,
                                    ty: oomir_target_type,
                                },
                            );
                        }

                        if matches!(source_mir_ty.kind(), TyKind::Int(..) | TyKind::Uint(..))
                            && matches!(target_mir_ty.kind(), TyKind::FnPtr(..))
                            && matches!(cast_kind, CastKind::Transmute)
                        {
                            let address = if oomir_source_type == oomir::Type::U64 {
                                oomir_operand
                            } else {
                                let address_dest = format!("{temp_cast_var}_function_address");
                                instructions.push(oomir::Instruction::Cast {
                                    op: oomir_operand,
                                    ty: oomir::Type::U64,
                                    dest: address_dest.clone(),
                                });
                                oomir::Operand::Variable {
                                    name: address_dest,
                                    ty: oomir::Type::U64,
                                }
                            };
                            let object_dest = format!("{temp_cast_var}_function_object");
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(object_dest.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "functionPointerFromAddress".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![("address".to_string(), oomir::Type::U64)],
                                    ret: Box::new(oomir::Type::Class(
                                        "java/lang/Object".to_string(),
                                    )),
                                    is_static: true,
                                },
                                args: vec![address],
                            });
                            instructions.push(oomir::Instruction::Cast {
                                op: oomir::Operand::Variable {
                                    name: object_dest,
                                    ty: oomir::Type::Class("java/lang/Object".to_string()),
                                },
                                ty: oomir_target_type.clone(),
                                dest: temp_cast_var.clone(),
                            });
                            return (
                                instructions,
                                oomir::Operand::Variable {
                                    name: temp_cast_var,
                                    ty: oomir_target_type,
                                },
                            );
                        }

                        if matches!(source_mir_ty.kind(), TyKind::RawPtr(..))
                            && matches!(target_mir_ty.kind(), TyKind::FnPtr(..))
                            && matches!(cast_kind, CastKind::Transmute)
                        {
                            let callable = emit_pointer_read(
                                oomir_operand,
                                &oomir_target_type,
                                &temp_cast_var,
                                &mut instructions,
                            );
                            return (instructions, callable);
                        }

                        if matches!(oomir_source_type, oomir::Type::Pointer(_))
                            && matches!(target_mir_ty.kind(), TyKind::Int(..) | TyKind::Uint(..))
                            && matches!(
                                cast_kind,
                                CastKind::Transmute | CastKind::PointerExposeProvenance
                            )
                        {
                            let address_dest = if oomir_target_type == oomir::Type::U64 {
                                temp_cast_var.clone()
                            } else {
                                format!("{temp_cast_var}_pointer_address")
                            };
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(address_dest.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                // A transmute reads address bits without exposing provenance.
                                // In particular, core::fmt uses it to inspect its tagged
                                // argument pointer; retaining that allocation leaks each format.
                                method_name: if matches!(cast_kind, CastKind::Transmute) {
                                    "addr"
                                } else {
                                    "address"
                                }
                                .to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![("pointer".to_string(), oomir_source_type)],
                                    ret: Box::new(oomir::Type::U64),
                                    is_static: true,
                                },
                                args: vec![oomir_operand],
                            });
                            if oomir_target_type != oomir::Type::U64 {
                                instructions.push(oomir::Instruction::Cast {
                                    op: oomir::Operand::Variable {
                                        name: address_dest,
                                        ty: oomir::Type::U64,
                                    },
                                    ty: oomir_target_type.clone(),
                                    dest: temp_cast_var.clone(),
                                });
                            }
                            return (
                                instructions,
                                oomir::Operand::Variable {
                                    name: temp_cast_var,
                                    ty: oomir_target_type,
                                },
                            );
                        }

                        if matches!(source_mir_ty.kind(), TyKind::Int(..) | TyKind::Uint(..))
                            && matches!(oomir_target_type, oomir::Type::Pointer(_))
                            && matches!(
                                cast_kind,
                                CastKind::PointerWithExposedProvenance | CastKind::IntToInt
                            )
                        {
                            let address = if oomir_source_type == oomir::Type::U64 {
                                oomir_operand
                            } else {
                                let address_dest = format!("{temp_cast_var}_pointer_address");
                                instructions.push(oomir::Instruction::Cast {
                                    op: oomir_operand,
                                    ty: oomir::Type::U64,
                                    dest: address_dest.clone(),
                                });
                                oomir::Operand::Variable {
                                    name: address_dest,
                                    ty: oomir::Type::U64,
                                }
                            };
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_cast_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "fromAddress".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("address".to_string(), oomir::Type::U64),
                                        ("view_size".to_string(), oomir::Type::U64),
                                        ("view_codec".to_string(), oomir::Type::java_string()),
                                    ],
                                    ret: Box::new(oomir_target_type.clone()),
                                    is_static: true,
                                },
                                args: vec![
                                    address,
                                    pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                    crate::lower1::types::pointer_view_codec_operand(
                                        pointer_pointee_ty(*target_mir_ty),
                                        tcx,
                                        data_types,
                                        instance,
                                    ),
                                ],
                            });
                            return (
                                instructions,
                                oomir::Operand::Variable {
                                    name: temp_cast_var,
                                    ty: oomir_target_type,
                                },
                            );
                        }

                        let callable_abi = matches!(
                            cast_kind,
                            CastKind::PointerCoercion(PointerCoercion::Unsize, _)
                        )
                        .then(|| {
                            crate::lower1::types::callable_trait_object_abi(
                                resolved_target_mir_ty,
                                tcx,
                                data_types,
                                instance,
                            )
                        })
                        .flatten();
                        let direct_callable_interface =
                            matches!(oomir_target_type, oomir::Type::Interface(_));
                        let callable_closure_bridge = direct_callable_interface
                            && callable_abi.as_ref().is_some_and(|callable_abi| {
                                ensure_closure_callable_bridge(
                                    source_mir_ty,
                                    callable_abi,
                                    data_types,
                                    tcx,
                                    instance,
                                )
                            });
                        let callable_fn_def_adapter = direct_callable_interface
                            .then(|| callable_abi.as_ref())
                            .flatten()
                            .and_then(|callable_abi| {
                                let callable_ty = match source_mir_ty.kind() {
                                    TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => {
                                        *pointee
                                    }
                                    _ => source_mir_ty,
                                };
                                let TyKind::FnDef(def_id, args) = callable_ty.kind() else {
                                    return None;
                                };
                                let function_instance = Instance::resolve_for_fn_ptr(
                                    tcx,
                                    TypingEnv::post_analysis(tcx, mir.source.def_id()),
                                    *def_id,
                                    args.no_bound_vars()?,
                                )?;
                                let target = fn_pointer_target(
                                    tcx,
                                    function_instance,
                                    &callable_abi.signature,
                                );
                                Some(ensure_fn_pointer_adapter_class(
                                    data_types,
                                    target.as_ref(),
                                    &callable_abi.signature,
                                    &callable_abi.interface_name,
                                    tcx,
                                    instance,
                                ))
                            });

                        let trait_object_adapter = if !callable_closure_bridge
                            && callable_fn_def_adapter.is_none()
                            && matches!(
                                cast_kind,
                                CastKind::PointerCoercion(PointerCoercion::Unsize, _)
                            )
                            && let oomir::Type::Interface(interface_name) = &oomir_target_type
                            && (carrier_needs_trait_object_adapter(
                                &oomir_source_type,
                                interface_name,
                                data_types,
                            ) || interface_name
                                .rsplit('/')
                                .next()
                                .is_some_and(|name| name.contains("_Dyn_")))
                        {
                            match ensure_trait_object_adapter_class(
                                source_mir_ty,
                                resolved_target_mir_ty,
                                &oomir_source_type,
                                interface_name,
                                data_types,
                                tcx,
                                instance,
                            ) {
                                Ok(class_name) => Some(class_name),
                                Err(error) => {
                                    breadcrumbs::log!(
                                        breadcrumbs::LogLevel::Warn,
                                        "mir-lowering",
                                        format!(
                                            "Could not build trait-object carrier adapter for {source_mir_ty:?} -> {target_mir_ty:?}: {error}"
                                        )
                                    );
                                    None
                                }
                            }
                        } else {
                            None
                        };

                        return representations::PreparedCast {
                            tcx,
                            instance,
                            data_types,
                            cast_kind,
                            target_mir_ty,
                            source_mir_ty,
                            resolved_target_mir_ty,
                            oomir_operand,
                            oomir_source_type,
                            oomir_target_type,
                            temp_cast_var,
                            instructions,
                            callable_closure_bridge,
                            callable_fn_def_adapter,
                            trait_object_adapter,
                        }
                        .lower();
                    }
                }
            }

            _ => unreachable!("rvalue routed to casts"),
        }
        (instructions, result_operand)
    }
}
