use super::*;
use crate::lower1::context::Definitions;

pub(super) fn is_integer_to_pointer_transmute_source(ty: Ty<'_>, tcx: TyCtxt<'_>) -> bool {
    matches!(ty.kind(), TyKind::Int(_) | TyKind::Uint(_))
        || matches!(
            ty.kind(),
            TyKind::Adt(adt_def, _) if tcx.is_diagnostic_item(sym::NonZero, adt_def.did())
        )
}

pub(super) fn emit_unprovenanced_pointer_from_union_bytes<'tcx>(
    target_ty: Ty<'tcx>,
    storage: &JvmUnionStorage,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<Option<oomir::Operand>, String> {
    let target_oomir_ty = ty_to_oomir_type(target_ty, tcx, data_types, instance_context);
    let target = match target_ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)
            if matches!(target_oomir_ty, oomir::Type::Pointer(_)) =>
        {
            Some((target_oomir_ty.clone(), *pointee, None))
        }
        TyKind::Adt(adt_def, args) if crate::lower1::is_non_null_lang_item(tcx, adt_def.did()) => {
            let Some(pointee) = args.iter().find_map(|arg| arg.as_type()) else {
                return Ok(None);
            };
            if matches!(target_oomir_ty, oomir::Type::Pointer(_)) {
                Some((target_oomir_ty.clone(), pointee, None))
            } else {
                let oomir::Type::Class(class_name) = &target_oomir_ty else {
                    return Ok(None);
                };
                let Some(oomir::DataType::Class { fields, .. }) = data_types.get(class_name) else {
                    return Ok(None);
                };
                fields
                    .iter()
                    .find(|(field_name, field_ty)| {
                        field_name == "pointer" && matches!(field_ty, oomir::Type::Pointer(_))
                    })
                    .map(|(_, field_ty)| (field_ty.clone(), pointee, Some(class_name.clone())))
            }
        }
        _ => None,
    };
    let Some((pointer_ty, pointee, wrapper_class)) = target else {
        return Ok(None);
    };

    let address = emit_bits_from_union_bytes(
        oomir::Type::U64,
        layout_size_bytes(tcx, target_ty)?,
        storage,
        0,
        instructions,
        temp_counter,
    );
    let pointer_name = next_union_temp("unprovenanced_pointer", temp_counter);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(pointer_name.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "fromUnprovenancedAddress".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("address".to_string(), oomir::Type::U64),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(pointer_ty.clone()),
            is_static: true,
        },
        args: vec![
            address,
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(layout_size_bytes(tcx, pointee)?)
                    .map_err(|_| "Rust pointer pointee layout exceeds u64")?,
            )),
            pointer_view_codec_operand(pointee, tcx, data_types, instance_context),
        ],
    });
    let pointer = operand_var(pointer_name, pointer_ty.clone());
    if let Some(wrapper_class) = wrapper_class {
        let result_name = next_union_temp("unprovenanced_pointer_wrapper", temp_counter);
        instructions.push(oomir::Instruction::ConstructObject {
            dest: result_name.clone(),
            class_name: wrapper_class,
            args: vec![(pointer, pointer_ty)],
        });
        Ok(Some(operand_var(result_name, target_oomir_ty)))
    } else {
        Ok(Some(pointer))
    }
}

pub(super) fn force_define_transmute_adts<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    visited: &mut HashSet<Ty<'tcx>>,
) {
    if !visited.insert(ty) {
        return;
    }

    match ty.kind() {
        TyKind::Adt(adt_def, substs) => {
            if !should_define_named_data_type(tcx, adt_def.did()) && substs.is_empty() {
                force_define_named_adt(ty, tcx, data_types, instance_context);
            }
            for field in adt_def
                .variants()
                .iter()
                .flat_map(|variant| variant.fields.iter())
            {
                force_define_transmute_adts(
                    field.ty(tcx, substs).skip_norm_wip(),
                    tcx,
                    data_types,
                    instance_context,
                    visited,
                );
            }
        }
        TyKind::Tuple(elements) => {
            for element in elements.iter() {
                force_define_transmute_adts(element, tcx, data_types, instance_context, visited);
            }
        }
        TyKind::Array(element, _) | TyKind::Slice(element) | TyKind::Pat(element, _) => {
            force_define_transmute_adts(*element, tcx, data_types, instance_context, visited);
        }
        TyKind::Closure(_, closure_args) => {
            for capture in closure_args.as_closure().upvar_tys() {
                force_define_transmute_adts(capture, tcx, data_types, instance_context, visited);
            }
        }
        _ => {}
    }
}

pub(crate) fn ensure_exact_transmute_helper<'tcx>(
    source_ty: Ty<'tcx>,
    target_ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<ExactTransmuteHelper, String> {
    let source_ty = resolve_union_ty(tcx, source_ty, instance_context)?;
    let target_ty = resolve_union_ty(tcx, target_ty, instance_context)?;
    let source_size = layout_size_bytes(tcx, source_ty)?;
    let target_size = layout_size_bytes(tcx, target_ty)?;
    if source_size != target_size {
        return Err(format!(
            "transmute layout sizes differ: {source_ty:?} is {source_size} bytes, {target_ty:?} is {target_size} bytes"
        ));
    }
    exact_bytes_supported(source_ty, tcx, instance_context)?;
    exact_bytes_supported(target_ty, tcx, instance_context)?;

    // Optimized downstream MIR can materialize a dependency-private ADT by
    // transmuting its scalar representation instead of constructing it. Emit
    // only the named dependency types reached by this generated helper.
    let mut visited = HashSet::default();
    force_define_transmute_adts(source_ty, tcx, data_types, instance_context, &mut visited);
    force_define_transmute_adts(target_ty, tcx, data_types, instance_context, &mut visited);

    let source_oomir_ty = ty_to_oomir_type(source_ty, tcx, data_types, instance_context);
    let target_oomir_ty = ty_to_oomir_type(target_ty, tcx, data_types, instance_context);
    let method_name = "transmute".to_string();
    let signature = oomir::Signature {
        params: source_oomir_ty
            .has_jvm_value()
            .then(|| vec![("value".to_string(), source_oomir_ty.clone())])
            .unwrap_or_default(),
        ret: Box::new(target_oomir_ty.clone()),
        is_static: true,
    };
    let readable = format!(
        "{}_to_{}",
        sanitize_name_token(&readable_rust_type_name(
            source_ty,
            tcx,
            data_types,
            instance_context,
        )),
        sanitize_name_token(&readable_rust_type_name(
            target_ty,
            tcx,
            data_types,
            instance_context,
        ))
    );
    let identity = format!(
        "{}:{}->{}:{}",
        stable_type_identity(tcx, source_ty),
        source_oomir_ty.to_jvm_descriptor(),
        stable_type_identity(tcx, target_ty),
        target_oomir_ty.to_jvm_descriptor()
    );
    // The same readable Rust types can use different JVM carriers across
    // upstream and downstream monomorphizations. Include the descriptors in
    // the class identity so their helper methods cannot collide at link time.
    let local_name = crate::stable_hash::readable_disambiguated_name(
        "ExactTransmute",
        &readable,
        &identity,
        180,
    );
    let class_name = jvm_names::synthetic_class_for_instance(tcx, instance_context, local_name);
    let helper = ExactTransmuteHelper {
        class_name: class_name.clone(),
        method_name: method_name.clone(),
        signature: signature.clone(),
    };
    if matches!(
        data_types.get(&class_name),
        Some(oomir::DataType::Class { methods, .. }) if methods.contains_key(&method_name)
    ) {
        return Ok(helper);
    }

    let pointer_pointee = |ty: Ty<'tcx>, oomir_ty: &oomir::Type| match ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => Some(*pointee),
        TyKind::Adt(adt_def, args)
            if crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
                && matches!(oomir_ty, oomir::Type::Pointer(_)) =>
        {
            args.iter().find_map(|arg| arg.as_type())
        }
        _ => None,
    };
    let source_pointee = pointer_pointee(source_ty, &source_oomir_ty);
    let target_pointee = pointer_pointee(target_ty, &target_oomir_ty);
    let source_is_u8_slice = source_pointee.is_some_and(
        |pointee| matches!(pointee.kind(), TyKind::Slice(element) if *element == tcx.types.u8),
    );
    let target_is_u8_slice = target_pointee.is_some_and(
        |pointee| matches!(pointee.kind(), TyKind::Slice(element) if *element == tcx.types.u8),
    );
    let source_is_str = source_pointee.is_some_and(Ty::is_str);
    let target_is_str = target_pointee.is_some_and(Ty::is_str);
    let direct_view_method = if source_is_u8_slice && target_is_str {
        Some("fromSlice")
    } else if source_is_str && target_is_u8_slice {
        Some("asSlice")
    } else {
        None
    };

    // Pointer/NonNull transmutes are transparent. Preserve the carrier rather
    // than publishing an address through temporary byte arrays and recovering it.
    let source_non_null = match (source_ty.kind(), &source_oomir_ty) {
        (TyKind::Adt(adt_def, args), oomir::Type::Class(class_name))
            if crate::lower1::is_non_null_lang_item(tcx, adt_def.did()) =>
        {
            args.iter()
                .find_map(|arg| arg.as_type())
                .map(|pointee| (class_name.clone(), pointee))
        }
        _ => None,
    };
    let target_non_null = match (target_ty.kind(), &target_oomir_ty) {
        (TyKind::Adt(adt_def, args), oomir::Type::Class(class_name))
            if crate::lower1::is_non_null_lang_item(tcx, adt_def.did()) =>
        {
            args.iter()
                .find_map(|arg| arg.as_type())
                .map(|pointee| (class_name.clone(), pointee))
        }
        _ => None,
    };
    let target_pointer = match target_ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)
            if target_oomir_ty.has_jvm_value() =>
        {
            Some((target_oomir_ty.clone(), *pointee, None))
        }
        TyKind::Adt(adt_def, args)
            if crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
                && matches!(target_oomir_ty, oomir::Type::Pointer(_)) =>
        {
            args.iter()
                .find_map(|arg| arg.as_type())
                .map(|pointee| (target_oomir_ty.clone(), pointee, None))
        }
        _ => target_non_null.as_ref().and_then(|(class_name, pointee)| {
            let oomir::DataType::Class { fields, .. } = data_types.get(class_name)? else {
                return None;
            };
            fields
                .iter()
                .find(|(field_name, field_ty)| field_name == "pointer" && field_ty.has_jvm_value())
                .map(|(_, field_ty)| (field_ty.clone(), *pointee, Some(class_name.clone())))
        }),
    };
    let target_struct_tail_view = target_pointee.and_then(|pointee| {
        let tail = tcx.struct_tail_for_codegen(pointee, TypingEnv::fully_monomorphized());
        let tail_view = if tail.is_str() {
            Some(oomir::UTF8_VIEW_CLASS)
        } else if matches!(tail.kind(), TyKind::Slice(_)) {
            Some(oomir::SLICE_VIEW_CLASS)
        } else {
            None
        }?;
        let oomir::Type::Pointer(target) = &target_oomir_ty else {
            return None;
        };
        let oomir::Type::Class(target_class) = target.as_ref() else {
            return None;
        };
        Some((target_class.clone(), tail_view.to_string()))
    });
    let mut pointer_instructions = Vec::new();
    let source_pointer = if source_pointee.is_some() && source_oomir_ty.has_jvm_value() {
        Some((
            operand_var("_1", source_oomir_ty.clone()),
            source_oomir_ty.clone(),
        ))
    } else {
        source_non_null.as_ref().and_then(|(class_name, _)| {
            let oomir::DataType::Class { fields, .. } = data_types.get(class_name)? else {
                return None;
            };
            let (_, field_ty) = fields.iter().find(|(field_name, field_ty)| {
                field_name == "pointer" && field_ty.has_jvm_value()
            })?;
            let field_ty = field_ty.clone();
            let pointer_name = "_source_pointer".to_string();
            pointer_instructions.push(oomir::Instruction::GetField {
                dest: pointer_name.clone(),
                object: operand_var("_1", source_oomir_ty.clone()),
                field_name: "pointer".to_string(),
                field_ty: field_ty.clone(),
                owner_class: class_name.clone(),
            });
            Some((operand_var(pointer_name, field_ty.clone()), field_ty))
        })
    };
    let source_pointer_pointee =
        source_pointee.or_else(|| source_non_null.as_ref().map(|(_, pointee)| *pointee));
    let direct_pointer_instructions = source_pointer
        .zip(target_pointer)
        .map(
            |((source_pointer, source_pointer_ty), (target_pointer_ty, pointee, wrapper))| {
                let retyped = if source_pointer_pointee == Some(pointee) {
                    source_pointer
                } else if let Some((target_class, tail_view_class)) = &target_struct_tail_view {
                    if !matches!(source_pointer_ty, oomir::Type::Pointer(_))
                        || !matches!(target_pointer_ty, oomir::Type::Pointer(_))
                    {
                        return Ok(None);
                    }
                    let retyped_name = "_retargeted_struct_tail_pointer".to_string();
                    pointer_instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(retyped_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "retargetStructTail".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("pointer".to_string(), source_pointer_ty),
                                ("target_class".to_string(), oomir::Type::java_string()),
                                ("tail_view_class".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(target_pointer_ty.clone()),
                            is_static: true,
                        },
                        args: vec![
                            source_pointer,
                            oomir::Operand::Constant(oomir::Constant::String(target_class.clone())),
                            oomir::Operand::Constant(oomir::Constant::String(
                                tail_view_class.clone(),
                            )),
                        ],
                    });
                    operand_var(retyped_name, target_pointer_ty.clone())
                } else if source_pointer_ty == target_pointer_ty
                    && matches!(source_pointer_ty, oomir::Type::Pointer(_))
                {
                    source_pointer
                } else if matches!(source_pointer_ty, oomir::Type::Pointer(_))
                    && matches!(target_pointer_ty, oomir::Type::Pointer(_))
                {
                    let retyped_name = "_retyped_pointer".to_string();
                    pointer_instructions.push(oomir::Instruction::InvokeVirtual {
                        dest: Some(retyped_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "retype".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("self".to_string(), source_pointer_ty),
                                ("view_size".to_string(), oomir::Type::U64),
                                ("view_codec".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(target_pointer_ty.clone()),
                            is_static: false,
                        },
                        args: vec![
                            oomir::Operand::Constant(oomir::Constant::U64(
                                u64::try_from(layout_size_bytes(tcx, pointee)?)
                                    .map_err(|_| "pointer transmute view exceeds u64")?,
                            )),
                            pointer_view_codec_operand(pointee, tcx, data_types, instance_context),
                        ],
                        operand: source_pointer,
                    });
                    operand_var(retyped_name, target_pointer_ty.clone())
                } else {
                    return Ok(None);
                };
                if let Some(wrapper_class) = wrapper {
                    let result_name = "_pointer_wrapper".to_string();
                    pointer_instructions.push(oomir::Instruction::ConstructObject {
                        dest: result_name.clone(),
                        class_name: wrapper_class,
                        args: vec![(retyped, target_pointer_ty)],
                    });
                    pointer_instructions.push(oomir::Instruction::Return {
                        operand: Some(operand_var(result_name, target_oomir_ty.clone())),
                    });
                } else {
                    pointer_instructions.push(oomir::Instruction::Return {
                        operand: Some(retyped),
                    });
                }
                Ok::<_, String>(Some(pointer_instructions))
            },
        )
        .transpose()?
        .flatten();

    let instructions = if let Some(instructions) = direct_pointer_instructions {
        instructions
    } else if let Some(view_method) = direct_view_method {
        let result = "_view_result".to_string();
        vec![
            oomir::Instruction::InvokeStatic {
                dest: Some(result.clone()),
                class_name: oomir::UTF8_VIEW_CLASS.to_string(),
                method_name: view_method.to_string(),
                method_ty: signature.clone(),
                args: vec![operand_var("_1", source_oomir_ty)],
            },
            oomir::Instruction::Return {
                operand: Some(operand_var(result, target_oomir_ty.clone())),
            },
        ]
    } else {
        let bytes_name = "_exact_bytes".to_string();
        let objects_name = "_exact_objects".to_string();
        let mut instructions = vec![
            oomir::Instruction::NewArray {
                dest: bytes_name.clone(),
                element_type: oomir::Type::I8,
                size: oomir::Operand::Constant(oomir::Constant::I32(source_size as i32)),
            },
            oomir::Instruction::NewArray {
                dest: objects_name.clone(),
                element_type: oomir::Type::Class("java/lang/Object".to_string()),
                size: oomir::Operand::Constant(oomir::Constant::I32(source_size.max(1) as i32)),
            },
        ];
        let storage = JvmUnionStorage::at_start(bytes_name, objects_name);
        let source = if source_oomir_ty.has_jvm_value() {
            operand_var("_1", source_oomir_ty)
        } else {
            oomir::Operand::Constant(oomir::Constant::Unit)
        };
        let mut temp_counter = 0;
        emit_ty_to_union_bytes(
            source_ty,
            source,
            &storage,
            0,
            tcx,
            data_types,
            instance_context,
            &mut instructions,
            &mut temp_counter,
        )?;
        let result = if is_integer_to_pointer_transmute_source(source_ty, tcx) {
            emit_unprovenanced_pointer_from_union_bytes(
                target_ty,
                &storage,
                tcx,
                data_types,
                instance_context,
                &mut instructions,
                &mut temp_counter,
            )?
        } else {
            None
        };
        let result = if let Some(result) = result {
            result
        } else {
            emit_ty_from_union_bytes(
                target_ty,
                &storage,
                0,
                tcx,
                data_types,
                instance_context,
                &mut instructions,
                &mut temp_counter,
            )?
        };
        instructions.push(oomir::Instruction::Return {
            operand: target_oomir_ty.has_jvm_value().then_some(result),
        });
        instructions
    };

    let function = oomir::Function {
        name: method_name.clone(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature,
        body: simple_body(instructions).into(),
    };
    match data_types.get_mut(&class_name) {
        Some(oomir::DataType::Class { methods, .. }) => {
            methods.insert(method_name, DataTypeMethod::Function(function));
        }
        Some(oomir::DataType::Interface { .. }) => {
            return Err(format!(
                "exact transmute helper name {class_name} is already an interface"
            ));
        }
        None => {
            data_types.insert(
                class_name,
                oomir::DataType::Class {
                    fields: Vec::new(),
                    is_abstract: false,
                    methods: HashMap::from_iter([(
                        method_name,
                        DataTypeMethod::Function(function),
                    )]),
                    super_class: Some("java/lang/Object".to_string()),
                    interfaces: Vec::new(),
                },
            );
        }
    }
    Ok(helper)
}
