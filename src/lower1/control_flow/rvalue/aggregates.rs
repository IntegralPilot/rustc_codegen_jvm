use super::*;

/// Returns whether an operand already has the value supplied by a freshly
/// allocated JVM array.  Rust repeat expressions occur in some very large
/// core-library buffers, so emitting one store per element can exceed the JVM
/// method-size limit even when every store only writes the array's default.
pub(super) fn is_jvm_array_default_value(value: &oomir::Operand, element_ty: &oomir::Type) -> bool {
    if !element_ty.has_jvm_value() {
        return true;
    }

    match (element_ty, value) {
        (oomir::Type::I8, oomir::Operand::Constant(oomir::Constant::I8(0)))
        | (oomir::Type::U8, oomir::Operand::Constant(oomir::Constant::U8(0)))
        | (oomir::Type::I16, oomir::Operand::Constant(oomir::Constant::I16(0)))
        | (oomir::Type::U16, oomir::Operand::Constant(oomir::Constant::U16(0)))
        | (oomir::Type::F16, oomir::Operand::Constant(oomir::Constant::F16(0)))
        | (oomir::Type::I32, oomir::Operand::Constant(oomir::Constant::I32(0)))
        | (oomir::Type::U32, oomir::Operand::Constant(oomir::Constant::U32(0)))
        | (oomir::Type::I64, oomir::Operand::Constant(oomir::Constant::I64(0)))
        | (oomir::Type::U64, oomir::Operand::Constant(oomir::Constant::U64(0))) => true,
        (oomir::Type::F32, oomir::Operand::Constant(oomir::Constant::F32(value))) => {
            value.to_bits() == 0
        }
        (oomir::Type::F64, oomir::Operand::Constant(oomir::Constant::F64(value))) => {
            value.to_bits() == 0
        }
        (oomir::Type::Boolean, oomir::Operand::Constant(oomir::Constant::Boolean(false)))
        | (oomir::Type::Char, oomir::Operand::Constant(oomir::Constant::Char('\0'))) => true,
        (ty, oomir::Operand::Constant(oomir::Constant::Null(_))) => ty.is_jvm_reference_type(),
        _ => false,
    }
}

pub(super) fn jvm_default_value(ty: &oomir::Type) -> oomir::Operand {
    let constant = match ty {
        oomir::Type::Boolean => oomir::Constant::Boolean(false),
        oomir::Type::Char => oomir::Constant::Char('\0'),
        oomir::Type::I8 => oomir::Constant::I8(0),
        oomir::Type::U8 => oomir::Constant::U8(0),
        oomir::Type::I16 => oomir::Constant::I16(0),
        oomir::Type::U16 => oomir::Constant::U16(0),
        oomir::Type::F16 => oomir::Constant::F16(0),
        oomir::Type::I32 => oomir::Constant::I32(0),
        oomir::Type::U32 => oomir::Constant::U32(0),
        oomir::Type::I64 => oomir::Constant::I64(0),
        oomir::Type::U64 => oomir::Constant::U64(0),
        oomir::Type::F32 => oomir::Constant::F32(0.0),
        oomir::Type::F64 => oomir::Constant::F64(0.0),
        oomir::Type::Unit | oomir::Type::Void => oomir::Constant::Unit,
        ty if ty.is_jvm_reference_type() => oomir::Constant::Null(ty.clone()),
        other => panic!("no JVM default value for {other:?}"),
    };
    oomir::Operand::Constant(constant)
}

pub(super) fn adapt_value_for_field<'tcx>(
    value_operand: oomir::Operand,
    field_rust_ty: rustc_middle::ty::Ty<'tcx>,
    field_ty: &oomir::Type,
    temp_base_name: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let value_operand = crate::lower1::value_repr::adapt_operand_to_rust_type(
        value_operand,
        field_rust_ty,
        temp_base_name,
        tcx,
        instance,
        data_types,
        instructions,
    );
    let Some(value_ty) = value_operand.get_type() else {
        return value_operand;
    };

    if let oomir::Type::Class(class_name) = field_ty
        && !value_ty.is_jvm_reference_type()
    {
        let temp_name = generate_temp_var_name(data_types, temp_base_name);
        let enum_value = adapt_simple_enum_operand(
            value_operand.clone(),
            field_ty,
            &temp_name,
            data_types,
            instructions,
        );
        if enum_value.get_type().as_ref() == Some(field_ty) {
            return enum_value;
        }
        if class_name == "java/lang/Object" {
            instructions.push(oomir::Instruction::Cast {
                op: value_operand,
                ty: field_ty.clone(),
                dest: temp_name.clone(),
            });
            return oomir::Operand::Variable {
                name: temp_name,
                ty: field_ty.clone(),
            };
        }

        let is_marker_class = matches!(
            data_types.get(class_name),
            Some(oomir::DataType::Class {
                fields,
                is_abstract: false,
                ..
            }) if fields.is_empty()
        );

        if is_marker_class {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: temp_name.clone(),
                class_name: class_name.clone(),
                args: Vec::new(),
            });
            return oomir::Operand::Variable {
                name: temp_name,
                ty: field_ty.clone(),
            };
        }
    }

    value_operand
}

impl<'tcx> RvalueContext<'_, 'tcx> {
    pub(super) fn lower_aggregates(
        self,
        rvalue: &Rvalue<'tcx>,
    ) -> (Vec<oomir::Instruction>, oomir::Operand) {
        let Self {
            original_dest_place,
            mir,
            tcx,
            instance,
            data_types,
            ..
        } = self;
        let mut instructions = Vec::new();
        let result_operand;
        let base_temp_name = place_to_string(original_dest_place, tcx);
        match rvalue {
            Rvalue::Aggregate(kind, operands) => {
                let kind = kind.as_ref();
                // Create a temporary variable to hold the aggregate
                let temp_aggregate_var = generate_temp_var_name(data_types, &base_temp_name);
                // Get the type from the original destination place
                let aggregate_oomir_type =
                    get_place_type(original_dest_place, mir, tcx, instance, data_types);
                let aggregate_has_jvm_value = aggregate_oomir_type.has_jvm_value();

                match kind {
                    rustc_middle::mir::AggregateKind::Tuple if !aggregate_has_jvm_value => {
                        // Coroutine MIR represents unit yields and returns as empty
                        // tuple aggregates, which have no JVM value or constructor.
                        debug_assert!(operands.is_empty());
                    }
                    rustc_middle::mir::AggregateKind::Tuple => {
                        let tuple_class_name = match &aggregate_oomir_type {
                            oomir::Type::Class(name) => name.clone(),
                            _ => panic!("Tuple aggregate type error"),
                        };
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Tuple Aggregate -> Temp Var '{}'",
                                temp_aggregate_var
                            )
                        );
                        let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                        let mut constructor_args = Vec::new();
                        for (i, mir_op) in operands.iter().enumerate() {
                            let element_mir_ty = if let TyKind::Tuple(elements) = place_ty.kind() {
                                elements[i]
                            } else {
                                panic!("...")
                            };
                            let element_oomir_type =
                                ty_to_oomir_type(element_mir_ty.clone(), tcx, data_types, instance);
                            let value_operand = convert_operand(
                                mir_op,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let value_operand = adapt_value_for_field(
                                value_operand,
                                element_mir_ty,
                                &element_oomir_type,
                                &format!("{temp_aggregate_var}_field_{i}"),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                            constructor_args.push((value_operand, element_oomir_type));
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: tuple_class_name.clone(),
                            args: constructor_args,
                        });
                    }
                    rustc_middle::mir::AggregateKind::Array(mir_element_ty) => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Array Aggregate -> Temp Var '{}'",
                                temp_aggregate_var
                            )
                        );
                        let oomir_element_type =
                            ty_to_oomir_type(*mir_element_ty, tcx, data_types, instance);
                        let array_size = operands.len();
                        let size_operand =
                            oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));
                        instructions.push(oomir::Instruction::NewArray {
                            dest: temp_aggregate_var.clone(),
                            element_type: oomir_element_type.clone(),
                            size: size_operand,
                        });
                        // Store elements into the temporary array
                        for (i, mir_operand) in operands.iter().enumerate() {
                            let value_operand = convert_operand(
                                mir_operand,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let value_operand = adapt_value_for_field(
                                value_operand,
                                *mir_element_ty,
                                &oomir_element_type,
                                &format!("{}_{}", temp_aggregate_var, i),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                            let index_operand =
                                oomir::Operand::Constant(oomir::Constant::I32(i as i32));
                            instructions.push(oomir::Instruction::ArrayStore {
                                array: oomir::Operand::Variable {
                                    name: temp_aggregate_var.clone(),
                                    ty: oomir::Type::Array(Box::new(oomir_element_type.clone())),
                                },
                                index: index_operand,
                                value: value_operand,
                                copy_value: false,
                            });
                        }
                    }
                    rustc_middle::mir::AggregateKind::Closure(_, _) => {
                        let closure_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                        if let Some(callable_abi) =
                            closure_callable_abi(closure_ty, tcx, data_types, instance)
                        {
                            ensure_closure_callable_bridge(
                                closure_ty,
                                &callable_abi,
                                data_types,
                                tcx,
                                instance,
                            );
                        }
                        let closure_class_name = match &aggregate_oomir_type {
                            oomir::Type::Class(name) => name.clone(),
                            _ => panic!("Closure aggregate type error"),
                        };
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Closure Aggregate -> Temp Var '{}' (Class: {})",
                                temp_aggregate_var, closure_class_name
                            )
                        );
                        let closure_fields = match data_types.get(&closure_class_name) {
                            Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
                            _ => Vec::new(),
                        };

                        let mut constructor_args = Vec::new();
                        for (i, mir_operand) in operands.iter().enumerate() {
                            let (_field_name, field_ty) =
                                closure_fields.get(i).cloned().unwrap_or_else(|| {
                                    let operand_mir_ty = mir_operand.ty(&mir.local_decls, tcx);
                                    (
                                        format!("arg{}", i),
                                        ty_to_oomir_type(operand_mir_ty, tcx, data_types, instance),
                                    )
                                });
                            let value_operand = convert_operand(
                                mir_operand,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let value_operand = adapt_value_for_field(
                                value_operand,
                                mir_operand.ty(&mir.local_decls, tcx),
                                &field_ty,
                                &format!("{temp_aggregate_var}_field_{i}"),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                            constructor_args.push((value_operand, field_ty));
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: closure_class_name.clone(),
                            args: constructor_args,
                        });
                    }
                    rustc_middle::mir::AggregateKind::Coroutine(_, _) => {
                        let coroutine_class_name = match &aggregate_oomir_type {
                            oomir::Type::Class(name) => name.clone(),
                            _ => panic!("Coroutine aggregate type error"),
                        };
                        let coroutine_fields = match data_types.get(&coroutine_class_name) {
                            Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
                            _ => Vec::new(),
                        };
                        let mut constructor_args = Vec::with_capacity(coroutine_fields.len());
                        for (field_name, field_ty) in &coroutine_fields {
                            let capture_index = field_name
                                .strip_prefix("arg")
                                .and_then(|index| index.parse::<usize>().ok());
                            if let Some((capture_index, mir_operand)) =
                                capture_index.and_then(|index| {
                                    operands
                                        .get(FieldIdx::from_usize(index))
                                        .map(|op| (index, op))
                                })
                            {
                                let value = convert_operand(
                                    mir_operand,
                                    tcx,
                                    instance,
                                    mir,
                                    data_types,
                                    &mut instructions,
                                );
                                let value = adapt_value_for_field(
                                    value,
                                    mir_operand.ty(&mir.local_decls, tcx),
                                    field_ty,
                                    &format!("{temp_aggregate_var}_field_{capture_index}"),
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                );
                                constructor_args.push((value, field_ty.clone()));
                            } else {
                                constructor_args
                                    .push((jvm_default_value(field_ty), field_ty.clone()));
                            }
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: coroutine_class_name,
                            args: constructor_args,
                        });
                    }
                    rustc_middle::mir::AggregateKind::Adt(
                        def_id,
                        variant_idx,
                        substs,
                        _,
                        active_field_idx,
                    ) => {
                        let adt_def = tcx.adt_def(*def_id);
                        let should_define_data_type = should_define_named_data_type(tcx, *def_id);
                        if !should_define_data_type {
                            crate::lower1::types::force_define_named_adt(
                                Ty::new_adt(tcx, adt_def, substs),
                                tcx,
                                data_types,
                                instance,
                            );
                        }
                        if crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
                            && matches!(aggregate_oomir_type, oomir::Type::Pointer(_))
                        {
                            let operand = operands
                                .iter()
                                .next()
                                .expect("NonNull aggregate has a pointer field");
                            let value = convert_operand(
                                operand,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            instructions.push(oomir::Instruction::Move {
                                dest: temp_aggregate_var.clone(),
                                src: value,
                            });
                        } else if adt_def.is_struct() {
                            let variant = adt_def.variant(*variant_idx);
                            let jvm_class_name = generate_adt_jvm_class_name(
                                &adt_def, substs, tcx, data_types, instance,
                            );
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mir-lowering",
                                format!(
                                    "Info: Handling Struct Aggregate -> Temp Var '{}' (Class: {})",
                                    temp_aggregate_var, jvm_class_name
                                )
                            );
                            let oomir_fields: Vec<(String, oomir::Type)> = variant
                                .fields
                                .iter()
                                .filter_map(|f| {
                                    let field_ty = ty_to_oomir_type(
                                        f.ty(tcx, substs).skip_norm_wip(),
                                        tcx,
                                        data_types,
                                        instance,
                                    );
                                    field_ty
                                        .has_jvm_value()
                                        .then(|| (f.ident(tcx).to_string(), field_ty))
                                })
                                .collect();
                            if should_define_data_type && !data_types.contains_key(&jvm_class_name)
                            {
                                let mut methods = HashMap::default();
                                methods.insert(
                                    "eq".to_string(),
                                    DataTypeMethod::AdtHelperMethod {
                                        kind: oomir::AdtHelperKind::PartialEqClass {
                                            fields: oomir_fields.clone(),
                                        },
                                    },
                                );
                                data_types.insert(
                                    jvm_class_name.clone(),
                                    oomir::DataType::Class {
                                        fields: oomir_fields.clone(),
                                        is_abstract: false,
                                        methods,
                                        super_class: None,
                                        interfaces: vec![],
                                    },
                                );
                            } else if should_define_data_type
                                && let Some(oomir::DataType::Class {
                                    fields, methods, ..
                                }) = data_types.get_mut(&jvm_class_name)
                            {
                                methods.entry("eq".to_string()).or_insert_with(|| {
                                    DataTypeMethod::AdtHelperMethod {
                                        kind: oomir::AdtHelperKind::PartialEqClass {
                                            fields: fields.clone(),
                                        },
                                    }
                                });
                            }

                            let mut constructor_args = Vec::new();
                            for (field_index, (field_def, mir_operand)) in
                                variant.fields.iter().zip(operands.iter()).enumerate()
                            {
                                let field_mir_ty = field_def.ty(tcx, substs).skip_norm_wip();
                                let field_oomir_type =
                                    ty_to_oomir_type(field_mir_ty, tcx, data_types, instance);
                                if !field_oomir_type.has_jvm_value() {
                                    continue;
                                }
                                let value_operand = convert_operand(
                                    mir_operand,
                                    tcx,
                                    instance,
                                    mir,
                                    data_types,
                                    &mut instructions,
                                );
                                let value_operand = adapt_value_for_field(
                                    value_operand,
                                    field_mir_ty,
                                    &field_oomir_type,
                                    &format!("{temp_aggregate_var}_field_{field_index}"),
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                );
                                constructor_args.push((value_operand, field_oomir_type));
                            }
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest: temp_aggregate_var.clone(),
                                class_name: jvm_class_name.clone(),
                                args: constructor_args,
                            });
                        } else if adt_def.is_enum() {
                            let variant_def = adt_def.variant(*variant_idx);
                            let base_enum_name = generate_adt_jvm_class_name(
                                &adt_def, substs, tcx, data_types, instance,
                            );
                            force_define_named_adt(
                                Ty::new_adt(tcx, adt_def, substs),
                                tcx,
                                data_types,
                                instance,
                            );
                            let variant_class_name = format!(
                                "{}${}",
                                base_enum_name,
                                jvm_names::member_name(&variant_def.name.to_string())
                            );
                            let transparent_payload =
                                jvm_subtype_payload_ty(&adt_def, variant_def, substs, tcx);

                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mir-lowering",
                                format!(
                                    "Info: Handling Enum Aggregate (Variant: {}) -> Temp Var '{}' (Class: {})",
                                    variant_def.name, temp_aggregate_var, variant_class_name
                                )
                            );

                            let mut constructor_args = Vec::new();
                            for (i, field) in variant_def.fields.iter().enumerate() {
                                let field_mir_ty = field.ty(tcx, substs).skip_norm_wip();
                                let field_oomir_type =
                                    ty_to_oomir_type(field_mir_ty, tcx, data_types, instance);
                                if !field_oomir_type.has_jvm_value() {
                                    continue;
                                }
                                let value_operand = convert_operand(
                                    &operands[FieldIdx::from_usize(i)],
                                    tcx,
                                    instance,
                                    mir,
                                    data_types,
                                    &mut instructions,
                                );
                                let value_operand = adapt_value_for_field(
                                    value_operand,
                                    field_mir_ty,
                                    &field_oomir_type,
                                    &format!("{temp_aggregate_var}_field_{i}"),
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                );
                                constructor_args.push((value_operand, field_oomir_type));
                            }
                            if transparent_payload.is_some() {
                                let (value, _) =
                                    constructor_args.into_iter().next().unwrap_or_else(|| {
                                        tcx.dcx().span_fatal(
                                            tcx.def_span(variant_def.def_id),
                                            "`#[jvm_codegen::subtype]` payload has no JVM value",
                                        )
                                    });
                                instructions.push(oomir::Instruction::Cast {
                                    dest: temp_aggregate_var.clone(),
                                    op: value,
                                    ty: oomir::Type::Class(base_enum_name.clone()),
                                });
                            } else {
                                instructions.push(oomir::Instruction::ConstructObject {
                                    dest: temp_aggregate_var.clone(),
                                    class_name: variant_class_name.clone(),
                                    args: constructor_args,
                                });
                            }
                        } else {
                            let union_class_name = if should_define_data_type {
                                ensure_union_data_type(&adt_def, substs, tcx, data_types, instance)
                            } else {
                                generate_adt_jvm_class_name(
                                    &adt_def, substs, tcx, data_types, instance,
                                )
                            };
                            let active_field_idx =
                                active_field_idx.unwrap_or(FieldIdx::from_usize(0));
                            let variant = adt_def.variant(*variant_idx);
                            let field_def = &variant.fields[active_field_idx];
                            let field_name = field_def.ident(tcx).to_string();
                            let field_mir_ty = field_def.ty(tcx, substs).skip_norm_wip();
                            let field_oomir_ty =
                                ty_to_oomir_type(field_mir_ty, tcx, data_types, instance);
                            let is_unit = !field_oomir_ty.has_jvm_value();
                            let value_operand = (!is_unit).then(|| {
                                let value = convert_operand(
                                    &operands[FieldIdx::from_usize(0)],
                                    tcx,
                                    instance,
                                    mir,
                                    data_types,
                                    &mut instructions,
                                );
                                adapt_value_for_field(
                                    value,
                                    field_mir_ty,
                                    &field_oomir_ty,
                                    &temp_aggregate_var,
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                )
                            });

                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mir-lowering",
                                format!(
                                    "Info: Handling Union Aggregate field '{}' -> Temp Var '{}' (Class: {})",
                                    field_name, temp_aggregate_var, union_class_name
                                )
                            );

                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_aggregate_var.clone()),
                                class_name: union_class_name.clone(),
                                method_name: union_from_method_name(&field_name),
                                method_ty: oomir::Signature {
                                    params: if is_unit {
                                        vec![]
                                    } else {
                                        vec![("value".to_string(), field_oomir_ty)]
                                    },
                                    ret: Box::new(oomir::Type::Class(union_class_name)),
                                    is_static: true,
                                },
                                args: value_operand.into_iter().collect(),
                            });
                        }
                    }
                    rustc_middle::mir::AggregateKind::RawPtr(pointee_ty, _) => {
                        let pointee_ty = EarlyBinder::bind(tcx, *pointee_ty)
                            .instantiate(tcx, instance.args)
                            .skip_norm_wip();
                        let destination_ty = normalize_unsize_ty(
                            original_dest_place.ty(&mir.local_decls, tcx).ty,
                            tcx,
                            instance,
                        );
                        let destination_pointee = match destination_ty.kind() {
                            TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
                            _ => pointee_ty,
                        };
                        let data = convert_operand(
                            &operands[FieldIdx::from_usize(0)],
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        if destination_pointee.is_slice()
                            || destination_pointee.is_str()
                            || matches!(
                                aggregate_oomir_type,
                                oomir::Type::Slice(_) | oomir::Type::Str
                            )
                        {
                            let metadata = convert_operand(
                                &operands[FieldIdx::from_usize(1)],
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let is_str = destination_pointee.is_str()
                                || matches!(aggregate_oomir_type, oomir::Type::Str);
                            let element_ty = if is_str {
                                tcx.types.u8
                            } else {
                                match destination_pointee.kind() {
                                    TyKind::Slice(element_ty) => *element_ty,
                                    _ => match pointee_ty.kind() {
                                        TyKind::Slice(element_ty) => *element_ty,
                                        _ => unreachable!("slice pointee changed during lowering"),
                                    },
                                }
                            };
                            let data = emit_retyped_slice_data_pointer(
                                data,
                                rust_layout_size_operand(element_ty, tcx, instance),
                                crate::lower1::types::pointer_view_codec_operand(
                                    element_ty, tcx, data_types, instance,
                                ),
                                &temp_aggregate_var,
                                &mut instructions,
                            );
                            let view_class = if is_str {
                                oomir::UTF8_VIEW_CLASS
                            } else {
                                oomir::SLICE_VIEW_CLASS
                            };
                            let (backing, offset) = emit_pointer_slice_parts(
                                data,
                                &temp_aggregate_var,
                                &mut instructions,
                            );
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest: temp_aggregate_var.clone(),
                                class_name: view_class.to_string(),
                                args: vec![
                                    (backing, oomir::Type::Class("java/lang/Object".to_string())),
                                    (offset, oomir::Type::I32),
                                    (
                                        metadata,
                                        if is_str {
                                            oomir::Type::I32
                                        } else {
                                            oomir::Type::U64
                                        },
                                    ),
                                ],
                            });
                        } else if {
                            let tail = tcx.struct_tail_for_codegen(
                                pointee_ty,
                                TypingEnv::fully_monomorphized(),
                            );
                            tail.is_slice() || tail.is_str()
                        } {
                            let data_ty = data
                                .get_type()
                                .expect("slice-tailed raw pointer data pointer is typed");
                            let metadata = convert_operand(
                                &operands[FieldIdx::from_usize(1)],
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let pointee_size =
                                crate::lower1::types::layout_size_bytes(tcx, pointee_ty)
                                    .expect("slice-tailed raw pointer has a static prefix layout");
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_aggregate_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "retypeWithMetadata".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("pointer".to_string(), data_ty),
                                        ("view_size".to_string(), oomir::Type::U64),
                                        ("view_codec".to_string(), oomir::Type::java_string()),
                                        ("metadata".to_string(), oomir::Type::U64),
                                    ],
                                    ret: Box::new(aggregate_oomir_type.clone()),
                                    is_static: true,
                                },
                                args: vec![
                                    data,
                                    oomir::Operand::Constant(oomir::Constant::U64(
                                        u64::try_from(pointee_size)
                                            .expect("Rust raw DST prefix layout exceeds u64"),
                                    )),
                                    crate::lower1::types::pointer_view_codec_operand(
                                        pointee_ty, tcx, data_types, instance,
                                    ),
                                    metadata,
                                ],
                            });
                        } else if matches!(pointee_ty.kind(), TyKind::Dynamic(_, _)) {
                            let data_ty = data
                                .get_type()
                                .expect("raw trait-object aggregate data pointer is typed");
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_aggregate_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "restoreErasedView".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![("pointer".to_string(), data_ty)],
                                    ret: Box::new(aggregate_oomir_type.clone()),
                                    is_static: true,
                                },
                                args: vec![data],
                            });
                        } else {
                            let data_ty = data
                                .get_type()
                                .expect("thin raw pointer aggregate data pointer is typed");
                            if let Ok(pointee_size) =
                                crate::lower1::types::layout_size_bytes(tcx, pointee_ty)
                            {
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: Some(temp_aggregate_var.clone()),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "retype".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("pointer".to_string(), data_ty),
                                            ("view_size".to_string(), oomir::Type::U64),
                                            ("view_codec".to_string(), oomir::Type::java_string()),
                                        ],
                                        ret: Box::new(aggregate_oomir_type.clone()),
                                        is_static: true,
                                    },
                                    args: vec![
                                        data,
                                        oomir::Operand::Constant(oomir::Constant::U64(
                                            u64::try_from(pointee_size)
                                                .expect("Rust raw pointer layout exceeds u64"),
                                        )),
                                        crate::lower1::types::pointer_view_codec_operand(
                                            pointee_ty, tcx, data_types, instance,
                                        ),
                                    ],
                                });
                            } else {
                                // A generic thin pointer has no metadata operand carrying T's
                                // layout. Its data pointer came through an erased (`*const ()`)
                                // view, which records the prior concrete view in Pointer.
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: Some(temp_aggregate_var.clone()),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "restoreErasedView".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![("pointer".to_string(), data_ty)],
                                        ret: Box::new(aggregate_oomir_type.clone()),
                                        is_static: true,
                                    },
                                    args: vec![data],
                                });
                            }
                        }
                    }
                    _ => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "mir-lowering",
                            format!(
                                "Warning: Unhandled non-pointer Aggregate Kind {:?} -> Temp Placeholder",
                                kind
                            )
                        );
                    }
                }

                result_operand = if aggregate_has_jvm_value {
                    oomir::Operand::Variable {
                        name: temp_aggregate_var,
                        ty: aggregate_oomir_type,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
            }
            _ => unreachable!("rvalue routed to aggregates"),
        }
        (instructions, result_operand)
    }
}
