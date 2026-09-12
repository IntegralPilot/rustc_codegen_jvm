use super::*;
use crate::lower1::context::Definitions;

pub(super) fn unsupported_union_body(message: String) -> oomir::CodeBlock {
    oomir::CodeBlock {
        entry: "bb0".to_string(),
        basic_blocks: HashMap::from_iter([(
            "bb0".to_string(),
            oomir::BasicBlock {
                label: "bb0".to_string(),
                instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/UnsupportedOperationException".to_string(),
                    message,
                }],
            },
        )]),
    }
}

pub(super) fn simple_body(instructions: Vec<oomir::Instruction>) -> oomir::CodeBlock {
    oomir::CodeBlock {
        entry: "bb0".to_string(),
        basic_blocks: HashMap::from_iter([(
            "bb0".to_string(),
            oomir::BasicBlock {
                label: "bb0".to_string(),
                instructions,
            },
        )]),
    }
}

pub(super) fn union_from_function<'tcx>(
    union_class: &str,
    union_size: usize,
    object_storage_size: usize,
    field_name: &str,
    field_ty: Ty<'tcx>,
    field_oomir_ty: oomir::Type,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let is_unit = !field_oomir_ty.has_jvm_value();
    let mut instructions = vec![
        oomir::Instruction::NewArray {
            dest: "_bytes".to_string(),
            element_type: oomir::Type::I8,
            size: oomir::Operand::Constant(oomir::Constant::I32(union_size as i32)),
        },
        allocate_union_object_storage("_objects", object_storage_size),
    ];
    let mut temp_counter = 0;
    let storage = JvmUnionStorage::at_start("_bytes", "_objects");
    let body = match emit_ty_to_union_bytes(
        field_ty,
        operand_var("_1", field_oomir_ty.clone()),
        &storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut instructions,
        &mut temp_counter,
    ) {
        Ok(()) => {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: "_ret".to_string(),
                class_name: union_class.to_string(),
                args: vec![
                    (operand_var("_bytes", byte_array_type()), byte_array_type()),
                    (
                        operand_var("_objects", object_array_type()),
                        object_array_type(),
                    ),
                ],
            });
            instructions.push(oomir::Instruction::Return {
                operand: Some(operand_var(
                    "_ret",
                    oomir::Type::Class(union_class.to_string()),
                )),
            });
            simple_body(instructions)
        }
        Err(err) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "type-mapping",
                format!(
                    "Union constructor helper for {}.{} is unsupported: {}",
                    union_class, field_name, err
                )
            );
            unsupported_union_body(err)
        }
    };

    oomir::Function {
        name: union_from_method_name(field_name),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: if is_unit {
                vec![]
            } else {
                vec![("value".to_string(), field_oomir_ty)]
            },
            ret: Box::new(oomir::Type::Class(union_class.to_string())),
            is_static: true,
        },
        body: body.into(),
    }
}

pub(super) fn union_getter_function<'tcx>(
    union_class: &str,
    field_name: &str,
    field_ty: Ty<'tcx>,
    field_oomir_ty: oomir::Type,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    if !field_oomir_ty.has_jvm_value() {
        return oomir::Function {
            name: union_getter_method_name(field_name),
            owner_class: None,
            debug_variables: Vec::new(),
            signature: oomir::Signature {
                params: vec![(
                    "self".to_string(),
                    oomir::Type::Class(union_class.to_string()),
                )],
                ret: Box::new(oomir::Type::Void),
                is_static: false,
            },
            body: simple_body(vec![oomir::Instruction::Return { operand: None }]).into(),
        };
    }

    let mut instructions = vec![
        oomir::Instruction::GetField {
            dest: "_bytes".to_string(),
            object: operand_var("_1", oomir::Type::Class(union_class.to_string())),
            field_name: UNION_BYTES_FIELD.to_string(),
            field_ty: byte_array_type(),
            owner_class: union_class.to_string(),
        },
        oomir::Instruction::GetField {
            dest: "_objects".to_string(),
            object: operand_var("_1", oomir::Type::Class(union_class.to_string())),
            field_name: UNION_OBJECTS_FIELD.to_string(),
            field_ty: object_array_type(),
            owner_class: union_class.to_string(),
        },
    ];
    let mut temp_counter = 0;
    let storage = JvmUnionStorage::at_start("_bytes", "_objects");
    let body = match emit_ty_from_union_bytes(
        field_ty,
        &storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut instructions,
        &mut temp_counter,
    ) {
        Ok(value) => {
            instructions.push(oomir::Instruction::Return {
                operand: Some(value),
            });
            simple_body(instructions)
        }
        Err(err) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "type-mapping",
                format!(
                    "Union getter helper for {}.{} is unsupported: {}",
                    union_class, field_name, err
                )
            );
            unsupported_union_body(err)
        }
    };

    oomir::Function {
        name: union_getter_method_name(field_name),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![(
                "self".to_string(),
                oomir::Type::Class(union_class.to_string()),
            )],
            ret: Box::new(field_oomir_ty),
            is_static: false,
        },
        body: body.into(),
    }
}

pub(super) fn union_setter_function<'tcx>(
    union_class: &str,
    field_name: &str,
    field_ty: Ty<'tcx>,
    field_oomir_ty: oomir::Type,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let is_unit = !field_oomir_ty.has_jvm_value();
    let mut instructions = vec![
        oomir::Instruction::GetField {
            dest: "_bytes".to_string(),
            object: operand_var("_1", oomir::Type::Class(union_class.to_string())),
            field_name: UNION_BYTES_FIELD.to_string(),
            field_ty: byte_array_type(),
            owner_class: union_class.to_string(),
        },
        oomir::Instruction::GetField {
            dest: "_objects".to_string(),
            object: operand_var("_1", oomir::Type::Class(union_class.to_string())),
            field_name: UNION_OBJECTS_FIELD.to_string(),
            field_ty: object_array_type(),
            owner_class: union_class.to_string(),
        },
    ];
    let mut temp_counter = 0;
    let storage = JvmUnionStorage::at_start("_bytes", "_objects");
    let body = match emit_ty_to_union_bytes(
        field_ty,
        operand_var("_2", field_oomir_ty.clone()),
        &storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut instructions,
        &mut temp_counter,
    ) {
        Ok(()) => {
            instructions.push(oomir::Instruction::Return { operand: None });
            simple_body(instructions)
        }
        Err(err) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "type-mapping",
                format!(
                    "Union setter helper for {}.{} is unsupported: {}",
                    union_class, field_name, err
                )
            );
            unsupported_union_body(err)
        }
    };

    oomir::Function {
        name: union_setter_method_name(field_name),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![(
                "self".to_string(),
                oomir::Type::Class(union_class.to_string()),
            )]
            .into_iter()
            .chain((!is_unit).then_some(("value".to_string(), field_oomir_ty)))
            .collect(),
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        body: body.into(),
    }
}

pub(crate) fn ensure_union_data_type<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    let union_class =
        generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance_context);

    // This function is the sole creator of union classes. An existing class is
    // therefore either complete or the placeholder installed by an outer call.
    // Returning for both cases breaks recursive ZST/union codec generation.
    if data_types.contains_key(&union_class) {
        return union_class;
    }
    data_types.insert(
        union_class.clone(),
        oomir::DataType::Class {
            fields: vec![
                (UNION_BYTES_FIELD.to_string(), byte_array_type()),
                (UNION_OBJECTS_FIELD.to_string(), object_array_type()),
            ],
            is_abstract: false,
            methods: HashMap::default(),
            super_class: Some("java/lang/Object".to_string()),
            interfaces: vec![],
        },
    );

    let union_ty = tcx
        .type_of(adt_def.did())
        .instantiate(tcx, substs)
        .skip_norm_wip();
    let union_ty = resolve_union_ty(tcx, union_ty, instance_context).unwrap_or(union_ty);
    // Unresolved generic unions use one byte plus the object slot at offset zero.
    // Concrete monomorphizations replace this with their exact rustc layout.
    let union_size = layout_size_bytes(tcx, union_ty).unwrap_or(1);
    let object_storage_size =
        union_object_storage_size(union_ty, union_size, tcx, instance_context);

    let variant = adt_def.variant(0usize.into());
    let mut methods = HashMap::default();
    for field_def in variant.fields.iter() {
        let field_name = field_def.ident(tcx).to_string();
        let raw_field_ty = field_def.ty(tcx, substs).skip_norm_wip();
        let field_ty =
            resolve_union_ty(tcx, raw_field_ty, instance_context).unwrap_or(raw_field_ty);
        let field_oomir_ty = ty_to_oomir_type(field_ty, tcx, data_types, instance_context);

        methods.insert(
            union_from_method_name(&field_name),
            DataTypeMethod::Function(union_from_function(
                &union_class,
                union_size,
                object_storage_size,
                &field_name,
                field_ty,
                field_oomir_ty.clone(),
                tcx,
                data_types,
                instance_context,
            )),
        );
        methods.insert(
            union_getter_method_name(&field_name),
            DataTypeMethod::Function(union_getter_function(
                &union_class,
                &field_name,
                field_ty,
                field_oomir_ty.clone(),
                tcx,
                data_types,
                instance_context,
            )),
        );
        methods.insert(
            union_setter_method_name(&field_name),
            DataTypeMethod::Function(union_setter_function(
                &union_class,
                &field_name,
                field_ty,
                field_oomir_ty,
                tcx,
                data_types,
                instance_context,
            )),
        );
    }

    let union_fields = vec![
        (UNION_BYTES_FIELD.to_string(), byte_array_type()),
        (UNION_OBJECTS_FIELD.to_string(), object_array_type()),
    ];
    match data_types.get_mut(&union_class) {
        Some(oomir::DataType::Class {
            fields,
            methods: existing_methods,
            ..
        }) => {
            if !fields.iter().any(|(name, _)| name == UNION_BYTES_FIELD) {
                fields.insert(0, (UNION_BYTES_FIELD.to_string(), byte_array_type()));
            }
            if !fields.iter().any(|(name, _)| name == UNION_OBJECTS_FIELD) {
                fields.push((UNION_OBJECTS_FIELD.to_string(), object_array_type()));
            }
            existing_methods.extend(methods);
        }
        Some(oomir::DataType::Interface { .. }) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Union class name '{}' already exists as an interface",
                    union_class
                )
            );
        }
        None => {
            data_types.insert(
                union_class.clone(),
                oomir::DataType::Class {
                    fields: union_fields,
                    is_abstract: false,
                    methods,
                    super_class: Some("java/lang/Object".to_string()),
                    interfaces: vec![],
                },
            );
        }
    }

    union_class
}
