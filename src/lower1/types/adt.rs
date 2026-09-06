use super::*;
use crate::lower1::context::Definitions;

pub(crate) fn should_define_named_data_type<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    def_id.is_local() || matches!(tcx.crate_name(def_id.krate), sym::core | sym::alloc)
}

pub(super) fn ensure_adt_data_type<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    jvm_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) {
    if adt_def.is_struct() {
        let variant = adt_def.variant(0usize.into());
        if !data_types.contains_key(jvm_name) {
            // Pre-populate with a placeholder class to break recursive resolution loops.
            data_types.insert(
                jvm_name.to_string(),
                oomir::DataType::Class {
                    fields: vec![],
                    is_abstract: false,
                    methods: HashMap::default(),
                    super_class: None,
                    interfaces: vec![],
                },
            );

            let oomir_fields = variant
                .fields
                .iter()
                .filter_map(|field_def| {
                    let field_name = field_def.ident(tcx).to_string();
                    let field_ty = field_def.ty(tcx, substs);
                    let field_mir_ty = if field_ty.has_param() || field_ty.has_escaping_bound_vars()
                    {
                        field_ty.skip_norm_wip()
                    } else {
                        tcx.try_normalize_erasing_regions(
                            TypingEnv::fully_monomorphized(),
                            field_ty,
                        )
                        .unwrap_or_else(|_| field_ty.skip_norm_wip())
                    };
                    let field_oomir_type =
                        ty_to_oomir_type(field_mir_ty, tcx, data_types, instance_context);
                    field_oomir_type
                        .has_jvm_value()
                        .then_some((field_name, field_oomir_type))
                })
                .collect::<Vec<_>>();
            let methods = HashMap::from_iter([(
                "eq".to_string(),
                DataTypeMethod::AdtHelperMethod {
                    kind: oomir::AdtHelperKind::PartialEqClass {
                        fields: oomir_fields.clone(),
                    },
                },
            )]);
            if let Some(oomir::DataType::Class {
                fields,
                methods: existing_methods,
                ..
            }) = data_types.get_mut(jvm_name)
            {
                *fields = oomir_fields;
                // Field type resolution may recursively enrich the placeholder.
                existing_methods.extend(methods);
            }
        } else if let Some(oomir::DataType::Class {
            fields, methods, ..
        }) = data_types.get_mut(jvm_name)
        {
            methods
                .entry("eq".to_string())
                .or_insert_with(|| DataTypeMethod::AdtHelperMethod {
                    kind: oomir::AdtHelperKind::PartialEqClass {
                        fields: fields.clone(),
                    },
                });
        }
    } else if adt_def.is_enum() {
        ensure_enum_data_types(adt_def, substs, jvm_name, tcx, data_types, instance_context);
    } else if adt_def.is_union() {
        ensure_union_data_type(adt_def, substs, tcx, data_types, instance_context);
    }

    let rust_ty = Ty::new_adt(tcx, *adt_def, substs);
    let needs_custom_drop_fields = adt_def.is_struct()
        && !adt_def.is_box()
        && adt_def.destructor(tcx).is_some()
        && !rust_ty.has_param()
        && !rust_ty.has_escaping_bound_vars()
        && matches!(
            data_types.get(jvm_name),
            Some(oomir::DataType::Class { methods, .. })
                if !methods.contains_key(ENUM_DROP_FIELDS_METHOD)
        );
    if needs_custom_drop_fields {
        if let Some(oomir::DataType::Class { methods, .. }) = data_types.get_mut(jvm_name) {
            methods.insert(
                ENUM_DROP_FIELDS_METHOD.to_string(),
                DataTypeMethod::SimpleConstantReturn(oomir::Type::Void, None),
            );
        }
        let fields_method = managed_struct_drop_fields_function(
            rust_ty,
            jvm_name,
            tcx,
            data_types,
            instance_context,
        );
        if let Some(oomir::DataType::Class { methods, .. }) = data_types.get_mut(jvm_name) {
            methods.insert(
                ENUM_DROP_FIELDS_METHOD.to_string(),
                DataTypeMethod::Function(fields_method),
            );
        }
    }
    let needs_managed_drop = !rust_ty.has_param()
        && !rust_ty.has_escaping_bound_vars()
        && rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
        && matches!(
            data_types.get(jvm_name),
            Some(oomir::DataType::Class { methods, .. })
                | Some(oomir::DataType::Interface { methods, .. })
                if !methods.contains_key(MANAGED_DROP_METHOD)
        );
    if needs_managed_drop {
        match data_types.get_mut(jvm_name) {
            Some(oomir::DataType::Class {
                methods,
                interfaces,
                ..
            })
            | Some(oomir::DataType::Interface {
                methods,
                interfaces,
                ..
            }) => {
                methods.insert(
                    MANAGED_DROP_METHOD.to_string(),
                    DataTypeMethod::SimpleConstantReturn(oomir::Type::Void, None),
                );
                if !interfaces
                    .iter()
                    .any(|interface| interface == MANAGED_DROP_INTERFACE)
                {
                    interfaces.push(MANAGED_DROP_INTERFACE.to_string());
                }
            }
            None => {}
        }
        let drop_method =
            managed_drop_glue_function(rust_ty, jvm_name, tcx, data_types, instance_context);
        match data_types.get_mut(jvm_name) {
            Some(oomir::DataType::Class { methods, .. })
            | Some(oomir::DataType::Interface { methods, .. }) => {
                methods.insert(
                    MANAGED_DROP_METHOD.to_string(),
                    DataTypeMethod::Function(drop_method),
                );
            }
            None => {}
        }
    }
}

pub(crate) fn force_define_named_adt<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Type {
    let TyKind::Adt(adt_def, substs) = ty.kind() else {
        return ty_to_oomir_type(ty, tcx, data_types, instance_context);
    };
    if crate::lower1::is_non_null_lang_item(tcx, adt_def.did()) {
        let lowered = ty_to_oomir_type(ty, tcx, data_types, instance_context);
        if matches!(lowered, oomir::Type::Pointer(_)) {
            return lowered;
        }
    }
    let jvm_name = generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance_context);
    ensure_adt_data_type(
        adt_def,
        substs,
        &jvm_name,
        tcx,
        data_types,
        instance_context,
    );
    oomir::Type::Class(jvm_name)
}
