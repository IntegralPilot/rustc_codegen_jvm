use super::*;
use crate::lower1::context::Definitions;

pub(crate) fn adt_class_kind<'tcx>(
    tcx: TyCtxt<'tcx>,
    adt: &AdtDef<'tcx>,
    args: GenericArgsRef<'tcx>,
) -> oomir::ClassKind {
    if !is_codegen_sized(Ty::new_adt(tcx, *adt, args), tcx) {
        oomir::ClassKind::MemoryView
    } else if has_java_adt_identity(tcx, adt) {
        oomir::ClassKind::JavaValue
    } else {
        oomir::ClassKind::Value
    }
}

pub(super) fn has_java_adt_identity(tcx: TyCtxt<'_>, adt: &AdtDef<'_>) -> bool {
    tcx.generics_of(adt.did()).own_params.is_empty()
        && tcx.visibility(adt.did()).is_public()
        && (crate::java_exports::is_exported(tcx, adt.did())
            || crate::java_exports::is_runtime_type(tcx, adt.did()))
}

/// Positional private fields permit shared storage classes. Java exports keep their declared names.
pub(crate) fn struct_field_name<'tcx>(
    tcx: TyCtxt<'tcx>,
    adt: &AdtDef<'tcx>,
    index: usize,
) -> String {
    // NonNull's fallback carrier participates in the compiler's pointer ABI.
    if !has_java_adt_identity(tcx, adt) && !crate::lower1::is_non_null_lang_item(tcx, adt.did()) {
        format!("field{index}")
    } else {
        adt.non_enum_variant().fields[FieldIdx::from_usize(index)]
            .ident(tcx)
            .to_string()
    }
}

/// Downstream codecs require the complete field schema, including fields whose helpers this crate
/// does not emit.
pub(super) fn remember_external_fields<'tcx>(
    adt: &AdtDef<'tcx>,
    args: GenericArgsRef<'tcx>,
    name: &str,
    tcx: TyCtxt<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instance: rustc_middle::ty::Instance<'tcx>,
) {
    if adt.is_union() || definitions.external_schemas.contains_key(name) {
        return;
    }
    let schema = |fields, interfaces| oomir::DataType::Class {
        kind: adt_class_kind(tcx, adt, args),
        fields,
        is_abstract: false,
        methods: HashMap::default(),
        super_class: None,
        interfaces,
    };
    definitions.external_schemas.insert(
        name.into(),
        if adt.is_enum() {
            oomir::DataType::Interface {
                methods: imported_enum_methods(adt, args, tcx),
                interfaces: Vec::new(),
                is_enum: true,
            }
        } else {
            schema(Vec::new(), Vec::new())
        },
    );
    for variant in adt.variants() {
        if adt.is_enum() && is_jvm_subtype_variant(tcx, variant) {
            continue;
        }
        let fields = variant
            .fields
            .iter()
            .enumerate()
            .filter_map(|(index, field)| {
                let ty = ty_to_oomir_type(
                    field.ty(tcx, args).skip_norm_wip(),
                    tcx,
                    definitions,
                    instance,
                );
                let name = if adt.is_enum() {
                    enum_variant_field_name(variant, index, tcx)
                } else {
                    struct_field_name(tcx, adt, index)
                };
                ty.has_jvm_value().then_some((name, ty))
            })
            .collect();
        let (owner, interfaces) = if adt.is_enum() {
            (
                format!(
                    "{name}${}",
                    crate::lower1::types::enum_variant_name(variant, tcx)
                ),
                vec![name.to_string()],
            )
        } else {
            (name.to_string(), Vec::new())
        };
        definitions
            .external_schemas
            .insert(owner, schema(fields, interfaces));
    }
}

// Imported enums need tag factory declarations for scalar conversion. They do not need duplicate
// factory bodies.
fn imported_enum_methods<'tcx>(
    adt: &AdtDef<'tcx>,
    args: GenericArgsRef<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> HashMap<String, DataTypeMethod> {
    let mut names = vec!["eq", "variantIndex"];
    if tcx.is_lang_item(adt.did(), rustc_attr_ir::lang_items::LangItem::Option) {
        names.extend(["is_none", "is_some"]);
    }
    let transparent = adt
        .variants()
        .iter()
        .any(|v| is_jvm_subtype_variant(tcx, v));
    if enum_union_discriminant_supported(adt, tcx) {
        names.push(ENUM_UNION_DISCRIMINANT_METHOD);
        if !transparent && adt_class_kind(tcx, adt, args) == oomir::ClassKind::Value {
            names.push(oomir::ENUM_TAG_METHOD);
        }
    }
    if !transparent && simple_enum_union_size(adt, tcx).is_ok() {
        names.push(ENUM_FROM_UNION_DISCRIMINANT_METHOD);
    }
    names
        .into_iter()
        .map(|name| {
            (
                name.to_string(),
                DataTypeMethod::SimpleConstantReturn(oomir::Type::Void, None),
            )
        })
        .collect()
}

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
                    kind: adt_class_kind(tcx, adt_def, substs),
                    is_abstract: false,
                    methods: HashMap::default(),
                    super_class: None,
                    interfaces: vec![],
                },
            );

            let oomir_fields = variant
                .fields
                .iter()
                .enumerate()
                .filter_map(|(index, field_def)| {
                    let field_name = struct_field_name(tcx, adt_def, index);
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
    ensure_managed_drop(rust_ty, jvm_name, tcx, data_types, instance_context);
}

pub(super) fn ensure_managed_drop<'tcx>(
    rust_ty: Ty<'tcx>,
    jvm_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) {
    // Rust drops call their glue directly. Only Java exports and trait adapters require object
    // callbacks.
    let java_surface = matches!(
        rust_ty.kind(),
        TyKind::Adt(def, _) if has_java_adt_identity(tcx, def)
    );
    if !java_surface {
        return;
    }
    ensure_drop_callback(rust_ty, jvm_name, tcx, data_types, instance_context);
}

/// Adds destruction only when an erased or Java-facing receiver needs it.
pub(crate) fn ensure_drop_callback<'tcx>(
    rust_ty: Ty<'tcx>,
    jvm_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) {
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
    let ty = data_types.normalize(tcx, ty, instance_context);
    let TyKind::Adt(adt_def, substs) = ty.kind() else {
        return ty_to_oomir_type(ty, tcx, data_types, instance_context);
    };
    if tcx.lang_items().phantom_data() == Some(adt_def.did()) {
        return oomir::Type::Unit;
    }
    if tagged_scalar(ty, tcx).is_some() {
        return oomir::Type::TaggedI64;
    }
    if let Some(payload) = direct_enum_payload(ty, tcx) {
        return ty_to_oomir_type(payload, tcx, data_types, instance_context);
    }
    if let Some(scalar) = value_scalar_ty(ty, tcx) {
        return ty_to_oomir_type(scalar, tcx, data_types, instance_context);
    }
    if let Some(payload) = transparent_payload(ty, tcx) {
        return ty_to_oomir_type(payload.ty, tcx, data_types, instance_context);
    }
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
