use super::*;
use crate::lower1::context::Definitions;

/// Returns the public JVM field name for a Rust enum variant payload.
///
/// Struct-like variants retain their source field names. Tuple-like variants
/// use `value` when there is exactly one payload and `_0`, `_1`, ... otherwise.
pub(crate) fn enum_variant_field_name(
    variant: &rustc_middle::ty::VariantDef,
    field_index: usize,
    tcx: TyCtxt<'_>,
) -> String {
    let field = variant
        .fields
        .get(FieldIdx::from_usize(field_index))
        .unwrap_or_else(|| {
            panic!(
                "enum variant {} has no field at index {field_index}",
                variant.name
            )
        });
    let source_name = field.ident(tcx).to_string();
    if source_name.parse::<usize>().is_ok() {
        if variant.fields.len() == 1 {
            "value".to_string()
        } else {
            format!("_{field_index}")
        }
    } else {
        jvm_names::member_name(&source_name)
    }
}

pub(crate) fn enum_scoped_method_name(enum_class: &str, method: &str) -> String {
    let mut hash = 0xcbf2_9ce4_8422_2325u64;
    for byte in enum_class.bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("{method}${hash:016x}")
}

pub(crate) fn union_from_method_name(field_name: &str) -> String {
    format!("from_{}", jvm_names::member_name(field_name))
}

pub(crate) fn union_getter_method_name(field_name: &str) -> String {
    format!("get_{}", jvm_names::member_name(field_name))
}

pub(crate) fn union_setter_method_name(field_name: &str) -> String {
    format!("set_{}", jvm_names::member_name(field_name))
}

pub(crate) fn is_jvm_subtype_variant<'tcx>(
    tcx: TyCtxt<'tcx>,
    variant: &rustc_middle::ty::VariantDef,
) -> bool {
    #[allow(deprecated)]
    tcx.get_all_attrs(variant.def_id).iter().any(|attribute| {
        let path = attribute.path();
        // `jvm` is retained for source compatibility. New code uses a separate
        // tool namespace so the optional `jvm` proc-macro crate can coexist.
        path.len() == 2
            && matches!(path[0].as_str(), "jvm_codegen" | "jvm")
            && path[1].as_str() == "subtype"
    })
}

pub(crate) fn jvm_subtype_payload_ty<'tcx>(
    outer_adt: &AdtDef<'tcx>,
    variant: &rustc_middle::ty::VariantDef,
    substs: GenericArgsRef<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> Option<Ty<'tcx>> {
    if !is_jvm_subtype_variant(tcx, variant) {
        return None;
    }
    let span = tcx.def_span(variant.def_id);
    if variant.fields.len() != 1 {
        tcx.dcx().span_fatal(
            span,
            "`#[jvm_codegen::subtype]` requires an enum variant with exactly one field",
        );
    }
    let payload_ty = variant.fields[FieldIdx::from_usize(0)]
        .ty(tcx, substs)
        .skip_norm_wip();
    let TyKind::Adt(inner_adt, _) = payload_ty.kind() else {
        tcx.dcx().span_fatal(
            span,
            "`#[jvm_codegen::subtype]` payload must be another Rust enum",
        );
    };
    if !inner_adt.is_enum() {
        tcx.dcx().span_fatal(
            span,
            "`#[jvm_codegen::subtype]` payload must be another Rust enum",
        );
    }
    if inner_adt.did() == outer_adt.did() {
        tcx.dcx().span_fatal(
            span,
            "`#[jvm_codegen::subtype]` cannot transparently embed the enum itself",
        );
    }
    if inner_adt.did().krate != outer_adt.did().krate {
        tcx.dcx().span_fatal(
            span,
            "`#[jvm_codegen::subtype]` requires both enums to be defined in the same crate",
        );
    }
    Some(payload_ty)
}

pub(super) fn add_enum_helper_methods(
    methods: &mut HashMap<String, DataTypeMethod>,
    enum_class: &str,
    variants_info: Vec<oomir::EnumVariantShape>,
    is_option: bool,
) {
    methods
        .entry("variantIndex".to_string())
        .or_insert(DataTypeMethod::AdtHelperMethod {
            kind: oomir::AdtHelperKind::EnumVariantIndex {
                enum_class: enum_class.to_string(),
                variants: variants_info.clone(),
            },
        });
    methods
        .entry("eq".to_string())
        .or_insert(DataTypeMethod::AdtHelperMethod {
            kind: oomir::AdtHelperKind::StaticPartialEqEnum {
                enum_class: enum_class.to_string(),
                variants: variants_info.clone(),
            },
        });

    if is_option {
        methods
            .entry("is_none".to_string())
            .or_insert(DataTypeMethod::AdtHelperMethod {
                kind: oomir::AdtHelperKind::EnumIsVariant {
                    enum_class: enum_class.to_string(),
                    runtime_type: variants_info[0].runtime_type.clone(),
                },
            });
        methods
            .entry("is_some".to_string())
            .or_insert(DataTypeMethod::AdtHelperMethod {
                kind: oomir::AdtHelperKind::EnumIsVariant {
                    enum_class: enum_class.to_string(),
                    runtime_type: variants_info[1].runtime_type.clone(),
                },
            });
    }
}

pub(super) fn enum_from_union_discriminant_function<'tcx>(
    adt_def: &AdtDef<'tcx>,
    union_size: usize,
    base_enum_name: &str,
    tcx: TyCtxt<'tcx>,
) -> oomir::Function {
    let mut basic_blocks = HashMap::default();
    let mut targets = Vec::new();

    for (variant_idx, discriminant) in adt_def.discriminants(tcx) {
        let variant = adt_def.variant(variant_idx);
        let variant_class_name = format!(
            "{}${}",
            base_enum_name,
            jvm_names::member_name(&variant.name.to_string())
        );
        let block_name = format!("variant_{}", variant_idx.as_u32());
        let result_name = format!("_variant_{}", variant_idx.as_u32());
        let masked_discriminant = masked_enum_discriminant(discriminant.val, union_size);
        targets.push((
            oomir::Constant::I64(masked_discriminant),
            block_name.clone(),
        ));
        let shift = 64 - union_size * 8;
        let signed_discriminant = (masked_discriminant << shift) >> shift;
        if signed_discriminant != masked_discriminant {
            targets.push((
                oomir::Constant::I64(signed_discriminant),
                block_name.clone(),
            ));
        }
        basic_blocks.insert(
            block_name.clone(),
            oomir::BasicBlock {
                label: block_name,
                instructions: vec![
                    oomir::Instruction::ConstructObject {
                        dest: result_name.clone(),
                        class_name: variant_class_name.clone(),
                        args: vec![],
                    },
                    oomir::Instruction::Return {
                        operand: Some(operand_var(
                            result_name,
                            oomir::Type::Class(variant_class_name),
                        )),
                    },
                ],
            },
        );
    }

    basic_blocks.insert(
        "entry".to_string(),
        oomir::BasicBlock {
            label: "entry".to_string(),
            instructions: vec![oomir::Instruction::Switch {
                discr: operand_var("_1", oomir::Type::I64),
                targets,
                otherwise: "invalid".to_string(),
            }],
        },
    );
    basic_blocks.insert(
        "invalid".to_string(),
        oomir::BasicBlock {
            label: "invalid".to_string(),
            instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                exception_class: "java/lang/IllegalArgumentException".to_string(),
                message: format!(
                    "invalid discriminant while reading enum {} from union storage",
                    base_enum_name
                ),
            }],
        },
    );

    oomir::Function {
        name: ENUM_FROM_UNION_DISCRIMINANT_METHOD.to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![("discriminant".to_string(), oomir::Type::I64)],
            ret: Box::new(oomir::Type::Class(base_enum_name.to_string())),
            is_static: true,
        },
        body: oomir::CodeBlock {
            entry: "entry".to_string(),
            basic_blocks,
        }
        .into(),
    }
}

pub(super) fn ensure_enum_data_types<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    base_enum_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) {
    let should_lower = data_types
        .enums_in_progress
        .insert(base_enum_name.to_owned());
    if !should_lower {
        return;
    }

    let mut created_placeholders = Vec::new();

    // 1. Insert a placeholder for the enum interface.
    if !data_types.contains_key(base_enum_name) {
        data_types.insert(
            base_enum_name.to_string(),
            oomir::DataType::Interface {
                methods: HashMap::default(),
                interfaces: vec![],
                is_enum: true,
            },
        );
        created_placeholders.push(base_enum_name.to_string());
    }

    // 2. Insert placeholders for all individual variant subclasses
    for variant in adt_def.variants().iter() {
        if is_jvm_subtype_variant(tcx, variant) {
            continue;
        }
        let variant_class_name = format!(
            "{}${}",
            base_enum_name,
            jvm_names::member_name(&variant.name.to_string())
        );
        if !data_types.contains_key(&variant_class_name) {
            data_types.insert(
                variant_class_name.clone(),
                oomir::DataType::Class {
                    fields: vec![],
                    is_abstract: false,
                    methods: HashMap::default(),
                    super_class: None,
                    interfaces: vec![base_enum_name.to_string()],
                },
            );
            created_placeholders.push(variant_class_name);
        }
    }

    let union_size = simple_enum_union_size(adt_def, tcx).ok();
    let has_numeric_discriminant = enum_union_discriminant_supported(adt_def, tcx);
    // A transparent case has no `$Variant` constructor. Its union reader is
    // emitted below with the full Rust layout information instead.
    let has_transparent_variant = adt_def
        .variants()
        .iter()
        .any(|variant| is_jvm_subtype_variant(tcx, variant));
    let union_factory = (!has_transparent_variant)
        .then_some(union_size)
        .flatten()
        .map(|size| enum_from_union_discriminant_function(adt_def, size, base_enum_name, tcx));
    let variants_info: Vec<_> = adt_def
        .variants()
        .iter()
        .map(|variant| {
            let variant_name = jvm_names::member_name(&variant.name.to_string());
            let fields = variant
                .fields
                .iter()
                .enumerate()
                .filter_map(|(field_index, field)| {
                    let field_ty = ty_to_oomir_type(
                        field.ty(tcx, substs).skip_norm_wip(),
                        tcx,
                        data_types,
                        instance_context,
                    );
                    field_ty
                        .has_jvm_value()
                        .then(|| (enum_variant_field_name(variant, field_index, tcx), field_ty))
                })
                .collect();
            let transparent_payload = jvm_subtype_payload_ty(adt_def, variant, substs, tcx)
                .map(|payload_ty| ty_to_oomir_type(payload_ty, tcx, data_types, instance_context));
            let runtime_type = match transparent_payload.as_ref() {
                Some(oomir::Type::Class(class_name)) | Some(oomir::Type::Interface(class_name)) => {
                    class_name.clone()
                }
                Some(other) => tcx.dcx().span_fatal(
                    tcx.def_span(variant.def_id),
                    format!(
                        "`#[jvm_codegen::subtype]` payload has unsupported JVM representation {other:?}"
                    ),
                ),
                None => format!("{base_enum_name}${variant_name}"),
            };
            oomir::EnumVariantShape {
                runtime_type,
                fields,
                transparent: transparent_payload.is_some(),
            }
        })
        .collect();

    for shape in variants_info.iter().filter(|shape| shape.transparent) {
        let Some(oomir::DataType::Interface { interfaces, .. }) =
            data_types.get_mut(&shape.runtime_type)
        else {
            tcx.dcx().fatal(format!(
                "`#[jvm_codegen::subtype]` payload {} was not lowered as an enum interface",
                shape.runtime_type
            ));
        };
        if !interfaces
            .iter()
            .any(|interface| interface == base_enum_name)
        {
            interfaces.push(base_enum_name.to_string());
        }
    }

    if let Some(oomir::DataType::Interface { methods, .. }) = data_types.get_mut(base_enum_name) {
        add_enum_helper_methods(
            methods,
            base_enum_name,
            variants_info.clone(),
            tcx.is_lang_item(
                adt_def.did(),
                rustc_hir::attrs::lang_items::LangItem::Option,
            ),
        );
        methods
            .entry(enum_scoped_method_name(
                base_enum_name,
                ENUM_DROP_FIELDS_METHOD,
            ))
            .or_insert(DataTypeMethod::Abstract(oomir::Signature {
                params: vec![],
                ret: Box::new(oomir::Type::Void),
                is_static: false,
            }));
        if has_numeric_discriminant {
            methods
                .entry(ENUM_UNION_DISCRIMINANT_METHOD.to_string())
                .or_insert(DataTypeMethod::AdtHelperMethod {
                    kind: oomir::AdtHelperKind::EnumDiscriminant {
                        enum_class: base_enum_name.to_string(),
                        variants: variants_info.clone(),
                        values: adt_def
                            .discriminants(tcx)
                            .map(|(_, discriminant)| discriminant.val as i64)
                            .collect(),
                    },
                });
        }
        if let Some(factory) = union_factory.clone() {
            methods
                .entry(ENUM_FROM_UNION_DISCRIMINANT_METHOD.to_string())
                .or_insert(DataTypeMethod::Function(factory));
        }
    }

    for (_variant_idx, variant) in adt_def.variants().iter().enumerate() {
        if let Some(payload_ty) = jvm_subtype_payload_ty(adt_def, variant, substs, tcx) {
            let receiver_class = ty_to_oomir_type(payload_ty, tcx, data_types, instance_context)
                .get_class_name()
                .expect("transparent enum payload is an enum interface")
                .to_string();
            let drop_function = enum_transparent_variant_drop_glue_function(
                payload_ty,
                base_enum_name,
                &receiver_class,
                tcx,
                data_types,
                instance_context,
            );
            let Some(oomir::DataType::Interface { methods, .. }) =
                data_types.get_mut(&receiver_class)
            else {
                tcx.dcx().fatal(format!(
                    "transparent enum payload {receiver_class} is not an interface"
                ));
            };
            methods.insert(
                enum_scoped_method_name(base_enum_name, ENUM_DROP_FIELDS_METHOD),
                DataTypeMethod::Function(drop_function),
            );
            continue;
        }
        let variant_class_name = format!(
            "{}${}",
            base_enum_name,
            jvm_names::member_name(&variant.name.to_string())
        );
        if !created_placeholders.contains(&variant_class_name) {
            if let Some(oomir::DataType::Class { .. }) = data_types.get_mut(&variant_class_name) {
                continue;
            }
        }

        let fields: Vec<_> = variant
            .fields
            .iter()
            .enumerate()
            .filter_map(|(field_index, field)| {
                let field_ty = ty_to_oomir_type(
                    field.ty(tcx, substs).skip_norm_wip(),
                    tcx,
                    data_types,
                    instance_context,
                );
                field_ty
                    .has_jvm_value()
                    .then(|| (enum_variant_field_name(variant, field_index, tcx), field_ty))
            })
            .collect();
        let mut methods = HashMap::default();
        methods.insert(
            enum_scoped_method_name(base_enum_name, ENUM_DROP_FIELDS_METHOD),
            DataTypeMethod::Function(enum_variant_drop_glue_function(
                variant,
                substs,
                base_enum_name,
                &variant_class_name,
                tcx,
                data_types,
                instance_context,
            )),
        );
        for (component_index, (field_name, field_ty)) in fields.iter().enumerate() {
            methods.insert(
                format!("component{}", component_index + 1),
                DataTypeMethod::AdtHelperMethod {
                    kind: oomir::AdtHelperKind::Component {
                        field_name: field_name.clone(),
                        field_ty: field_ty.clone(),
                    },
                },
            );
        }

        let Some(oomir::DataType::Class {
            fields: existing_fields,
            is_abstract,
            methods: existing_methods,
            super_class,
            interfaces,
            ..
        }) = data_types.get_mut(&variant_class_name)
        else {
            continue;
        };
        *existing_fields = fields;
        *is_abstract = false;
        *super_class = None;
        if !interfaces
            .iter()
            .any(|interface| interface == base_enum_name)
        {
            interfaces.push(base_enum_name.to_string());
        }
        // Resolving a variant's fields can recursively request this enum's
        // memory codec. Preserve any codec methods installed on the placeholder
        // while completing the concrete variant definition.
        existing_methods.extend(methods);
    }

    data_types.enums_in_progress.remove(base_enum_name);
}

pub(crate) fn adapt_simple_enum_operand(
    source: oomir::Operand,
    target_ty: &oomir::Type,
    temp_prefix: &str,
    data_types: &HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let oomir::Type::Class(enum_class) = target_ty else {
        return source;
    };
    if source
        .get_type()
        .is_some_and(|source_ty| source_ty.is_jvm_reference_type())
    {
        return source;
    }
    let has_factory = matches!(
        data_types.get(enum_class),
        Some(oomir::DataType::Interface { methods, .. })
            if methods.contains_key(ENUM_FROM_UNION_DISCRIMINANT_METHOD)
    );
    if !has_factory {
        return source;
    }

    let bits_dest = format!("{}_enum_discriminant", temp_prefix);
    instructions.push(oomir::Instruction::Cast {
        op: source,
        ty: oomir::Type::I64,
        dest: bits_dest.clone(),
    });
    let enum_dest = format!("{}_enum_value", temp_prefix);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(enum_dest.clone()),
        class_name: enum_class.clone(),
        method_name: ENUM_FROM_UNION_DISCRIMINANT_METHOD.to_string(),
        method_ty: oomir::Signature {
            params: vec![("discriminant".to_string(), oomir::Type::I64)],
            ret: Box::new(target_ty.clone()),
            is_static: true,
        },
        args: vec![operand_var(bits_dest, oomir::Type::I64)],
    });
    operand_var(enum_dest, target_ty.clone())
}
