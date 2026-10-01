//! Private enum storage shares proven shapes. Java exports, codecs, and custom methods keep nominal
//! representations.
use super::*;

fn reference(recipe: &mut Vec<u8>, name: &str) {
    recipe.extend_from_slice(format!("0:L{name};;").as_bytes());
}

fn fields(recipe: &mut Vec<u8>, fields: &[(String, Type)], module: &oomir::Module) -> Option<()> {
    recipe.extend(super::shapes::carrier_recipe(
        oomir::ClassKind::Value,
        fields,
        &HashMap::default(),
        &[],
        "java/lang/Object",
        false,
        |ty| super::enums::object_equality(module, ty),
    )?);
    Some(())
}

pub(super) fn variant(
    name: &str,
    kind: oomir::ClassKind,
    fields_: &[(String, Type)],
    methods: &HashMap<String, DataTypeMethod>,
    interfaces: &[String],
    superclass: &str,
    abstract_class: bool,
    module: &oomir::Module,
) -> Option<Vec<u8>> {
    if kind != oomir::ClassKind::Value
        || abstract_class
        || superclass != "java/lang/Object"
        || interfaces.len() != 1
        || methods.len() != 1
        || !matches!(
            module.data_type(&interfaces[0]),
            Some(oomir::DataType::Interface { is_enum: true, .. })
        )
    {
        return None;
    }
    let DataTypeMethod::SimpleConstantReturn(Type::I64, Some(oomir::Constant::I64(tag))) =
        methods.get(oomir::ENUM_TAG_METHOD)?
    else {
        return None;
    };
    let mut recipe = format!(
        "carrier-v2;variant-v1;{};{tag};",
        jvm::names::inner_name(name)
    )
    .into_bytes();
    reference(&mut recipe, &interfaces[0]);
    fields(&mut recipe, fields_, module)?;
    Some(recipe)
}

pub(super) fn interface(
    methods: &HashMap<String, DataTypeMethod>,
    interfaces: &[String],
    module: &oomir::Module,
    subclasses: &[String],
) -> Option<Vec<u8>> {
    if !interfaces.is_empty() || !methods.contains_key(oomir::ENUM_TAG_METHOD) {
        return None;
    }
    let mut recipe = b"carrier-v2;enum-v1;".to_vec();
    // Kotlin requires the empty Context variant and Poll nested-class names. Dead helper removal
    // must preserve them.
    for (empty, full) in [("None", "Some"), ("Pending", "Ready")] {
        if let Some(variant) = subclasses
            .iter()
            .find(|n| jvm::names::inner_name(n) == empty)
            && subclasses.iter().any(|n| jvm::names::inner_name(n) == full)
        {
            recipe.extend_from_slice(format!("nested={empty},{full};").as_bytes());
            reference(&mut recipe, variant);
        }
    }
    let mut methods = methods.iter().collect::<Vec<_>>();
    methods.sort_unstable_by_key(|(name, _)| *name);
    for (name, method) in methods {
        recipe.extend_from_slice(format!("method={name};").as_bytes());
        let (variants, compare_fields) = match method {
            DataTypeMethod::SimpleConstantReturn(Type::I64, None)
                if name == oomir::ENUM_TAG_METHOD =>
            {
                recipe.extend_from_slice(b"abstract-tag;");
                continue;
            }
            DataTypeMethod::AdtHelperMethod { kind } => match kind {
                AdtHelperKind::EnumVariantIndex {
                    enum_class,
                    variants,
                } => {
                    recipe.extend_from_slice(b"index;");
                    reference(&mut recipe, enum_class);
                    (variants, false)
                }
                AdtHelperKind::StaticPartialEqEnum {
                    enum_class,
                    variants,
                } => {
                    recipe.extend_from_slice(b"eq;");
                    reference(&mut recipe, enum_class);
                    (variants, true)
                }
                AdtHelperKind::EnumDiscriminant {
                    enum_class,
                    dispatch,
                    ..
                } if dispatch.as_deref() == Some(oomir::ENUM_TAG_METHOD) => {
                    // The helper calls the receiver tag method. Each variant recipe supplies its
                    // own discriminant.
                    recipe.extend_from_slice(b"dispatch-tag;");
                    reference(&mut recipe, enum_class);
                    continue;
                }
                AdtHelperKind::EnumIsVariant {
                    enum_class,
                    runtime_type,
                } => {
                    recipe.extend_from_slice(b"is;");
                    reference(&mut recipe, enum_class);
                    reference(&mut recipe, runtime_type);
                    continue;
                }
                _ => return None,
            },
            _ => return None,
        };
        for variant in variants {
            if variant.transparent {
                return None;
            }
            reference(&mut recipe, &variant.runtime_type);
            if compare_fields {
                fields(&mut recipe, &variant.fields, module)?;
            }
        }
    }
    Some(recipe)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn module() -> oomir::Module {
        oomir::Module {
            name: "test".into(),
            source_file: None,
            functions: HashMap::default(),
            data_types: HashMap::default(),
            suppressed_data_types: HashSet::default(),
            shared_data_types: None,
            shared_context: None,
            external_interfaces: HashSet::default(),
            statics: HashMap::default(),
        }
    }

    #[test]
    fn dispatched_discriminants_do_not_depend_on_variant_layouts_or_values() {
        let module = module();
        let recipe = |values| {
            interface(
                &HashMap::from_iter([
                    (
                        oomir::ENUM_TAG_METHOD.to_owned(),
                        DataTypeMethod::SimpleConstantReturn(Type::I64, None),
                    ),
                    (
                        "_unionDiscriminant".into(),
                        DataTypeMethod::AdtHelperMethod {
                            kind: AdtHelperKind::EnumDiscriminant {
                                enum_class: "test/Enum".into(),
                                variants: Vec::new(),
                                values,
                                dispatch: Some(oomir::ENUM_TAG_METHOD.into()),
                            },
                        },
                    ),
                ]),
                &[],
                &module,
                &[],
            )
            .unwrap()
        };
        assert_eq!(recipe(vec![0, 1]), recipe(vec![-19, 101, 999]));
        assert!(
            String::from_utf8(recipe(vec![]))
                .unwrap()
                .contains("dispatch-tag;")
        );
    }

    #[test]
    fn recipes_retain_tags_and_exclude_exports_and_custom_methods() {
        let mut module = module();
        module.data_types.insert(
            "test/Enum".into(),
            oomir::DataType::Interface {
                methods: HashMap::default(),
                interfaces: vec![],
                is_enum: true,
            },
        );
        let methods = |tag| {
            HashMap::from_iter([(
                oomir::ENUM_TAG_METHOD.to_owned(),
                DataTypeMethod::SimpleConstantReturn(Type::I64, Some(oomir::Constant::I64(tag))),
            )])
        };
        let recipe = |methods: &_, kind| {
            variant(
                "test/Enum$Value",
                kind,
                &[("value".into(), Type::I32)],
                methods,
                &["test/Enum".into()],
                "java/lang/Object",
                false,
                &module,
            )
        };
        let a = recipe(&methods(-19), oomir::ClassKind::Value).unwrap();
        let b = recipe(&methods(101), oomir::ClassKind::Value).unwrap();
        assert_ne!(a, b);
        assert!(recipe(&methods(-19), oomir::ClassKind::JavaValue).is_none());
        let mut custom = methods(-19);
        custom.insert(
            "_writeUnionStorage".into(),
            DataTypeMethod::SimpleConstantReturn(Type::Void, None),
        );
        assert!(recipe(&custom, oomir::ClassKind::Value).is_none());
        let mut interface_methods = HashMap::from_iter([(
            oomir::ENUM_TAG_METHOD.to_owned(),
            DataTypeMethod::SimpleConstantReturn(Type::I64, None),
        )]);
        let ordinary = interface(&interface_methods, &[], &module, &[]).unwrap();
        let option = interface(
            &interface_methods,
            &[],
            &module,
            &["test/Enum$None".into(), "test/Enum$Some".into()],
        )
        .unwrap();
        let other_payload = interface(
            &interface_methods,
            &[],
            &module,
            &["test/Enum$None".into(), "different/Payload$Some".into()],
        )
        .unwrap();
        let poll = interface(
            &interface_methods,
            &[],
            &module,
            &["test/Enum$Pending".into(), "test/Enum$Ready".into()],
        )
        .unwrap();
        assert_eq!(
            option, other_payload,
            "unused payloads must not constrain the protocol"
        );
        assert_ne!(ordinary, option);
        assert_ne!(option, poll);
        assert!(
            String::from_utf8(option)
                .unwrap()
                .contains("0:Ltest/Enum$None;;")
        );
        assert!(interface(&interface_methods, &[], &module, &[]).is_some());
        assert!(interface(&interface_methods, &["test/Parent".into()], &module, &[]).is_none());
        interface_methods.insert(
            "callback".into(),
            DataTypeMethod::SimpleConstantReturn(Type::Void, None),
        );
        assert!(interface(&interface_methods, &[], &module, &[]).is_none());
    }
}
