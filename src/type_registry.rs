//! Canonical ownership of generated class schemas and method contributions.
use super::*;

/// Crate-wide class contributions collected while ordinary function shards
/// continue through lower2. Method bodies move here instead of being cloned;
/// compatible fragments are merged and emitted exactly once after discovery.
#[derive(Default)]
pub(super) struct CanonicalDataTypeRegistry {
    variants: HashMap<String, Vec<oomir::DataType>>,
    external_interfaces: HashSet<String>,
}

impl CanonicalDataTypeRegistry {
    pub(super) fn collect(&mut self, module: &mut oomir::Module) {
        self.external_interfaces
            .extend(module.external_interfaces.iter().cloned());
        let relative_static_methods = Arc::make_mut(&mut module.relative_static_methods);
        for (name, data_type) in &mut module.data_types {
            let contribution = match data_type {
                oomir::DataType::Class {
                    is_abstract,
                    super_class,
                    fields,
                    methods,
                    interfaces,
                } => oomir::DataType::Class {
                    is_abstract: *is_abstract,
                    super_class: super_class.clone(),
                    fields: fields.clone(),
                    methods: std::mem::take(methods),
                    interfaces: interfaces.clone(),
                },
                oomir::DataType::Interface {
                    methods,
                    interfaces,
                    is_enum,
                } => oomir::DataType::Interface {
                    methods: std::mem::take(methods),
                    interfaces: interfaces.clone(),
                    is_enum: *is_enum,
                },
            };
            Self::record_relative_static_methods(name, &contribution, relative_static_methods);
            module.suppressed_data_types.insert(name.clone());

            let variants = self.variants.entry(name.clone()).or_default();
            let mut contribution = Some(contribution);
            for existing in variants.iter_mut() {
                if Self::try_merge(existing, contribution.as_mut().unwrap()) {
                    contribution = None;
                    break;
                }
            }
            if let Some(contribution) = contribution {
                variants.push(contribution);
            }
        }
    }

    pub(super) fn record_relative_static_methods(
        class_name: &str,
        data_type: &oomir::DataType,
        relative_static_methods: &mut HashSet<oomir::FunctionKey>,
    ) {
        let methods = match data_type {
            oomir::DataType::Class { methods, .. } | oomir::DataType::Interface { methods, .. } => {
                methods
            }
        };
        for (method_name, method) in methods {
            let Some(signature) = method.function_signature() else {
                continue;
            };
            if signature.is_static && signature.supports_relative_pointer_abi() {
                relative_static_methods.insert(oomir::FunctionKey::new(
                    class_name,
                    method_name,
                    signature,
                ));
            }
        }
    }

    /// Validate before moving anything: a rejected contribution is tried against
    /// the next variant, while an accepted body is never cloned.
    pub(super) fn try_merge(
        existing: &mut oomir::DataType,
        incoming: &mut oomir::DataType,
    ) -> bool {
        match (existing, incoming) {
            (
                oomir::DataType::Class {
                    is_abstract: existing_abstract,
                    super_class: existing_super,
                    fields: existing_fields,
                    methods: existing_methods,
                    interfaces: existing_interfaces,
                },
                oomir::DataType::Class {
                    is_abstract: incoming_abstract,
                    super_class: incoming_super,
                    fields: incoming_fields,
                    methods: incoming_methods,
                    interfaces: incoming_interfaces,
                },
            ) => {
                if existing_abstract != incoming_abstract
                    || existing_super != incoming_super
                    || incoming_fields.iter().any(|(name, ty)| {
                        existing_fields
                            .iter()
                            .find(|(existing_name, _)| existing_name == name)
                            .is_some_and(|(_, existing_ty)| existing_ty != ty)
                    })
                    || incoming_methods.iter().any(|(name, method)| {
                        existing_methods
                            .get(name)
                            .is_some_and(|existing| existing != method)
                    })
                {
                    return false;
                }
                for (name, ty) in incoming_fields.drain(..) {
                    if !existing_fields
                        .iter()
                        .any(|(existing_name, _)| existing_name == &name)
                    {
                        existing_fields.push((name, ty));
                    }
                }
                for (name, method) in incoming_methods.drain() {
                    existing_methods.entry(name).or_insert(method);
                }
                for interface in incoming_interfaces.drain(..) {
                    if !existing_interfaces.contains(&interface) {
                        existing_interfaces.push(interface);
                    }
                }
                true
            }
            (
                oomir::DataType::Interface {
                    methods: existing_methods,
                    interfaces: existing_interfaces,
                    is_enum: existing_is_enum,
                },
                oomir::DataType::Interface {
                    methods: incoming_methods,
                    interfaces: incoming_interfaces,
                    is_enum: incoming_is_enum,
                },
            ) => {
                if existing_is_enum != incoming_is_enum
                    || incoming_methods.iter().any(|(name, signature)| {
                        existing_methods
                            .get(name)
                            .is_some_and(|existing| existing != signature)
                    })
                {
                    return false;
                }
                for (name, signature) in incoming_methods.drain() {
                    existing_methods.entry(name).or_insert(signature);
                }
                for interface in incoming_interfaces.drain(..) {
                    if !existing_interfaces.contains(&interface) {
                        existing_interfaces.push(interface);
                    }
                }
                true
            }
            _ => false,
        }
    }

    pub(super) fn into_modules(
        self,
        module_name: &str,
        source_file: Option<String>,
    ) -> Vec<oomir::Module> {
        let Self {
            variants,
            external_interfaces,
        } = self;
        let shared_data_types = Arc::new(Self::shared_schemas(&variants));
        let mut buckets = Vec::<HashMap<String, oomir::DataType>>::new();
        let mut names = variants.into_iter().collect::<Vec<_>>();
        names.sort_unstable_by(|(left, _), (right, _)| left.cmp(right));
        for (name, variants) in names {
            let nest_root = name.split('$').next().unwrap_or(&name);
            let base_bucket = stable_hash::hash_value(&nest_root) as usize % MAX_CODEGEN_WORKERS;
            for (variant, data_type) in variants.into_iter().enumerate() {
                let bucket = base_bucket + variant * MAX_CODEGEN_WORKERS;
                if buckets.len() <= bucket {
                    buckets.resize_with(bucket + 1, HashMap::default);
                }
                buckets[bucket].insert(name.clone(), data_type);
            }
        }

        let modules = buckets
            .into_iter()
            .filter(|data_types| !data_types.is_empty())
            .map(|data_types| oomir::Module {
                name: module_name.to_string(),
                source_file: source_file.clone(),
                functions: HashMap::default(),
                data_types,
                suppressed_data_types: HashSet::default(),
                shared_data_types: Some(Arc::clone(&shared_data_types)),
                relative_static_methods: Arc::new(HashSet::default()),
                external_interfaces: external_interfaces.clone(),
                statics: HashMap::default(),
            })
            .collect();
        modules
    }

    pub(super) fn shared_schemas(
        variants: &HashMap<String, Vec<oomir::DataType>>,
    ) -> HashMap<String, oomir::DataType> {
        let mut schemas = HashMap::default();
        for (name, variants) in variants {
            let Some(first) = variants.first() else {
                continue;
            };
            let mut schema = Self::schema_for(first);
            for variant in variants.iter().skip(1) {
                let _ = Self::try_merge(&mut schema, &mut Self::schema_for(variant));
            }
            if variants
                .iter()
                .any(|variant| matches!(variant, oomir::DataType::Interface { .. }))
                && !matches!(schema, oomir::DataType::Interface { .. })
            {
                schema = variants
                    .iter()
                    .find(|variant| matches!(variant, oomir::DataType::Interface { .. }))
                    .map(Self::schema_for)
                    .expect("an interface variant was observed");
            }
            schemas.insert(name.clone(), schema);
        }
        schemas
    }

    pub(super) fn schema_for(data_type: &oomir::DataType) -> oomir::DataType {
        match data_type {
            oomir::DataType::Class {
                is_abstract,
                super_class,
                fields,
                methods,
                interfaces,
            } => oomir::DataType::Class {
                is_abstract: *is_abstract,
                super_class: super_class.clone(),
                fields: fields.clone(),
                methods: methods
                    .keys()
                    .map(|name| {
                        (
                            name.clone(),
                            oomir::DataTypeMethod::SimpleConstantReturn(oomir::Type::Void, None),
                        )
                    })
                    .collect(),
                interfaces: interfaces.clone(),
            },
            oomir::DataType::Interface {
                methods,
                interfaces,
                is_enum,
            } => oomir::DataType::Interface {
                methods: methods
                    .keys()
                    .map(|name| {
                        (
                            name.clone(),
                            oomir::DataTypeMethod::SimpleConstantReturn(oomir::Type::Void, None),
                        )
                    })
                    .collect(),
                interfaces: interfaces.clone(),
                is_enum: *is_enum,
            },
        }
    }
}

#[cfg(test)]
mod canonical_data_type_registry_tests {
    use super::*;

    #[test]
    pub(super) fn mixed_interface_schema_keeps_only_method_stubs() {
        let interface_method = oomir::DataTypeMethod::AdtHelperMethod {
            kind: oomir::AdtHelperKind::StaticPartialEqEnum {
                enum_class: "example/Mixed".to_string(),
                variants: Vec::new(),
            },
        };
        let variants = HashMap::from_iter([(
            "example/Mixed".to_string(),
            vec![
                oomir::DataType::Class {
                    is_abstract: false,
                    super_class: None,
                    fields: Vec::new(),
                    methods: HashMap::default(),
                    interfaces: Vec::new(),
                },
                oomir::DataType::Interface {
                    methods: HashMap::from_iter([("eq".to_string(), interface_method)]),
                    interfaces: Vec::new(),
                    is_enum: true,
                },
            ],
        )]);

        let schemas = CanonicalDataTypeRegistry::shared_schemas(&variants);
        let oomir::DataType::Interface {
            methods, is_enum, ..
        } = &schemas["example/Mixed"]
        else {
            panic!("the interface schema must take precedence");
        };
        assert!(*is_enum);
        assert_eq!(
            methods.get("eq"),
            Some(&oomir::DataTypeMethod::SimpleConstantReturn(
                oomir::Type::Void,
                None,
            ))
        );
    }
}
