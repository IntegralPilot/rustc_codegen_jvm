//! Private storage classes share complete layout recipes. Java exports, callbacks, and custom
//! methods retain their identity.
use super::enums::ObjectEquality;
use super::*;

pub(super) fn carrier_recipe(
    kind: oomir::ClassKind,
    fields: &[(String, Type)],
    methods: &HashMap<String, DataTypeMethod>,
    interfaces: &[String],
    superclass: &str,
    abstract_class: bool,
    equality: impl Fn(&Type) -> ObjectEquality,
) -> Option<Vec<u8>> {
    if kind != oomir::ClassKind::Value
        || abstract_class
        || !interfaces.is_empty()
        || superclass != "java/lang/Object"
        || methods.iter().any(|(name, method)| {
            name != "eq"
                || !matches!(method, DataTypeMethod::AdtHelperMethod {
                    kind: AdtHelperKind::PartialEqClass { fields: compared }
                } if compared.iter().filter(|(_, ty)| ty.has_jvm_value())
                    .eq(fields.iter().filter(|(_, ty)| ty.has_jvm_value())))
        })
    {
        return None;
    }
    let mut recipe = String::from("carrier-v2;");
    recipe.push_str(if methods.is_empty() { "plain;" } else { "eq;" });
    for (name, ty) in fields.iter().filter(|(_, ty)| ty.has_jvm_value()) {
        // Copy helpers dispatch through the actual value type. Equality also requires the nominal
        // helper and dispatch kind.
        match ty {
            ty if ty.is_jvm_primitive() => recipe.push_str("scalar;"),
            Type::Pointer(_) | Type::Slice(_) | Type::Str => recipe.push_str("borrow;"),
            Type::TaggedI64 => recipe.push_str("tagged-long;"),
            Type::Class(_) | Type::Interface(_) => {
                recipe.push_str(&format!("object:{:?};", equality(ty)));
            }
            Type::Array(inner) if matches!(inner.as_ref(), Type::Pointer(_)) => {
                recipe.push_str("address-array;");
            }
            Type::Array(_) => recipe.push_str("array;"),
            _ => return None,
        }
        recipe.push_str(&format!("{}:{name}{};", name.len(), ty.to_jvm_descriptor()));
        if let Type::Pointer(p) = ty
            && let Some(layout) = &p.layout
        {
            recipe.push_str(&format!("layout:{}:{:?};", layout.size, layout.codec));
        }
    }
    for (name, ty) in oomir::fields::physical(fields)
        .into_iter()
        .filter(|(_, ty)| ty.has_jvm_value())
    {
        recipe.push_str(&format!("{}:{name}{};", name.len(), ty.to_jvm_descriptor()));
    }
    Some(recipe.into_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn erased_fields_do_not_change_the_storage_or_equality_recipe() {
        let fields = vec![("field0".into(), Type::I32)];
        let mut with_erased = fields.clone();
        with_erased.push(("field1".into(), Type::Unit));
        let methods = HashMap::from_iter([(
            "eq".into(),
            DataTypeMethod::AdtHelperMethod {
                kind: AdtHelperKind::PartialEqClass {
                    fields: with_erased.clone(),
                },
            },
        )]);
        let recipe = |fields: &_, methods: &_| {
            carrier_recipe(
                oomir::ClassKind::Value,
                fields,
                methods,
                &[],
                "java/lang/Object",
                false,
                |_| ObjectEquality::Object,
            )
        };
        let with_eq = recipe(&fields, &methods);
        assert!(with_eq.is_some());
        assert_eq!(with_eq, recipe(&with_erased, &methods));
        assert_eq!(
            recipe(&fields, &HashMap::default()),
            recipe(&with_erased, &HashMap::default())
        );
        assert_ne!(with_eq, recipe(&fields, &HashMap::default()));
        let wrong_fields = vec![("different".into(), Type::I32)];
        assert!(recipe(&wrong_fields, &methods).is_none());
    }

    fn recipe(ty: Type) -> Option<Vec<u8>> {
        recipe_with_equality(ty, ObjectEquality::Object)
    }

    fn recipe_with_equality(ty: Type, equality: ObjectEquality) -> Option<Vec<u8>> {
        let fields = vec![("value".into(), ty)];
        let methods = HashMap::from_iter([(
            "eq".into(),
            DataTypeMethod::AdtHelperMethod {
                kind: AdtHelperKind::PartialEqClass {
                    fields: fields.clone(),
                },
            },
        )]);
        carrier_recipe(
            oomir::ClassKind::Value,
            &fields,
            &methods,
            &[],
            "java/lang/Object",
            false,
            |_| equality,
        )
    }

    #[test]
    fn physical_shape_sharing_keeps_address_layout_and_nominal_methods_separate() {
        let exact = |codec: &str| {
            Type::pointer(Type::Array(Box::new(Type::U8)))
                .with_address_layout(8, Some(codec.into()))
        };
        assert_ne!(recipe(exact("a#b#[B#8")), recipe(exact("a#c#[B#8")));
        assert_eq!(
            recipe(Type::pointer(Type::I32)),
            recipe(Type::pointer(Type::U32))
        );
        assert_ne!(
            recipe(Type::pointer(Type::I32)),
            recipe(Type::pointer(Type::I64))
        );
        assert_eq!(
            recipe(Type::Slice(Box::new(Type::I32))),
            recipe(Type::Slice(Box::new(Type::I64)))
        );
        assert_ne!(recipe(Type::Str), recipe(Type::Slice(Box::new(Type::U8))));
        assert!(recipe(Type::Class("test/Payload".into())).is_some());
        assert_ne!(
            recipe(Type::Class("test/Payload".into())),
            recipe(Type::Class("test/Other".into()))
        );
        assert_eq!(
            recipe(Type::Array(Box::new(Type::I8))),
            recipe(Type::Array(Box::new(Type::U8)))
        );
        assert_ne!(
            recipe(Type::Array(Box::new(Type::I8))),
            recipe(Type::Array(Box::new(Type::I16)))
        );
        assert!(recipe(Type::TaggedI64).is_some());
        assert!(recipe(Type::Array(Box::new(Type::Class("test/Element".into())))).is_some());
        assert_ne!(
            recipe(Type::Array(Box::new(Type::pointer(Type::I32)))),
            recipe(Type::Array(Box::new(Type::Class(
                oomir::POINTER_CLASS.into()
            )))),
            "pointer arrays compare addresses, ordinary object arrays compare identity"
        );
        for left in [
            ObjectEquality::Object,
            ObjectEquality::Class,
            ObjectEquality::Interface,
            ObjectEquality::Enum,
        ] {
            for right in [
                ObjectEquality::Object,
                ObjectEquality::Class,
                ObjectEquality::Interface,
                ObjectEquality::Enum,
            ] {
                let payload = Type::Class("test/Payload".into());
                assert_eq!(
                    recipe_with_equality(payload.clone(), left)
                        == recipe_with_equality(payload, right),
                    left == right
                );
            }
        }
        let methods = HashMap::from_iter([(
            "rustDrop".into(),
            DataTypeMethod::SimpleConstantReturn(Type::Void, None),
        )]);
        assert!(
            carrier_recipe(
                oomir::ClassKind::Value,
                &[],
                &methods,
                &[],
                "java/lang/Object",
                false,
                |_| ObjectEquality::Object
            )
            .is_none()
        );
        assert!(
            carrier_recipe(
                oomir::ClassKind::JavaValue,
                &[],
                &HashMap::default(),
                &[],
                "java/lang/Object",
                false,
                |_| ObjectEquality::Object
            )
            .is_none()
        );
        assert!(
            carrier_recipe(
                oomir::ClassKind::Value,
                &[],
                &HashMap::default(),
                &["test/Trait".into()],
                "java/lang/Object",
                false,
                |_| ObjectEquality::Object
            )
            .is_none()
        );
    }
}
