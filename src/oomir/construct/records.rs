//! Scalar parameters for small internal value records.
use super::*;

pub(crate) const RECORD_ENTRY: &str = "$scalars";

impl Context {
    pub(crate) fn scalar_fields(&self, ty: &oomir::Type) -> Option<&[(String, oomir::Type)]> {
        let oomir::Type::Class(owner) = ty else {
            return None;
        };
        let layout = self.fields.get(owner)?;
        layout.scalar_record.then_some(layout.members.as_slice())
    }

    pub(crate) fn record_signature(
        &self,
        owner: &str,
        name: &str,
        signature: &oomir::Signature,
    ) -> Option<oomir::Signature> {
        if !signature.is_static || !owner.contains("/mono/Mono_") || name.ends_with(RECORD_ENTRY) {
            return None;
        }
        if !signature
            .params
            .iter()
            .any(|(_, ty)| self.scalar_fields(ty).is_some())
        {
            return None;
        }
        let mut params = Vec::new();
        for (name, ty) in &signature.params {
            if let Some(fields) = self.scalar_fields(ty) {
                params.extend(
                    fields
                        .iter()
                        .map(|(field, ty)| (format!("{name}${field}"), ty.clone())),
                );
            } else {
                params.push((name.clone(), ty.clone()));
            }
        }
        let result = oomir::Signature {
            params,
            ..signature.clone()
        };
        let components = result.component_signature();
        if result.needs_component_abi() && components == result {
            return None;
        }
        let slots: usize = components
            .params
            .iter()
            .map(|(_, ty)| match ty {
                oomir::Type::Void | oomir::Type::Unit => 0,
                oomir::Type::I64 | oomir::Type::U64 | oomir::Type::F64 => 2,
                _ => 1,
            })
            .sum();
        (slots <= 254).then_some(result)
    }
}

#[cfg(test)]
#[test]
fn scalar_record_entries_keep_external_signatures_and_component_slot_limits() {
    use oomir::{Signature, Type};
    let mut context = super::tests::empty_context();
    context.fields.insert(
        "Record".into(),
        super::context::FieldLayout {
            members: (0..4).map(|i| (format!("field{i}"), Type::F64)).collect(),
            direct: true,
            split_borrows: false,
            scalar_record: true,
        },
    );
    for count in [31, 32] {
        let mut signature = Signature {
            params: (0..count)
                .map(|i| (format!("arg{i}"), Type::Class("Record".into())))
                .collect(),
            ret: Box::new(Type::Slice(Box::new(Type::F64))),
            is_static: true,
        };
        signature
            .params
            .push(("slice".into(), Type::Slice(Box::new(Type::F64))));
        assert_eq!(
            context
                .record_signature("test/mono/Mono_1", "read", &signature)
                .is_some(),
            count == 31
        );
        assert!(
            context
                .record_signature("java/Api", "read", &signature)
                .is_none()
        );
    }
}

#[cfg(test)]
#[test]
fn private_record_abi_does_not_depend_on_trait_interfaces() {
    use oomir::{ClassKind, DataType, Type};
    for (kind, interfaces, expected) in [
        (ClassKind::Value, vec![], true),
        (ClassKind::Value, vec!["test/Trait".into()], true),
        (ClassKind::JavaValue, vec![], false),
        (ClassKind::MemoryView, vec![], false),
    ] {
        let mut module = super::tests::empty_module();
        module.data_types.insert(
            "Record".into(),
            DataType::Class {
                kind,
                is_abstract: false,
                super_class: None,
                fields: vec![("a".into(), Type::F64), ("b".into(), Type::F64)],
                methods: HashMap::default(),
                interfaces,
            },
        );
        let context = Context::new(&module);
        assert_eq!(
            context
                .scalar_fields(&Type::Class("Record".into()))
                .is_some(),
            expected
        );
    }
}
