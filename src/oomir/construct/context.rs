//! Representation recipes retain only zero-sized and single-value carriers.
use super::*;

pub(crate) struct Context {
    pub(super) zero_sized: HashSet<String>,
    pub(super) interfaces: HashSet<String>,
    pub(super) wrappers: HashMap<String, Vec<(String, oomir::Type)>>,
    pub(super) constructors: HashMap<String, Vec<oomir::Type>>,
    parents: HashMap<String, Vec<String>>,
}
impl Context {
    pub(crate) fn new(module: &oomir::Module) -> Self {
        fn zero(
            ty: &oomir::Type,
            module: &oomir::Module,
            memo: &mut HashMap<String, bool>,
        ) -> bool {
            if !ty.has_jvm_value() {
                return true;
            }
            let oomir::Type::Class(name) = ty else {
                return false;
            };
            if let Some(&value) = memo.get(name) {
                return value;
            }
            memo.insert(name.clone(), false);
            let result = matches!(module.data_type(name), Some(oomir::DataType::Class { fields, is_abstract: false, .. }) if fields.iter().all(|(_, ty)| zero(ty, module, memo)));
            memo.insert(name.clone(), result);
            result
        }
        let mut context = Self {
            zero_sized: HashSet::default(),
            interfaces: module.external_interfaces.clone(),
            wrappers: HashMap::default(),
            constructors: HashMap::default(),
            parents: HashMap::default(),
        };
        let mut memo = HashMap::default();
        for (name, data) in module
            .data_types
            .iter()
            .chain(module.shared_data_types.iter().flat_map(|data| data.iter()))
        {
            let (interfaces, superclass) = match data {
                oomir::DataType::Class {
                    interfaces,
                    super_class,
                    ..
                } => (interfaces, super_class.as_ref()),
                oomir::DataType::Interface { interfaces, .. } => (interfaces, None),
            };
            if !interfaces.is_empty() || superclass.is_some() {
                context.parents.insert(
                    name.clone(),
                    interfaces.iter().chain(superclass).cloned().collect(),
                );
            }
            match data {
                oomir::DataType::Interface { .. } => {
                    context.interfaces.insert(name.clone());
                }
                oomir::DataType::Class {
                    fields,
                    is_abstract: false,
                    ..
                } => {
                    context.constructors.insert(
                        name.clone(),
                        fields
                            .iter()
                            .filter(|(_, ty)| ty.has_jvm_value())
                            .map(|(_, ty)| ty.clone())
                            .collect(),
                    );
                    let count = fields
                        .iter()
                        .filter(|(_, ty)| !zero(ty, module, &mut memo))
                        .count();
                    if count <= 1 {
                        context.wrappers.insert(name.clone(), fields.clone());
                    }
                    if count == 0 {
                        context.zero_sized.insert(name.clone());
                    }
                }
                _ => {}
            }
        }
        context
    }
    pub(super) fn is_zero(&self, ty: &oomir::Type) -> bool {
        !ty.has_jvm_value()
            || matches!(ty, oomir::Type::Class(name) if self.zero_sized.contains(name))
    }
    /// A class implementing an interface retains its dispatch behavior even
    /// when it happens to contain only one value (for example an ABI adapter).
    pub(super) fn is_subtype(&self, from: &oomir::Type, to: &oomir::Type) -> bool {
        let (
            oomir::Type::Class(from) | oomir::Type::Interface(from),
            oomir::Type::Class(to) | oomir::Type::Interface(to),
        ) = (from, to)
        else {
            return false;
        };
        self.named_subtype(from, to, 0)
    }
    fn named_subtype(&self, from: &str, to: &str, depth: usize) -> bool {
        from == to
            || to == "java/lang/Object"
            || (depth < 64
                && self.parents.get(from).is_some_and(|parents| {
                    parents
                        .iter()
                        .any(|parent| self.named_subtype(parent, to, depth + 1))
                }))
    }
    pub(super) fn path(
        &self,
        mut from: oomir::Type,
        to: &oomir::Type,
    ) -> Option<Vec<(String, String, oomir::Type)>> {
        let mut path = Vec::new();
        for _ in 0..64 {
            if from.to_jvm_descriptor() == to.to_jvm_descriptor() {
                return Some(path);
            }
            let oomir::Type::Class(owner) = from else {
                return None;
            };
            let fields = self.wrappers.get(&owner)?;
            let (name, ty) = fields.iter().find(|(_, ty)| !self.is_zero(ty))?;
            path.push((owner, name.clone(), ty.clone()));
            from = ty.clone();
        }
        None
    }
}
