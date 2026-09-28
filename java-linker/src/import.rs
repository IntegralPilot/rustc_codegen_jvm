//! Import only constants reachable from retained methods and interfaces.
use crate::*;
use std::cell::RefCell;

pub(crate) struct ConstantImporter<'a>(RefCell<Importer<'a>>);
struct Importer<'a> {
    source: &'a ConstantPool<'static>,
    target: &'a mut ConstantPool<'static>,
    constants: &'a mut HashMap<ConstantKey, u16>,
    indexes: Vec<Option<u16>>,
    active: Vec<bool>,
    source_bootstrap: &'a [BootstrapMethod],
    target_bootstrap: &'a mut Vec<BootstrapMethod>,
    bootstrap_indexes: Vec<Option<u16>>,
}
impl<'a> ConstantImporter<'a> {
    pub(crate) fn new(
        source: &'a ClassFile<'static>,
        target: &'a mut ConstantPool<'static>,
        constants: &'a mut HashMap<ConstantKey, u16>,
        target_bootstrap: &'a mut Vec<BootstrapMethod>,
    ) -> Self {
        let source_bootstrap = source
            .attributes
            .iter()
            .find_map(|a| match a {
                Attribute::BootstrapMethods { methods, .. } => Some(methods.as_slice()),
                _ => None,
            })
            .unwrap_or_default();
        Self(RefCell::new(Importer {
            source: &source.constant_pool,
            target,
            constants,
            indexes: vec![None; source.constant_pool.len() + 1],
            active: vec![false; source.constant_pool.len() + 1],
            source_bootstrap,
            target_bootstrap,
            bootstrap_indexes: vec![None; source_bootstrap.len()],
        }))
    }
}
impl ConstantIndexes for ConstantImporter<'_> {
    fn remap(&self, index: u16) -> io::Result<u16> {
        self.0.borrow_mut().constant(index)
    }
}
impl Importer<'_> {
    fn bootstrap(&mut self, index: u16) -> io::Result<u16> {
        let old = usize::from(index);
        if let Some(index) = self.bootstrap_indexes.get(old).copied().flatten() {
            return Ok(index);
        }
        let mut method = self
            .source_bootstrap
            .get(old)
            .cloned()
            .ok_or_else(|| constant_pool_error("invalid bootstrap-method index", index))?;
        method.bootstrap_method_ref = self.constant(method.bootstrap_method_ref)?;
        for argument in &mut method.arguments {
            *argument = self.constant(*argument)?;
        }
        // Distinct source bootstrap entries retain their identities. In
        // particular, constant-dynamic resolution can return mutable objects.
        let index = u16::try_from(self.target_bootstrap.len())
            .map_err(|e| constant_pool_error("too many bootstrap methods", e))?;
        self.target_bootstrap.push(method);
        self.bootstrap_indexes[old] = Some(index);
        Ok(index)
    }
    fn constant(&mut self, index: u16) -> io::Result<u16> {
        if let Some(mapped) = self.indexes.get(usize::from(index)).copied().flatten() {
            return Ok(mapped);
        }
        let constant = self
            .source
            .try_get(index)
            .map_err(|e| constant_pool_error("invalid incoming constant-pool reference", e))?
            .clone()
            .into_owned();
        if std::mem::replace(&mut self.active[usize::from(index)], true) {
            return Err(constant_pool_error(
                "cyclic incoming constant-pool reference",
                index,
            ));
        }
        let imported = match constant {
            Constant::Class(i) => Constant::Class(self.constant(i)?),
            Constant::String(i) => Constant::String(self.constant(i)?),
            Constant::MethodType(i) => Constant::MethodType(self.constant(i)?),
            Constant::Module(i) => Constant::Module(self.constant(i)?),
            Constant::Package(i) => Constant::Package(self.constant(i)?),
            Constant::FieldRef {
                class_index,
                name_and_type_index,
            } => Constant::FieldRef {
                class_index: self.constant(class_index)?,
                name_and_type_index: self.constant(name_and_type_index)?,
            },
            Constant::MethodRef {
                class_index,
                name_and_type_index,
            } => Constant::MethodRef {
                class_index: self.constant(class_index)?,
                name_and_type_index: self.constant(name_and_type_index)?,
            },
            Constant::InterfaceMethodRef {
                class_index,
                name_and_type_index,
            } => Constant::InterfaceMethodRef {
                class_index: self.constant(class_index)?,
                name_and_type_index: self.constant(name_and_type_index)?,
            },
            Constant::NameAndType {
                name_index,
                descriptor_index,
            } => Constant::NameAndType {
                name_index: self.constant(name_index)?,
                descriptor_index: self.constant(descriptor_index)?,
            },
            Constant::MethodHandle {
                reference_kind,
                reference_index,
            } => Constant::MethodHandle {
                reference_kind,
                reference_index: self.constant(reference_index)?,
            },
            Constant::Dynamic {
                bootstrap_method_attr_index,
                name_and_type_index,
            } => Constant::Dynamic {
                bootstrap_method_attr_index: self.bootstrap(bootstrap_method_attr_index)?,
                name_and_type_index: self.constant(name_and_type_index)?,
            },
            Constant::InvokeDynamic {
                bootstrap_method_attr_index,
                name_and_type_index,
            } => Constant::InvokeDynamic {
                bootstrap_method_attr_index: self.bootstrap(bootstrap_method_attr_index)?,
                name_and_type_index: self.constant(name_and_type_index)?,
            },
            constant => constant,
        };
        let key = ConstantKey::from(&imported);
        let mapped = if let Some(&index) = self.constants.get(&key) {
            index
        } else {
            let index = self
                .target
                .add(imported)
                .map_err(|e| constant_pool_error("merged JVM constant pool is full", e))?;
            self.constants.insert(key, index);
            index
        };
        self.indexes[usize::from(index)] = Some(mapped);
        self.active[usize::from(index)] = false;
        Ok(mapped)
    }
}
