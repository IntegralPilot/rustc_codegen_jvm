use super::jvm::{self, ClassFile, Constant, ConstantPool, ReferenceKind};
use std::{collections::HashMap, ops::Deref};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum ConstantKey {
    Utf8(String),
    Integer(i32),
    Float(u32),
    Long(i64),
    Double(u64),
    Class(u16),
    String(u16),
    FieldRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    MethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    InterfaceMethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    NameAndType {
        name_index: u16,
        descriptor_index: u16,
    },
    MethodHandle {
        reference_kind: u8,
        reference_index: u16,
    },
    MethodType(u16),
    Dynamic {
        bootstrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
    InvokeDynamic {
        bootstrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
    Module(u16),
    Package(u16),
}

impl From<&Constant<'_>> for ConstantKey {
    fn from(constant: &Constant<'_>) -> Self {
        match constant {
            Constant::Utf8(value) => ConstantKey::Utf8(value.to_string()),
            Constant::Integer(value) => ConstantKey::Integer(*value),
            Constant::Float(value) => ConstantKey::Float(value.to_bits()),
            Constant::Long(value) => ConstantKey::Long(*value),
            Constant::Double(value) => ConstantKey::Double(value.to_bits()),
            Constant::Class(name_index) => ConstantKey::Class(*name_index),
            Constant::String(string_index) => ConstantKey::String(*string_index),
            Constant::FieldRef {
                class_index,
                name_and_type_index,
            } => ConstantKey::FieldRef {
                class_index: *class_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::MethodRef {
                class_index,
                name_and_type_index,
            } => ConstantKey::MethodRef {
                class_index: *class_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::InterfaceMethodRef {
                class_index,
                name_and_type_index,
            } => ConstantKey::InterfaceMethodRef {
                class_index: *class_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::NameAndType {
                name_index,
                descriptor_index,
            } => ConstantKey::NameAndType {
                name_index: *name_index,
                descriptor_index: *descriptor_index,
            },
            Constant::MethodHandle {
                reference_kind,
                reference_index,
            } => ConstantKey::MethodHandle {
                reference_kind: reference_kind.kind(),
                reference_index: *reference_index,
            },
            Constant::MethodType(descriptor_index) => ConstantKey::MethodType(*descriptor_index),
            Constant::Dynamic {
                bootstrap_method_attr_index,
                name_and_type_index,
            } => ConstantKey::Dynamic {
                bootstrap_method_attr_index: *bootstrap_method_attr_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::InvokeDynamic {
                bootstrap_method_attr_index,
                name_and_type_index,
            } => ConstantKey::InvokeDynamic {
                bootstrap_method_attr_index: *bootstrap_method_attr_index,
                name_and_type_index: *name_and_type_index,
            },
            Constant::Module(name_index) => ConstantKey::Module(*name_index),
            Constant::Package(name_index) => ConstantKey::Package(*name_index),
        }
    }
}

#[derive(Clone, Debug)]
pub(super) struct InternedConstantPool {
    pool: ConstantPool<'static>,
    constants: HashMap<ConstantKey, u16>,
}

impl Default for InternedConstantPool {
    fn default() -> Self {
        Self {
            pool: ConstantPool::default(),
            constants: HashMap::new(),
        }
    }
}

impl Deref for InternedConstantPool {
    type Target = ConstantPool<'static>;

    fn deref(&self) -> &Self::Target {
        &self.pool
    }
}

impl InternedConstantPool {
    pub(super) fn into_inner(self) -> ConstantPool<'static> {
        self.pool
    }

    pub(super) fn add(&mut self, constant: Constant<'static>) -> jvm::Result<u16> {
        let key = ConstantKey::from(&constant);
        if let Some(index) = self.constants.get(&key) {
            return Ok(*index);
        }
        let index = self.pool.add(constant)?;
        self.constants.insert(key, index);
        Ok(index)
    }

    pub(super) fn add_utf8<S: AsRef<str>>(&mut self, value: S) -> jvm::Result<u16> {
        self.add(Constant::Utf8(
            jvm::JavaString::from(value.as_ref().to_string()).into(),
        ))
    }

    pub(super) fn add_integer(&mut self, value: i32) -> jvm::Result<u16> {
        self.add(Constant::Integer(value))
    }

    pub(super) fn add_float(&mut self, value: f32) -> jvm::Result<u16> {
        self.add(Constant::Float(value))
    }

    pub(super) fn add_long(&mut self, value: i64) -> jvm::Result<u16> {
        self.add(Constant::Long(value))
    }

    pub(super) fn add_double(&mut self, value: f64) -> jvm::Result<u16> {
        self.add(Constant::Double(value))
    }

    pub(super) fn add_class<S: AsRef<str>>(&mut self, name: S) -> jvm::Result<u16> {
        let name_index = self.add_utf8(name)?;
        self.add(Constant::Class(name_index))
    }

    pub(super) fn add_string<S: AsRef<str>>(&mut self, value: S) -> jvm::Result<u16> {
        let string_index = self.add_utf8(value)?;
        self.add(Constant::String(string_index))
    }

    pub(super) fn add_field_ref<N: AsRef<str>, D: AsRef<str>>(
        &mut self,
        class_index: u16,
        name: N,
        descriptor: D,
    ) -> jvm::Result<u16> {
        let name_and_type_index = self.add_name_and_type(name, descriptor)?;
        self.add(Constant::FieldRef {
            class_index,
            name_and_type_index,
        })
    }

    pub(super) fn add_method_ref<N: AsRef<str>, D: AsRef<str>>(
        &mut self,
        class_index: u16,
        name: N,
        descriptor: D,
    ) -> jvm::Result<u16> {
        let name_and_type_index = self.add_name_and_type(name, descriptor)?;
        self.add(Constant::MethodRef {
            class_index,
            name_and_type_index,
        })
    }

    pub(super) fn add_interface_method_ref<N: AsRef<str>, D: AsRef<str>>(
        &mut self,
        class_index: u16,
        name: N,
        descriptor: D,
    ) -> jvm::Result<u16> {
        let name_and_type_index = self.add_name_and_type(name, descriptor)?;
        self.add(Constant::InterfaceMethodRef {
            class_index,
            name_and_type_index,
        })
    }

    pub(super) fn add_name_and_type<N: AsRef<str>, D: AsRef<str>>(
        &mut self,
        name: N,
        descriptor: D,
    ) -> jvm::Result<u16> {
        let name_index = self.add_utf8(name)?;
        let descriptor_index = self.add_utf8(descriptor)?;
        self.add(Constant::NameAndType {
            name_index,
            descriptor_index,
        })
    }

    #[allow(dead_code)]
    pub(super) fn add_method_handle(
        &mut self,
        reference_kind: ReferenceKind,
        reference_index: u16,
    ) -> jvm::Result<u16> {
        self.add(Constant::MethodHandle {
            reference_kind,
            reference_index,
        })
    }

    #[allow(dead_code)]
    pub(super) fn add_method_type<S: AsRef<str>>(&mut self, descriptor: S) -> jvm::Result<u16> {
        let descriptor_index = self.add_utf8(descriptor)?;
        self.add(Constant::MethodType(descriptor_index))
    }

    #[allow(dead_code)]
    pub(super) fn add_dynamic<N: AsRef<str>, D: AsRef<str>>(
        &mut self,
        bootstrap_method_attr_index: u16,
        name: N,
        descriptor: D,
    ) -> jvm::Result<u16> {
        let name_and_type_index = self.add_name_and_type(name, descriptor)?;
        self.add(Constant::Dynamic {
            bootstrap_method_attr_index,
            name_and_type_index,
        })
    }

    #[allow(dead_code)]
    pub(super) fn add_invoke_dynamic<N: AsRef<str>, D: AsRef<str>>(
        &mut self,
        bootstrap_method_attr_index: u16,
        name: N,
        descriptor: D,
    ) -> jvm::Result<u16> {
        let name_and_type_index = self.add_name_and_type(name, descriptor)?;
        self.add(Constant::InvokeDynamic {
            bootstrap_method_attr_index,
            name_and_type_index,
        })
    }

    #[allow(dead_code)]
    pub(super) fn add_module<S: AsRef<str>>(&mut self, name: S) -> jvm::Result<u16> {
        let name_index = self.add_utf8(name)?;
        self.add(Constant::Module(name_index))
    }

    #[allow(dead_code)]
    pub(super) fn add_package<S: AsRef<str>>(&mut self, name: S) -> jvm::Result<u16> {
        let name_index = self.add_utf8(name)?;
        self.add(Constant::Package(name_index))
    }
}

pub(super) fn verify_no_duplicate_constants(class_file: &ClassFile<'_>) -> jvm::Result<()> {
    let mut seen = HashMap::<ConstantKey, u16>::new();
    for index in 1..=class_file.constant_pool.len() {
        let index = index as u16;
        let Ok(constant) = class_file.constant_pool.try_get(index) else {
            continue;
        };
        let key = ConstantKey::from(constant);
        if let Some(first_index) = seen.insert(key, index) {
            return Err(jvm::Error::VerificationError {
                context: format!("Class constant pool for #{}", class_file.this_class),
                message: format!(
                    "duplicate constant pool entry #{index}; first canonical entry is #{first_index}"
                ),
            });
        }
    }
    Ok(())
}
