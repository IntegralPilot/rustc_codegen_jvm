use super::{self as jvm, ClassFile, Constant, ConstantPool, ReferenceKind};
use rustc_hash::FxHashMap as HashMap;
use std::ops::Deref;

use super::key::ConstantKey;

#[derive(Clone, Debug)]
pub struct InternedConstantPool {
    pool: ConstantPool<'static>,
    constants: HashMap<ConstantKey, u16>,
    strings: HashMap<jvm::JavaString, u16>,
}

impl Default for InternedConstantPool {
    fn default() -> Self {
        Self {
            pool: ConstantPool::default(),
            constants: HashMap::default(),
            strings: HashMap::default(),
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
    pub fn into_inner(self) -> ConstantPool<'static> {
        self.pool
    }

    pub fn add(&mut self, constant: Constant<'static>) -> jvm::Result<u16> {
        if let Constant::Utf8(value) = constant {
            return self.intern_utf8(value);
        }
        let key = ConstantKey::from(&constant);
        if let Some(index) = self.constants.get(&key) {
            return Ok(*index);
        }
        let index = self.pool.add(constant)?;
        self.constants.insert(key, index);
        Ok(index)
    }

    pub fn add_utf8<S: AsRef<str>>(&mut self, value: S) -> jvm::Result<u16> {
        self.intern_utf8(jvm::JavaStr::cow_from_str(value.as_ref()))
    }

    fn intern_utf8(&mut self, value: std::borrow::Cow<'_, jvm::JavaStr>) -> jvm::Result<u16> {
        if let Some(&index) = self.strings.get(value.as_ref()) {
            return Ok(index);
        }
        let value = value.into_owned();
        let index = self.pool.add(Constant::Utf8(value.clone().into()))?;
        self.strings.insert(value, index);
        Ok(index)
    }

    pub fn add_integer(&mut self, value: i32) -> jvm::Result<u16> {
        self.add(Constant::Integer(value))
    }

    pub fn add_float(&mut self, value: f32) -> jvm::Result<u16> {
        self.add(Constant::Float(value))
    }

    pub fn add_long(&mut self, value: i64) -> jvm::Result<u16> {
        self.add(Constant::Long(value))
    }

    pub fn add_double(&mut self, value: f64) -> jvm::Result<u16> {
        self.add(Constant::Double(value))
    }

    pub fn add_class<S: AsRef<str>>(&mut self, name: S) -> jvm::Result<u16> {
        let name_index = self.add_utf8(name)?;
        self.add(Constant::Class(name_index))
    }

    pub fn add_string<S: AsRef<str>>(&mut self, value: S) -> jvm::Result<u16> {
        let string_index = self.add_utf8(value)?;
        self.add(Constant::String(string_index))
    }

    pub fn add_field_ref<N: AsRef<str>, D: AsRef<str>>(
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

    pub fn add_method_ref<N: AsRef<str>, D: AsRef<str>>(
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

    pub fn add_interface_method_ref<N: AsRef<str>, D: AsRef<str>>(
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

    pub fn add_name_and_type<N: AsRef<str>, D: AsRef<str>>(
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

    pub fn add_method_handle(
        &mut self,
        reference_kind: ReferenceKind,
        reference_index: u16,
    ) -> jvm::Result<u16> {
        self.add(Constant::MethodHandle {
            reference_kind,
            reference_index,
        })
    }

    pub fn add_method_type<S: AsRef<str>>(&mut self, descriptor: S) -> jvm::Result<u16> {
        let descriptor_index = self.add_utf8(descriptor)?;
        self.add(Constant::MethodType(descriptor_index))
    }

    #[allow(dead_code)]
    pub fn add_dynamic<N: AsRef<str>, D: AsRef<str>>(
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

    pub fn add_invoke_dynamic<N: AsRef<str>, D: AsRef<str>>(
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
    pub fn add_module<S: AsRef<str>>(&mut self, name: S) -> jvm::Result<u16> {
        let name_index = self.add_utf8(name)?;
        self.add(Constant::Module(name_index))
    }

    #[allow(dead_code)]
    pub fn add_package<S: AsRef<str>>(&mut self, name: S) -> jvm::Result<u16> {
        let name_index = self.add_utf8(name)?;
        self.add(Constant::Package(name_index))
    }
}

// Every generated class obtains its constants through InternedConstantPool,
// which enforces this invariant as entries are added. Keep the full scan in
// development builds as an assertion over that implementation, but do not
// rebuild and hash the entire pool immediately before every production
// serialization.
#[cfg(not(debug_assertions))]
#[inline]
pub fn verify_no_duplicate_constants(_class_file: &ClassFile<'_>) -> jvm::Result<()> {
    Ok(())
}

#[cfg(debug_assertions)]
pub fn verify_no_duplicate_constants(class_file: &ClassFile<'_>) -> jvm::Result<()> {
    let mut seen = HashMap::<ConstantKey, u16>::default();
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn borrowed_strings_share_indexes_with_raw_modified_utf8() {
        let mut pool = InternedConstantPool::default();
        for text in ["Code", "méthode", "null\0byte", "crab🦀"] {
            let index = pool.add_utf8(text).unwrap();
            assert_eq!(pool.add_utf8(text).unwrap(), index);
            assert_eq!(
                pool.add(Constant::Utf8(jvm::JavaString::from(text).into()))
                    .unwrap(),
                index
            );
            assert_eq!(pool.try_get_utf8(index).unwrap().to_rust_string(), text);
        }
        // An unpaired Java surrogate has no lossless Rust String equivalent.
        let raw = jvm::JavaStr::from_mutf8(&[0xed, 0xa0, 0x80]).unwrap();
        let index = pool.add(Constant::Utf8(raw.to_owned().into())).unwrap();
        assert_eq!(
            pool.add(Constant::Utf8(raw.to_owned().into())).unwrap(),
            index
        );
        assert_ne!(pool.add_utf8("�").unwrap(), index);
        assert_eq!(pool.try_get_utf8(index).unwrap().as_bytes(), raw.as_bytes());
        let wide = pool.add_long(42).unwrap();
        assert_eq!(pool.add_long(42).unwrap(), wide);
        assert_eq!(pool.add_utf8("after wide").unwrap(), wide + 2);
    }
}
