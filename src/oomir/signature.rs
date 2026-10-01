//! Source and JVM ABI signatures.
use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<(String, Type)>,
    pub ret: Box<Type>,
    pub is_static: bool,
}

/// Private method namespaces share an internal ABI. Java exports and imports keep their declared
/// descriptors.
pub fn component_method(owner: &str, name: &str) -> bool {
    owner.contains("/mono/Mono_")
        || name == "_fn_ptr_call"
        || name.starts_with("_fp$")
        || (name == "call"
            && (owner.starts_with("org/rustlang/runtime/FnPtr_")
                || owner.rsplit('/').next().is_some_and(|n| {
                    n.starts_with("FnPtrImpl_") || n.starts_with("ClosureFnPtrImpl_")
                })))
}

impl Signature {
    /// Whether the first OOMIR parameter is represented by the JVM's implicit
    /// receiver slot rather than appearing in the method descriptor.
    pub fn has_implicit_jvm_receiver(&self) -> bool {
        !self.is_static
            && self
                .params
                .first()
                .is_some_and(|(_, ty)| ty.is_jvm_reference_type())
    }

    pub fn explicit_jvm_params(&self) -> &[(String, Type)] {
        if self.has_implicit_jvm_receiver() {
            &self.params[1..]
        } else {
            &self.params
        }
    }

    fn write_jvm_params(&self, result: &mut String, params: &[(String, Type)]) {
        for (_param_name, param_type) in params {
            if param_type.has_jvm_value() {
                param_type.write_jvm_descriptor(result);
            }
        }
    }

    pub fn to_jvm_descriptor_with_explicit_params(&self) -> String {
        let mut result = String::new();
        result.push('(');
        self.write_jvm_params(&mut result, &self.params);
        result.push(')');
        self.ret.write_jvm_return_descriptor(&mut result);
        result
    }

    pub fn needs_component_abi(&self) -> bool {
        self.ret.component_shape().is_some()
            || self
                .explicit_jvm_params()
                .iter()
                .any(|(_, ty)| ty.component_shape().is_some())
    }

    pub fn component_signature(&self) -> Signature {
        let slots: usize = self
            .explicit_jvm_params()
            .iter()
            .map(|(_, ty)| match ty {
                Type::Slice(_) | Type::Str | Type::TaggedI64 => 4,
                Type::Pointer(_) => 3,
                Type::I64 | Type::U64 | Type::F64 => 2,
                Type::Void | Type::Unit => 0,
                _ => 1,
            })
            .sum();
        if slots + usize::from(self.ret.component_shape().is_some()) > 254
            || !self.needs_component_abi()
        {
            return self.clone();
        }
        let implicit = self.has_implicit_jvm_receiver();
        let mut params = Vec::new();
        for (index, (name, ty)) in self.params.iter().enumerate() {
            if let Some(parts) = ty.components().filter(|_| !(implicit && index == 0)) {
                for (index, part) in parts.into_iter().enumerate() {
                    params.push((format!("{name}${index}"), part));
                }
            } else {
                params.push((name.clone(), ty.clone()));
            }
        }

        let ret = if self.ret.component_shape().is_some() {
            params.push(("$return".into(), Type::Array(Box::new(Type::I64))));
            Box::new(self.ret.components().unwrap().next().unwrap())
        } else {
            self.ret.clone()
        };
        Signature {
            params,
            ret,
            is_static: self.is_static,
        }
    }

    pub fn fn_ptr_interface_name(&self) -> String {
        let descriptor = self
            .component_signature()
            .to_jvm_descriptor_with_explicit_params();
        fn token(ty: &Type) -> Option<&'static str> {
            Some(match ty {
                Type::Void | Type::Unit => "void",
                Type::Boolean => "boolean",
                Type::I8 | Type::U8 => "byte",
                Type::I16 => "short",
                Type::U16 | Type::Char => "char",
                Type::I32 | Type::U32 => "int",
                Type::I64 | Type::U64 => "long",
                Type::F16 => "binary16",
                Type::F32 => "float",
                Type::F64 => "double",
                _ => return None,
            })
        }
        if let Some(ret) = token(&self.ret)
            && let Some(params) = self
                .explicit_jvm_params()
                .iter()
                .filter(|(_, ty)| ty.has_jvm_value())
                .map(|(_, ty)| token(ty))
                .collect::<Option<Vec<_>>>()
        {
            let params = if params.is_empty() {
                "no_args".into()
            } else {
                params.join("_")
            };
            let name = format!("FnPtr_{params}_to_{ret}");
            if name.len() <= 80 {
                return name;
            }
        }
        format!("FnPtr_{}", crate::stable_hash::short_hash(&descriptor, 16))
    }

    pub fn fn_ptr_interface_method_signature(&self) -> Signature {
        Signature {
            params: self.params.clone(),
            ret: self.ret.clone(),
            is_static: false,
        }
    }

    /// Replaces all occurrences of `Type::Class(old_name)` with `Type::Class(new_name)`
    /// in the signature's parameters and return type.
    /// Returns a tuple (params_changed, return_changed) indicating whether any replacements were made.
    pub fn replace_class_in_signature(
        &mut self,
        old_class_name: &str,
        new_class_name: &str,
    ) -> (bool, bool) {
        let mut params_changed = false;
        let mut return_changed = false;

        // Replace in parameters
        for (_param_name, param_type) in self.params.iter_mut() {
            let result = param_type.replace_class(old_class_name, new_class_name);
            if result {
                params_changed = true;
            }
        }

        // Replace in return type (accessing the Type inside the Box)
        if self.ret.replace_class(old_class_name, new_class_name) {
            return_changed = true;
        }

        (params_changed, return_changed)
    }
}

// impl Display for Signature, to make it so we can get the signature as a string suitable for the JVM bytecode, i.e. (I)V etc.
impl Signature {
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        result.push('(');
        self.write_jvm_params(&mut result, self.explicit_jvm_params());
        result.push(')');
        self.ret.write_jvm_return_descriptor(&mut result);
        result
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (_param_name, param_ty) in self.explicit_jvm_params() {
            if param_ty.has_jvm_value() {
                write!(f, "{}", param_ty.to_jvm_descriptor())?;
            }
        }
        write!(f, "){}", self.ret.to_jvm_return_descriptor())
    }
}
