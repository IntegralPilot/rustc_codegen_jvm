//! Source and JVM ABI signatures.
use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<(String, Type)>,
    pub ret: Box<Type>,
    pub is_static: bool,
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

    pub fn fn_ptr_interface_name(&self) -> String {
        let descriptor = self.to_jvm_descriptor_with_explicit_params();
        let params = self
            .explicit_jvm_params()
            .iter()
            .filter_map(|(_, ty)| ty.has_jvm_value().then(|| ty.jvm_abi_name_token()))
            .collect::<Vec<_>>();
        let params = if params.is_empty() {
            "no_args".to_string()
        } else {
            params.join("_")
        };
        let readable = format!("{params}_to_{}", self.ret.jvm_abi_name_token());
        crate::stable_hash::readable_or_hashed_name("FnPtr", &readable, &descriptor, 180)
    }

    pub fn fn_ptr_interface_method_signature(&self) -> Signature {
        Signature {
            params: self.params.clone(),
            ret: self.ret.clone(),
            is_static: false,
        }
    }

    /// Internal generated methods may carry a thin pointer as its stable base
    /// plus deferred element and byte offsets. Public/JVM-facing entry points
    /// retain the ordinary `Pointer` descriptor and bridge into this ABI.
    pub fn relative_pointer_abi_signature(&self) -> Signature {
        let implicit_receiver = self.has_implicit_jvm_receiver();
        let mut params = Vec::with_capacity(self.params.len() * 3);
        for (index, (name, ty)) in self.params.iter().enumerate() {
            params.push((name.clone(), ty.clone()));
            if !(implicit_receiver && index == 0) && matches!(ty, Type::Pointer(_)) {
                params.push((format!("{name}$element_offset"), Type::I64));
                params.push((format!("{name}$byte_offset"), Type::I64));
            }
        }
        Signature {
            params,
            ret: self.ret.clone(),
            is_static: self.is_static,
        }
    }

    pub fn supports_relative_pointer_abi(&self) -> bool {
        let implicit_receiver = self.has_implicit_jvm_receiver();
        let mut has_pointer = false;
        let slots = self
            .params
            .iter()
            .enumerate()
            .filter(|(index, (_, ty))| !(implicit_receiver && *index == 0) && ty.has_jvm_value())
            .map(|(_, (_, ty))| {
                if matches!(ty, Type::Pointer(_)) {
                    has_pointer = true;
                    5u16
                } else if matches!(ty, Type::I64 | Type::U64 | Type::F64) {
                    2
                } else {
                    1
                }
            })
            .sum::<u16>();
        // JVMS 4.3.3 limits a method descriptor to 255 parameter units;
        // instance methods also consume one unit for the receiver.
        has_pointer && slots + u16::from(!self.is_static) <= 255
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
