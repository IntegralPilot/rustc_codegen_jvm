//! One computational-body emitter for module, class and interface methods.
use super::*;
use std::borrow::Cow;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(in crate::lower2) enum BodyOwner {
    Module,
    Class,
    Interface,
}

pub(in crate::lower2) struct BodyEmitter<'a> {
    pub cp: &'a mut InternedConstantPool,
    pub bootstrap: &'a mut Vec<BootstrapMethod>,
    pub methods: &'a mut Vec<jvm::Method>,
    pub next_factory: &'a mut usize,
    pub owner: &'a str,
    pub kind: BodyOwner,
    pub relative_methods: &'a HashSet<oomir::FunctionKey>,
    pub debug: DebugInfoOptions,
    pub context: &'a oomir::construct::Context,
}

impl BodyEmitter<'_> {
    pub fn emit(&mut self, name: &str, function: &oomir::Function) -> jvm::Result<()> {
        let mut function = function.clone();
        function.name = name.to_owned();
        self.emit_owned(function)
    }

    pub fn emit_owned(&mut self, function: oomir::Function) -> jvm::Result<()> {
        let function = oomir::construct::seal(function, self.context).map_err(|message| {
            jvm::Error::VerificationError {
                context: "SSA construction".into(),
                message,
            }
        })?;
        crate::metrics::record_sealed_function(&function);
        self.emit_body(None, Cow::Owned(function), true)
    }

    fn emit_body(
        &mut self,
        name: Option<&str>,
        mut prepared: Cow<'_, oomir::SsaFunction>,
        allow_outline: bool,
    ) -> jvm::Result<()> {
        if allow_outline && oomir::outline::needed(&prepared) {
            let functions = oomir::outline::split(
                prepared.into_owned(),
                self.owner,
                self.kind == BodyOwner::Interface,
            )
            .map_err(|message| jvm::Error::VerificationError {
                context: "SSA method outlining".into(),
                message,
            })?;
            for function in functions {
                self.emit_body(None, Cow::Owned(function), false)?;
            }
            return Ok(());
        }
        if crate::lower2::constants::function_needs_constant_preparation(&prepared) {
            crate::lower2::constants::prepare_function_constants(
                prepared.to_mut(),
                self.cp,
                self.owner,
                self.methods,
                self.next_factory,
            )
            .map_err(|error| jvm::Error::VerificationError {
                context: format!(
                    "Constants for {}::{}",
                    self.owner,
                    name.unwrap_or(&prepared.name)
                ),
                message: format!(
                    "Failed after creating {} constant factories: {error:?}",
                    self.next_factory
                ),
            })?;
        }
        let function = &*prepared;
        let name = name.unwrap_or(&function.name);
        let module_owner = self.kind == BodyOwner::Module;
        let interface = self.kind == BodyOwner::Interface;
        let outlined = !interface && name.starts_with(oomir::outline::METHOD_PREFIX);
        let is_static = module_owner || function.signature.is_static;
        let signature = &function.signature;
        let relative_adapter = !module_owner
            && name.ends_with(oomir::RELATIVE_POINTER_METHOD_SUFFIX)
            && signature.supports_relative_pointer_abi();
        let relative_static = is_static
            && self
                .relative_methods
                .contains(&oomir::FunctionKey::new(self.owner, name, signature));
        let relative = relative_adapter || relative_static;
        let body = &function.body;
        let code =
            crate::lower2::select::compile(body, self.cp, self.bootstrap, self.debug, relative)
                .map_err(|error| jvm::Error::VerificationError {
                    context: format!("Function {}::{name}", self.owner),
                    message: format!("Failed to translate function: {error:?}"),
                })?;
        crate::metrics::record_selection_method(body, &code, || format!("{}::{name}", self.owner));
        let emitted_signature = if relative {
            Cow::Owned(signature.relative_pointer_abi_signature())
        } else {
            Cow::Borrowed(signature)
        };
        let emitted_name = if relative_static {
            Cow::Owned(format!("{name}{}", oomir::RELATIVE_POINTER_METHOD_SUFFIX))
        } else {
            Cow::Borrowed(name)
        };
        let mut attributes = vec![Attribute::Code {
            name_index: self.cp.add_utf8("Code")?,
            max_stack: code.max_stack,
            max_locals: code.max_locals,
            code: code.instructions,
            exception_table: code.exceptions,
            attributes: code.attributes,
        }];
        if !outlined && (interface || name != "<init>") {
            let mut parameters = Vec::new();
            for (name, ty) in emitted_signature.explicit_jvm_params() {
                if ty.has_jvm_value() {
                    parameters.push(jvm::attributes::MethodParameter {
                        name_index: self.cp.add_utf8(name)?,
                        access_flags: MethodAccessFlags::empty(),
                    });
                }
            }
            attributes.push(Attribute::MethodParameters {
                name_index: self.cp.add_utf8("MethodParameters")?,
                parameters,
            });
        }
        let instance_bridge = !module_owner
            && !signature.is_static
            && !signature.params.is_empty()
            && !relative_adapter;
        // Enum-interface static calls select the nominal implementation. These
        // remaining generated computational bodies still need that static entry.
        let static_code = (interface && instance_bridge).then(|| attributes[0].clone());
        let access_flags = if outlined {
            MethodAccessFlags::PRIVATE | MethodAccessFlags::STATIC | MethodAccessFlags::SYNTHETIC
        } else {
            MethodAccessFlags::PUBLIC
                | if is_static && name != "<init>" {
                    MethodAccessFlags::STATIC
                } else {
                    MethodAccessFlags::empty()
                }
        };
        self.methods.push(jvm::Method {
            access_flags,
            name_index: self.cp.add_utf8(emitted_name.as_ref())?,
            descriptor_index: self.cp.add_utf8(emitted_signature.to_string())?,
            attributes,
        });
        if relative_static {
            self.methods.push(create_relative_pointer_bridge(
                self.cp,
                self.owner,
                name,
                signature,
                access_flags,
                interface,
            )?);
        }
        if let Some(code) = static_code {
            let mut signature = signature.clone();
            signature.is_static = true;
            signature.params[0].1 = Type::Class(self.owner.to_owned());
            self.methods.push(jvm::Method {
                access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
                name_index: self.cp.add_utf8(name)?,
                descriptor_index: self.cp.add_utf8(signature.to_string())?,
                attributes: vec![code],
            });
        } else if instance_bridge {
            self.methods.push(create_static_instance_bridge(
                self.cp, self.owner, name, signature, false,
            )?);
        }
        Ok(())
    }
}
