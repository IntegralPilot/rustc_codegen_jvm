//! Native JVM interfaces emission.
use super::*;

/// Creates a ClassFile (as bytes) for a given OOMIR DataType that's an interface
pub(in crate::lower2) fn create_data_type_classfile_for_interface(
    interface_name_jvm: &str,
    methods: HashMap<String, DataTypeMethod>,
    super_interfaces: &[String],
    module: &oomir::Module,
    subclasses: Vec<String>,
    nest_host: Option<String>,
    debug_info: DebugInfoOptions,
    relative_static_methods: &HashSet<oomir::FunctionKey>,
    context: &oomir::construct::Context,
) -> jvm::Result<Vec<u8>> {
    let source_files = methods
        .values()
        .filter_map(|method| match method {
            DataTypeMethod::Function(function) => function.source_file(),
            DataTypeMethod::Forwarder(forwarder) => forwarder.source_file.as_deref(),
            _ => None,
        })
        .collect::<std::collections::BTreeSet<_>>();
    let source_file_name = source_files.first().map(|file| (*file).to_string());
    let mut cp = InternedConstantPool::default();

    let this_class_index = cp.add_class(interface_name_jvm)?;

    // Interfaces always implicitly extend Object, and must specify it in the classfile
    let super_class_index = cp.add_class("java/lang/Object")?;

    let mut jvm_methods: Vec<jvm::Method> = Vec::new();
    let mut class_attributes = Vec::new();
    let mut bootstrap_methods: Vec<BootstrapMethod> = Vec::new();
    let mut next_factory = 0;
    for (method_name, method) in methods {
        let method_name = method_name.as_str();
        if let DataTypeMethod::Function(mut function) = method {
            function.name = method_name.to_owned();
            body::BodyEmitter {
                cp: &mut cp,
                bootstrap: &mut bootstrap_methods,
                methods: &mut jvm_methods,
                next_factory: &mut next_factory,
                owner: interface_name_jvm,
                kind: body::BodyOwner::Interface,
                relative_methods: relative_static_methods,
                debug: debug_info,
                context,
            }
            .emit_owned(function)?;
            continue;
        }
        match &method {
            DataTypeMethod::Abstract(signature) => {
                if signature.is_static {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Interface {interface_name_jvm}"),
                        message: format!(
                            "static interface method {method_name} cannot be abstract"
                        ),
                    });
                }
                // Abstract interface signatures contain only their explicit
                // JVM parameters. There is no OOMIR function body here, so no
                // synthetic receiver local needs to be stripped.
                let descriptor = signature.to_jvm_descriptor_with_explicit_params();
                jvm_methods.push(jvm::Method {
                    access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT,
                    name_index: cp.add_utf8(method_name)?,
                    descriptor_index: cp.add_utf8(&descriptor)?,
                    attributes: Vec::new(),
                });

                if method_name == "call"
                    && interface_name_jvm.starts_with("org/rustlang/runtime/FnPtr_")
                {
                    let mut explicit_signature = signature.clone();
                    explicit_signature.is_static = true;
                    if explicit_signature.supports_relative_pointer_abi() {
                        let relative_signature =
                            explicit_signature.relative_pointer_abi_signature();
                        let relative_descriptor =
                            relative_signature.to_jvm_descriptor_with_explicit_params();
                        let call_descriptor =
                            explicit_signature.to_jvm_descriptor_with_explicit_params();
                        let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
                        let materialize = cp.add_method_ref(
                            pointer_class,
                            "materializeRelative",
                            &format!("(L{};JJ)L{};", oomir::POINTER_CLASS, oomir::POINTER_CLASS),
                        )?;
                        let call_ref = cp.add_interface_method_ref(
                            this_class_index,
                            "call",
                            &call_descriptor,
                        )?;

                        let mut instructions = vec![Instruction::Aload_0];
                        let mut local = 1u16;

                        let mut call_slots = 1u16;
                        for (_, ty) in &explicit_signature.params {
                            if !ty.has_jvm_value() {
                                continue;
                            }
                            if matches!(ty, Type::Pointer(_)) {
                                instructions.push(get_load_instruction(ty, local)?);
                                instructions.push(get_load_instruction(&Type::I64, local + 1)?);
                                instructions.push(get_load_instruction(&Type::I64, local + 3)?);

                                instructions.push(Instruction::Invokestatic(materialize));
                                local += 5;

                                call_slots += 1;
                            } else {
                                let size = get_type_size(ty);
                                instructions.push(get_load_instruction(ty, local)?);
                                local += size;

                                call_slots += size;
                            }
                        }
                        instructions.push(Instruction::Invokeinterface(
                            call_ref,
                            call_slots
                                .try_into()
                                .map_err(|_| jvm::Error::VerificationError {
                                    context: format!(
                                        "Relative function-pointer bridge {interface_name_jvm}"
                                    ),
                                    message: "interface call exceeds 255 JVM parameter slots"
                                        .to_string(),
                                })?,
                        ));
                        instructions.push(return_instruction_for_type(&explicit_signature.ret));
                        jvm_methods.push(jvm::Method {
                            access_flags: MethodAccessFlags::PUBLIC,
                            name_index: cp.add_utf8(&format!(
                                "call{}",
                                oomir::RELATIVE_POINTER_METHOD_SUFFIX
                            ))?,
                            descriptor_index: cp.add_utf8(&relative_descriptor)?,
                            attributes: vec![code_attribute_for_descriptor(
                                &mut cp,
                                local,
                                instructions,
                                &relative_descriptor,
                                false,
                                Some(interface_name_jvm),
                                "call$relative",
                            )?],
                        });
                    }
                }
            }
            DataTypeMethod::SimpleConstantReturn(return_type, return_const) => {
                let descriptor = format!("(){}", return_type.to_jvm_descriptor());
                let (access_flags, attributes) = if let Some(return_const) = return_const {
                    (
                        MethodAccessFlags::PUBLIC,
                        vec![create_code_from_method_name_and_constant_return(
                            return_const,
                            &mut cp,
                        )?],
                    )
                } else {
                    (
                        MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT,
                        Vec::new(),
                    )
                };
                jvm_methods.push(jvm::Method {
                    access_flags,
                    name_index: cp.add_utf8(method_name)?,
                    descriptor_index: cp.add_utf8(&descriptor)?,
                    attributes,
                });
            }
            DataTypeMethod::Forwarder(recipe) => {
                jvm_methods.extend(forward::emit(
                    &mut cp,
                    interface_name_jvm,
                    method_name,
                    recipe,
                    module,
                    relative_static_methods,
                    true,
                )?);
            }
            DataTypeMethod::Function(_) => unreachable!("body was consumed above"),
            DataTypeMethod::AdtHelperMethod { kind } => {
                let method = match kind {
                    AdtHelperKind::EnumVariantIndex { .. }
                    | AdtHelperKind::EnumDiscriminant { .. }
                    | AdtHelperKind::EnumIsVariant { .. }
                    | AdtHelperKind::StaticPartialEqEnum { .. } => create_enum_adt_helper_method(
                        &mut cp,
                        module,
                        interface_name_jvm,
                        method_name,
                        kind,
                    )?,
                    AdtHelperKind::PartialEqClass { .. } | AdtHelperKind::Component { .. } => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Interface {interface_name_jvm}"),
                            message: "class-only helper cannot be emitted on an interface"
                                .to_string(),
                        });
                    }
                };
                jvm_methods.push(method);
            }
        }
    }

    let interface_indices = super_interfaces
        .iter()
        .map(|interface| cp.add_class(interface))
        .collect::<jvm::Result<Vec<_>>>()?;

    if !subclasses.is_empty() || nest_host.is_some() {
        let mut classes = Vec::new();
        for subclass_name in subclasses {
            classes.push(InnerClass {
                class_info_index: cp.add_class(&subclass_name)?,
                outer_class_info_index: this_class_index,
                name_index: cp
                    .add_utf8(subclass_name.rsplit('$').next().unwrap_or(&subclass_name))?,
                access_flags: NestedClassAccessFlags::PUBLIC | NestedClassAccessFlags::STATIC,
            });
        }
        if let Some(nest_host_name) = nest_host {
            classes.push(InnerClass {
                class_info_index: this_class_index,
                outer_class_info_index: cp.add_class(&nest_host_name)?,
                name_index: cp.add_utf8(
                    interface_name_jvm
                        .rsplit('$')
                        .next()
                        .unwrap_or(interface_name_jvm),
                )?,
                access_flags: NestedClassAccessFlags::PUBLIC | NestedClassAccessFlags::STATIC,
            });
        }
        class_attributes.push(Attribute::InnerClasses {
            name_index: cp.add_utf8("InnerClasses")?,
            classes,
        });
    }
    if let Some(source_file_name) = source_file_name {
        class_attributes.push(Attribute::SourceFile {
            name_index: cp.add_utf8("SourceFile")?,
            source_file_index: cp.add_utf8(source_file_name)?,
        });
    }
    if !bootstrap_methods.is_empty() {
        class_attributes.push(Attribute::BootstrapMethods {
            name_index: cp.add_utf8("BootstrapMethods")?,
            methods: bootstrap_methods,
        });
    }

    let class_file = ClassFile {
        code_source_url: None,
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC
            | ClassAccessFlags::INTERFACE
            | ClassAccessFlags::ABSTRACT,
        this_class: this_class_index,
        super_class: super_class_index,
        interfaces: interface_indices,
        fields: Vec::new(),
        methods: jvm_methods,
        attributes: class_attributes,
    };
    verify_no_duplicate_constants(&class_file)?;

    crate::lower2::serialize_class_file(&class_file, &format!("Interface {interface_name_jvm}"))
}
