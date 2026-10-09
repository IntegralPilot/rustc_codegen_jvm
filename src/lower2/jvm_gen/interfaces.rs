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
    context: &oomir::construct::Context,
    output: &mut crate::lower2::output::ClassOutput,
    registry: &crate::lower2::EmittedClassRegistry,
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
    cp.set_resource_anchor(this_class_index);

    // Interfaces always implicitly extend Object, and must specify it in the classfile
    let super_class_index = cp.add_class("java/lang/Object")?;

    let mut jvm_methods: Vec<jvm::Method> = Vec::new();
    let mut class_attributes = Vec::new();
    // Public Java enum classes remain linker roots. Private carriers follow their users.
    let private = interface_name_jvm.starts_with("org/rustlang/runtime/FnPtr_")
        || (matches!(
            module.data_type(interface_name_jvm),
            Some(oomir::DataType::Interface { is_enum: true, .. })
        ) && !subclasses.is_empty()
            && subclasses.iter().all(|name| {
                matches!(
                    module.data_type(name),
                    Some(oomir::DataType::Class {
                        kind: oomir::ClassKind::Value,
                        ..
                    })
                )
            }));
    if private {
        class_attributes.push(Attribute::Unknown {
            name_index: cp.add_utf8(jvm::summary::PRIVATE_ATTRIBUTE)?,
            info: Vec::new(),
        });
        if let Some(info) =
            super::enum_shapes::interface(&methods, super_interfaces, module, &subclasses)
        {
            class_attributes.push(Attribute::Unknown {
                name_index: cp.add_utf8(jvm::summary::CARRIER_ATTRIBUTE)?,
                info,
            });
        }
    }
    let mut bootstrap_methods: Vec<BootstrapMethod> = Vec::new();
    let mut next_factory = 0;
    let share_function = methods.len() == 1
        && super_interfaces.is_empty()
        && subclasses.is_empty()
        && nest_host.is_none();
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
                let mut signature = signature.clone();
                if oomir::component_method(interface_name_jvm, method_name) {
                    // Abstract signatures contain no implicit receiver.
                    signature.is_static = true;
                    signature = signature.component_signature();
                }
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
                    jvm_methods.push(super::function_handles::bridge(
                        &mut cp,
                        &signature,
                        &mut bootstrap_methods,
                    )?);
                    if share_function {
                        class_attributes.push(Attribute::Unknown {
                            name_index: cp.add_utf8(jvm::summary::CARRIER_ATTRIBUTE)?,
                            info: super::function_handles::carrier_recipe(&signature),
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
                    true,
                )?);
            }
            DataTypeMethod::Function(_) => unreachable!("body was consumed above"),
            DataTypeMethod::AdtHelperMethod { kind } => {
                if let AdtHelperKind::StaticPartialEqEnum {
                    enum_class,
                    variants,
                } = kind
                {
                    jvm_methods.extend(create_enum_equality_methods(
                        &mut cp,
                        module,
                        interface_name_jvm,
                        method_name,
                        enum_class,
                        variants,
                    )?);
                    continue;
                }
                let method = match kind {
                    AdtHelperKind::EnumVariantIndex { .. }
                    | AdtHelperKind::EnumDiscriminant { .. }
                    | AdtHelperKind::EnumIsVariant { .. } => create_enum_adt_helper_method(
                        &mut cp,
                        module,
                        interface_name_jvm,
                        method_name,
                        kind,
                    )?,
                    AdtHelperKind::PartialEqClass { .. }
                    | AdtHelperKind::Component { .. }
                    | AdtHelperKind::StaticPartialEqEnum { .. } => {
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
                name_index: cp.add_utf8(jvm::names::inner_name(&subclass_name))?,
                access_flags: NestedClassAccessFlags::PUBLIC | NestedClassAccessFlags::STATIC,
            });
        }
        if let Some(nest_host_name) = nest_host {
            classes.push(InnerClass {
                class_info_index: this_class_index,
                outer_class_info_index: cp.add_class(&nest_host_name)?,
                name_index: cp.add_utf8(jvm::names::inner_name(interface_name_jvm))?,
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

    output.resources(registry, &mut cp)?;
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
