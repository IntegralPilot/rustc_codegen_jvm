//! Native JVM classes emission.
use super::*;

/// Creates a ClassFile (as bytes) for a given OOMIR DataType that's a class
pub(in crate::lower2) fn create_data_type_classfile_for_class(
    class_name_jvm: &str,
    fields: &[(String, Type)],
    is_abstract: bool,
    methods: &HashMap<String, DataTypeMethod>,
    super_class_name_jvm: &str,
    implements_interfaces: &[String],
    module: &oomir::Module,
    subclasses: &[String],
    nest_host: Option<&str>,
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
    if source_files.len() > 1 {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!(
                "JVM class {class_name_jvm} contains Rust methods from multiple files: {source_files:?}"
            )
        );
    }
    let source_file_name = source_files.first().map(|file| (*file).to_string());
    let fields: Vec<_> = fields
        .iter()
        .filter(|(_, field_ty)| field_ty.has_jvm_value())
        .cloned()
        .collect();
    let mut cp = InternedConstantPool::default();

    let this_class_index = cp.add_class(class_name_jvm)?;

    let super_class_index = cp.add_class(super_class_name_jvm)?;

    let mut seen_interfaces = HashSet::default();
    let mut interface_indices: Vec<u16> = Vec::with_capacity(implements_interfaces.len());
    for interface_name in implements_interfaces {
        if !seen_interfaces.insert(interface_name.as_str()) {
            continue;
        }
        // Add the interface name to the constant pool as a Class reference
        let interface_index = cp.add_class(interface_name)?;
        interface_indices.push(interface_index);
    }
    if !is_abstract {
        let rust_copy_interface = "org/rustlang/runtime/RustCopy";
        if seen_interfaces.insert(rust_copy_interface) {
            interface_indices.push(cp.add_class(rust_copy_interface)?);
        }
    }

    let mut jvm_fields: Vec<jvm::Field> = Vec::new();
    for (field_name, field_ty) in &fields {
        let name_index = cp.add_utf8(field_name)?;
        let descriptor = field_ty.to_jvm_descriptor(); // Ensure this method exists on oomir::Type
        let descriptor_index = cp.add_utf8(&descriptor)?;

        let field = jvm::Field {
            access_flags: FieldAccessFlags::PUBLIC,
            name_index,
            descriptor_index,
            field_type: oomir_type_to_ristretto_field_type(field_ty), // Use helper
            attributes: Vec::new(),
        };
        jvm_fields.push(field);
        if matches!(field_ty, Type::Pointer(_)) {
            for offset_name in [
                oomir::relative_pointer_element_offset_field(field_name),
                oomir::relative_pointer_byte_offset_field(field_name),
            ] {
                jvm_fields.push(jvm::Field {
                    access_flags: FieldAccessFlags::PUBLIC | FieldAccessFlags::SYNTHETIC,
                    name_index: cp.add_utf8(offset_name)?,
                    descriptor_index: cp.add_utf8("J")?,
                    field_type: oomir_type_to_ristretto_field_type(&Type::I64),
                    attributes: Vec::new(),
                });
            }
        }
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("  - Added field: {} {}", field_name, descriptor)
        );
    }

    // Fielded Rust structs/enums must be initialized with all fields. Only genuinely
    // fieldless classes keep a no-args constructor.
    let constructor = if fields.is_empty() {
        create_default_constructor(&mut cp, super_class_index)?
    } else {
        create_field_constructor(&mut cp, this_class_index, super_class_index, &fields)?
    };
    let mut jvm_methods = vec![constructor];
    if fields
        .iter()
        .any(|(_, field_ty)| matches!(field_ty, Type::Pointer(_)))
    {
        jvm_methods.push(create_relative_pointer_field_constructor(
            &mut cp,
            this_class_index,
            super_class_index,
            &fields,
        )?);
    }
    if !is_abstract {
        jvm_methods.push(create_managed_copy_method(
            &mut cp,
            this_class_index,
            class_name_jvm,
            &fields,
        )?);
    }
    let mut class_attributes = Vec::new();
    let mut bootstrap_methods: Vec<BootstrapMethod> = Vec::new();
    let mut next_factory = 0;

    // Check for jvm_methods
    for (method_name, method) in methods.iter() {
        match method {
            DataTypeMethod::Abstract(signature) => {
                let name_index = cp.add_utf8(method_name)?;
                let descriptor_index = cp.add_utf8(signature.to_string())?;
                let mut access_flags = MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT;
                if signature.is_static {
                    access_flags |= MethodAccessFlags::STATIC;
                }
                jvm_methods.push(jvm::Method {
                    access_flags,
                    name_index,
                    descriptor_index,
                    attributes: Vec::new(),
                });
            }
            DataTypeMethod::SimpleConstantReturn(return_type, return_const) => {
                let method_desc = format!("(){}", return_type.to_jvm_descriptor());

                // Add the method to the class file
                let name_index = cp.add_utf8(&method_name)?;
                let descriptor_index: u16 = cp.add_utf8(method_desc)?;

                let mut attributes = vec![];
                let mut is_abstract = false;

                match return_const {
                    Some(rc) => attributes.push(create_code_from_method_name_and_constant_return(
                        &rc, &mut cp,
                    )?),
                    None => {
                        is_abstract = true;
                    }
                }

                let jvm_method = jvm::Method {
                    access_flags: MethodAccessFlags::PUBLIC
                        | if is_abstract {
                            MethodAccessFlags::ABSTRACT
                        } else {
                            MethodAccessFlags::FINAL
                        },
                    name_index,
                    descriptor_index,
                    attributes,
                };

                jvm_methods.push(jvm_method);
            }
            DataTypeMethod::Forwarder(recipe) => {
                jvm_methods.extend(forward::emit(
                    &mut cp,
                    class_name_jvm,
                    method_name,
                    recipe,
                    module,
                    relative_static_methods,
                    false,
                )?);
            }
            DataTypeMethod::Function(function) => {
                body::BodyEmitter {
                    cp: &mut cp,
                    bootstrap: &mut bootstrap_methods,
                    methods: &mut jvm_methods,
                    next_factory: &mut next_factory,
                    owner: class_name_jvm,
                    kind: body::BodyOwner::Class,
                    relative_methods: relative_static_methods,
                    debug: debug_info,
                    context,
                }
                .emit(method_name, function)?;
            }
            DataTypeMethod::AdtHelperMethod { kind } => {
                let jvm_method = match kind {
                    AdtHelperKind::EnumVariantIndex { .. }
                    | AdtHelperKind::EnumDiscriminant { .. }
                    | AdtHelperKind::EnumIsVariant { .. }
                    | AdtHelperKind::StaticPartialEqEnum { .. } => create_enum_adt_helper_method(
                        &mut cp,
                        module,
                        class_name_jvm,
                        method_name,
                        kind,
                    )?,
                    AdtHelperKind::PartialEqClass { fields } => {
                        let method_desc = format!("(L{};)Z", class_name_jvm);
                        let name_index = cp.add_utf8(method_name)?;
                        let descriptor_index = cp.add_utf8(&method_desc)?;

                        let this_class_idx = this_class_index;
                        let mut instructions = Vec::new();
                        let mut false_fixups = Vec::new();

                        for (field_name, field_ty) in fields {
                            if !field_ty.has_jvm_value() {
                                continue;
                            }
                            append_field_equality_check(
                                module,
                                &mut cp,
                                &mut instructions,
                                &mut false_fixups,
                                this_class_idx,
                                field_name,
                                field_ty,
                            )?;
                        }

                        instructions.push(Instruction::Iconst_1);
                        instructions.push(Instruction::Ireturn);

                        if !false_fixups.is_empty() {
                            let false_target = instructions.len() as u16;
                            instructions.push(Instruction::Iconst_0);
                            instructions.push(Instruction::Ireturn);

                            for fixup in false_fixups {
                                patch_branch_target(&mut instructions, fixup, false_target);
                            }
                        }

                        let code_attribute = code_attribute_for_descriptor(
                            &mut cp,
                            2,
                            instructions,
                            &method_desc,
                            false,
                            Some(class_name_jvm),
                            method_name,
                        )?;

                        jvm::Method {
                            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::FINAL,
                            name_index,
                            descriptor_index,
                            attributes: vec![code_attribute],
                        }
                    }
                    AdtHelperKind::Component {
                        field_name,
                        field_ty,
                    } => {
                        let method_desc = format!("(){}", field_ty.to_jvm_descriptor());
                        let field_ref = cp.add_field_ref(
                            this_class_index,
                            field_name,
                            &field_ty.to_jvm_descriptor(),
                        )?;
                        let code_attribute = code_attribute_for_descriptor(
                            &mut cp,
                            1,
                            vec![
                                Instruction::Aload_0,
                                Instruction::Getfield(field_ref),
                                return_instruction_for_type(field_ty),
                            ],
                            &method_desc,
                            false,
                            Some(class_name_jvm),
                            method_name,
                        )?;
                        jvm::Method {
                            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::FINAL,
                            name_index: cp.add_utf8(method_name)?,
                            descriptor_index: cp.add_utf8(&method_desc)?,
                            attributes: vec![code_attribute],
                        }
                    }
                };
                jvm_methods.push(jvm_method);
            }
        }
    }

    if !subclasses.is_empty() || nest_host.is_some() {
        let mut inner_classes_vec: Vec<InnerClass> = Vec::with_capacity(subclasses.len());

        for subclass_name in subclasses {
            // Ensure subclass class_info is in the constant pool
            let class_info_index = cp.add_class(subclass_name)?;

            // The outer class is this class
            let outer_class_info_index = this_class_index;

            // Derive simple name: part after last '$'. If there's no '$', treat as unnamed (0).
            let simple_name_part = subclass_name.rsplit('$').next().unwrap_or(subclass_name);

            // If the simple name looks like an anonymous class (all digits), set name_index = 0
            let name_index = if simple_name_part.chars().all(|c| c.is_ascii_digit()) {
                0
            } else if simple_name_part == subclass_name && !subclass_name.contains('$') {
                // No '$' present -> not an inner/member class; leave name_index = 0
                0
            } else {
                cp.add_utf8(simple_name_part)?
            };

            // Default to PUBLIC | STATIC for generated nested classes. This can be adjusted
            // if more precise access info becomes available.
            let access_flags = NestedClassAccessFlags::PUBLIC | NestedClassAccessFlags::STATIC;

            inner_classes_vec.push(InnerClass {
                class_info_index,
                outer_class_info_index,
                name_index,
                access_flags,
            });
        }

        // If this class has a nest host, add it as well
        // make it like [us]=class Host$[us] of class Host
        if let Some(nest_host_name) = nest_host {
            let class_info_index = cp.add_class(class_name_jvm)?;
            let outer_class_info_index = cp.add_class(nest_host_name)?;
            let name_index =
                cp.add_utf8(class_name_jvm.rsplit('$').next().unwrap_or(class_name_jvm))?;
            let access_flags = NestedClassAccessFlags::PUBLIC | NestedClassAccessFlags::STATIC;
            inner_classes_vec.push(InnerClass {
                class_info_index,
                outer_class_info_index,
                name_index,
                access_flags,
            });
        }

        let inner_classes_attr_name_index = cp.add_utf8("InnerClasses")?;
        class_attributes.push(Attribute::InnerClasses {
            name_index: inner_classes_attr_name_index,
            classes: inner_classes_vec,
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
            | ClassAccessFlags::SUPER
            | if is_abstract {
                ClassAccessFlags::ABSTRACT
            } else {
                ClassAccessFlags::FINAL
            },
        this_class: this_class_index,
        super_class: super_class_index,
        interfaces: interface_indices,
        fields: jvm_fields,
        methods: jvm_methods,
        attributes: class_attributes,
    };
    verify_no_duplicate_constants(&class_file)?;

    crate::lower2::serialize_class_file(&class_file, &format!("Class {class_name_jvm}"))
}
