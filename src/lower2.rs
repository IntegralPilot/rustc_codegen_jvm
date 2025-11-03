// src/lower2/mod.rs

//! This module converts OOMIR into JVM bytecode.

use crate::oomir::{self, DataType};
use jvm_gen::{
    create_data_type_classfile_for_class, create_data_type_classfile_for_interface,
    create_default_constructor,
};
use translator::FunctionTranslator;

use ristretto_classfile::{
    self as jvm, ClassAccessFlags, ClassFile, ConstantPool, MethodAccessFlags, Version,
    attributes::{Attribute, MaxStack},
};
use rustc_middle::ty::TyCtxt;
use std::collections::HashMap;

mod consts;
mod helpers;
mod jvm_gen;
mod shim;
mod translator;

pub const BIG_INTEGER_CLASS: &str = "java/math/BigInteger";
pub const BIG_DECIMAL_CLASS: &str = "java/math/BigDecimal";

/// Converts an OOMIR module into JVM class files
/// Returns a HashMap where the key is the JVM class name (with '/') and the value is the bytecode
pub fn oomir_to_jvm_bytecode(
    module: &oomir::Module,
    _tcx: TyCtxt, // Keep tcx in signature if needed later, but unused now
) -> jvm::Result<HashMap<String, Vec<u8>>> {
    // Map to store the generated class files (Class Name -> Bytes)
    let mut generated_classes: HashMap<String, Vec<u8>> = HashMap::new();

    // --- 1. Generate the Main Module Class (containing functions) ---
    {
        // Scope block for the main class generation
        let mut main_cp = ConstantPool::default();
        // Convert module name to JVM internal format (replace '.' with '/')
        let main_class_name_jvm = module.name.replace('.', "/");
        let super_class_name_jvm = "java/lang/Object"; // Standard superclass

        let super_class_index = main_cp.add_class(super_class_name_jvm)?;
        let this_class_index = main_cp.add_class(&main_class_name_jvm)?;
        let code_attribute_name_index = main_cp.add_utf8("Code")?;

        let mut methods: Vec<jvm::Method> = Vec::new();
        let mut has_constructor = false;

        for function in module.functions.values() {
            // Don't create a default constructor if the OOMIR provided one
            if function.name == "<init>" {
                has_constructor = true;
            }

            let name_index = main_cp.add_utf8(&function.name)?;
            let descriptor_index = main_cp.add_utf8(&function.signature.to_string())?;

            // Translate the function body using its own constant pool reference
            let translator = FunctionTranslator::new(
                function,
                &mut main_cp, // Use the main class's constant pool
                module,
                true,
            );
            let (jvm_code, max_locals_val) = translator.translate()?;

            let max_stack_val = jvm_code.max_stack(&main_cp)?;

            let code_attribute = Attribute::Code {
                name_index: code_attribute_name_index,
                max_stack: max_stack_val,
                max_locals: max_locals_val,
                code: jvm_code,
                exception_table: Vec::new(),
                attributes: Vec::new(),
            };

            // Create MethodParameters attribute to preserve parameter names
            let mut parameters_for_attribute = Vec::new();
            for (name, _) in &function.signature.params {
                let name_index = main_cp.add_utf8(name)?;
                parameters_for_attribute.push(jvm::attributes::MethodParameter {
                    name_index,
                    access_flags: MethodAccessFlags::empty(), // No special flags
                });
            }
            let method_parameters_attribute_name_index = main_cp.add_utf8("MethodParameters")?;
            let method_parameters_attribute = Attribute::MethodParameters {
                name_index: method_parameters_attribute_name_index,
                parameters: parameters_for_attribute,
            };

            let mut method = jvm::Method::default();
            // Assume static for now, adjust if instance methods are needed
            method.access_flags = MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC;
            if function.name == "<init>" {
                // Constructors cannot be static
                method.access_flags = MethodAccessFlags::PUBLIC;
            }
            method.name_index = name_index;
            method.descriptor_index = descriptor_index;
            method.attributes.push(code_attribute);
            // Add MethodParameters attribute (skip for constructors as they often have synthetic params)
            if function.name != "<init>" {
                method.attributes.push(method_parameters_attribute);
            }

            methods.push(method);
        }

        // Add a default constructor if none was provided in OOMIR
        if !has_constructor && !module.functions.contains_key("<init>") {
            methods.push(create_default_constructor(&mut main_cp, super_class_index)?);
        }

        let mut class_file = ClassFile {
            version: Version::Java8 { minor: 0 },
            constant_pool: main_cp, // Move the main constant pool here
            access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
            this_class: this_class_index,
            super_class: super_class_index,
            interfaces: Vec::new(),
            fields: Vec::new(), // Main class might not have fields unless they are static globals
            methods,
            attributes: Vec::new(),
        };

        // Add SourceFile attribute
        let source_file_name = format!(
            "{}.rs",
            module.name.split('.').last().unwrap_or(&module.name)
        ); // Simple name
        let source_file_utf8_index = class_file.constant_pool.add_utf8(&source_file_name)?;
        let source_file_attr_name_index = class_file.constant_pool.add_utf8("SourceFile")?;
        class_file.attributes.push(Attribute::SourceFile {
            name_index: source_file_attr_name_index,
            source_file_index: source_file_utf8_index,
        });

        // Serialize the main class file
        let mut byte_vector = Vec::new();
        class_file.to_bytes(&mut byte_vector)?;
        generated_classes.insert(main_class_name_jvm.clone(), byte_vector);

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("Generated main class: {}", main_class_name_jvm)
        );
    }

    // --- 2. Generate Class Files for Data Types ---
    for (dt_name_oomir, data_type) in &module.data_types {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("Generating data type class: {}", dt_name_oomir)
        );

        let mut data_type = data_type.clone();

        data_type.clean_duplicates();

        match data_type {
            DataType::Class {
                is_abstract,
                super_class,
                fields,
                methods,
                interfaces,
            } => {
                let mut subclasses = Vec::new();
                let mut nest_host = None;
                // does our class name contain '$'?
                let mut potential_nest_host = None;
                if dt_name_oomir.contains('$') {
                    // strip everything after the last '$', including $
                    let last_dollar_index = dt_name_oomir.rfind('$').unwrap();
                    potential_nest_host = Some(dt_name_oomir[..last_dollar_index].to_string());
                }
                for (other_dt_name, _) in &module.data_types {
                    if other_dt_name.starts_with(&format!("{}$", dt_name_oomir)) {
                        subclasses.push(other_dt_name.clone());
                    }
                    if let Some(potential_nest_host) = &potential_nest_host {
                        if other_dt_name == potential_nest_host {
                            nest_host = Some(potential_nest_host.clone());
                        }
                    }
                }
                // Create and serialize the class file for this data type
                let dt_bytecode = create_data_type_classfile_for_class(
                    &dt_name_oomir,
                    fields.clone(),
                    is_abstract,
                    methods.clone(),
                    super_class.as_deref().unwrap_or("java/lang/Object"),
                    interfaces.clone(),
                    &module,
                    subclasses,
                    nest_host,
                )?;
                generated_classes.insert(dt_name_oomir.clone(), dt_bytecode);
            }
            DataType::Interface { methods } => {
                // Create and serialize the class file for this data type
                let dt_bytecode =
                    create_data_type_classfile_for_interface(&dt_name_oomir, &methods)?;
                generated_classes.insert(dt_name_oomir.clone(), dt_bytecode);
            }
        }
    }

    Ok(generated_classes) // Return the map containing all generated classes
}
