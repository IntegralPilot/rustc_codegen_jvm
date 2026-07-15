// src/lower2/mod.rs

//! This module converts OOMIR into JVM bytecode.

use crate::oomir::{self, DataType};
use helpers::{get_cast_instructions, oomir_function_stack_floor};
use jvm_gen::{
    create_data_type_classfile_for_class, create_data_type_classfile_for_interface,
    create_default_constructor, create_slice_view_classfile, create_utf8_view_classfile,
    oomir_type_to_ristretto_field_type,
};
use translator::FunctionTranslator;

use self::jvm::{
    ClassAccessFlags, ClassFile, FieldAccessFlags, MethodAccessFlags, Version,
    attributes::{Attribute, Instruction, MaxStack},
};
use constant_pool::{InternedConstantPool, verify_no_duplicate_constants};
use consts::load_constant;
use rustc_middle::ty::TyCtxt;
use std::collections::{BTreeMap, BTreeSet, HashMap};

mod constant_pool;
mod consts;
mod helpers;
mod jvm;
mod jvm_gen;
mod optimise2;
mod stackmaps;
mod translator;

pub const F128_CLASS: &str = "org/rustlang/runtime/F128";
pub const I128_CLASS: &str = "org/rustlang/runtime/I128";
pub const U128_CLASS: &str = "org/rustlang/runtime/U128";

fn factory_return_instruction(ty: &oomir::Type) -> Instruction {
    match ty {
        oomir::Type::I8
        | oomir::Type::U8
        | oomir::Type::I16
        | oomir::Type::U16
        | oomir::Type::F16
        | oomir::Type::I32
        | oomir::Type::U32
        | oomir::Type::Boolean
        | oomir::Type::Char => Instruction::Ireturn,
        oomir::Type::I64 | oomir::Type::U64 => Instruction::Lreturn,
        oomir::Type::F32 => Instruction::Freturn,
        oomir::Type::F64 => Instruction::Dreturn,
        oomir::Type::Str
        | oomir::Type::Class(_)
        | oomir::Type::Array(_)
        | oomir::Type::Slice(_)
        | oomir::Type::Reference(_)
        | oomir::Type::Pointer(_)
        | oomir::Type::MutableReference(_)
        | oomir::Type::Interface(_) => Instruction::Areturn,
        oomir::Type::Void | oomir::Type::Unit => Instruction::Return,
    }
}

fn create_constant_factory(
    cp: &mut InternedConstantPool,
    owner_class: &str,
    constant: &oomir::Constant,
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<oomir::Constant> {
    let prepared = match constant {
        oomir::Constant::Array(element_type, elements) => oomir::Constant::Array(
            element_type.clone(),
            elements
                .iter()
                .map(|element| {
                    create_constant_factory(cp, owner_class, element, methods, next_factory)
                })
                .collect::<jvm::Result<Vec<_>>>()?,
        ),
        oomir::Constant::Slice(element_type, elements) => oomir::Constant::Slice(
            element_type.clone(),
            elements
                .iter()
                .map(|element| {
                    create_constant_factory(cp, owner_class, element, methods, next_factory)
                })
                .collect::<jvm::Result<Vec<_>>>()?,
        ),
        oomir::Constant::SliceRef {
            backing,
            element_type,
            length,
        } => oomir::Constant::SliceRef {
            backing: Box::new(create_constant_factory(
                cp,
                owner_class,
                backing,
                methods,
                next_factory,
            )?),
            element_type: element_type.clone(),
            length: *length,
        },
        oomir::Constant::Instance {
            class_name,
            fields,
            params,
        } => oomir::Constant::Instance {
            class_name: class_name.clone(),
            fields: fields.clone(),
            params: params
                .iter()
                .map(|param| create_constant_factory(cp, owner_class, param, methods, next_factory))
                .collect::<jvm::Result<Vec<_>>>()?,
        },
        _ => return Ok(constant.clone()),
    };

    let return_type = oomir::Type::from_constant(&prepared);
    let method_name = format!("__constant_factory_{}", *next_factory);
    *next_factory += 1;
    let descriptor = format!("(){}", return_type.to_jvm_descriptor());
    let mut instructions = Vec::new();
    load_constant(&mut instructions, cp, &prepared)?;
    instructions.push(factory_return_instruction(&return_type));
    let max_stack = instructions.max_stack(cp)?.saturating_mul(2).max(4);
    let code = Attribute::Code {
        name_index: cp.add_utf8("Code")?,
        max_stack,
        max_locals: 0,
        code: instructions,
        exception_table: Vec::new(),
        attributes: Vec::new(),
    };
    methods.push(jvm::Method {
        access_flags: MethodAccessFlags::PRIVATE
            | MethodAccessFlags::STATIC
            | MethodAccessFlags::SYNTHETIC,
        name_index: cp.add_utf8(&method_name)?,
        descriptor_index: cp.add_utf8(&descriptor)?,
        attributes: vec![code],
    });

    Ok(oomir::Constant::FactoryCall {
        owner_class: owner_class.to_string(),
        method_name,
        ty: return_type,
    })
}

fn create_static_initializer_method(
    cp: &mut InternedConstantPool,
    this_class_index: u16,
    owner_class: &str,
    statics: &[&oomir::Static],
    methods: &mut Vec<jvm::Method>,
) -> jvm::Result<jvm::Method> {
    let mut instructions = Vec::new();
    let mut next_factory = 0;
    for static_value in statics {
        if static_value.is_thread_local {
            return Err(jvm::Error::VerificationError {
                context: format!(
                    "Static {}::{}",
                    static_value.owner_class, static_value.field_name
                ),
                message: "thread-local statics are not yet representable".to_string(),
            });
        }

        let initializer = create_constant_factory(
            cp,
            owner_class,
            &static_value.initializer,
            methods,
            &mut next_factory,
        )?;
        let initializer_type = oomir::Type::from_constant(&initializer);
        load_constant(&mut instructions, cp, &initializer)?;

        if matches!(static_value.storage_type, oomir::Type::Pointer(_)) {
            if initializer_type.has_jvm_value() {
                instructions.extend(get_cast_instructions(
                    "<clinit>",
                    &initializer_type,
                    &oomir::Type::Class("java/lang/Object".to_string()),
                    cp,
                )?);
            } else {
                instructions.push(Instruction::Aconst_null);
            }
            let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
            let cell = cp.add_method_ref(
                pointer_class,
                "cell",
                &format!("(Ljava/lang/Object;)L{};", oomir::POINTER_CLASS),
            )?;
            instructions.push(Instruction::Invokestatic(cell));
        }

        let field_ref = cp.add_field_ref(
            this_class_index,
            &static_value.field_name,
            &static_value.storage_type.to_jvm_descriptor(),
        )?;
        instructions.push(Instruction::Putstatic(field_ref));
    }
    instructions.push(Instruction::Return);

    let max_stack = instructions.max_stack(cp)?.saturating_mul(2).max(4);
    let code = Attribute::Code {
        name_index: cp.add_utf8("Code")?,
        max_stack,
        max_locals: 0,
        code: instructions,
        exception_table: Vec::new(),
        attributes: Vec::new(),
    };
    Ok(jvm::Method {
        access_flags: MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("<clinit>")?,
        descriptor_index: cp.add_utf8("()V")?,
        attributes: vec![code],
    })
}

/// Converts an OOMIR module into JVM class files
/// Returns a HashMap where the key is the JVM class name (with '/') and the value is the bytecode
pub fn oomir_to_jvm_bytecode(
    module: &oomir::Module,
    _tcx: TyCtxt, // Keep tcx in signature if needed later, but unused now
) -> jvm::Result<HashMap<String, Vec<u8>>> {
    // Map to store the generated class files (Class Name -> Bytes)
    let mut generated_classes: HashMap<String, Vec<u8>> = HashMap::new();
    generated_classes.insert(
        oomir::SLICE_VIEW_CLASS.to_string(),
        create_slice_view_classfile()?,
    );
    generated_classes.insert(
        oomir::UTF8_VIEW_CLASS.to_string(),
        create_utf8_view_classfile()?,
    );

    let mut functions_by_class: BTreeMap<String, Vec<&oomir::Function>> = BTreeMap::new();
    for function in module.functions.values() {
        functions_by_class
            .entry(module.owner_class_for_function(function).to_string())
            .or_default()
            .push(function);
    }

    let mut statics_by_class: BTreeMap<String, Vec<&oomir::Static>> = BTreeMap::new();
    for static_value in module.statics.values() {
        statics_by_class
            .entry(static_value.owner_class.clone())
            .or_default()
            .push(static_value);
    }

    let mut class_names: Vec<_> = functions_by_class
        .keys()
        .chain(statics_by_class.keys())
        .cloned()
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    class_names.sort_by(|left, right| {
        match (
            statics_by_class.contains_key(left),
            statics_by_class.contains_key(right),
        ) {
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            _ => left.cmp(right),
        }
    });

    let mut current_index = 0;
    let total_functions: usize = functions_by_class.values().map(|v| v.len()).sum();
    for class_name_jvm in class_names {
        let functions = functions_by_class
            .remove(&class_name_jvm)
            .unwrap_or_default();
        let source_files = functions
            .iter()
            .filter_map(|function| function.source_file())
            .collect::<BTreeSet<_>>();
        if source_files.len() > 1 {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "bytecode-gen",
                format!(
                    "JVM class {class_name_jvm} contains Rust functions from multiple files: {source_files:?}"
                )
            );
        }
        let source_file_name = (class_name_jvm == module.name)
            .then(|| module.source_file.clone())
            .flatten()
            .or_else(|| source_files.first().map(|file| (*file).to_string()));
        let mut class_statics = statics_by_class.remove(&class_name_jvm).unwrap_or_default();
        class_statics.sort_by(|left, right| left.field_name.cmp(&right.field_name));
        let mut main_cp = InternedConstantPool::default();
        let super_class_name_jvm = "java/lang/Object"; // Standard superclass

        let super_class_index = main_cp.add_class(super_class_name_jvm)?;
        let this_class_index = main_cp.add_class(&class_name_jvm)?;
        let code_attribute_name_index = main_cp.add_utf8("Code")?;

        let mut methods: Vec<jvm::Method> = Vec::new();
        let mut fields = Vec::new();
        let mut has_constructor = false;

        for static_value in &class_statics {
            fields.push(jvm::Field {
                access_flags: FieldAccessFlags::PUBLIC
                    | FieldAccessFlags::STATIC
                    | FieldAccessFlags::FINAL,
                name_index: main_cp.add_utf8(&static_value.field_name)?,
                descriptor_index: main_cp
                    .add_utf8(static_value.storage_type.to_jvm_descriptor())?,
                field_type: oomir_type_to_ristretto_field_type(&static_value.storage_type),
                attributes: Vec::new(),
            });
        }

        if !class_statics.is_empty() {
            let static_initializer = create_static_initializer_method(
                &mut main_cp,
                this_class_index,
                &class_name_jvm,
                &class_statics,
                &mut methods,
            )?;
            methods.push(static_initializer);
        }

        for function in functions {
            current_index += 1;
            let instrumented_fn_name = format!("{class_name_jvm}::{}", function.name);
            let _timer =
                crate::instrumentation::Timer::function("lower2", None, &instrumented_fn_name);

            // Don't create a default constructor if the OOMIR provided one
            if function.name == "<init>" {
                has_constructor = true;
            }

            let name_index = main_cp.add_utf8(&function.name)?;
            let descriptor_index = main_cp.add_utf8(&function.signature.to_string())?;

            // Translate the function body using its own constant pool reference
            // Free functions at module level don't have an owner class
            let translator = FunctionTranslator::new(
                function,
                &mut main_cp, // Use the main class's constant pool
                module,
                true,
                None, // No owner class for free functions
            );
            let (jvm_code, max_locals_val, code_attributes) =
                translator
                    .translate()
                    .map_err(|error| jvm::Error::VerificationError {
                        context: format!(
                            "Function {class_name_jvm}::{} - {} of {}",
                            function.name, current_index, total_functions
                        ),
                        message: format!("Failed to translate function: {error:?}"),
                    })?;

            let stack_floor = oomir_function_stack_floor(function);
            let max_stack_val = match jvm_code.max_stack(&main_cp) {
                Ok(max_stack) => max_stack.saturating_mul(2).max(stack_floor),
                Err(error) => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "bytecode-gen",
                        format!(
                            "Falling back to conservative max_stack for {}::{} after max_stack failed: {:?}",
                            class_name_jvm, function.name, error
                        )
                    );
                    stack_floor.max(1024)
                }
            };

            let code_attribute = Attribute::Code {
                name_index: code_attribute_name_index,
                max_stack: max_stack_val,
                max_locals: max_locals_val,
                code: jvm_code,
                exception_table: Vec::new(),
                attributes: code_attributes,
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
        if !has_constructor {
            methods.push(create_default_constructor(&mut main_cp, super_class_index)?);
        }

        // Add SourceFile attribute
        let attributes = if let Some(source_file_name) = source_file_name {
            vec![Attribute::SourceFile {
                name_index: main_cp.add_utf8("SourceFile")?,
                source_file_index: main_cp.add_utf8(source_file_name)?,
            }]
        } else {
            Vec::new()
        };

        let class_file = ClassFile {
            code_source_url: None,
            version: Version::Java8 { minor: 0 },
            constant_pool: main_cp.into_inner(),
            access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
            this_class: this_class_index,
            super_class: super_class_index,
            interfaces: Vec::new(),
            fields,
            methods,
            attributes,
        };
        verify_no_duplicate_constants(&class_file)?;

        // Serialize the main class file
        let mut byte_vector = Vec::new();
        class_file
            .to_bytes(&mut byte_vector)
            .map_err(|error| jvm::Error::VerificationError {
                context: format!("Class {class_name_jvm}"),
                message: format!("Failed to serialize class file: {error:?}"),
            })?;
        generated_classes.insert(class_name_jvm.clone(), byte_vector);

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("Generated module class: {}", class_name_jvm)
        );
    }

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
