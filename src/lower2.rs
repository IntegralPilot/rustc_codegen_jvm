//! This module converts OOMIR into JVM bytecode.

use crate::oomir::{self, DataType};
use jvm_compiler_core::jvm::MethodCode;
use jvm_gen::{
    create_data_type_classfile_for_class, create_data_type_classfile_for_interface,
    create_default_constructor, create_slice_view_classfile, create_utf8_view_classfile,
    oomir_type_to_ristretto_field_type,
};
#[derive(Clone, Copy)]
pub(crate) struct DebugInfoOptions {
    pub line_numbers: bool,
    pub local_variables: bool,
}

use self::jvm::{
    ClassAccessFlags, ClassFile, FieldAccessFlags, Version,
    attributes::{Attribute, BootstrapMethod},
};
use constant_pool::{InternedConstantPool, verify_no_duplicate_constants};
use constants::create_static_initializer_method;
use rustc_hash::FxHashMap as HashMap;
use rustc_middle::ty::TyCtxt;
use std::{
    collections::{BTreeMap, BTreeSet},
    path::PathBuf,
    sync::Arc,
};

use jvm::constant_pool;
mod constants;
mod output;
pub(crate) use jvm_compiler_core::classfile::registry::ClassRegistry as EmittedClassRegistry;
use output::serialize_class_file;
mod abi;
mod helpers;
use jvm_compiler_core::classfile as jvm;
mod jvm_gen;
use jvm_compiler_core::jvm::frames as stackmaps;
pub(crate) mod select;

pub const F128_CLASS: &str = "org/rustlang/runtime/F128";
pub const I128_CLASS: &str = "org/rustlang/runtime/I128";
pub const U128_CLASS: &str = "org/rustlang/runtime/U128";

/// Converts an OOMIR module into JVM class files, streaming each completed
/// class into one shard bundle so rustc does not manage tens of thousands of
/// temporary object paths.
pub fn oomir_to_jvm_bytecode(
    mut module: oomir::Module,
    debug_info: DebugInfoOptions,
    emit_runtime_views: bool,
    registry: &EmittedClassRegistry,
) -> jvm::Result<Vec<(String, PathBuf)>> {
    let context = oomir::construct::Context::new(&module);

    let function_relative_methods = module
        .functions
        .values()
        .filter_map(|function| {
            (function.signature.is_static
                && function.name != "<init>"
                && function.signature.supports_relative_pointer_abi())
            .then(|| {
                oomir::FunctionKey::new(
                    module.owner_class_for_function(function),
                    &function.name,
                    &function.signature,
                )
            })
        })
        .collect::<Vec<_>>();
    for method in function_relative_methods {
        if !module.relative_static_methods.contains(&method) {
            Arc::make_mut(&mut module.relative_static_methods).insert(method);
        }
    }
    for (class_name, data_type) in &module.data_types {
        let oomir::DataType::Class { methods, .. } = data_type else {
            continue;
        };
        for (method_name, method) in methods {
            let Some(signature) = method.function_signature() else {
                continue;
            };
            if signature.is_static
                && method_name != "<init>"
                && signature.supports_relative_pointer_abi()
            {
                let method = oomir::FunctionKey::new(class_name, method_name, signature);
                if !module.relative_static_methods.contains(&method) {
                    Arc::make_mut(&mut module.relative_static_methods).insert(method);
                }
            }
        }
    }
    let mut output = output::ClassOutput::create(&module.name)?;
    if emit_runtime_views {
        output.emit(
            registry,
            oomir::SLICE_VIEW_CLASS.to_string(),
            create_slice_view_classfile()?,
            crate::metrics::ClassOrigin::Runtime,
        )?;
        output.emit(
            registry,
            oomir::UTF8_VIEW_CLASS.to_string(),
            create_utf8_view_classfile()?,
            crate::metrics::ClassOrigin::Runtime,
        )?;
    }

    // Consume functions class-by-class so their OOMIR is released as soon as
    // the corresponding classfile has been serialized.
    let mut functions_by_class: BTreeMap<String, Vec<oomir::Function>> = BTreeMap::new();
    for (_, function) in std::mem::take(&mut module.functions) {
        functions_by_class
            .entry(module.owner_class_for_function(&function).to_string())
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
                breadcrumbs::LogLevel::Info,
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

        let mut methods: Vec<jvm::Method> = Vec::new();
        let mut bootstrap_methods: Vec<BootstrapMethod> = Vec::new();
        let mut next_factory = 0;
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
                &mut next_factory,
            )?;
            methods.push(static_initializer);
        }

        for function in functions {
            has_constructor |= function.name == "<init>";
            jvm_gen::BodyEmitter {
                cp: &mut main_cp,
                bootstrap: &mut bootstrap_methods,
                methods: &mut methods,
                next_factory: &mut next_factory,
                owner: &class_name_jvm,
                kind: jvm_gen::BodyOwner::Module,
                relative_methods: &module.relative_static_methods,
                debug: debug_info,
                context: &context,
            }
            .emit_owned(function)?;
        }

        // Add a default constructor if none was provided in OOMIR
        if !has_constructor {
            methods.push(create_default_constructor(&mut main_cp, super_class_index)?);
        }

        // Add SourceFile attribute
        let mut attributes = if let Some(source_file_name) = source_file_name {
            vec![Attribute::SourceFile {
                name_index: main_cp.add_utf8("SourceFile")?,
                source_file_index: main_cp.add_utf8(source_file_name)?,
            }]
        } else {
            Vec::new()
        };
        if !bootstrap_methods.is_empty() {
            attributes.push(Attribute::BootstrapMethods {
                name_index: main_cp.add_utf8("BootstrapMethods")?,
                methods: bootstrap_methods,
            });
        }

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
        let byte_vector = serialize_class_file(&class_file, &format!("Class {class_name_jvm}"))?;
        output.emit(
            registry,
            class_name_jvm.clone(),
            byte_vector,
            crate::metrics::ClassOrigin::Module,
        )?;

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("Generated module class: {}", class_name_jvm)
        );
    }

    let data_type_names = module
        .data_types
        .keys()
        .map(String::as_str)
        .collect::<rustc_hash::FxHashSet<_>>();
    let mut subclasses_by_host = HashMap::<String, Vec<String>>::default();
    let mut nest_host_by_class = HashMap::<String, String>::default();
    for class_name in module.data_types.keys() {
        for (separator, _) in class_name.match_indices('$') {
            let host = &class_name[..separator];
            if data_type_names.contains(host) {
                subclasses_by_host
                    .entry(host.to_string())
                    .or_default()
                    .push(class_name.clone());
            }
        }
        if let Some(separator) = class_name.rfind('$') {
            let host = &class_name[..separator];
            if data_type_names.contains(host) {
                nest_host_by_class.insert(class_name.clone(), host.to_string());
            }
        }
    }
    for subclasses in subclasses_by_host.values_mut() {
        subclasses.sort();
        subclasses.dedup();
    }

    // Retain method names for dispatch lookup while consuming each computational
    // body once. Schema lookups never need a second copy of its instructions.
    let methods_to_emit = module
        .data_types
        .iter_mut()
        .filter_map(|(name, data_type)| {
            if module.suppressed_data_types.contains(name) {
                return None;
            }
            let (DataType::Class { methods, .. } | DataType::Interface { methods, .. }) = data_type;
            let stubs = methods
                .keys()
                .map(|name| {
                    (
                        name.clone(),
                        oomir::DataTypeMethod::SimpleConstantReturn(oomir::Type::Void, None),
                    )
                })
                .collect();
            Some((name.clone(), std::mem::replace(methods, stubs)))
        })
        .collect::<Vec<_>>();
    for (dt_name_oomir, methods) in methods_to_emit {
        let dt_name_oomir = dt_name_oomir.as_str();
        let data_type = &module.data_types[dt_name_oomir];
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("Generating data type class: {}", dt_name_oomir)
        );

        match data_type {
            DataType::Class {
                is_abstract,
                super_class,
                fields,
                methods: _,
                interfaces,
            } => {
                let subclasses = subclasses_by_host.remove(dt_name_oomir).unwrap_or_default();
                let nest_host = nest_host_by_class.remove(dt_name_oomir);
                // Create and serialize the class file for this data type
                let dt_bytecode = create_data_type_classfile_for_class(
                    dt_name_oomir,
                    fields,
                    *is_abstract,
                    methods,
                    super_class.as_deref().unwrap_or("java/lang/Object"),
                    interfaces,
                    &module,
                    &subclasses,
                    nest_host.as_deref(),
                    debug_info,
                    &module.relative_static_methods,
                    &context,
                )?;
                output.emit(
                    registry,
                    dt_name_oomir.to_owned(),
                    dt_bytecode,
                    crate::metrics::ClassOrigin::DataTypeClass,
                )?;
            }
            DataType::Interface {
                methods: _,
                interfaces,
                ..
            } => {
                let subclasses = subclasses_by_host.remove(dt_name_oomir).unwrap_or_default();
                let nest_host = nest_host_by_class.remove(dt_name_oomir);
                // Create and serialize the class file for this data type
                let dt_bytecode = create_data_type_classfile_for_interface(
                    dt_name_oomir,
                    methods,
                    interfaces,
                    &module,
                    subclasses,
                    nest_host,
                    debug_info,
                    &module.relative_static_methods,
                    &context,
                )?;
                output.emit(
                    registry,
                    dt_name_oomir.to_owned(),
                    dt_bytecode,
                    crate::metrics::ClassOrigin::DataTypeInterface,
                )?;
            }
        }
    }

    output.finish(module.name)
}

pub(crate) fn debug_info_options(tcx: TyCtxt<'_>) -> DebugInfoOptions {
    DebugInfoOptions {
        line_numbers: tcx.sess.opts.debuginfo != rustc_session::config::DebugInfo::None,
        local_variables: matches!(
            tcx.sess.opts.debuginfo,
            rustc_session::config::DebugInfo::Limited | rustc_session::config::DebugInfo::Full
        ),
    }
}
