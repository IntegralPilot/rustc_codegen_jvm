use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::fs::rename;
use std::io::{self, BufReader, Cursor, Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use regex::Regex;
use ristretto_classfile::attributes::{
    Attribute, BootstrapMethod, Instruction, StackFrame, VerificationType,
};
use ristretto_classfile::{
    ClassAccessFlags, ClassFile, Constant, ConstantPool, MethodAccessFlags,
};
use tempfile::tempdir;
use zip::write::{SimpleFileOptions, ZipWriter};
use zip::{CompressionMethod, ZipArchive};

const CLASS_BUNDLE_MAGIC: &[u8; 8] = b"RCJVMB1\0";

#[derive(Debug)]
struct ClassInfo {
    jar_entry_name: String,
    data: Vec<u8>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum ConstantKey {
    Utf8(String),
    Integer(i32),
    Float(u32),
    Long(i64),
    Double(u64),
    Class(u16),
    String(u16),
    FieldRef(u16, u16),
    MethodRef(u16, u16),
    InterfaceMethodRef(u16, u16),
    NameAndType(u16, u16),
    MethodHandle(u8, u16),
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16),
}

impl From<&Constant<'_>> for ConstantKey {
    fn from(constant: &Constant<'_>) -> Self {
        match constant {
            Constant::Utf8(value) => Self::Utf8(value.to_string()),
            Constant::Integer(value) => Self::Integer(*value),
            Constant::Float(value) => Self::Float(value.to_bits()),
            Constant::Long(value) => Self::Long(*value),
            Constant::Double(value) => Self::Double(value.to_bits()),
            Constant::Class(index) => Self::Class(*index),
            Constant::String(index) => Self::String(*index),
            Constant::FieldRef {
                class_index,
                name_and_type_index,
            } => Self::FieldRef(*class_index, *name_and_type_index),
            Constant::MethodRef {
                class_index,
                name_and_type_index,
            } => Self::MethodRef(*class_index, *name_and_type_index),
            Constant::InterfaceMethodRef {
                class_index,
                name_and_type_index,
            } => Self::InterfaceMethodRef(*class_index, *name_and_type_index),
            Constant::NameAndType {
                name_index,
                descriptor_index,
            } => Self::NameAndType(*name_index, *descriptor_index),
            Constant::MethodHandle {
                reference_kind,
                reference_index,
            } => Self::MethodHandle(reference_kind.kind(), *reference_index),
            Constant::MethodType(index) => Self::MethodType(*index),
            Constant::Dynamic {
                bootstrap_method_attr_index,
                name_and_type_index,
            } => Self::Dynamic(*bootstrap_method_attr_index, *name_and_type_index),
            Constant::InvokeDynamic {
                bootstrap_method_attr_index,
                name_and_type_index,
            } => Self::InvokeDynamic(*bootstrap_method_attr_index, *name_and_type_index),
            Constant::Module(index) => Self::Module(*index),
            Constant::Package(index) => Self::Package(*index),
        }
    }
}

fn constant_pool_error(context: &str, error: impl std::fmt::Display) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, format!("{context}: {error}"))
}

fn import_constant(
    source_index: u16,
    source: &ConstantPool<'static>,
    target: &mut ConstantPool<'static>,
    target_constants: &mut HashMap<ConstantKey, u16>,
    indexes: &mut HashMap<u16, u16>,
    bootstrap_method_offset: u16,
) -> io::Result<u16> {
    if let Some(index) = indexes.get(&source_index) {
        return Ok(*index);
    }

    let constant = source
        .try_get(source_index)
        .map_err(|error| constant_pool_error("invalid incoming constant-pool reference", error))?
        .clone()
        .into_owned();
    let imported = match constant {
        Constant::Class(index) => Constant::Class(import_constant(
            index,
            source,
            target,
            target_constants,
            indexes,
            bootstrap_method_offset,
        )?),
        Constant::String(index) => Constant::String(import_constant(
            index,
            source,
            target,
            target_constants,
            indexes,
            bootstrap_method_offset,
        )?),
        Constant::MethodType(index) => Constant::MethodType(import_constant(
            index,
            source,
            target,
            target_constants,
            indexes,
            bootstrap_method_offset,
        )?),
        Constant::Module(index) => Constant::Module(import_constant(
            index,
            source,
            target,
            target_constants,
            indexes,
            bootstrap_method_offset,
        )?),
        Constant::Package(index) => Constant::Package(import_constant(
            index,
            source,
            target,
            target_constants,
            indexes,
            bootstrap_method_offset,
        )?),
        Constant::FieldRef {
            class_index,
            name_and_type_index,
        } => Constant::FieldRef {
            class_index: import_constant(
                class_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
            name_and_type_index: import_constant(
                name_and_type_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
        },
        Constant::MethodRef {
            class_index,
            name_and_type_index,
        } => Constant::MethodRef {
            class_index: import_constant(
                class_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
            name_and_type_index: import_constant(
                name_and_type_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
        },
        Constant::InterfaceMethodRef {
            class_index,
            name_and_type_index,
        } => Constant::InterfaceMethodRef {
            class_index: import_constant(
                class_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
            name_and_type_index: import_constant(
                name_and_type_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
        },
        Constant::NameAndType {
            name_index,
            descriptor_index,
        } => Constant::NameAndType {
            name_index: import_constant(
                name_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
            descriptor_index: import_constant(
                descriptor_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
        },
        Constant::MethodHandle {
            reference_kind,
            reference_index,
        } => Constant::MethodHandle {
            reference_kind,
            reference_index: import_constant(
                reference_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
        },
        Constant::Dynamic {
            bootstrap_method_attr_index,
            name_and_type_index,
        } => Constant::Dynamic {
            bootstrap_method_attr_index: bootstrap_method_attr_index
                .checked_add(bootstrap_method_offset)
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "merged bootstrap-method index exceeds the JVM limit",
                    )
                })?,
            name_and_type_index: import_constant(
                name_and_type_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
        },
        Constant::InvokeDynamic {
            bootstrap_method_attr_index,
            name_and_type_index,
        } => Constant::InvokeDynamic {
            bootstrap_method_attr_index: bootstrap_method_attr_index
                .checked_add(bootstrap_method_offset)
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "merged bootstrap-method index exceeds the JVM limit",
                    )
                })?,
            name_and_type_index: import_constant(
                name_and_type_index,
                source,
                target,
                target_constants,
                indexes,
                bootstrap_method_offset,
            )?,
        },
        primitive => primitive,
    };

    let key = ConstantKey::from(&imported);
    let target_index = if let Some(index) = target_constants.get(&key) {
        *index
    } else {
        let index = target
            .add(imported)
            .map_err(|error| constant_pool_error("merged JVM constant pool is full", error))?;
        target_constants.insert(key, index);
        index
    };
    indexes.insert(source_index, target_index);
    Ok(target_index)
}

fn import_constant_pool(
    source: &ConstantPool<'static>,
    target: &mut ConstantPool<'static>,
    bootstrap_method_offset: u16,
) -> io::Result<HashMap<u16, u16>> {
    let mut indexes = HashMap::new();
    let mut target_constants = HashMap::with_capacity(target.len() + source.len());
    for raw_index in 1..=target.len() {
        let Ok(index) = u16::try_from(raw_index) else {
            continue;
        };
        if let Ok(constant) = target.try_get(index) {
            target_constants
                .entry(ConstantKey::from(constant))
                .or_insert(index);
        }
    }
    for raw_index in 1..=source.len() {
        let index = u16::try_from(raw_index).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "incoming JVM constant pool exceeds the index limit",
            )
        })?;
        if source.try_get(index).is_ok() {
            import_constant(
                index,
                source,
                target,
                &mut target_constants,
                &mut indexes,
                bootstrap_method_offset,
            )?;
        }
    }
    Ok(indexes)
}

fn remapped_constant_index(index: u16, indexes: &HashMap<u16, u16>) -> io::Result<u16> {
    indexes.get(&index).copied().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("missing imported constant-pool index for #{index}"),
        )
    })
}

fn remap_verification_type(
    verification_type: &mut VerificationType,
    indexes: &HashMap<u16, u16>,
) -> io::Result<()> {
    if let VerificationType::Object { cpool_index } = verification_type {
        *cpool_index = remapped_constant_index(*cpool_index, indexes)?;
    }
    Ok(())
}

fn remap_stack_frame(frame: &mut StackFrame, indexes: &HashMap<u16, u16>) -> io::Result<()> {
    match frame {
        StackFrame::SameLocals1StackItemFrame { stack, .. }
        | StackFrame::SameLocals1StackItemFrameExtended { stack, .. } => {
            for item in stack {
                remap_verification_type(item, indexes)?;
            }
        }
        StackFrame::AppendFrame { locals, .. } => {
            for item in locals {
                remap_verification_type(item, indexes)?;
            }
        }
        StackFrame::FullFrame { locals, stack, .. } => {
            for item in locals.iter_mut().chain(stack.iter_mut()) {
                remap_verification_type(item, indexes)?;
            }
        }
        StackFrame::SameFrame { .. }
        | StackFrame::ChopFrame { .. }
        | StackFrame::SameFrameExtended { .. } => {}
    }
    Ok(())
}

fn remap_instruction(instruction: &mut Instruction, indexes: &HashMap<u16, u16>) -> io::Result<()> {
    match instruction {
        Instruction::Ldc(index) => {
            let shifted = remapped_constant_index(u16::from(*index), indexes)?;
            if let Ok(short_index) = u8::try_from(shifted) {
                *index = short_index;
            } else {
                *instruction = Instruction::Ldc_w(shifted);
            }
        }
        Instruction::Ldc_w(index)
        | Instruction::Ldc2_w(index)
        | Instruction::Getstatic(index)
        | Instruction::Putstatic(index)
        | Instruction::Getfield(index)
        | Instruction::Putfield(index)
        | Instruction::Invokevirtual(index)
        | Instruction::Invokespecial(index)
        | Instruction::Invokestatic(index)
        | Instruction::Invokedynamic(index)
        | Instruction::New(index)
        | Instruction::Anewarray(index)
        | Instruction::Checkcast(index)
        | Instruction::Instanceof(index) => *index = remapped_constant_index(*index, indexes)?,
        Instruction::Invokeinterface(index, _) | Instruction::Multianewarray(index, _) => {
            *index = remapped_constant_index(*index, indexes)?;
        }
        _ => {}
    }
    Ok(())
}

fn instruction_byte_offsets(instructions: &[Instruction]) -> io::Result<Vec<u16>> {
    let mut bytes = Cursor::new(Vec::new());
    let mut offsets = Vec::with_capacity(instructions.len() + 1);
    for instruction in instructions {
        offsets.push(u16::try_from(bytes.position()).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "JVM method exceeds the bytecode offset limit",
            )
        })?);
        instruction.to_bytes(&mut bytes).map_err(|error| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("could not measure JVM instruction: {error}"),
            )
        })?;
    }
    offsets.push(u16::try_from(bytes.position()).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "JVM method exceeds the bytecode offset limit",
        )
    })?);
    Ok(offsets)
}

fn remap_code_offset(pc: u16, old_offsets: &[u16], new_offsets: &[u16]) -> io::Result<u16> {
    let instruction_index = old_offsets.binary_search(&pc).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("local-variable PC {pc} is not an instruction boundary"),
        )
    })?;
    new_offsets.get(instruction_index).copied().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "local-variable PC has no remapped instruction boundary",
        )
    })
}

fn remap_local_variable_ranges(
    attributes: &mut [Attribute],
    old_offsets: &[u16],
    new_offsets: &[u16],
) -> io::Result<()> {
    for attribute in attributes {
        if let Attribute::LocalVariableTable { variables, .. } = attribute {
            for variable in variables {
                let old_end = variable
                    .start_pc
                    .checked_add(variable.length)
                    .ok_or_else(|| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            "local-variable bytecode range overflows u16",
                        )
                    })?;
                let new_start = remap_code_offset(variable.start_pc, old_offsets, new_offsets)?;
                let new_end = remap_code_offset(old_end, old_offsets, new_offsets)?;
                variable.start_pc = new_start;
                variable.length = new_end.checked_sub(new_start).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "remapped local-variable bytecode range is reversed",
                    )
                })?;
            }
        }
    }
    Ok(())
}

fn remap_attribute(attribute: &mut Attribute, indexes: &HashMap<u16, u16>) -> io::Result<()> {
    match attribute {
        Attribute::Code {
            name_index,
            code,
            exception_table,
            attributes,
            ..
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            let old_byte_offsets = instruction_byte_offsets(code)?;
            for instruction in code.iter_mut() {
                remap_instruction(instruction, indexes)?;
            }
            let new_byte_offsets = instruction_byte_offsets(code)?;
            remap_local_variable_ranges(attributes, &old_byte_offsets, &new_byte_offsets)?;
            for exception in exception_table {
                if exception.catch_type != 0 {
                    exception.catch_type = remapped_constant_index(exception.catch_type, indexes)?;
                }
            }
            for nested in attributes {
                remap_attribute(nested, indexes)?;
            }
        }
        Attribute::StackMapTable { name_index, frames } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            for frame in frames {
                remap_stack_frame(frame, indexes)?;
            }
        }
        Attribute::Exceptions {
            name_index,
            exception_indexes,
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            for index in exception_indexes {
                *index = remapped_constant_index(*index, indexes)?;
            }
        }
        Attribute::Signature {
            name_index,
            signature_index,
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            *signature_index = remapped_constant_index(*signature_index, indexes)?;
        }
        Attribute::LineNumberTable { name_index, .. }
        | Attribute::Synthetic { name_index }
        | Attribute::Deprecated { name_index } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
        }
        Attribute::LocalVariableTable {
            name_index,
            variables,
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            for variable in variables {
                variable.name_index = remapped_constant_index(variable.name_index, indexes)?;
                variable.descriptor_index =
                    remapped_constant_index(variable.descriptor_index, indexes)?;
            }
        }
        Attribute::MethodParameters {
            name_index,
            parameters,
        } => {
            *name_index = remapped_constant_index(*name_index, indexes)?;
            for parameter in parameters {
                if parameter.name_index != 0 {
                    parameter.name_index = remapped_constant_index(parameter.name_index, indexes)?;
                }
            }
        }
        other => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "cannot merge generated JVM method with unsupported {} attribute",
                    other.name()
                ),
            ));
        }
    }
    Ok(())
}

fn class_file_from_data(data: &[u8]) -> io::Result<ClassFile<'static>> {
    ClassFile::from_bytes(data).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("invalid JVM class while merging generic specializations: {error}"),
        )
    })
}

fn method_identity(
    class_file: &ClassFile<'_>,
    method_index: usize,
) -> io::Result<(String, String)> {
    let method = &class_file.methods[method_index];
    let name = class_file
        .constant_pool
        .try_get_utf8(method.name_index)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.to_string()))?;
    let descriptor = class_file
        .constant_pool
        .try_get_utf8(method.descriptor_index)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.to_string()))?;
    Ok((name.to_rust_string(), descriptor.to_rust_string()))
}

fn interface_name(class_file: &ClassFile<'_>, interface_index: usize) -> io::Result<String> {
    let constant_index = *class_file.interfaces.get(interface_index).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("missing interface at index {interface_index}"),
        )
    })?;
    class_file
        .constant_pool
        .try_get_class(constant_index)
        .map(|name| name.to_rust_string())
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.to_string()))
}

fn merge_class_data(base_data: &[u8], incoming_data: &[u8]) -> io::Result<Vec<u8>> {
    let mut base = class_file_from_data(base_data).map_err(|error| {
        io::Error::new(
            error.kind(),
            format!("could not parse the accumulated base fragment: {error}"),
        )
    })?;
    let incoming = class_file_from_data(incoming_data).map_err(|error| {
        io::Error::new(
            error.kind(),
            format!("could not parse the incoming fragment: {error}"),
        )
    })?;
    if base.class_name().ok() != incoming.class_name().ok() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "cannot merge class files with different JVM names",
        ));
    }

    // A trait interface can receive static helper methods from an ordinary
    // holder fragment. Preserve the interface identity and never retain the
    // holder's synthetic constructor.
    let merged_is_interface = base.access_flags.contains(ClassAccessFlags::INTERFACE)
        || incoming.access_flags.contains(ClassAccessFlags::INTERFACE);
    let mut base_changed = false;
    if merged_is_interface {
        if incoming.access_flags.contains(ClassAccessFlags::INTERFACE)
            && base.access_flags != incoming.access_flags
        {
            base.access_flags = incoming.access_flags;
            base_changed = true;
        }
        let original_method_count = base.methods.len();
        let mut retained_methods = Vec::with_capacity(original_method_count);
        for index in 0..original_method_count {
            if method_identity(&base, index)?.0 != "<init>" {
                retained_methods.push(base.methods[index].clone());
            }
        }
        base.methods = retained_methods;
        base_changed |= base.methods.len() != original_method_count;
    }

    let mut existing_methods = HashSet::new();
    for index in 0..base.methods.len() {
        existing_methods.insert(method_identity(&base, index)?);
    }
    let mut missing_method_indexes = Vec::new();
    for index in 0..incoming.methods.len() {
        let identity = method_identity(&incoming, index)?;
        if !(merged_is_interface && identity.0 == "<init>") && !existing_methods.contains(&identity)
        {
            missing_method_indexes.push(index);
        }
    }

    let mut existing_interfaces = HashSet::new();
    for index in 0..base.interfaces.len() {
        existing_interfaces.insert(interface_name(&base, index)?);
    }
    let mut missing_interface_indexes = Vec::new();
    for index in 0..incoming.interfaces.len() {
        if !existing_interfaces.contains(&interface_name(&incoming, index)?) {
            missing_interface_indexes.push(index);
        }
    }

    if missing_method_indexes.is_empty() && missing_interface_indexes.is_empty() {
        if !base_changed {
            return Ok(base_data.to_vec());
        }
        let mut merged = Vec::new();
        base.to_bytes(&mut merged).map_err(|error| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("failed to serialize merged JVM class: {error}"),
            )
        })?;
        return Ok(merged);
    }

    let base_bootstrap_count = base
        .attributes
        .iter()
        .find_map(|attribute| match attribute {
            Attribute::BootstrapMethods { methods, .. } => Some(methods.len()),
            _ => None,
        })
        .unwrap_or(0);
    let bootstrap_method_offset = u16::try_from(base_bootstrap_count).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "base class has too many bootstrap methods",
        )
    })?;
    let incoming_bootstrap_methods = incoming.attributes.iter().find_map(|attribute| {
        let Attribute::BootstrapMethods {
            name_index,
            methods,
        } = attribute
        else {
            return None;
        };
        Some((*name_index, methods.clone()))
    });

    let constant_indexes = import_constant_pool(
        &incoming.constant_pool,
        &mut base.constant_pool,
        bootstrap_method_offset,
    )?;

    for index in missing_interface_indexes {
        base.interfaces.push(remapped_constant_index(
            incoming.interfaces[index],
            &constant_indexes,
        )?);
    }

    if let Some((name_index, mut methods)) = incoming_bootstrap_methods {
        for BootstrapMethod {
            bootstrap_method_ref,
            arguments,
        } in &mut methods
        {
            *bootstrap_method_ref =
                remapped_constant_index(*bootstrap_method_ref, &constant_indexes)?;
            for argument in arguments {
                *argument = remapped_constant_index(*argument, &constant_indexes)?;
            }
        }
        if let Some(Attribute::BootstrapMethods {
            methods: base_methods,
            ..
        }) = base
            .attributes
            .iter_mut()
            .find(|attribute| matches!(attribute, Attribute::BootstrapMethods { .. }))
        {
            base_methods.extend(methods);
        } else {
            base.attributes.push(Attribute::BootstrapMethods {
                name_index: remapped_constant_index(name_index, &constant_indexes)?,
                methods,
            });
        }
    }

    for index in missing_method_indexes {
        let mut method = incoming.methods[index].clone();
        method.name_index = remapped_constant_index(method.name_index, &constant_indexes)?;
        method.descriptor_index =
            remapped_constant_index(method.descriptor_index, &constant_indexes)?;
        for attribute in &mut method.attributes {
            remap_attribute(attribute, &constant_indexes)?;
        }
        base.methods.push(method);
    }

    let mut merged = Vec::new();
    base.to_bytes(&mut merged).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("failed to serialize merged JVM class: {error}"),
        )
    })?;
    Ok(merged)
}

fn merge_duplicate_classes(classes: Vec<ClassInfo>) -> io::Result<Vec<ClassInfo>> {
    let mut positions: HashMap<String, usize> = HashMap::new();
    let mut groups: Vec<Vec<ClassInfo>> = Vec::new();
    for class_info in classes {
        if let Some(&index) = positions.get(&class_info.jar_entry_name) {
            groups[index].push(class_info);
        } else {
            positions.insert(class_info.jar_entry_name.clone(), groups.len());
            groups.push(vec![class_info]);
        }
    }

    groups
        .into_iter()
        .map(|mut fragments| {
            // Preserve the largest fragment's compact constant indexes. This
            // avoids growing its near-limit methods through ldc-to-ldc_w remaps.
            if class_fragments_can_be_reordered(&fragments)? {
                fragments.sort_by(|left, right| right.data.len().cmp(&left.data.len()));
            }
            let mut merged = fragments.remove(0);
            for fragment in fragments {
                merged.data = merge_class_data(&merged.data, &fragment.data).map_err(|error| {
                    io::Error::new(
                        error.kind(),
                        format!(
                            "failed to merge duplicate JVM class {}: {error}",
                            merged.jar_entry_name
                        ),
                    )
                })?;
            }
            Ok(merged)
        })
        .collect()
}

fn class_fragments_can_be_reordered(fragments: &[ClassInfo]) -> io::Result<bool> {
    let mut methods = HashSet::new();
    for fragment in fragments {
        let class_file = class_file_from_data(&fragment.data)?;
        if !class_file.fields.is_empty() {
            return Ok(false);
        }
        for index in 0..class_file.methods.len() {
            let identity = method_identity(&class_file, index)?;
            if identity.0 != "<init>" && !methods.insert(identity) {
                return Ok(false);
            }
        }
    }
    Ok(true)
}

struct InstrumentationTimer {
    stage: &'static str,
    start: Instant,
}

impl InstrumentationTimer {
    fn new(stage: &'static str) -> Self {
        Self {
            stage,
            start: Instant::now(),
        }
    }
}

impl Drop for InstrumentationTimer {
    fn drop(&mut self) {
        record_instrumentation_duration(self.stage, self.start.elapsed());
    }
}

fn json_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn json_optional_string(value: Option<&str>) -> String {
    value.map(json_string).unwrap_or_else(|| "null".to_string())
}

fn record_instrumentation_duration(stage: &'static str, duration: Duration) {
    let Some(path) = env::var_os("RCGJ_INSTRUMENT_PATH") else {
        return;
    };
    if path.as_os_str().is_empty() {
        return;
    }
    let path = PathBuf::from(path);
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }

    let test = env::var("RCGJ_INSTRUMENT_TEST").ok();
    let mode = env::var("RCGJ_INSTRUMENT_MODE").ok();
    let seconds = duration.as_secs_f64();
    let line = format!(
        "{{\"schema_version\":1,\"kind\":\"phase\",\"stage\":{},\"crate_name\":null,\"item\":null,\"seconds\":{:.9},\"millis\":{:.6},\"pid\":{},\"test\":{},\"mode\":{}}}\n",
        json_string(stage),
        seconds,
        seconds * 1000.0,
        std::process::id(),
        json_optional_string(test.as_deref()),
        json_optional_string(mode.as_deref()),
    );

    if let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) {
        let _ = file.write_all(line.as_bytes());
    }
}

fn msvc_output_path(arg: &str) -> Option<&str> {
    let prefix = arg.get(..5)?;
    prefix.eq_ignore_ascii_case("/out:").then(|| &arg[5..])
}

fn jar_output_path(mut output_name: String) -> String {
    if output_name.to_ascii_lowercase().ends_with(".exe") {
        output_name.truncate(output_name.len() - ".exe".len());
    }
    if !output_name.to_ascii_lowercase().ends_with(".jar") {
        output_name.push_str(".jar");
    }
    output_name
}

fn parse_response_lines(content: &str, msvc_quoting: bool) -> Vec<String> {
    content
        .lines()
        .filter_map(|line| {
            let line = line.trim_end_matches('\r');
            if line.is_empty() {
                return None;
            }

            let line = if msvc_quoting {
                line.strip_prefix('"')
                    .and_then(|line| line.strip_suffix('"'))
                    .unwrap_or(line)
            } else {
                line
            };

            let mut parsed = String::with_capacity(line.len());
            let mut chars = line.chars().peekable();
            while let Some(character) = chars.next() {
                if character == '\\' {
                    if msvc_quoting {
                        if chars.peek() == Some(&'"') {
                            parsed.push(chars.next().unwrap());
                        } else {
                            parsed.push(character);
                        }
                    } else if let Some(escaped) = chars.next() {
                        parsed.push(escaped);
                    } else {
                        parsed.push(character);
                    }
                } else {
                    parsed.push(character);
                }
            }
            Some(parsed)
        })
        .collect()
}

fn read_response_file(path: &Path) -> io::Result<Vec<String>> {
    let bytes = fs::read(path)?;
    if bytes.starts_with(&[0xff, 0xfe]) {
        let body = &bytes[2..];
        if body.len() % 2 != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "UTF-16 response file has an odd byte length",
            ));
        }
        let units: Vec<u16> = body
            .chunks_exact(2)
            .map(|bytes| u16::from_le_bytes([bytes[0], bytes[1]]))
            .collect();
        let content = String::from_utf16(&units)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
        Ok(parse_response_lines(&content, true))
    } else {
        let content =
            std::str::from_utf8(bytes.strip_prefix(&[0xef, 0xbb, 0xbf]).unwrap_or(&bytes))
                .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
        Ok(parse_response_lines(content, false))
    }
}

fn linker_args() -> io::Result<Vec<String>> {
    let mut command_line = env::args();
    let mut expanded = vec![command_line.next().unwrap_or_else(|| "java-linker".into())];
    for arg in command_line {
        if let Some(path) = arg.strip_prefix('@') {
            expanded.extend(read_response_file(Path::new(path))?);
        } else {
            expanded.push(arg);
        }
    }
    Ok(expanded)
}

fn main() -> Result<(), i32> {
    let args = linker_args().map_err(|error| {
        eprintln!("Error: Failed to read linker response file: {error}");
        1
    })?;
    if args.len() < 3 {
        eprintln!("Usage: java-linker <input_files...> -o <output_jar_file>");
        return Err(1);
    }
    let _linker_timer = InstrumentationTimer::new("java-linker");

    let mut input_class_files: Vec<String> = Vec::new();
    let mut input_class_bundles: Vec<String> = Vec::new();
    let mut input_jar_files: Vec<String> = Vec::new(); // Separate JARs
    let mut input_rlib_files: Vec<String> = Vec::new();
    let mut output_file: Option<String> = None;

    // --- Argument Parsing ---
    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        if arg == "-o" {
            if i + 1 < args.len() {
                output_file = Some(jar_output_path(args[i + 1].clone()));
                i += 2;
            } else {
                eprintln!("Error: -o flag requires an output file path");
                return Err(1);
            }
        } else if let Some(output_name) = msvc_output_path(arg) {
            if output_name.is_empty() {
                eprintln!("Error: /OUT: flag requires an output file path");
                return Err(1);
            }
            output_file = Some(jar_output_path(output_name.to_owned()));
            i += 1;
        } else if !arg.starts_with('-') {
            // Collect potential input files, differentiating classes and JARs
            if arg.ends_with(".class") {
                input_class_files.push(arg.clone());
                i += 1;
            } else if arg.ends_with(".jvmbundle") {
                input_class_bundles.push(arg.clone());
                i += 1;
            } else if arg.ends_with(".jar") {
                input_jar_files.push(arg.clone());
                i += 1;
            } else if arg.ends_with(".rlib") {
                input_rlib_files.push(arg.clone());
                i += 1;
            } else {
                // smth native - not useful to us
                i += 1; // Move to the next argument
            }
        } else {
            i += 1;
        }
    }

    if input_class_files.is_empty()
        && input_class_bundles.is_empty()
        && input_jar_files.is_empty()
        && input_rlib_files.is_empty()
    {
        eprintln!("Error: No JVM input files provided.");
        return Err(1);
    }

    let output_file_path = match output_file {
        Some(path) => path,
        None => {
            eprintln!("Error: Output file (-o or /OUT:) not specified.");
            return Err(1);
        }
    };

    // Load generated classes once. They are retained for duplicate merging and
    // final JAR output, so scanning them again here only adds I/O and parsing.
    let app_classes = collect_input_classes(
        &input_class_files,
        &input_class_bundles,
        &input_rlib_files,
    )
    .map_err(|e| {
        eprintln!("Error collecting JVM classes: {e}");
        1
    })?;
    let main_classes = find_main_classes(&app_classes).map_err(|e| {
        eprintln!("Error during main class scan: {}", e);
        1
    })?;
    if main_classes.len() > 1 {
        eprintln!("Error: Multiple entry-point classes found:");
        for c in main_classes {
            eprintln!("  - {}", c);
        }
        eprintln!("Specify the main class explicitly or ensure only one exists.");
        return Err(1);
    }
    let main_class_name = main_classes.into_iter().next();

    create_jar(
        app_classes,
        &input_jar_files,
        &output_file_path,
        main_class_name.as_deref(),
    )
    .map_err(|e| {
        eprintln!("Error creating JAR file: {}", e);
        1 // Propagate error code
    })?;

    // Don't print success message if used as a linker, rustc handles that.
    // println!("JAR file created successfully: {}", output_file_path);
    Ok(())
}

fn find_main_classes(classes: &[ClassInfo]) -> io::Result<Vec<String>> {
    let mut main_classes = Vec::new();
    let main_method_name = "main";
    let main_method_descriptor = "([Ljava/lang/String;)V";

    for class_info in classes {
        if let Some(class_name) = check_class_data_for_main(
            &class_info.data,
            main_method_name,
            main_method_descriptor,
        )? {
            main_classes.push(class_name);
        }
    }
    Ok(main_classes)
}

fn check_class_data_for_main(
    data: &[u8],
    main_method_name: &str,
    main_method_descriptor: &str,
) -> io::Result<Option<String>> {
    let class_file = match ClassFile::from_bytes(data) {
        Ok(cf) => cf,
        Err(_e) => {
            // Ignore parse error details for this check
            // Treat parse failure as "no main method found in this file"
            // eprintln!("Debug (check_class_data): Parse error: {}", e); // Optional debug
            return Ok(None);
        }
    };

    for method in &class_file.methods {
        let flags = &method.access_flags;
        if flags.contains(MethodAccessFlags::PUBLIC) && flags.contains(MethodAccessFlags::STATIC) {
            // Avoid panics if constant pool is malformed, treat as not found
            let name = class_file
                .constant_pool
                .try_get_utf8(method.name_index)
                .ok()
                .map(|name| name.to_rust_string());
            let descriptor = class_file
                .constant_pool
                .try_get_utf8(method.descriptor_index)
                .ok()
                .map(|descriptor| descriptor.to_rust_string());

            if let (Some(n), Some(d)) = (name, descriptor) {
                if n == main_method_name && d == main_method_descriptor {
                    return match class_file.class_name() {
                        Ok(class_name_ref) => {
                            Ok(Some(class_name_ref.to_rust_string().replace('/', ".")))
                        }
                        Err(e) => {
                            eprintln!(
                                "Warning (check_class_data): Found main method but failed to get class name: {}",
                                e
                            );
                            Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!(
                                    "Failed to get class name after finding main method: {}",
                                    e
                                ),
                            ))
                        }
                    };
                }
            }
        }
    }
    Ok(None)
}

// --- create_jar ---
fn create_jar(
    app_classes: Vec<ClassInfo>,
    input_jar_files: &[String],
    final_output_jar_path: &str,
    main_class_name: Option<&str>,
) -> io::Result<()> {
    let app_classes = merge_duplicate_classes(app_classes)?;

    // Input JARs are bundled into the final artifact alongside generated classes.
    let library_jar_paths: Vec<PathBuf> = input_jar_files.iter().map(PathBuf::from).collect();

    if app_classes.is_empty() && library_jar_paths.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "No input .class or .jar files specified.",
        ));
    }

    let temp_dir = tempdir()?;
    let final_jar_temp_path = temp_dir.path().join("output.jar");
    write_final_jar(
        &app_classes,
        &library_jar_paths,
        &final_jar_temp_path,
        main_class_name,
    )?;

    if let Some(parent_dir) = Path::new(final_output_jar_path).parent() {
        fs::create_dir_all(parent_dir)?;
    }
    match rename(&final_jar_temp_path, final_output_jar_path) {
        Ok(_) => {
            //println!("Moved temporary JAR to final destination.");
        }
        Err(e) => {
            // Error cross-device link might occur, fall back to copy
            if e.kind() == io::ErrorKind::CrossesDevices {
                eprintln!(
                    "Warning: Failed to rename temporary JAR across devices ({}). Attempting copy.",
                    e
                );
                fs::copy(&final_jar_temp_path, final_output_jar_path)?;
                // We might want to manually clean up the source temp file after copy, but tempdir should handle it on drop.
            } else {
                eprintln!(
                    "Error: Failed to move temporary JAR to final destination: {}",
                    e
                );
                // Preserve the temp dir for inspection
                let preserved_path = temp_dir.into_path();
                eprintln!(
                    "Temporary JAR preserved at: {}",
                    final_jar_temp_path.display()
                );
                eprintln!("Preserved Directory: {}", preserved_path.display());
                return Err(e);
            }
        }
    }

    Ok(())
}

fn collect_input_classes(
    input_class_files: &[String],
    input_class_bundles: &[String],
    input_rlib_files: &[String],
) -> io::Result<Vec<ClassInfo>> {
    let re_strip_hash = Regex::new(r"^(?P<name>[^-]+)(?:-[0-9a-fA-F]+)?\.class$").unwrap();
    let mut classes = Vec::new();
    for path_str in input_class_files {
        let path = Path::new(path_str);
        if !path.is_file() {
            eprintln!("Warning: Input class is not a file: {path_str}. Skipping.");
            continue;
        }
        let file_name = path
            .file_name()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid class path"))?
            .to_string_lossy();
        let base_name = re_strip_hash
            .captures(&file_name)
            .and_then(|caps| caps.name("name").map(|name| format!("{}.class", name.as_str())))
            .unwrap_or_else(|| file_name.to_string());
        classes.push(class_info_from_class_data(fs::read(path)?, base_name));
    }
    for path_str in input_class_bundles {
        let path = Path::new(path_str);
        if !path.is_file() {
            eprintln!("Warning: Input class bundle is not a file: {path_str}. Skipping.");
            continue;
        }
        classes.extend(collect_class_bundle(path)?);
    }
    for path_str in input_rlib_files {
        let path = Path::new(path_str);
        if !path.is_file() {
            eprintln!("Warning: Input rlib is not a file: {path_str}. Skipping.");
            continue;
        }
        classes.extend(collect_rlib_classes(path)?);
    }
    Ok(classes)
}

fn write_final_jar(
    app_classes: &[ClassInfo],
    library_jar_paths: &[PathBuf],
    output_path: &Path,
    main_class_name: Option<&str>,
) -> io::Result<()> {
    let output_file = fs::File::create(output_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let options = SimpleFileOptions::default()
        .compression_method(CompressionMethod::DEFLATE)
        .unix_permissions(0o644);
    let mut seen_entries = HashSet::new();

    zip_writer.start_file("META-INF/MANIFEST.MF", options)?;
    zip_writer.write_all(create_manifest_content(main_class_name).as_bytes())?;
    seen_entries.insert("META-INF/MANIFEST.MF".to_string());
    seen_entries.insert("META-INF/".to_string());

    for class_info in app_classes {
        if seen_entries.insert(class_info.jar_entry_name.clone()) {
            zip_writer.start_file(&class_info.jar_entry_name, options)?;
            zip_writer.write_all(&class_info.data)?;
        }
    }
    for library_jar_path in library_jar_paths {
        copy_jar_entries(library_jar_path, &mut zip_writer, &mut seen_entries)?;
    }
    zip_writer.finish()?;
    Ok(())
}

fn class_info_from_class_data(data: Vec<u8>, fallback_name: String) -> ClassInfo {
    let class_file = ClassFile::from_bytes(&data).ok();

    let jar_entry_name = class_name_from_header(&data)
        .or_else(|| {
            class_file.and_then(|class_file| {
                class_file
                    .class_name()
                    .ok()
                    .map(|class_name| class_name.to_rust_string())
            })
        })
        .map(|class_name| format!("{class_name}.class"))
        .unwrap_or(fallback_name);

    ClassInfo {
        jar_entry_name,
        data,
    }
}

fn class_name_from_header(data: &[u8]) -> Option<String> {
    fn read_u8(data: &[u8], offset: &mut usize) -> Option<u8> {
        let value = *data.get(*offset)?;
        *offset += 1;
        Some(value)
    }

    fn read_u16(data: &[u8], offset: &mut usize) -> Option<u16> {
        let bytes: [u8; 2] = data.get(*offset..*offset + 2)?.try_into().ok()?;
        *offset += 2;
        Some(u16::from_be_bytes(bytes))
    }

    fn skip(data: &[u8], offset: &mut usize, count: usize) -> Option<()> {
        data.get(*offset..*offset + count)?;
        *offset += count;
        Some(())
    }

    #[derive(Clone)]
    enum HeaderConstant {
        Other,
        Utf8(String),
        Class(u16),
    }

    if data.get(..4)? != b"\xca\xfe\xba\xbe" {
        return None;
    }
    let mut offset = 4;
    skip(data, &mut offset, 4)?; // minor_version and major_version
    let constant_pool_count = usize::from(read_u16(data, &mut offset)?);
    let mut constants = vec![HeaderConstant::Other; constant_pool_count];
    let mut index = 1usize;
    while index < constant_pool_count {
        let tag = read_u8(data, &mut offset)?;
        constants[index] = match tag {
            1 => {
                let length = usize::from(read_u16(data, &mut offset)?);
                let bytes = data.get(offset..offset + length)?;
                offset += length;
                HeaderConstant::Utf8(std::str::from_utf8(bytes).ok()?.to_string())
            }
            7 => HeaderConstant::Class(read_u16(data, &mut offset)?),
            3 | 4 => {
                skip(data, &mut offset, 4)?;
                HeaderConstant::Other
            }
            5 | 6 => {
                skip(data, &mut offset, 8)?;
                // Long and double constants occupy two constant-pool slots.
                index += 1;
                HeaderConstant::Other
            }
            8 | 16 | 19 | 20 => {
                skip(data, &mut offset, 2)?;
                HeaderConstant::Other
            }
            9 | 10 | 11 | 12 | 17 | 18 => {
                skip(data, &mut offset, 4)?;
                HeaderConstant::Other
            }
            15 => {
                skip(data, &mut offset, 3)?;
                HeaderConstant::Other
            }
            _ => return None,
        };
        index += 1;
    }

    skip(data, &mut offset, 2)?; // access_flags
    let this_class = usize::from(read_u16(data, &mut offset)?);
    let HeaderConstant::Class(name_index) = constants.get(this_class)? else {
        return None;
    };
    let HeaderConstant::Utf8(name) = constants.get(usize::from(*name_index))? else {
        return None;
    };
    Some(name.clone())
}

fn try_class_info_from_class_data(data: Vec<u8>, fallback_name: String) -> Option<ClassInfo> {
    let header_class_name = class_name_from_header(&data);
    let class_file = ClassFile::from_bytes(&data).ok();
    let jar_entry_name = header_class_name
        .or_else(|| {
            class_file.and_then(|class_file| {
                class_file
                    .class_name()
                    .ok()
                    .map(|class_name| class_name.to_rust_string())
            })
        })
        .map(|class_name| format!("{class_name}.class"))
        .unwrap_or(fallback_name);

    Some(ClassInfo {
        jar_entry_name,
        data,
    })
}

fn collect_class_bundle(path: &Path) -> io::Result<Vec<ClassInfo>> {
    let reader = BufReader::new(fs::File::open(path)?);
    collect_class_bundle_reader(reader, path)
}

fn collect_class_bundle_bytes(data: &[u8], path: &Path) -> io::Result<Vec<ClassInfo>> {
    collect_class_bundle_reader(Cursor::new(data), path)
}

fn collect_class_bundle_reader(
    mut reader: impl Read,
    path: &Path,
) -> io::Result<Vec<ClassInfo>> {
    let mut magic = [0u8; CLASS_BUNDLE_MAGIC.len()];
    reader.read_exact(&mut magic)?;
    if &magic != CLASS_BUNDLE_MAGIC {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("{} is not a JVM class bundle", path.display()),
        ));
    }
    let mut classes = Vec::new();
    loop {
        let mut name_len_bytes = [0u8; 4];
        match reader.read(&mut name_len_bytes[..1])? {
            0 => break,
            1 => reader.read_exact(&mut name_len_bytes[1..])?,
            _ => unreachable!(),
        }
        let name_len = u32::from_le_bytes(name_len_bytes) as usize;
        let mut class_len_bytes = [0u8; 8];
        reader.read_exact(&mut class_len_bytes)?;
        let class_len = usize::try_from(u64::from_le_bytes(class_len_bytes)).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("class length exceeds this host's address space in {}", path.display()),
            )
        })?;
        let mut name = vec![0; name_len];
        reader.read_exact(&mut name)?;
        let name = String::from_utf8(name)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
        let mut class = vec![0; class_len];
        reader.read_exact(&mut class)?;
        classes.push(ClassInfo {
            jar_entry_name: format!("{name}.class"),
            data: class,
        });
    }
    Ok(classes)
}

fn gnu_long_name(table: &[u8], offset_text: &str, path: &Path) -> io::Result<String> {
    let offset = offset_text.trim().parse::<usize>().map_err(|e| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "invalid GNU ar long-name offset '{}' in {}: {}",
                offset_text,
                path.display(),
                e
            ),
        )
    })?;

    if offset >= table.len() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "GNU ar long-name offset {} is outside table in {}",
                offset,
                path.display()
            ),
        ));
    }

    let tail = &table[offset..];
    let end = tail
        .windows(2)
        .position(|window| window == b"/\n")
        .unwrap_or(tail.len());
    Ok(String::from_utf8_lossy(&tail[..end]).to_string())
}

fn collect_rlib_classes(path: &Path) -> io::Result<Vec<ClassInfo>> {
    let archive = fs::read(path)?;
    if !archive.starts_with(b"!<arch>\n") {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("{} is not an ar archive", path.display()),
        ));
    }

    let mut classes = Vec::new();
    let mut offset = 8usize;
    let mut gnu_long_names = Vec::new();
    while offset + 60 <= archive.len() {
        let header = &archive[offset..offset + 60];
        let raw_name = String::from_utf8_lossy(&header[0..16]).trim().to_string();
        let size_text = String::from_utf8_lossy(&header[48..58]).trim().to_string();
        let size = size_text.parse::<usize>().map_err(|e| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "invalid ar member size '{}' in {}: {}",
                    size_text,
                    path.display(),
                    e
                ),
            )
        })?;

        if &header[58..60] != b"`\n" {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid ar member header in {}", path.display()),
            ));
        }

        let mut data_start = offset + 60;
        let mut data_len = size;
        let member_name = if raw_name == "/" {
            "__gnu_symbol_table__".to_string()
        } else if raw_name == "//" {
            "__gnu_long_names__".to_string()
        } else if let Some(name_offset_text) = raw_name.strip_prefix('/') {
            gnu_long_name(&gnu_long_names, name_offset_text, path)?
        } else if let Some(name_len_text) = raw_name.strip_prefix("#1/") {
            let name_len = name_len_text.trim().parse::<usize>().map_err(|e| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "invalid BSD ar name length '{}' in {}: {}",
                        name_len_text,
                        path.display(),
                        e
                    ),
                )
            })?;
            if data_start + name_len > archive.len() || name_len > data_len {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("invalid BSD ar name in {}", path.display()),
                ));
            }
            let name = String::from_utf8_lossy(&archive[data_start..data_start + name_len])
                .trim_end_matches('\0')
                .to_string();
            data_start += name_len;
            data_len -= name_len;
            name
        } else {
            raw_name.trim_end_matches('/').to_string()
        };

        if data_start + data_len > archive.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "ar member '{}' extends past end of {}",
                    member_name,
                    path.display()
                ),
            ));
        }

        let data = &archive[data_start..data_start + data_len];
        if member_name == "__gnu_long_names__" {
            gnu_long_names = data.to_vec();
        } else if data.starts_with(CLASS_BUNDLE_MAGIC) {
            classes.extend(collect_class_bundle_bytes(data, path)?);
        } else if member_name.ends_with(".class") || data.starts_with(b"\xca\xfe\xba\xbe") {
            if let Some(class_info) =
                try_class_info_from_class_data(data.to_vec(), member_name.clone())
            {
                classes.push(class_info);
            }
        }

        offset += 60 + size;
        if offset % 2 == 1 {
            offset += 1;
        }
    }

    Ok(classes)
}

#[cfg(test)]
fn merge_input_jars(
    app_jar_path: Option<&Path>,
    library_jar_paths: &[PathBuf],
    output_jar_path: &Path,
) -> io::Result<()> {
    let output_file = fs::File::create(output_jar_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let mut seen_entries = HashSet::new();

    // Compiled Rust classes are authoritative. In particular, a stale runtime
    // JAR must never shadow classes produced by compiling the real core crate.
    if let Some(app_jar) = app_jar_path {
        copy_jar_entries(app_jar, &mut zip_writer, &mut seen_entries)?;
    }

    for library_jar_path in library_jar_paths {
        copy_jar_entries(library_jar_path, &mut zip_writer, &mut seen_entries)?;
    }

    zip_writer.finish()?;
    Ok(())
}

fn copy_jar_entries<W: Write + Seek>(
    input_jar_path: &Path,
    zip_writer: &mut ZipWriter<W>,
    seen_entries: &mut HashSet<String>,
) -> io::Result<()> {
    let input_file = fs::File::open(input_jar_path)?;
    let reader = BufReader::new(input_file);
    let mut input_archive = ZipArchive::new(reader)?;

    for i in 0..input_archive.len() {
        let entry = input_archive.by_index_raw(i)?;
        let entry_name = entry.name().to_string();

        if entry_name == "META-INF/" || entry_name == "META-INF/MANIFEST.MF" {
            continue;
        }

        if seen_entries.insert(entry_name) {
            zip_writer.raw_copy_file(entry)?;
        }
    }

    Ok(())
}

fn create_manifest_content(main_class_name: Option<&str>) -> String {
    let mut manifest = String::new();
    manifest.push_str("Manifest-Version: 1.0\r\n");
    // Common practice to include Created-By
    manifest.push_str("Created-By: java-linker-rs (rust)\r\n");
    if let Some(main_class) = main_class_name {
        // Ensure FQN uses dots
        let main_class_fqn = main_class.replace('/', ".");
        manifest.push_str(&format!("Main-Class: {}\r\n", main_class_fqn));
    }
    // Crucial: Ensure the manifest ends with a blank line (CRLF CRLF)
    manifest.push_str("\r\n");
    manifest
}

#[cfg(test)]
mod tests {
    use super::{
        CLASS_BUNDLE_MAGIC, ClassInfo, class_file_from_data, collect_class_bundle_bytes,
        instruction_byte_offsets, interface_name, jar_output_path, merge_class_data,
        merge_duplicate_classes, merge_input_jars, method_identity, msvc_output_path,
        parse_response_lines, remap_local_variable_ranges, write_final_jar,
    };
    use ristretto_classfile::attributes::{
        Attribute, BootstrapMethod, Instruction, LocalVariableTable,
    };
    use ristretto_classfile::{
        ClassAccessFlags, ClassFile, Constant, ConstantPool, Method, MethodAccessFlags,
        ReferenceKind, Version,
    };
    use std::{collections::HashSet, fs::File, io::Read, io::Write};
    use tempfile::tempdir;
    use zip::{ZipArchive, write::SimpleFileOptions, write::ZipWriter};

    fn abstract_class_with_method(method_name: &str) -> Vec<u8> {
        abstract_class_with_method_and_interface(method_name, None)
    }

    fn interface_with_method(method_name: &str) -> Vec<u8> {
        let bytes = abstract_class_with_method(method_name);
        let mut class_file = class_file_from_data(&bytes).unwrap();
        class_file.access_flags =
            ClassAccessFlags::PUBLIC | ClassAccessFlags::ABSTRACT | ClassAccessFlags::INTERFACE;
        let mut bytes = Vec::new();
        class_file.to_bytes(&mut bytes).unwrap();
        bytes
    }

    fn holder_class_with_method_and_constructor(method_name: &str) -> Vec<u8> {
        let bytes = abstract_class_with_method(method_name);
        let mut class_file = class_file_from_data(&bytes).unwrap();
        let name_index = class_file.constant_pool.add_utf8("<init>").unwrap();
        let descriptor_index = class_file.constant_pool.add_utf8("()V").unwrap();
        class_file.methods.push(Method {
            access_flags: MethodAccessFlags::PUBLIC,
            name_index,
            descriptor_index,
            attributes: Vec::new(),
        });
        let mut bytes = Vec::new();
        class_file.to_bytes(&mut bytes).unwrap();
        bytes
    }

    fn abstract_class_with_method_and_interface(
        method_name: &str,
        interface_name: Option<&str>,
    ) -> Vec<u8> {
        let mut constant_pool = ConstantPool::default();
        let this_class = constant_pool.add_class("test/Generic").unwrap();
        let super_class = constant_pool.add_class("java/lang/Object").unwrap();
        let interfaces = interface_name
            .map(|name| vec![constant_pool.add_class(name).unwrap()])
            .unwrap_or_default();
        let name_index = constant_pool.add_utf8(method_name).unwrap();
        let descriptor_index = constant_pool.add_utf8("()V").unwrap();
        let class_file = ClassFile {
            version: Version::Java8 { minor: 0 },
            constant_pool,
            access_flags: ClassAccessFlags::PUBLIC
                | ClassAccessFlags::ABSTRACT
                | ClassAccessFlags::SUPER,
            this_class,
            super_class,
            interfaces,
            methods: vec![Method {
                access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT,
                name_index,
                descriptor_index,
                attributes: Vec::new(),
            }],
            ..Default::default()
        };
        let mut bytes = Vec::new();
        class_file.to_bytes(&mut bytes).unwrap();
        bytes
    }

    fn abstract_class_with_lambda_bootstrap(method_name: &str) -> Vec<u8> {
        let mut constant_pool = ConstantPool::default();
        let this_class = constant_pool.add_class("test/Generic").unwrap();
        let super_class = constant_pool.add_class("java/lang/Object").unwrap();
        let name_index = constant_pool.add_utf8(method_name).unwrap();
        let descriptor_index = constant_pool.add_utf8("()V").unwrap();
        let metafactory_class = constant_pool
            .add_class("java/lang/invoke/LambdaMetafactory")
            .unwrap();
        let metafactory = constant_pool
            .add_method_ref(
                metafactory_class,
                "metafactory",
                "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;",
            )
            .unwrap();
        let metafactory_handle = constant_pool
            .add_method_handle(ReferenceKind::InvokeStatic, metafactory)
            .unwrap();
        let sam_type = constant_pool.add_method_type("()V").unwrap();
        let target_class = constant_pool
            .add_class(format!("test/Target_{method_name}"))
            .unwrap();
        let target_method = constant_pool
            .add_method_ref(target_class, "run", "()V")
            .unwrap();
        let target_handle = constant_pool
            .add_method_handle(ReferenceKind::InvokeStatic, target_method)
            .unwrap();
        constant_pool
            .add_invoke_dynamic(0, "run", "()Ljava/lang/Runnable;")
            .unwrap();
        let bootstrap_name = constant_pool.add_utf8("BootstrapMethods").unwrap();
        let class_file = ClassFile {
            version: Version::Java8 { minor: 0 },
            constant_pool,
            access_flags: ClassAccessFlags::PUBLIC
                | ClassAccessFlags::ABSTRACT
                | ClassAccessFlags::SUPER,
            this_class,
            super_class,
            methods: vec![Method {
                access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT,
                name_index,
                descriptor_index,
                attributes: Vec::new(),
            }],
            attributes: vec![Attribute::BootstrapMethods {
                name_index: bootstrap_name,
                methods: vec![BootstrapMethod {
                    bootstrap_method_ref: metafactory_handle,
                    arguments: vec![sam_type, target_handle, sam_type],
                }],
            }],
            ..Default::default()
        };
        let mut bytes = Vec::new();
        class_file.to_bytes(&mut bytes).unwrap();
        bytes
    }

    fn class_with_near_limit_ldc_method() -> Vec<u8> {
        let mut constant_pool = ConstantPool::default();
        let this_class = constant_pool.add_class("test/Generic").unwrap();
        let super_class = constant_pool.add_class("java/lang/Object").unwrap();
        let name_index = constant_pool.add_utf8("large").unwrap();
        let descriptor_index = constant_pool.add_utf8("()V").unwrap();
        let code_name_index = constant_pool.add_utf8("Code").unwrap();
        let value_index = constant_pool.add_integer(123_456).unwrap();
        let value_index = u8::try_from(value_index).unwrap();
        let mut code = Vec::with_capacity(43_601);
        for _ in 0..21_800 {
            code.extend([Instruction::Ldc(value_index), Instruction::Pop]);
        }
        code.push(Instruction::Return);
        let class_file = ClassFile {
            version: Version::Java8 { minor: 0 },
            constant_pool,
            access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
            this_class,
            super_class,
            methods: vec![Method {
                access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
                name_index,
                descriptor_index,
                attributes: vec![Attribute::Code {
                    name_index: code_name_index,
                    max_stack: 1,
                    max_locals: 0,
                    code,
                    exception_table: Vec::new(),
                    attributes: Vec::new(),
                }],
            }],
            ..Default::default()
        };
        let mut bytes = Vec::new();
        class_file.to_bytes(&mut bytes).unwrap();
        bytes
    }

    fn class_with_large_constant_pool() -> Vec<u8> {
        let bytes = abstract_class_with_method("small");
        let mut class_file = class_file_from_data(&bytes).unwrap();
        for value in 0..260 {
            class_file.constant_pool.add_integer(value).unwrap();
        }
        let mut bytes = Vec::new();
        class_file.to_bytes(&mut bytes).unwrap();
        bytes
    }

    fn write_test_jar(path: &std::path::Path, name: &str, contents: &[u8]) {
        let mut writer = ZipWriter::new(File::create(path).unwrap());
        writer
            .start_file(name, SimpleFileOptions::default())
            .unwrap();
        writer.write_all(contents).unwrap();
        writer.finish().unwrap();
    }

    #[test]
    fn recognizes_msvc_output_argument_case_insensitively() {
        assert_eq!(
            msvc_output_path("/OUT:C:\\tmp\\app.exe"),
            Some("C:\\tmp\\app.exe")
        );
        assert_eq!(
            msvc_output_path("/out:C:\\tmp\\app.exe"),
            Some("C:\\tmp\\app.exe")
        );
        assert_eq!(msvc_output_path("/DEBUG"), None);
    }

    #[test]
    fn remaps_local_variable_ranges_when_ldc_widens() {
        let old_offsets = instruction_byte_offsets(&[Instruction::Ldc(1), Instruction::Return])
            .expect("old bytecode offsets");
        let new_offsets = instruction_byte_offsets(&[Instruction::Ldc_w(256), Instruction::Return])
            .expect("new bytecode offsets");
        let mut attributes = vec![Attribute::LocalVariableTable {
            name_index: 1,
            variables: vec![LocalVariableTable {
                start_pc: 2,
                length: 1,
                name_index: 2,
                descriptor_index: 3,
                index: 0,
            }],
        }];

        remap_local_variable_ranges(&mut attributes, &old_offsets, &new_offsets)
            .expect("remapped local variable range");

        let Attribute::LocalVariableTable { variables, .. } = &attributes[0] else {
            panic!("expected local variable table")
        };
        assert_eq!(variables[0].start_pc, 3);
        assert_eq!(variables[0].length, 1);
    }

    #[test]
    fn converts_native_output_names_to_jar_names() {
        assert_eq!(jar_output_path("target/app".into()), "target/app.jar");
        assert_eq!(jar_output_path("target/app.exe".into()), "target/app.jar");
        assert_eq!(jar_output_path("target/app.EXE".into()), "target/app.jar");
        assert_eq!(jar_output_path("target/app.jar".into()), "target/app.jar");
    }

    #[test]
    fn parses_rustc_msvc_response_file_lines() {
        let content = concat!(
            "\"C:\\project with spaces\\Main.class\"\n",
            "\"/OUT:C:\\project with spaces\\app.exe\"\n",
            "\"/PDBALTPATH:%_PDB%\"\n",
        );
        assert_eq!(
            parse_response_lines(content, true),
            [
                "C:\\project with spaces\\Main.class",
                "/OUT:C:\\project with spaces\\app.exe",
                "/PDBALTPATH:%_PDB%",
            ]
        );
    }

    #[test]
    fn parses_rustc_gnu_response_file_lines() {
        assert_eq!(
            parse_response_lines("path\\ with\\ spaces/Main.class\n-o\noutput\n", false),
            ["path with spaces/Main.class", "-o", "output"]
        );
    }

    #[test]
    fn reads_compiler_class_bundles() {
        let class = abstract_class_with_method("bundled");
        let name = b"test/Generic";
        let mut bundle = CLASS_BUNDLE_MAGIC.to_vec();
        bundle.extend_from_slice(&(name.len() as u32).to_le_bytes());
        bundle.extend_from_slice(&(class.len() as u64).to_le_bytes());
        bundle.extend_from_slice(name);
        bundle.extend_from_slice(&class);

        let classes = collect_class_bundle_bytes(&bundle, std::path::Path::new("test.jvmbundle"))
            .unwrap();
        assert_eq!(classes.len(), 1);
        assert_eq!(classes[0].jar_entry_name, "test/Generic.class");
        assert_eq!(classes[0].data, class);
    }

    #[test]
    fn merges_complementary_generic_class_methods() {
        let first = abstract_class_with_method("first");
        let second = abstract_class_with_method("second");
        let first_constant_count = class_file_from_data(&first).unwrap().constant_pool.len();
        let merged = merge_class_data(&first, &second).unwrap();
        let class_file = class_file_from_data(&merged).unwrap();
        let methods: Vec<_> = (0..class_file.methods.len())
            .map(|index| method_identity(&class_file, index).unwrap())
            .collect();

        assert_eq!(
            methods,
            [
                ("first".into(), "()V".into()),
                ("second".into(), "()V".into())
            ]
        );
        assert_eq!(
            class_file.constant_pool.len(),
            first_constant_count + 1,
            "merging should reuse every shared class, descriptor, and attribute constant"
        );
    }

    #[test]
    fn merge_keeps_near_limit_method_constant_indexes_compact() {
        let small = class_with_large_constant_pool();
        let large = class_with_near_limit_ldc_method();
        assert!(merge_class_data(&small, &large).is_err());

        let classes = vec![
            ClassInfo {
                jar_entry_name: "test/Generic.class".to_string(),
                data: small,
            },
            ClassInfo {
                jar_entry_name: "test/Generic.class".to_string(),
                data: large,
            },
        ];
        let merged = merge_duplicate_classes(classes).unwrap();
        assert_eq!(merged.len(), 1);
        assert_eq!(class_file_from_data(&merged[0].data).unwrap().methods.len(), 2);
    }

    #[test]
    fn merges_complementary_implemented_interfaces() {
        let first = abstract_class_with_method_and_interface("same", Some("test/First"));
        let second = abstract_class_with_method_and_interface("same", Some("test/Second"));
        let merged = merge_class_data(&first, &second).unwrap();
        let class_file = class_file_from_data(&merged).unwrap();
        let interfaces: Vec<_> = (0..class_file.interfaces.len())
            .map(|index| interface_name(&class_file, index).unwrap())
            .collect();

        assert_eq!(interfaces, ["test/First", "test/Second"]);
        assert_eq!(class_file.methods.len(), 1);
    }

    #[test]
    fn merging_trait_helpers_preserves_interface_and_drops_holder_constructor() {
        let holder = holder_class_with_method_and_constructor("helper");
        let interface = interface_with_method("next");

        for (base, incoming) in [(&holder, &interface), (&interface, &holder)] {
            let merged = merge_class_data(base, incoming).unwrap();
            let class_file = class_file_from_data(&merged).unwrap();
            let mut methods: Vec<_> = (0..class_file.methods.len())
                .map(|index| method_identity(&class_file, index).unwrap().0)
                .collect();
            methods.sort();

            assert!(
                class_file
                    .access_flags
                    .contains(ClassAccessFlags::INTERFACE)
            );
            assert_eq!(methods, ["helper", "next"]);
        }
    }

    #[test]
    fn merging_matching_trait_fragment_still_removes_holder_constructor() {
        let holder = holder_class_with_method_and_constructor("same");
        let interface = interface_with_method("same");
        let merged = merge_class_data(&holder, &interface).unwrap();
        let class_file = class_file_from_data(&merged).unwrap();
        let methods: Vec<_> = (0..class_file.methods.len())
            .map(|index| method_identity(&class_file, index).unwrap().0)
            .collect();

        assert!(
            class_file
                .access_flags
                .contains(ClassAccessFlags::INTERFACE)
        );
        assert_eq!(methods, ["same"]);
    }

    #[test]
    fn merges_and_reindexes_lambda_bootstrap_methods() {
        let first = abstract_class_with_lambda_bootstrap("first");
        let second = abstract_class_with_lambda_bootstrap("second");
        let merged = merge_class_data(&first, &second).unwrap();
        let class_file = class_file_from_data(&merged).unwrap();
        let bootstrap_methods = class_file
            .attributes
            .iter()
            .find_map(|attribute| match attribute {
                Attribute::BootstrapMethods { methods, .. } => Some(methods),
                _ => None,
            })
            .expect("merged BootstrapMethods attribute");
        assert_eq!(bootstrap_methods.len(), 2);

        let mut bootstrap_indexes = HashSet::new();
        for raw_index in 1..=class_file.constant_pool.len() {
            let Ok(index) = u16::try_from(raw_index) else {
                continue;
            };
            if let Ok(Constant::InvokeDynamic {
                bootstrap_method_attr_index,
                ..
            }) = class_file.constant_pool.try_get(index)
            {
                bootstrap_indexes.insert(*bootstrap_method_attr_index);
            }
        }
        assert_eq!(bootstrap_indexes, HashSet::from([0, 1]));
    }

    #[test]
    fn compiled_classes_take_precedence_over_runtime_jar_entries() {
        let directory = tempdir().unwrap();
        let app = directory.path().join("app.jar");
        let runtime = directory.path().join("runtime.jar");
        let output = directory.path().join("output.jar");
        let entry = "example/Owner.class";
        write_test_jar(&app, entry, b"compiled");
        write_test_jar(&runtime, entry, b"runtime");

        merge_input_jars(Some(&app), &[runtime], &output).unwrap();

        let mut archive = ZipArchive::new(File::open(output).unwrap()).unwrap();
        let mut contents = Vec::new();
        archive
            .by_name(entry)
            .unwrap()
            .read_to_end(&mut contents)
            .unwrap();
        assert_eq!(contents, b"compiled");
    }

    #[test]
    fn final_jar_is_written_directly_with_manifest_and_app_precedence() {
        let directory = tempdir().unwrap();
        let runtime = directory.path().join("runtime.jar");
        let output = directory.path().join("output.jar");
        let entry = "test/Generic.class";
        let class = abstract_class_with_method("compiled");
        write_test_jar(&runtime, entry, b"runtime");

        write_final_jar(
            &[ClassInfo {
                jar_entry_name: entry.to_string(),
                data: class.clone(),
            }],
            &[runtime],
            &output,
            Some("test.Generic"),
        )
        .unwrap();

        let mut archive = ZipArchive::new(File::open(output).unwrap()).unwrap();
        let mut contents = Vec::new();
        archive
            .by_name(entry)
            .unwrap()
            .read_to_end(&mut contents)
            .unwrap();
        assert_eq!(contents, class);
        contents.clear();
        archive
            .by_name("META-INF/MANIFEST.MF")
            .unwrap()
            .read_to_end(&mut contents)
            .unwrap();
        assert!(String::from_utf8(contents).unwrap().contains("Main-Class: test.Generic\r\n"));
    }
}
