// src/lower2/mod.rs

//! This module converts OOMIR into JVM bytecode.

use crate::oomir::{self, DataType};
use helpers::{get_cast_instructions, oomir_function_stack_floor};
use jvm_gen::{
    create_data_type_classfile_for_class, create_data_type_classfile_for_interface,
    create_default_constructor, create_slice_view_classfile, create_utf8_view_classfile,
    oomir_type_to_ristretto_field_type,
};
pub(crate) use translator::DebugInfoOptions;
use translator::FunctionTranslator;

use self::jvm::{
    ClassAccessFlags, ClassFile, FieldAccessFlags, MethodAccessFlags, Version,
    attributes::{Attribute, BootstrapMethod, Instruction, MaxStack},
};
use constant_pool::{InternedConstantPool, verify_no_duplicate_constants};
use consts::load_constant;
use rustc_middle::ty::TyCtxt;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    io::{BufWriter, Write},
    path::{Path, PathBuf},
    sync::{
        Arc, Condvar, Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

mod constant_pool;
mod consts;
mod helpers;
mod jvm;
mod jvm_gen;
mod large_methods;
mod optimise2;
mod stackmaps;
mod translator;

pub const F128_CLASS: &str = "org/rustlang/runtime/F128";
pub const I128_CLASS: &str = "org/rustlang/runtime/I128";
pub const U128_CLASS: &str = "org/rustlang/runtime/U128";

static OUTPUT_DIRECTORY_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Default)]
pub(crate) struct EmittedClassRegistry {
    variants: Mutex<HashMap<String, Vec<Arc<EmittedClassVariant>>>>,
}

struct EmittedClassVariant {
    hash: u64,
    len: usize,
    state: Mutex<EmittedClassState>,
    ready: Condvar,
}

enum EmittedClassState {
    Pending,
    Ready(Arc<[u8]>),
}

fn bytecode_hash(bytecode: &[u8]) -> u64 {
    bytecode.iter().fold(0xcbf29ce484222325u64, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3)
    })
}

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
            offset,
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
            offset: *offset,
            length: *length,
        },
        oomir::Constant::InternedPointer {
            identity,
            value,
            view_size,
            alignment,
            view_codec,
            pointee,
        } => oomir::Constant::InternedPointer {
            identity: identity.clone(),
            value: Box::new(create_constant_factory(
                cp,
                owner_class,
                value,
                methods,
                next_factory,
            )?),
            view_size: *view_size,
            alignment: *alignment,
            view_codec: view_codec.clone(),
            pointee: pointee.clone(),
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
                .map(|param| {
                    if constant_instruction_cost(param) > MAX_INLINE_CONSTANT_INSTRUCTIONS {
                        create_constant_factory(cp, owner_class, param, methods, next_factory)
                    } else {
                        Ok(param.clone())
                    }
                })
                .collect::<jvm::Result<Vec<_>>>()?,
        },
        oomir::Constant::StaticCall {
            owner_class: call_owner,
            method_name,
            args,
            ty,
        } => oomir::Constant::StaticCall {
            owner_class: call_owner.clone(),
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| create_constant_factory(cp, owner_class, arg, methods, next_factory))
                .collect::<jvm::Result<Vec<_>>>()?,
            ty: ty.clone(),
        },
        _ => return Ok(constant.clone()),
    };

    let return_type = oomir::Type::from_constant(&prepared);
    let method_name = format!("_constant_factory_{}", *next_factory);
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

const MAX_INLINE_CONSTANT_INSTRUCTIONS: usize = 1_024;

fn constant_instruction_cost(constant: &oomir::Constant) -> usize {
    use oomir::Constant as C;
    match constant {
        C::Unit => 0,
        C::StaticRef { .. } | C::FactoryCall { .. } => 1,
        C::StaticCall { args, .. } => args.iter().fold(1usize, |cost, arg| {
            cost.saturating_add(constant_instruction_cost(arg))
        }),
        C::FunctionPointer { .. } => 3,
        C::PointerAddress { .. } => 3,
        C::RepeatedBytePointer { .. } => 8,
        C::InternedPointer { value, .. } => 7usize.saturating_add(constant_instruction_cost(value)),
        C::I64(_) | C::U64(_) | C::F64(_) => 1,
        C::I8(_)
        | C::U8(_)
        | C::I16(_)
        | C::U16(_)
        | C::I32(_)
        | C::U32(_)
        | C::F16(_)
        | C::F32(_)
        | C::Boolean(_)
        | C::Char(_)
        | C::String(_)
        | C::Null(_) => 1,
        C::Str(_) => 2,
        C::Array(element_type, elements) => {
            let element_cost = if element_type.has_jvm_value() {
                elements.iter().fold(0usize, |cost, element| {
                    cost.saturating_add(3 + constant_instruction_cost(element))
                })
            } else {
                0
            };
            2usize.saturating_add(element_cost)
        }
        C::Slice(_, elements) => elements.iter().fold(7usize, |cost, element| {
            cost.saturating_add(3 + constant_instruction_cost(element))
        }),
        C::SliceRef { backing, .. } => 5usize.saturating_add(constant_instruction_cost(backing)),
        C::Instance { params, .. } => params.iter().fold(3usize, |cost, param| {
            cost.saturating_add(constant_instruction_cost(param))
        }),
    }
}

fn prepare_constant_operand(
    operand: &mut oomir::Operand,
    cp: &mut InternedConstantPool,
    owner_class: &str,
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<()> {
    let oomir::Operand::Constant(constant) = operand else {
        return Ok(());
    };
    if constant_instruction_cost(constant) > MAX_INLINE_CONSTANT_INSTRUCTIONS {
        *constant = create_constant_factory(cp, owner_class, constant, methods, next_factory)?;
    }
    Ok(())
}

fn prepare_function_constants(
    function: &mut oomir::Function,
    cp: &mut InternedConstantPool,
    owner_class: &str,
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<()> {
    use oomir::Instruction as I;
    for block in function.body.basic_blocks.values_mut() {
        for instruction in &mut block.instructions {
            let mut prepare = |operand: &mut oomir::Operand| {
                prepare_constant_operand(operand, cp, owner_class, methods, next_factory)
            };
            match instruction {
                I::Add { op1, op2, .. }
                | I::Sub { op1, op2, .. }
                | I::Mul { op1, op2, .. }
                | I::Div { op1, op2, .. }
                | I::Rem { op1, op2, .. }
                | I::Eq { op1, op2, .. }
                | I::Ne { op1, op2, .. }
                | I::Lt { op1, op2, .. }
                | I::Le { op1, op2, .. }
                | I::Gt { op1, op2, .. }
                | I::Ge { op1, op2, .. }
                | I::BitAnd { op1, op2, .. }
                | I::BitOr { op1, op2, .. }
                | I::BitXor { op1, op2, .. }
                | I::Shl { op1, op2, .. }
                | I::Shr { op1, op2, .. } => {
                    prepare(op1)?;
                    prepare(op2)?;
                }
                I::Not { src, .. } | I::Neg { src, .. } | I::Move { src, .. } => prepare(src)?,
                I::Branch { condition, .. } => prepare(condition)?,
                I::Return { operand } => {
                    if let Some(operand) = operand {
                        prepare(operand)?;
                    }
                }
                I::CallIndirect {
                    function_ptr, args, ..
                } => {
                    prepare(function_ptr)?;
                    for argument in args {
                        prepare(argument)?;
                    }
                }
                I::InvokeInterface { args, operand, .. }
                | I::InvokeVirtual { args, operand, .. } => {
                    prepare(operand)?;
                    for argument in args {
                        prepare(argument)?;
                    }
                }
                I::InvokeStatic { args, .. } => {
                    for argument in args {
                        prepare(argument)?;
                    }
                }
                I::NewArray { size, .. } => prepare(size)?,
                I::ArrayStore { index, value, .. } => {
                    prepare(index)?;
                    prepare(value)?;
                }
                I::ArrayFill { value, .. } => prepare(value)?,
                I::ArrayGet { array, index, .. } => {
                    prepare(array)?;
                    prepare(index)?;
                }
                I::Length { array, .. } => prepare(array)?,
                I::ConstructObject { args, .. } => {
                    for (argument, _) in args {
                        prepare(argument)?;
                    }
                }
                I::SetField { value, .. } => prepare(value)?,
                I::GetField { object, .. } => prepare(object)?,
                I::Cast { op, .. } => prepare(op)?,
                I::Switch { discr, .. } => prepare(discr)?,
                I::SourceLocation(_)
                | I::LocalVariableScope(_)
                | I::UnwindStart { .. }
                | I::UnwindEnd
                | I::Rethrow
                | I::Jump { .. }
                | I::CreateFunctionPointer { .. }
                | I::ThrowNewWithMessage { .. }
                | I::Label { .. } => {}
            }
        }
    }
    Ok(())
}

fn create_static_initializer_method(
    cp: &mut InternedConstantPool,
    this_class_index: u16,
    owner_class: &str,
    statics: &[&oomir::Static],
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<jvm::Method> {
    let mut instructions = Vec::new();
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
            next_factory,
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

fn emit_generated_class(
    generated_classes: &mut Vec<(String, Arc<[u8]>)>,
    registry: &EmittedClassRegistry,
    class_name: String,
    bytecode: Vec<u8>,
) -> jvm::Result<()> {
    let hash = bytecode_hash(&bytecode);
    let bytecode_len = bytecode.len();
    let mut checked = std::collections::HashSet::new();
    loop {
        let (candidates, reservation) = {
            let mut variants =
                registry
                    .variants
                    .lock()
                    .map_err(|_| jvm::Error::VerificationError {
                        context: format!("Class {class_name}"),
                        message: "Emitted-class registry lock was poisoned".to_string(),
                    })?;
            let variants = variants.entry(class_name.clone()).or_default();
            let candidates = variants
                .iter()
                .filter(|variant| {
                    variant.hash == hash
                        && variant.len == bytecode_len
                        && !checked.contains(&(Arc::as_ptr(variant) as usize))
                })
                .cloned()
                .collect::<Vec<_>>();
            if candidates.is_empty() {
                let reservation = Arc::new(EmittedClassVariant {
                    hash,
                    len: bytecode_len,
                    state: Mutex::new(EmittedClassState::Pending),
                    ready: Condvar::new(),
                });
                variants.push(Arc::clone(&reservation));
                (candidates, Some(reservation))
            } else {
                (candidates, None)
            }
        };

        if let Some(reservation) = reservation {
            let bytecode: Arc<[u8]> = bytecode.into();
            let mut state =
                reservation
                    .state
                    .lock()
                    .map_err(|_| jvm::Error::VerificationError {
                        context: format!("Class {class_name}"),
                        message: "Emitted-class reservation lock was poisoned".to_string(),
                    })?;
            *state = EmittedClassState::Ready(Arc::clone(&bytecode));
            reservation.ready.notify_all();
            generated_classes.push((class_name, bytecode));
            return Ok(());
        }

        for candidate in candidates {
            checked.insert(Arc::as_ptr(&candidate) as usize);
            let mut state = candidate
                .state
                .lock()
                .map_err(|_| jvm::Error::VerificationError {
                    context: format!("Class {class_name}"),
                    message: "Emitted-class reservation lock was poisoned".to_string(),
                })?;
            while matches!(*state, EmittedClassState::Pending) {
                state = candidate
                    .ready
                    .wait(state)
                    .map_err(|_| jvm::Error::VerificationError {
                        context: format!("Class {class_name}"),
                        message: "Emitted-class reservation lock was poisoned".to_string(),
                    })?;
            }
            let previous = match &*state {
                EmittedClassState::Ready(bytecode) => Arc::clone(bytecode),
                EmittedClassState::Pending => unreachable!(),
            };
            drop(state);
            if previous.as_ref() == bytecode.as_slice() {
                return Ok(());
            }
        }
    }
}

fn serialize_class_file(class_file: &ClassFile<'_>, context: &str) -> jvm::Result<Vec<u8>> {
    let mut bytecode = Vec::new();
    if let Err(error) = class_file.to_bytes(&mut bytecode) {
        let failing_method = class_file.methods.iter().find_map(|method| {
            let mut method_bytes = Vec::new();
            method
                .to_bytes(&mut method_bytes)
                .err()
                .map(|method_error| {
                    let name = class_file
                        .constant_pool
                        .try_get_utf8(method.name_index)
                        .map_or_else(|_| format!("#{}", method.name_index), ToString::to_string);
                    let descriptor = class_file
                        .constant_pool
                        .try_get_utf8(method.descriptor_index)
                        .map_or_else(
                            |_| format!("#{}", method.descriptor_index),
                            ToString::to_string,
                        );
                    format!("method {name}{descriptor}: {method_error:?}")
                })
        });
        return Err(jvm::Error::VerificationError {
            context: context.to_string(),
            message: failing_method.map_or_else(
                || format!("Failed to serialize class file: {error:?}"),
                |method| format!("Failed to serialize class file ({method})"),
            ),
        });
    }
    Ok(bytecode)
}

fn write_class_bundle(
    output_directory: &Path,
    classes: &[(String, Arc<[u8]>)],
    context: &str,
) -> jvm::Result<PathBuf> {
    let path = output_directory.join("classes.jvmbundle");
    let file = std::fs::File::create(&path).map_err(|error| jvm::Error::VerificationError {
        context: context.to_string(),
        message: format!("Failed to create temporary class bundle: {error}"),
    })?;
    let mut output = BufWriter::new(file);
    output
        .write_all(super::CLASS_BUNDLE_MAGIC)
        .map_err(|error| jvm::Error::VerificationError {
            context: context.to_string(),
            message: format!("Failed to write temporary class bundle: {error}"),
        })?;
    for (name, bytecode) in classes {
        let name = name.as_bytes();
        let name_len = u32::try_from(name.len()).map_err(|_| jvm::Error::VerificationError {
            context: context.to_string(),
            message: "JVM class name exceeds bundle format limit".to_string(),
        })?;
        let bytecode_len =
            u64::try_from(bytecode.len()).map_err(|_| jvm::Error::VerificationError {
                context: context.to_string(),
                message: "JVM class data exceeds bundle format limit".to_string(),
            })?;
        for bytes in [
            name_len.to_le_bytes().as_slice(),
            bytecode_len.to_le_bytes().as_slice(),
            name,
            bytecode.as_ref(),
        ] {
            output
                .write_all(bytes)
                .map_err(|error| jvm::Error::VerificationError {
                    context: context.to_string(),
                    message: format!("Failed to write temporary class bundle: {error}"),
                })?;
        }
    }
    output
        .flush()
        .map_err(|error| jvm::Error::VerificationError {
            context: context.to_string(),
            message: format!("Failed to flush temporary class bundle: {error}"),
        })?;
    Ok(path)
}

/// Converts an OOMIR module into JVM class files, streaming each completed
/// class into one shard bundle so rustc does not manage tens of thousands of
/// temporary object paths.
pub fn oomir_to_jvm_bytecode(
    mut module: oomir::Module,
    debug_info: DebugInfoOptions,
    emit_runtime_views: bool,
    registry: &EmittedClassRegistry,
) -> jvm::Result<Vec<(String, PathBuf)>> {
    large_methods::outline_large_functions(&mut module)?;
    let output_ordinal = OUTPUT_DIRECTORY_COUNTER.fetch_add(1, Ordering::Relaxed);
    let output_directory = std::env::temp_dir().join(format!(
        "rustc-codegen-jvm-{}-{}-{output_ordinal}",
        std::process::id(),
        crate::stable_hash::short_hash(&module.name, 12)
    ));
    std::fs::create_dir_all(&output_directory).map_err(|error| jvm::Error::VerificationError {
        context: module.name.clone(),
        message: format!("Failed to create temporary class directory: {error}"),
    })?;
    let mut generated_classes = Vec::new();
    if emit_runtime_views {
        emit_generated_class(
            &mut generated_classes,
            registry,
            oomir::SLICE_VIEW_CLASS.to_string(),
            create_slice_view_classfile()?,
        )?;
        emit_generated_class(
            &mut generated_classes,
            registry,
            oomir::UTF8_VIEW_CLASS.to_string(),
            create_utf8_view_classfile()?,
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
        let code_attribute_name_index = main_cp.add_utf8("Code")?;

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

        for mut function in functions {
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
            prepare_function_constants(
                &mut function,
                &mut main_cp,
                &class_name_jvm,
                &mut methods,
                &mut next_factory,
            )
            .map_err(|error| jvm::Error::VerificationError {
                context: format!("Constants for {class_name_jvm}::{}", function.name),
                message: format!(
                    "Failed after creating {next_factory} constant factories: {error:?}"
                ),
            })?;

            // Translate the function body using its own constant pool reference
            // Free functions at module level don't have an owner class
            let translator = FunctionTranslator::new(
                &function,
                &mut main_cp, // Use the main class's constant pool
                &mut bootstrap_methods,
                &module,
                true,
                None, // No owner class for free functions
                debug_info,
            );
            let (jvm_code, max_locals_val, code_attributes, exception_table) = translator
                .translate()
                .map_err(|error| jvm::Error::VerificationError {
                    context: format!(
                        "Function {class_name_jvm}::{} - {} of {}",
                        function.name, current_index, total_functions
                    ),
                    message: format!("Failed to translate function: {error:?}"),
                })?;

            let stack_floor = oomir_function_stack_floor(&function);
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
                exception_table,
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
            method.access_flags = if function.name.starts_with(large_methods::METHOD_PREFIX) {
                MethodAccessFlags::PRIVATE
                    | MethodAccessFlags::STATIC
                    | MethodAccessFlags::SYNTHETIC
            } else {
                MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC
            };
            if function.name == "<init>" {
                // Constructors cannot be static
                method.access_flags = MethodAccessFlags::PUBLIC;
            }
            method.name_index = name_index;
            method.descriptor_index = descriptor_index;
            method.attributes.push(code_attribute);
            // Add MethodParameters attribute (skip for constructors as they often have synthetic params)
            if function.name != "<init>" && !function.name.starts_with(large_methods::METHOD_PREFIX)
            {
                method.attributes.push(method_parameters_attribute);
            }

            methods.push(method);
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
        emit_generated_class(
            &mut generated_classes,
            registry,
            class_name_jvm.clone(),
            byte_vector,
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
        .collect::<std::collections::HashSet<_>>();
    let mut subclasses_by_host = HashMap::<String, Vec<String>>::new();
    let mut nest_host_by_class = HashMap::<String, String>::new();
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
                let subclasses = subclasses_by_host.remove(dt_name_oomir).unwrap_or_default();
                let nest_host = nest_host_by_class.remove(dt_name_oomir);
                // Create and serialize the class file for this data type
                let dt_bytecode = create_data_type_classfile_for_class(
                    &dt_name_oomir,
                    fields,
                    is_abstract,
                    methods,
                    super_class.as_deref().unwrap_or("java/lang/Object"),
                    interfaces,
                    &module,
                    subclasses,
                    nest_host,
                    debug_info,
                )?;
                emit_generated_class(
                    &mut generated_classes,
                    registry,
                    dt_name_oomir.clone(),
                    dt_bytecode,
                )?;
            }
            DataType::Interface { methods } => {
                // Create and serialize the class file for this data type
                let dt_bytecode =
                    create_data_type_classfile_for_interface(&dt_name_oomir, &methods)?;
                emit_generated_class(
                    &mut generated_classes,
                    registry,
                    dt_name_oomir.clone(),
                    dt_bytecode,
                )?;
            }
        }
    }

    if generated_classes.is_empty() {
        std::fs::remove_dir(&output_directory).map_err(|error| jvm::Error::VerificationError {
            context: module.name.clone(),
            message: format!("Failed to remove empty temporary class directory: {error}"),
        })?;
        return Ok(Vec::new());
    }

    let bundle = write_class_bundle(&output_directory, &generated_classes, &module.name)?;
    Ok(vec![(module.name, bundle)])
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
