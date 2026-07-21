// src/lower2/mod.rs

//! This module converts OOMIR into JVM bytecode.

use crate::oomir::{self, DataType};
use helpers::{get_cast_instructions, oomir_function_stack_floor};
use jvm_gen::{
    create_data_type_classfile_for_class, create_data_type_classfile_for_interface,
    create_default_constructor, create_slice_view_classfile, create_utf8_view_classfile,
    oomir_type_to_ristretto_field_type,
};
use translator::{DebugInfoOptions, FunctionTranslator};

use self::jvm::{
    ClassAccessFlags, ClassFile, FieldAccessFlags, MethodAccessFlags, Version,
    attributes::{Attribute, BootstrapMethod, Instruction, MaxStack},
};
use constant_pool::{InternedConstantPool, verify_no_duplicate_constants};
use consts::load_constant;
use rustc_middle::ty::TyCtxt;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    path::{Path, PathBuf},
    sync::atomic::{AtomicUsize, Ordering},
};

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

static OUTPUT_DIRECTORY_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Default)]
pub(crate) struct EmittedClassRegistry {
    variants: HashMap<String, Vec<EmittedClassVariant>>,
}

struct EmittedClassVariant {
    hash: u64,
    len: usize,
    path: PathBuf,
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
    output_directory: &Path,
    generated_classes: &mut Vec<(String, PathBuf)>,
    registry: &mut EmittedClassRegistry,
    class_name: String,
    bytecode: Vec<u8>,
) -> jvm::Result<()> {
    let hash = bytecode_hash(&bytecode);
    let bytecode_len = bytecode.len();
    if let Some(variants) = registry.variants.get(&class_name) {
        for variant in variants
            .iter()
            .filter(|variant| variant.hash == hash && variant.len == bytecode_len)
        {
            let previous =
                std::fs::read(&variant.path).map_err(|error| jvm::Error::VerificationError {
                    context: format!("Class {class_name}"),
                    message: format!("Failed to compare an emitted class fragment: {error}"),
                })?;
            if previous == bytecode {
                return Ok(());
            }
        }
    }

    let path = output_directory.join(format!("{}.class", generated_classes.len()));
    std::fs::write(&path, bytecode).map_err(|error| jvm::Error::VerificationError {
        context: format!("Class {class_name}"),
        message: format!("Failed to write temporary class file: {error}"),
    })?;
    registry
        .variants
        .entry(class_name.clone())
        .or_default()
        .push(EmittedClassVariant {
            hash,
            len: bytecode_len,
            path: path.clone(),
        });
    generated_classes.push((class_name, path));
    Ok(())
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

/// Converts an OOMIR module into JVM class files, streaming each completed
/// class to disk so crate-wide bytecode does not coexist with crate-wide OOMIR.
pub fn oomir_to_jvm_bytecode(
    mut module: oomir::Module,
    tcx: TyCtxt,
    emit_runtime_views: bool,
    registry: &mut EmittedClassRegistry,
) -> jvm::Result<Vec<(String, PathBuf)>> {
    let debug_info = DebugInfoOptions {
        line_numbers: tcx.sess.opts.debuginfo != rustc_session::config::DebugInfo::None,
        local_variables: matches!(
            tcx.sess.opts.debuginfo,
            rustc_session::config::DebugInfo::Limited | rustc_session::config::DebugInfo::Full
        ),
    };
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
            &output_directory,
            &mut generated_classes,
            registry,
            oomir::SLICE_VIEW_CLASS.to_string(),
            create_slice_view_classfile()?,
        )?;
        emit_generated_class(
            &output_directory,
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
            &output_directory,
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
                    debug_info,
                )?;
                emit_generated_class(
                    &output_directory,
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
                    &output_directory,
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
    }

    Ok(generated_classes)
}
