#![feature(alloc_error_hook)]
#![feature(box_patterns)]
#![feature(rustc_private)]
#![feature(f16)]
#![feature(f128)]
#![warn(clippy::pedantic)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]

//! Rustc Codegen JVM (Upgraded Version)
//!
//! Compiler backend for rustc that generates JVM bytecode, using a two-stage lowering process:
//! MIR -> OOMIR -> JVM Bytecode.

extern crate rustc_abi;
extern crate rustc_codegen_ssa;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

use oomir::{Operand, Type};
use rustc_codegen_ssa::back::archive::{ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder};
use rustc_codegen_ssa::{
    CompiledModule, CompiledModules, CrateInfo, ModuleKind, traits::CodegenBackend,
};
use std::collections::{HashMap, HashSet};

use rustc_data_structures::unord::UnordMap;
use rustc_hir::{QPath, TyKind as HirTyKind};
use rustc_metadata::EncodedMetadata;
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_middle::ty::TyCtxt;
use rustc_session::{Session, config::OutputFilenames};
use std::{any::Any, io::Write, path::Path};

use misc::ToIdent;

mod lower1;
mod lower2;
mod misc;
mod oomir;
mod optimise1;

/// An instance of our Java bytecode codegen backend.
struct MyBackend;

fn should_lower_non_local(tcx: TyCtxt<'_>, def_id: rustc_hir::def_id::DefId) -> bool {
    let crate_name = tcx.crate_name(def_id.krate);
    matches!(crate_name.as_str(), "core" | "alloc" | "std")
}

/// Helper function to lower a closure definition to OOMIR
///
/// This function is called when we encounter a closure call and need to ensure
/// the closure's implementation is available in the OOMIR module.
fn lower_closure_to_oomir<'tcx>(
    tcx: TyCtxt<'tcx>,
    closure_def_id: rustc_hir::def_id::DefId,
    oomir_module: &mut oomir::Module,
) {
    // Generate the closure function name
    let closure_name = lower1::generate_closure_function_name(tcx, closure_def_id);

    // Check if we've already lowered this closure
    if oomir_module.get_function(None, &closure_name).is_some() {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "closure-lowering",
            format!("Closure {} already lowered, skipping", closure_name)
        );
        return;
    }

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "closure-lowering",
        format!(
            "Lowering closure: {} (DefId: {:?})",
            closure_name, closure_def_id
        )
    );

    // Get the closure's MIR using expect_resolve with fully monomorphized typing environment
    // We use fully_monomorphized() since we've already filtered out closures with captures.
    use rustc_middle::ty::TypingEnv;
    let typing_env = TypingEnv::fully_monomorphized();
    let generic_args = rustc_middle::ty::GenericArgs::empty();

    let instance = rustc_middle::ty::Instance::expect_resolve(
        tcx,
        typing_env,
        closure_def_id,
        generic_args,
        rustc_span::DUMMY_SP,
    );

    let mut mir = tcx.optimized_mir(instance.def_id()).clone();

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "closure-lowering",
        format!("Closure MIR for {}: {:?}", closure_name, mir)
    );

    // Lower the closure MIR to OOMIR, providing the closure name as an override
    // since closures don't have proper item names in rustc
    let (oomir_function, data_types) = lower1::mir_to_oomir(
        tcx,
        instance,
        &mut mir,
        Some(lower1::naming::FnNameData {
            class_to_call_on: Some(oomir_module.name.clone()),
            method_name: closure_name.clone(),
        }),
        true,
    );

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "closure-lowering",
        format!("Successfully lowered closure: {}", closure_name)
    );

    // Add the closure function to the module
    oomir_module.insert_function(oomir_function);
    oomir_module.merge_data_types(&data_types);
}

fn placeholder_operand_for_constructor_field(ty: &Type) -> Operand {
    let constant = match ty {
        Type::Boolean => oomir::Constant::Boolean(false),
        Type::Char => oomir::Constant::Char('\0'),
        Type::I8 => oomir::Constant::I8(0),
        Type::I16 => oomir::Constant::I16(0),
        Type::I32 => oomir::Constant::I32(0),
        Type::I64 => oomir::Constant::I64(0),
        Type::F32 => oomir::Constant::F32(0.0),
        Type::F64 => oomir::Constant::F64(0.0),
        Type::String
        | Type::Class(_)
        | Type::Interface(_)
        | Type::Array(_)
        | Type::MutableReference(_)
        | Type::Reference(_) => oomir::Constant::Null(ty.clone()),
        Type::Void => panic!("unexpected Void field type in constructor placeholder"),
    };
    Operand::Constant(constant)
}

fn constructor_args_from_fields(
    fields: &[(String, Type)],
    values: &[(&str, Operand)],
) -> Vec<(Operand, Type)> {
    fields
        .iter()
        .map(|(field_name, field_ty)| {
            let operand = values
                .iter()
                .find_map(|(name, operand)| (*name == field_name).then(|| operand.clone()))
                .unwrap_or_else(|| {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "shim",
                        format!(
                            "Info: Using constructor placeholder for missing shim field '{}' ({:?})",
                            field_name, field_ty
                        )
                    );
                    placeholder_operand_for_constructor_field(field_ty)
                });
            (operand, field_ty.clone())
        })
        .collect()
}

fn pad_construct_object_args_in_function(
    function: &mut oomir::Function,
    class_fields: &HashMap<String, Vec<(String, Type)>>,
) {
    for block in function.body.basic_blocks.values_mut() {
        for instruction in &mut block.instructions {
            let oomir::Instruction::ConstructObject {
                class_name, args, ..
            } = instruction
            else {
                continue;
            };

            let Some(fields) = class_fields.get(class_name) else {
                continue;
            };

            if args.len() >= fields.len() {
                continue;
            }

            for (field_name, field_ty) in fields.iter().skip(args.len()) {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "shim",
                    format!(
                        "Info: Padding constructor for '{}' with placeholder field '{}' ({:?})",
                        class_name, field_name, field_ty
                    )
                );
                args.push((
                    placeholder_operand_for_constructor_field(field_ty),
                    field_ty.clone(),
                ));
            }
        }
    }
}

fn pad_construct_object_args_for_field_constructors(oomir_module: &mut oomir::Module) {
    let class_fields: HashMap<String, Vec<(String, Type)>> = oomir_module
        .data_types
        .iter()
        .filter_map(|(class_name, data_type)| match data_type {
            oomir::DataType::Class { fields, .. } => {
                let mut fields = fields.clone();
                let mut seen_fields = HashSet::new();
                fields.retain(|(name, _)| seen_fields.insert(name.clone()));
                Some((class_name.clone(), fields))
            }
            oomir::DataType::Interface { .. } => None,
        })
        .collect();

    for function in oomir_module.functions.values_mut() {
        pad_construct_object_args_in_function(function, &class_fields);
    }

    for data_type in oomir_module.data_types.values_mut() {
        let oomir::DataType::Class { methods, .. } = data_type else {
            continue;
        };

        for method in methods.values_mut() {
            if let oomir::DataTypeMethod::Function(function) = method {
                pad_construct_object_args_in_function(function, &class_fields);
            }
        }
    }
}

fn install_fmt_arguments_shim(oomir_module: &mut oomir::Module) {
    let class_name = "Arguments__".to_string();
    let Some(oomir::DataType::Class {
        fields, methods, ..
    }) = oomir_module.data_types.get_mut(&class_name)
    else {
        return;
    };

    if !fields.iter().any(|(name, _)| name == "message") {
        fields.push(("message".to_string(), Type::String));
    }
    let template_ty = fields
        .iter()
        .find(|(name, _)| name == "template")
        .map(|(_, ty)| ty.clone())
        .unwrap_or_else(|| {
            let ty = Type::Class("java/lang/Object".to_string());
            fields.push(("template".to_string(), ty.clone()));
            ty
        });
    let args_ty = fields
        .iter()
        .find(|(name, _)| name == "args")
        .map(|(_, ty)| ty.clone())
        .unwrap_or_else(|| {
            let ty = Type::Class("java/lang/Object".to_string());
            fields.push(("args".to_string(), ty.clone()));
            ty
        });

    let from_str_fields = fields.clone();
    methods.entry("from_str".to_string()).or_insert_with(|| {
        let block = oomir::BasicBlock {
            label: "bb0".to_string(),
            instructions: vec![
                oomir::Instruction::ConstructObject {
                    dest: "_args".to_string(),
                    class_name: class_name.clone(),
                    args: constructor_args_from_fields(
                        &from_str_fields,
                        &[(
                            "message",
                            Operand::Variable {
                                name: "_1".to_string(),
                                ty: Type::String,
                            },
                        )],
                    ),
                },
                oomir::Instruction::Return {
                    operand: Some(Operand::Variable {
                        name: "_args".to_string(),
                        ty: Type::Class(class_name.clone()),
                    }),
                },
            ],
        };
        oomir::DataTypeMethod::Function(oomir::Function {
            name: "from_str".to_string(),
            owner_class: None,
            signature: oomir::Signature {
                params: vec![("s".to_string(), Type::String)],
                ret: Box::new(Type::Class(class_name.clone())),
                is_static: true,
            },
            body: oomir::CodeBlock {
                entry: "bb0".to_string(),
                basic_blocks: HashMap::from([("bb0".to_string(), block)]),
            },
        })
    });

    let new_fields = fields.clone();
    methods.entry("new".to_string()).or_insert_with(|| {
        let argument_class = "core_fmt_rt_Argument__".to_string();
        let block = oomir::BasicBlock {
            label: "bb0".to_string(),
            instructions: vec![
                oomir::Instruction::InvokeStatic {
                    dest: Some("_message".to_string()),
                    class_name: "org/rustlang/core/Core".to_string(),
                    method_name: "formatArgs".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![
                            ("template".to_string(), Type::Array(Box::new(Type::I16))),
                            (
                                "args".to_string(),
                                Type::Array(Box::new(Type::Class("java/lang/Object".to_string()))),
                            ),
                        ],
                        ret: Box::new(Type::String),
                        is_static: true,
                    },
                    args: vec![
                        Operand::Variable {
                            name: "_1".to_string(),
                            ty: Type::Array(Box::new(Type::I16)),
                        },
                        Operand::Variable {
                            name: "_2".to_string(),
                            ty: Type::Array(Box::new(Type::Class(argument_class.clone()))),
                        },
                    ],
                },
                oomir::Instruction::ConstructObject {
                    dest: "_args".to_string(),
                    class_name: class_name.clone(),
                    args: constructor_args_from_fields(
                        &new_fields,
                        &[
                            (
                                "message",
                                Operand::Variable {
                                    name: "_message".to_string(),
                                    ty: Type::String,
                                },
                            ),
                            (
                                "template",
                                Operand::Variable {
                                    name: "_1".to_string(),
                                    ty: Type::Array(Box::new(Type::I16)),
                                },
                            ),
                            (
                                "args",
                                Operand::Variable {
                                    name: "_2".to_string(),
                                    ty: Type::Array(Box::new(Type::Class(argument_class.clone()))),
                                },
                            ),
                        ],
                    ),
                },
                oomir::Instruction::Return {
                    operand: Some(Operand::Variable {
                        name: "_args".to_string(),
                        ty: Type::Class(class_name.clone()),
                    }),
                },
            ],
        };
        oomir::DataTypeMethod::Function(oomir::Function {
            name: "new".to_string(),
            owner_class: None,
            signature: oomir::Signature {
                params: vec![
                    ("template".to_string(), Type::Array(Box::new(Type::I16))),
                    (
                        "args".to_string(),
                        Type::Array(Box::new(Type::Class(argument_class))),
                    ),
                ],
                ret: Box::new(Type::Class(class_name.clone())),
                is_static: true,
            },
            body: oomir::CodeBlock {
                entry: "bb0".to_string(),
                basic_blocks: HashMap::from([("bb0".to_string(), block)]),
            },
        })
    });

    methods.entry("toString".to_string()).or_insert_with(|| {
        let block = oomir::BasicBlock {
            label: "bb0".to_string(),
            instructions: vec![
                oomir::Instruction::GetField {
                    dest: "_message".to_string(),
                    object: Operand::Variable {
                        name: "_1".to_string(),
                        ty: Type::Class(class_name.clone()),
                    },
                    field_name: "message".to_string(),
                    field_ty: Type::String,
                    owner_class: class_name.clone(),
                },
                oomir::Instruction::GetField {
                    dest: "_template".to_string(),
                    object: Operand::Variable {
                        name: "_1".to_string(),
                        ty: Type::Class(class_name.clone()),
                    },
                    field_name: "template".to_string(),
                    field_ty: template_ty.clone(),
                    owner_class: class_name.clone(),
                },
                oomir::Instruction::GetField {
                    dest: "_args_field".to_string(),
                    object: Operand::Variable {
                        name: "_1".to_string(),
                        ty: Type::Class(class_name.clone()),
                    },
                    field_name: "args".to_string(),
                    field_ty: args_ty.clone(),
                    owner_class: class_name.clone(),
                },
                oomir::Instruction::InvokeStatic {
                    dest: Some("_formatted".to_string()),
                    class_name: "org/rustlang/core/Core".to_string(),
                    method_name: "formatArguments".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![
                            ("message".to_string(), Type::String),
                            (
                                "template".to_string(),
                                Type::Class("java/lang/Object".to_string()),
                            ),
                            (
                                "args".to_string(),
                                Type::Class("java/lang/Object".to_string()),
                            ),
                        ],
                        ret: Box::new(Type::String),
                        is_static: true,
                    },
                    args: vec![
                        Operand::Variable {
                            name: "_message".to_string(),
                            ty: Type::String,
                        },
                        Operand::Variable {
                            name: "_template".to_string(),
                            ty: template_ty.clone(),
                        },
                        Operand::Variable {
                            name: "_args_field".to_string(),
                            ty: args_ty.clone(),
                        },
                    ],
                },
                oomir::Instruction::Return {
                    operand: Some(Operand::Variable {
                        name: "_formatted".to_string(),
                        ty: Type::String,
                    }),
                },
            ],
        };
        oomir::DataTypeMethod::Function(oomir::Function {
            name: "toString".to_string(),
            owner_class: None,
            signature: oomir::Signature {
                params: vec![("self".to_string(), Type::Class(class_name.clone()))],
                ret: Box::new(Type::String),
                is_static: false,
            },
            body: oomir::CodeBlock {
                entry: "bb0".to_string(),
                basic_blocks: HashMap::from([("bb0".to_string(), block)]),
            },
        })
    });
}

fn install_fmt_argument_shim(oomir_module: &mut oomir::Module) {
    let class_name = "core_fmt_rt_Argument__".to_string();
    let Some(oomir::DataType::Class {
        fields, methods, ..
    }) = oomir_module.data_types.get_mut(&class_name)
    else {
        return;
    };

    if !fields.iter().any(|(name, _)| name == "value") {
        fields.push(("value".to_string(), Type::String));
    }

    let argument_fields = fields.clone();
    methods.entry("new_display".to_string()).or_insert_with(|| {
        let block = oomir::BasicBlock {
            label: "bb0".to_string(),
            instructions: vec![
                oomir::Instruction::InvokeStatic {
                    dest: Some("_value".to_string()),
                    class_name: "java/lang/String".to_string(),
                    method_name: "valueOf".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("value".to_string(), Type::I32)],
                        ret: Box::new(Type::String),
                        is_static: true,
                    },
                    args: vec![Operand::Variable {
                        name: "_1".to_string(),
                        ty: Type::I32,
                    }],
                },
                oomir::Instruction::ConstructObject {
                    dest: "_arg".to_string(),
                    class_name: class_name.clone(),
                    args: constructor_args_from_fields(
                        &argument_fields,
                        &[(
                            "value",
                            Operand::Variable {
                                name: "_value".to_string(),
                                ty: Type::String,
                            },
                        )],
                    ),
                },
                oomir::Instruction::Return {
                    operand: Some(Operand::Variable {
                        name: "_arg".to_string(),
                        ty: Type::Class(class_name.clone()),
                    }),
                },
            ],
        };
        oomir::DataTypeMethod::Function(oomir::Function {
            name: "new_display".to_string(),
            owner_class: None,
            signature: oomir::Signature {
                params: vec![("value".to_string(), Type::I32)],
                ret: Box::new(Type::Class(class_name.clone())),
                is_static: true,
            },
            body: oomir::CodeBlock {
                entry: "bb0".to_string(),
                basic_blocks: HashMap::from([("bb0".to_string(), block)]),
            },
        })
    });

    methods.entry("toString".to_string()).or_insert_with(|| {
        let block = oomir::BasicBlock {
            label: "bb0".to_string(),
            instructions: vec![
                oomir::Instruction::GetField {
                    dest: "_value".to_string(),
                    object: Operand::Variable {
                        name: "_1".to_string(),
                        ty: Type::Class(class_name.clone()),
                    },
                    field_name: "value".to_string(),
                    field_ty: Type::String,
                    owner_class: class_name.clone(),
                },
                oomir::Instruction::Return {
                    operand: Some(Operand::Variable {
                        name: "_value".to_string(),
                        ty: Type::String,
                    }),
                },
            ],
        };
        oomir::DataTypeMethod::Function(oomir::Function {
            name: "toString".to_string(),
            owner_class: None,
            signature: oomir::Signature {
                params: vec![("self".to_string(), Type::Class(class_name.clone()))],
                ret: Box::new(Type::String),
                is_static: false,
            },
            body: oomir::CodeBlock {
                entry: "bb0".to_string(),
                basic_blocks: HashMap::from([("bb0".to_string(), block)]),
            },
        })
    });
}

impl CodegenBackend for MyBackend {
    fn name(&self) -> &'static str {
        "rustc_codegen_jvm"
    }

    fn target_cpu(&self, sess: &Session) -> String {
        match sess.opts.cg.target_cpu {
            Some(ref name) => name,
            None => sess.target.cpu.as_ref(),
        }
        .to_owned()
    }

    fn codegen_crate<'a>(&self, tcx: TyCtxt<'_>) -> Box<dyn Any> {
        let crate_name = tcx
            .crate_name(rustc_hir::def_id::CRATE_DEF_ID.to_def_id().krate)
            .to_string();

        let mut oomir_module = oomir::Module {
            name: crate_name.clone(),
            functions: std::collections::HashMap::new(),
            data_types: std::collections::HashMap::new(),
        };

        // Track closures we need to lower
        let mut closures_to_lower: std::collections::HashSet<rustc_hir::def_id::DefId> =
            std::collections::HashSet::new();

        // Track monomorphized function instances to lower and avoid duplicates by owner+name
        let mut fn_instances_to_lower: Vec<(
            rustc_middle::ty::Instance<'_>,
            lower1::naming::FnNameData,
        )> = Vec::new();
        let mut seen_fn_names: std::collections::HashSet<String> = std::collections::HashSet::new();
        use rustc_middle::ty::TypingEnv;

        // Iterate through all items in the crate and find functions
        let module_items = tcx.hir_crate_items(());

        for item_id in module_items.free_items() {
            let item = tcx.hir_item(item_id);
            if let rustc_hir::ItemKind::Fn {
                ident: i,
                sig: _,
                generics: _,
                body: _,
                has_body: _,
            } = item.kind
            {
                let def_id = item_id.owner_id.to_def_id();

                // Skip directly lowering generic functions; collect concrete instantiations instead
                let generics = tcx.generics_of(def_id);
                if !generics.own_params.is_empty() {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "backend",
                        format!(
                            "Skipping direct lowering of generic function {} (DefId: {:?}); will lower its monomorphized instances",
                            i, def_id
                        )
                    );
                    continue;
                }

                let instance = rustc_middle::ty::Instance::mono(tcx, def_id);
                let mut mir = tcx.optimized_mir(instance.def_id()).clone(); // Clone the MIR

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "backend",
                    format!("MIR for function {i}: {:?}", mir)
                );

                // Collect closures from mentioned_items in the MIR
                if let Some(mentioned_items) = &mir.mentioned_items {
                    for mentioned in mentioned_items.iter() {
                        // Check if this mentioned item is a closure
                        if let rustc_middle::mir::MentionedItem::Fn(fn_ty) = &mentioned.node {
                            if let rustc_middle::ty::TyKind::FnDef(fn_def_id, fn_args) =
                                fn_ty.kind()
                            {
                                // Check the first argument to see if it's a closure type
                                let mut is_closure = false;
                                if let Some(first_arg) = fn_args.get(0) {
                                    if let Some(ty) = first_arg.as_type() {
                                        if let rustc_middle::ty::TyKind::Closure(
                                            closure_def_id,
                                            _,
                                        ) = ty.kind()
                                        {
                                            breadcrumbs::log!(
                                                breadcrumbs::LogLevel::Info,
                                                "closure-discovery",
                                                format!(
                                                    "Found closure {:?} in function {}",
                                                    closure_def_id, i
                                                )
                                            );
                                            closures_to_lower.insert(*closure_def_id);
                                            is_closure = true;
                                        }
                                    }
                                }
                                if !is_closure {
                                    // Non-closure function reference; enqueue monomorphized instance
                                    let typing_env = TypingEnv::fully_monomorphized();

                                    // Only lower functions defined in this crate
                                    if fn_def_id.is_local() {
                                        let instance = rustc_middle::ty::Instance::expect_resolve(
                                            tcx,
                                            typing_env,
                                            *fn_def_id,
                                            *fn_args,
                                            rustc_span::DUMMY_SP,
                                        );
                                        // Skip virtual trait method calls (handled via trait objects at runtime)
                                        if let rustc_middle::ty::InstanceKind::Virtual(_, _) =
                                            instance.def
                                        {
                                            breadcrumbs::log!(
                                                breadcrumbs::LogLevel::Info,
                                                "backend",
                                                format!(
                                                    "Skipping virtual instance: {:?}",
                                                    instance
                                                )
                                            );
                                            continue;
                                        }
                                        // Skip trait method implementations (already lowered by impl block code with Type_method naming)
                                        /*if let Some(_) = tcx.opt_associated_item(*fn_def_id) {
                                            breadcrumbs::log!(
                                                breadcrumbs::LogLevel::Info,
                                                "backend",
                                                format!("Skipping impl method: {:?}", fn_def_id)
                                            );
                                            continue;
                                        }*/
                                        let name = lower1::naming::mono_fn_name_from_instance(
                                            tcx, instance,
                                        );
                                        if seen_fn_names.insert(name.key(&crate_name)) {
                                            fn_instances_to_lower.push((instance, name));
                                        }
                                    } else if should_lower_non_local(tcx, *fn_def_id) {
                                        let instance = rustc_middle::ty::Instance::expect_resolve(
                                            tcx,
                                            typing_env,
                                            *fn_def_id,
                                            *fn_args,
                                            rustc_span::DUMMY_SP,
                                        );
                                        if let rustc_middle::ty::InstanceKind::Virtual(_, _) =
                                            instance.def
                                        {
                                            breadcrumbs::log!(
                                                breadcrumbs::LogLevel::Info,
                                                "backend",
                                                format!(
                                                    "Skipping virtual instance: {:?}",
                                                    instance
                                                )
                                            );
                                            continue;
                                        }
                                        let name = lower1::naming::mono_fn_name_from_instance(
                                            tcx, instance,
                                        );
                                        if seen_fn_names.insert(name.key(&crate_name)) {
                                            fn_instances_to_lower.push((instance, name));
                                        }
                                    } else {
                                        breadcrumbs::log!(
                                            breadcrumbs::LogLevel::Info,
                                            "non_local",
                                            format!(
                                                "Skipping non-local function instance: {:?}",
                                                fn_def_id
                                            )
                                        );
                                    }
                                }
                            }
                        }
                    }
                }

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!("--- Starting MIR to OOMIR Lowering for function: {i} ---")
                );
                let oomir_result = lower1::mir_to_oomir(tcx, instance, &mut mir, None, true);
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!("--- Finished MIR to OOMIR Lowering for function: {i} ---")
                );

                let oomir_function = oomir_result.0;

                oomir_module.insert_function(oomir_function);

                oomir_module.merge_data_types(&oomir_result.1);
            } else if let rustc_hir::ItemKind::Impl(impl_a) = item.kind {
                // Get the DefId of the impl block itself. The `item_id` from the
                // outer loop refers to the `impl` item.
                let impl_def_id = item_id.owner_id.to_def_id();
                let impl_generics = tcx.generics_of(impl_def_id);

                // If the `impl` block itself has generic parameters (e.g., `impl<T> for Foo<T>`),
                // we must skip direct lowering of all its methods. They are not
                // concrete and will be lowered when their monomorphized instances
                // are discovered in the MIR of other functions.
                if !impl_generics.own_params.is_empty() {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "backend",
                        format!(
                            "Skipping direct lowering of entire generic impl block (DefId: {:?})",
                            impl_def_id
                        )
                    );
                    continue; // Skip to the next item in the crate
                }
                let ident = match impl_a.self_ty.kind {
                    HirTyKind::Path(qpath) => match qpath {
                        QPath::Resolved(_, p) => {
                            format!("{}", p.segments[0].ident)
                        }
                        QPath::TypeRelative(_, ps) => {
                            format!("{}", ps.ident)
                        }
                        _ => {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "backend",
                                format!("Warning: {:?} is an unknown qpath", qpath)
                            );
                            "unknown_qpath_kind".into()
                        }
                    },
                    _ => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "backend",
                            format!("Warning: {:?} has unknown kind", impl_a.self_ty)
                        );
                        "unknown_type_kind".into()
                    }
                };
                let ident = lower1::place::make_jvm_safe(&ident);
                let of_trait = match impl_a.of_trait {
                    Some(trait_impl_header) => Some(lower1::place::make_jvm_safe(
                        trait_impl_header
                            .trait_ref
                            .path
                            .segments
                            .last()
                            .unwrap()
                            .ident
                            .as_str(),
                    )),
                    None => None,
                };
                for item in impl_a.items {
                    let i = item.to_ident(tcx).to_string();
                    let def_id = item.owner_id.to_def_id();

                    if tcx.def_kind(def_id) != rustc_hir::def::DefKind::AssocFn {
                        continue; // Skip non-function items
                    }

                    // Skip direct lowering of generic methods; rely on monomorphized uses
                    let generics = tcx.generics_of(def_id);
                    if !generics.own_params.is_empty() {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "backend",
                            format!(
                                "Skipping direct lowering of generic impl method {} (DefId: {:?})",
                                i, def_id
                            )
                        );
                        continue;
                    }

                    let instance = rustc_middle::ty::Instance::mono(tcx, def_id);
                    let mut mir = tcx.optimized_mir(instance.def_id()).clone(); // Clone the MIR

                    let i2 = format!("{}_{}", ident, i);

                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "backend",
                        format!("MIR for function {i2}: {:?}", mir)
                    );

                    // Collect closures from mentioned_items in the MIR
                    if let Some(mentioned_items) = &mir.mentioned_items {
                        for mentioned in mentioned_items.iter() {
                            if let rustc_middle::mir::MentionedItem::Fn(fn_ty) = &mentioned.node {
                                if let rustc_middle::ty::TyKind::FnDef(fn_def_id, fn_args) =
                                    fn_ty.kind()
                                {
                                    let mut is_closure = false;
                                    if let Some(first_arg) = fn_args.get(0) {
                                        if let Some(ty) = first_arg.as_type() {
                                            if let rustc_middle::ty::TyKind::Closure(
                                                closure_def_id,
                                                _,
                                            ) = ty.kind()
                                            {
                                                breadcrumbs::log!(
                                                    breadcrumbs::LogLevel::Info,
                                                    "closure-discovery",
                                                    format!(
                                                        "Found closure {:?} in impl method {}",
                                                        closure_def_id, i2
                                                    )
                                                );
                                                closures_to_lower.insert(*closure_def_id);
                                                is_closure = true;
                                            }
                                        }
                                    }
                                    if !is_closure {
                                        let typing_env = TypingEnv::fully_monomorphized();
                                        if fn_def_id.is_local() {
                                            let instance =
                                                rustc_middle::ty::Instance::expect_resolve(
                                                    tcx,
                                                    typing_env,
                                                    *fn_def_id,
                                                    *fn_args,
                                                    rustc_span::DUMMY_SP,
                                                );
                                            // Skip virtual trait method calls (handled via trait objects at runtime)
                                            if let rustc_middle::ty::InstanceKind::Virtual(_, _) =
                                                instance.def
                                            {
                                                breadcrumbs::log!(
                                                    breadcrumbs::LogLevel::Info,
                                                    "backend",
                                                    format!(
                                                        "Skipping virtual instance: {:?}",
                                                        instance
                                                    )
                                                );
                                                continue;
                                            }
                                            /*// Skip trait method implementations (already lowered by impl block code with Type_method naming)
                                            if let Some(assoc_item) =
                                                tcx.opt_associated_item(*fn_def_id)
                                            {
                                                if assoc_item.trait_item_def_id().is_some() {
                                                    breadcrumbs::log!(
                                                        breadcrumbs::LogLevel::Info,
                                                        "backend",
                                                        format!(
                                                            "Skipping trait impl method: {:?}",
                                                            fn_def_id
                                                        )
                                                    );
                                                    continue;
                                                }
                                            }
                                            */
                                            let name = lower1::naming::mono_fn_name_from_instance(
                                                tcx, instance,
                                            );
                                            if seen_fn_names.insert(name.key(&crate_name)) {
                                                fn_instances_to_lower.push((instance, name));
                                            }
                                        } else if should_lower_non_local(tcx, *fn_def_id) {
                                            let instance =
                                                rustc_middle::ty::Instance::expect_resolve(
                                                    tcx,
                                                    typing_env,
                                                    *fn_def_id,
                                                    *fn_args,
                                                    rustc_span::DUMMY_SP,
                                                );
                                            if let rustc_middle::ty::InstanceKind::Virtual(_, _) =
                                                instance.def
                                            {
                                                breadcrumbs::log!(
                                                    breadcrumbs::LogLevel::Info,
                                                    "backend",
                                                    format!(
                                                        "Skipping virtual instance: {:?}",
                                                        instance
                                                    )
                                                );
                                                continue;
                                            }
                                            let name = lower1::naming::mono_fn_name_from_instance(
                                                tcx, instance,
                                            );
                                            if seen_fn_names.insert(name.key(&crate_name)) {
                                                fn_instances_to_lower.push((instance, name));
                                            }
                                        } else {
                                            breadcrumbs::log!(
                                                breadcrumbs::LogLevel::Info,
                                                "non_local",
                                                format!(
                                                    "2. Skipping non-local function instance: {:?}",
                                                    fn_def_id
                                                )
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }

                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!("--- Starting MIR to OOMIR Lowering for function: {i2} ---")
                    );
                    let oomir_result: (oomir::Function, HashMap<String, oomir::DataType>) =
                        lower1::mir_to_oomir(tcx, instance, &mut mir, None, true);
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!("--- Finished MIR to OOMIR Lowering for function: {i2} ---")
                    );

                    let mut oomir_function = oomir_result.0;
                    oomir_function.name = i.to_string();

                    // For trait implementations, replace trait type references with concrete type
                    if of_trait.is_some() {
                        let trait_name = of_trait.clone().unwrap();
                        let new_params: Vec<(String, Type)> = oomir_function
                            .signature
                            .params
                            .iter()
                            .map(|(param_name, param_ty)| match param_ty {
                                Type::MutableReference(box Type::Class(name))
                                    if *name == trait_name =>
                                {
                                    (
                                        param_name.clone(),
                                        Type::MutableReference(Box::new(Type::Class(
                                            ident.clone(),
                                        ))),
                                    )
                                }
                                Type::Class(name) if *name == trait_name => {
                                    (param_name.clone(), Type::Class(ident.clone()))
                                }
                                _ => (param_name.clone(), param_ty.clone()),
                            })
                            .collect();
                        oomir_function.signature.params = new_params;
                    }

                    // Check if this is a method (has an explicit self parameter).
                    let is_method = tcx
                        .opt_associated_item(def_id)
                        .is_some_and(|assoc_item| assoc_item.is_method());

                    if is_method {
                        // Mark as instance method (keep self as first param)
                        oomir_function.signature.is_static = false;
                    }

                    let mut args = vec![];

                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "backend",
                        format!("Function signature: {:?}", oomir_function.signature)
                    );

                    let mut idx = 1;
                    for (_param_name, param_ty) in &oomir_function.signature.params {
                        let arg_name = format!("_{idx}");
                        let arg = Operand::Variable {
                            name: arg_name.clone(),
                            ty: param_ty.clone(),
                        };
                        args.push(arg);
                        idx += 1;
                    }

                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "backend",
                        format!("Function args: {:?}", args)
                    );

                    oomir_module.merge_data_types(&oomir_result.1);

                    // find the data type we are implementing the trait for
                    let dt = oomir_module.data_types.get(&ident).cloned();
                    match dt {
                        Some(oomir::DataType::Class {
                            methods,
                            is_abstract,
                            interfaces,
                            super_class,
                            fields,
                        }) => {
                            let mut new_methods = methods.clone();
                            new_methods.insert(
                                i.to_string(),
                                oomir::DataTypeMethod::Function(oomir_function),
                            );
                            oomir_module.data_types.insert(
                                ident.clone(),
                                oomir::DataType::Class {
                                    methods: new_methods,
                                    is_abstract,
                                    super_class,
                                    fields,
                                    interfaces,
                                },
                            );
                        }
                        Some(oomir::DataType::Interface { .. }) => {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "backend",
                                format!(
                                    "Warning: Skipping method implementation for Interface type '{}'.",
                                    ident
                                )
                            );
                        }
                        None => {
                            // create a new one with reasonable defaults that will be overriden by merge_data_types once it's eventually resolved
                            let mut new_methods = HashMap::new();
                            new_methods.insert(
                                i.to_string(),
                                oomir::DataTypeMethod::Function(oomir_function),
                            );
                            oomir_module.data_types.insert(
                                ident.clone(),
                                oomir::DataType::Class {
                                    methods: new_methods,
                                    is_abstract: false,
                                    super_class: Some("java/lang/Object".to_string()),
                                    fields: vec![],
                                    interfaces: vec![],
                                },
                            );
                        }
                    }
                }
                if let Some(of_trait) = of_trait {
                    oomir_module
                        .data_types
                        .entry(of_trait.clone())
                        .or_insert_with(|| oomir::DataType::Interface {
                            methods: HashMap::new(),
                        });

                    if let Some(data) = oomir_module.data_types.get(&ident).cloned() {
                        match data {
                            oomir::DataType::Class {
                                is_abstract,
                                super_class,
                                fields,
                                methods,
                                interfaces,
                            } => {
                                let mut new_interfaces = interfaces.clone();
                                new_interfaces.push(of_trait);
                                oomir_module.data_types.remove(&ident);
                                oomir_module.data_types.insert(
                                    ident,
                                    oomir::DataType::Class {
                                        is_abstract,
                                        super_class,
                                        fields,
                                        methods,
                                        interfaces: new_interfaces,
                                    },
                                );
                            }
                            oomir::DataType::Interface { .. } => {
                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Warn,
                                    "backend",
                                    format!(
                                        "Warning: Skipping trait implementation declaration for Interface type '{}'.",
                                        ident
                                    )
                                );
                            }
                        }
                    }
                }
            } else if let rustc_hir::ItemKind::Trait {
                ident,
                items: item_refs,
                ..
            } = item.kind
            {
                let ident = lower1::place::make_jvm_safe(ident.as_str());
                let mut fn_data = HashMap::new();
                for item in item_refs {
                    let name = item.to_ident(tcx).as_str().to_string();
                    let def_id = item.owner_id.to_def_id(); // Get the DefId of the trait item (e.g., get_number)
                    if tcx.def_kind(def_id) != rustc_hir::def::DefKind::AssocFn {
                        continue; // Skip non-function items
                    }
                    let mir_sig = tcx.type_of(def_id).skip_binder().fn_sig(tcx);

                    let params_ty = mir_sig.inputs();
                    let return_ty = mir_sig.output();

                    let data_types = &mut HashMap::new(); // Consider if this should be shared across loop iterations or functions

                    let instance = rustc_middle::ty::Instance::new_raw(
                        def_id,
                        rustc_middle::ty::GenericArgs::identity_for_item(tcx, def_id),
                    );

                    // Use skip_binder here too, as inputs/outputs are bound by the same binder as the fn_sig
                    let params_inputs = params_ty.skip_binder();

                    // For trait methods, skip all self-related parameters
                    // Trait method signatures include both explicit and implicit self parameters
                    // We want to skip anything that looks like self to get only the real parameters
                    let params_oomir: Vec<(String, oomir::Type)> = params_inputs
                        .iter()
                        .enumerate()
                        .filter_map(|(i, ty)| {
                            // Skip parameters that are self-related
                            // This includes: Self, &Self, &mut Self, etc.
                            let is_self_param = matches!(
                                ty.peel_refs().kind(),
                                rustc_middle::ty::TyKind::Param(param) if param.name.as_str() == "Self"
                            );

                            if is_self_param {
                                None // Skip this parameter
                            } else {
                                // For trait methods, we don't have MIR, so use generic names
                                let param_name = format!("arg{}", i);
                                let oomir_type =
                                    lower1::types::ty_to_oomir_type(*ty, tcx, data_types, instance);
                                Some((param_name, oomir_type))
                            }
                        })
                        .collect();
                    let return_oomir_ty: oomir::Type = lower1::types::ty_to_oomir_type(
                        return_ty.skip_binder(),
                        tcx,
                        data_types,
                        instance,
                    );

                    // If we filtered out any parameters, it was a self parameter, so it's an instance method
                    let is_instance_method = params_inputs.len() > params_oomir.len();

                    let mut signature = oomir::Signature {
                        params: params_oomir,
                        ret: Box::new(return_oomir_ty.clone()),
                        is_static: !is_instance_method,
                    };
                    let (params_changed, _) = signature.replace_class_in_signature("Self", &ident);

                    if params_changed {
                        signature.is_static = false;
                    }

                    fn_data.insert(name.clone(), signature.clone());

                    let mut args = vec![];
                    let mut idx = 1;
                    for (_arg_name_from_sig, arg_ty) in signature.clone().params {
                        let arg_name = format!("_{idx}");
                        let arg = Operand::Variable {
                            name: arg_name.clone(),
                            ty: arg_ty,
                        };
                        args.push(arg);
                        idx += 1;
                    }
                }

                oomir_module
                    .data_types
                    .insert(ident, oomir::DataType::Interface { methods: fn_data });
            }
        }

        // Lower all discovered monomorphized function instances
        for (instance, name) in fn_instances_to_lower {
            let def_id = instance.def_id();
            if !def_id.is_local() {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!("Skipping non-local instance (no MIR access): {:?}", def_id)
                );
                continue;
            }

            let mut mir = tcx.instance_mir(instance.def).clone();
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "mir-lowering",
                format!(
                    "--- Lowering monomorphized function instance: {} ---",
                    name.method_name
                )
            );
            let (mut oomir_function, data_types) =
                lower1::mir_to_oomir(tcx, instance, &mut mir, Some(name.clone()), true);

            oomir_module.merge_data_types(&data_types);

            // Idiomatic placement:
            // If this is a method of a generic struct, place it inside the OOMIR Class for that struct.
            let mut placed_in_class = false;
            if let Some(assoc_item) = tcx.opt_associated_item(instance.def_id()) {
                // It's an associated item (method). Check the container.
                let container_id = assoc_item.container_id(tcx);
                let is_trait_container =
                    matches!(tcx.def_kind(container_id), rustc_hir::def::DefKind::Trait);
                if !is_trait_container {
                    // Instantiate the container type (e.g. Pair<i32>)
                    let container_ty = tcx
                        .type_of(container_id)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();

                    // Determine the OOMIR type of the container
                    let mut temp_dt = HashMap::new();
                    let self_oomir_ty =
                        lower1::types::ty_to_oomir_type(container_ty, tcx, &mut temp_dt, instance);

                    if let oomir::Type::Class(class_name) = self_oomir_ty {
                        // Check if this is a user-defined class (not java/ or stdlib shim)
                        if !class_name.starts_with("java/")
                            && !class_name.starts_with("org/rustlang/")
                        {
                            // It is a user class!
                            // 1. Rename function to simple name (e.g. "new" instead of "new__hash")
                            oomir_function.name = tcx.item_name(instance.def_id()).to_string();
                            oomir_function.owner_class = None;

                            // 2. Check if this is an instance method.
                            if assoc_item.is_method() {
                                oomir_function.signature.is_static = false;
                            }

                            // 3. Insert into the class in oomir_module
                            if let Some(oomir::DataType::Class { methods, .. }) =
                                oomir_module.data_types.get_mut(&class_name)
                            {
                                methods.insert(
                                    oomir_function.name.clone(),
                                    oomir::DataTypeMethod::Function(oomir_function.clone()),
                                );
                                placed_in_class = true;

                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Info,
                                    "backend",
                                    format!(
                                        "Placed monomorphized method {} into class {}",
                                        oomir_function.name, class_name
                                    )
                                );
                            } else {
                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Warn,
                                    "backend",
                                    format!(
                                        "Warning: Class {} not found in module for method {}, falling back to free function",
                                        class_name, name.method_name
                                    )
                                );
                            }
                        }
                    }
                }
            }

            if !placed_in_class {
                oomir_module.insert_function(oomir_function);
            }
        }

        // Now lower all discovered closures
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "closure-lowering",
            format!(
                "Attempting to lower {} discovered closures",
                closures_to_lower.len()
            )
        );

        for closure_def_id in closures_to_lower {
            lower_closure_to_oomir(tcx, closure_def_id, &mut oomir_module);
        }

        install_fmt_argument_shim(&mut oomir_module);
        install_fmt_arguments_shim(&mut oomir_module);
        pad_construct_object_args_for_field_constructors(&mut oomir_module);

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "backend",
            format!("OOMIR module: {:?}", oomir_module)
        );

        // Emit checked arithmetic intrinsics for all needed operations
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "intrinsics",
            "Emitting checked arithmetic intrinsics..."
        );
        let needed_intrinsics = lower1::control_flow::take_needed_intrinsics();
        if !needed_intrinsics.is_empty() {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "intrinsics",
                format!(
                    "Emitting {} intrinsics: {:?}",
                    needed_intrinsics.len(),
                    needed_intrinsics
                )
            );
            let intrinsic_class =
                lower1::control_flow::checked_intrinsics::emit_all_needed_intrinsics(
                    &needed_intrinsics,
                );
            oomir_module
                .data_types
                .insert("RustcCodegenJVMIntrinsics".to_string(), intrinsic_class);
        }

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "optimisation",
            format!(
                "--- Starting OOMIR Optimisation for module: {} ---",
                crate_name
            )
        );

        let oomir_module = optimise1::optimise_module(oomir_module);

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "optimisation",
            format!("Optimised OOMIR module: {:?}", oomir_module)
        );

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "optimisation",
            format!(
                "--- Finished OOMIR Optimisation for module: {} ---",
                crate_name
            )
        );

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!(
                "--- Starting OOMIR to JVM Bytecode Lowering for module: {} ---",
                crate_name
            )
        );
        let bytecode = lower2::oomir_to_jvm_bytecode(&oomir_module, tcx).unwrap();
        //let bytecode = vec![0; 1024];
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!(
                "--- Finished OOMIR to JVM Bytecode Lowering for module: {} ---",
                crate_name
            )
        );

        Box::new((
            bytecode,
            crate_name,
            // metadata,
            CrateInfo::new(tcx, "java_bytecode_basic_class".to_string()),
        ))
    }

    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn Any>,
        _sess: &Session,
        outputs: &OutputFilenames,
        _crate_info: &CrateInfo,
    ) -> (CompiledModules, UnordMap<WorkProductId, WorkProduct>) {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // Update the downcast to expect a HashMap now.
            // panic!("{:#?}", ongoing_codegen.downcast::<std::collections::HashMap<String, Vec<u8>>>());
            let (bytecode_map, _, crate_info) = *ongoing_codegen
                .downcast::<(
                    std::collections::HashMap<String, Vec<u8>>,
                    String,
                    // EncodedMetadata,
                    CrateInfo,
                )>()
                .expect("in join_codegen: ongoing_codegen is not a bytecode map");

            let mut compiled_modules = Vec::new();

            // Iterate over each (file_name, bytecode) pair in the map.
            for (name, bytecode) in bytecode_map.into_iter() {
                let cgu_name = name.replace('/', "_");
                let file_path = outputs.temp_path_ext_for_cgu("class", &cgu_name);

                // extract the directory from the file path
                let dir = file_path.parent().unwrap();

                // make the actual file path by adding {name}.class to the directory
                let file_path = dir.join(format!("{}.class", name));
                if let Some(parent) = file_path.parent() {
                    std::fs::create_dir_all(parent).unwrap_or_else(|e| {
                        panic!(
                            "Could not create class output directory {}: {}",
                            parent.display(),
                            e
                        )
                    });
                }

                // Write the bytecode to the file
                let mut file = std::fs::File::create(&file_path).unwrap_or_else(|e| {
                    panic!("Could not create file {}: {}", file_path.display(), e)
                });
                file.write_all(&bytecode).unwrap_or_else(|e| {
                    panic!(
                        "Could not write bytecode to file {}: {}",
                        file_path.display(),
                        e
                    )
                });

                // Create a CompiledModule for this file
                compiled_modules.push(CompiledModule {
                    name: name.clone(),
                    kind: ModuleKind::Regular,
                    object: Some(file_path),
                    global_asm_object: None,
                    bytecode: None,
                    dwarf_object: None,
                    llvm_ir: None,
                    links_from_incr_cache: Vec::new(),
                    assembly: None,
                });
            }

            let compiled_modules = CompiledModules {
                modules: compiled_modules,
                allocator_module: None,
            };
            let _ = crate_info;
            (compiled_modules, UnordMap::default())
        }))
        .expect("Could not join_codegen")
    }

    fn link(
        &self,
        sess: &Session,
        compiled_modules: CompiledModules,
        crate_info: CrateInfo,
        metadata: EncodedMetadata,
        outputs: &OutputFilenames,
    ) {
        breadcrumbs::log!(breadcrumbs::LogLevel::Info, "backend", "linking!");
        use rustc_codegen_ssa::back::link::link_binary;
        link_binary(
            sess,
            &RlibArchiveBuilder,
            compiled_modules,
            crate_info,
            metadata,
            outputs,
            "jvm",
        );
    }
}

struct RustcCodegenJvmLogListener;

const LISTENING_CHANNELS: &[&str] = &[];

impl breadcrumbs::LogListener for RustcCodegenJvmLogListener {
    fn on_log(&mut self, log: breadcrumbs::Log) {
        if log.level.is_at_least(breadcrumbs::LogLevel::Warn)
            || LISTENING_CHANNELS.contains(&log.channel.as_str())
        {
            println!("{}", log);
        } else {
            log.remove();
        }
    }
}

#[unsafe(no_mangle)]
pub extern "Rust" fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    std::alloc::set_alloc_error_hook(custom_alloc_error_hook);
    breadcrumbs::init!(RustcCodegenJvmLogListener);
    Box::new(MyBackend)
}

use std::alloc::Layout;

/// # Panics
///
/// Panics when called, every time, with a message stating the memory allocation of the bytes
/// corresponding to the provided layout failed.
pub fn custom_alloc_error_hook(layout: Layout) {
    panic!("Memory allocation failed: {} bytes", layout.size());
}

struct RlibArchiveBuilder;
impl ArchiveBuilderBuilder for RlibArchiveBuilder {
    fn new_archive_builder<'a>(&self, sess: &'a Session) -> Box<dyn ArchiveBuilder + 'a> {
        Box::new(ArArchiveBuilder::new(
            sess,
            &rustc_codegen_ssa::back::archive::DEFAULT_OBJECT_READER,
        ))
    }
    fn create_dll_import_lib(
        &self,
        _sess: &Session,
        _lib_name: &str,
        _dll_imports: std::vec::Vec<rustc_codegen_ssa::back::archive::ImportLibraryItem>,
        _tmpdir: &Path,
    ) {
        unimplemented!("creating dll imports is not supported");
    }
}
