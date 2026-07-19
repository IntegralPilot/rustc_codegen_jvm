#![feature(alloc_error_hook)]
#![feature(box_patterns)]
#![feature(rustc_private)]
#![warn(clippy::pedantic)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]

//! Rustc Codegen JVM
//!
//! Compiler backend for rustc that generates JVM bytecode, using a two-stage lowering process:
//! MIR -> OOMIR -> JVM Bytecode.

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_codegen_ssa;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

use oomir::Type;
use rustc_codegen_ssa::back::archive::{ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder};
use rustc_codegen_ssa::{
    CompiledModule, CompiledModules, CrateInfo, ModuleKind, traits::CodegenBackend,
};
use rustc_hir::def::DefKind;
use std::collections::{HashMap, HashSet, VecDeque};

use rustc_data_structures::unord::UnordMap;
use rustc_metadata::EncodedMetadata;
use rustc_middle::{
    dep_graph::{WorkProduct, WorkProductId},
    mono::MonoItem,
    ty::{EarlyBinder, GenericArgs, Instance, InstanceKind, TyCtxt, TyKind, TypingEnv},
};
use rustc_session::{
    Session,
    config::{CrateType, OutputFilenames},
};
use rustc_span::def_id::{DefId, LOCAL_CRATE};
use std::{
    any::Any,
    ffi::OsString,
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

mod instrumentation;
mod lower1;
mod lower2;
mod oomir;
mod optimise1;
mod stable_hash;

/// An instance of our Java bytecode codegen backend.
struct MyBackend;

fn write_linker_response_file(path: &Path, arguments: &[OsString]) -> std::io::Result<()> {
    let mut contents = vec![0xff, 0xfe];
    for argument in arguments {
        let argument = argument.to_string_lossy();
        if argument.contains('\r') || argument.contains('\n') {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "linker response-file arguments cannot contain newlines",
            ));
        }

        let line = format!("\"{}\"\r\n", argument.replace('"', "\\\""));
        for code_unit in line.encode_utf16() {
            contents.extend_from_slice(&code_unit.to_le_bytes());
        }
    }

    std::fs::write(path, contents)
}

fn emit_library_sidecar_jar(
    sess: &Session,
    outputs: &OutputFilenames,
    crate_info: &CrateInfo,
    compiled_modules: &[CompiledModule],
) {
    if !outputs.outputs.should_link() {
        return;
    }

    if !crate_info
        .crate_types
        .iter()
        .any(|crate_type| !matches!(crate_type, CrateType::Executable))
    {
        return;
    }

    let linker = sess.opts.cg.linker.clone().or_else(|| {
        sess.target
            .linker
            .as_ref()
            .map(|linker| PathBuf::from(linker.as_ref()))
    });
    let Some(linker) = linker else {
        panic!("cannot emit JVM library sidecar jar: no linker was configured");
    };

    let class_files: Vec<_> = compiled_modules
        .iter()
        .filter_map(|module| module.object.as_ref())
        .filter(|path| path.extension().is_some_and(|ext| ext == "class"))
        .collect();
    if class_files.is_empty() {
        return;
    }

    let jar_path = outputs.with_extension("jar");
    if let Some(parent) = jar_path.parent() {
        std::fs::create_dir_all(parent).unwrap_or_else(|e| {
            panic!(
                "Could not create JVM sidecar jar output directory {}: {}",
                parent.display(),
                e
            )
        });
    }

    let mut linker_arguments: Vec<OsString> = class_files
        .iter()
        .map(|class_file| class_file.as_os_str().to_os_string())
        .collect();
    for link_arg in &sess.opts.cg.link_args {
        linker_arguments.push(OsString::from(link_arg));
    }
    linker_arguments.push(OsString::from("-o"));
    linker_arguments.push(jar_path.as_os_str().to_os_string());

    let response_path = jar_path.with_extension("jar.rsp");
    write_linker_response_file(&response_path, &linker_arguments).unwrap_or_else(|e| {
        panic!(
            "Could not write JVM linker response file {} for library sidecar jar {}: {}",
            response_path.display(),
            jar_path.display(),
            e
        )
    });

    let mut response_argument = OsString::from("@");
    response_argument.push(&response_path);
    let output = Command::new(&linker).arg(response_argument).output();
    let _ = std::fs::remove_file(&response_path);

    let output = output.unwrap_or_else(|e| {
        panic!(
            "Could not run JVM linker {} for library sidecar jar {}: {}",
            linker.display(),
            jar_path.display(),
            e
        )
    });
    if !output.status.success() {
        panic!(
            "JVM linker exited with status {} while emitting library sidecar jar {}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            jar_path.display(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    if sess.opts.json_artifact_notifications {
        sess.dcx().emit_artifact_notification(&jar_path, "link");
    }
}

fn mono_item_name<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> lower1::naming::FnNameData {
    let instance_ty = tcx
        .type_of(instance.def_id())
        .instantiate(tcx, instance.args)
        .skip_norm_wip();

    if matches!(instance_ty.kind(), TyKind::Closure(..)) {
        return lower1::naming::FnNameData {
            class_to_call_on: Some(lower1::naming::mono_owner_class(tcx, instance)),
            method_name: lower1::generate_closure_function_name(tcx, instance),
        };
    }

    lower1::naming::mono_fn_name_from_instance(tcx, instance)
}

fn materialize_instance_receiver_pointer<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    receiver_ty: rustc_middle::ty::Ty<'tcx>,
    receiver_class: &str,
    function: &mut oomir::Function,
    data_types: &mut HashMap<String, oomir::DataType>,
) {
    let Some((_, pointer_ty @ Type::Pointer(_))) = function.signature.params.first() else {
        return;
    };
    let pointer_ty = pointer_ty.clone();
    let size = lower1::types::layout_size_bytes(tcx, receiver_ty)
        .unwrap_or_else(|error| panic!("could not determine instance receiver layout: {error}"));
    let alignment = lower1::types::layout_align_bytes(tcx, receiver_ty)
        .unwrap_or_else(|error| panic!("could not determine instance receiver alignment: {error}"));
    let codec = lower1::types::pointer_memory_codec_operand(receiver_ty, tcx, data_types, instance);
    let materialize = oomir::Instruction::InvokeStatic {
        dest: Some(oomir::INSTANCE_RECEIVER_POINTER_LOCAL.to_string()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "receiverCellAligned".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "value".to_string(),
                    Type::Class("java/lang/Object".to_string()),
                ),
                ("size".to_string(), Type::I32),
                ("codec".to_string(), Type::java_string()),
                ("alignment".to_string(), Type::I32),
            ],
            ret: Box::new(pointer_ty),
            is_static: true,
        },
        args: vec![
            oomir::Operand::Variable {
                name: "_1".to_string(),
                ty: Type::Class(receiver_class.to_string()),
            },
            oomir::Operand::Constant(oomir::Constant::I32(
                i32::try_from(size).expect("instance receiver exceeds the JVM address space"),
            )),
            codec,
            oomir::Operand::Constant(oomir::Constant::I32(
                i32::try_from(alignment)
                    .expect("instance receiver alignment exceeds the JVM address space"),
            )),
        ],
    };
    let entry = function
        .body
        .basic_blocks
        .get_mut(&function.body.entry)
        .expect("OOMIR function has an entry block");
    let insert_at = entry
        .instructions
        .iter()
        .take_while(|instruction| {
            matches!(
                instruction,
                oomir::Instruction::SourceLocation(_) | oomir::Instruction::LocalVariableScope(_)
            )
        })
        .count();
    entry.instructions.insert(insert_at, materialize);
}

fn place_or_insert_mono_function<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    name: &lower1::naming::FnNameData,
    mut oomir_function: oomir::Function,
    oomir_module: &mut oomir::Module,
) {
    let has_global_linkage = name
        .class_to_call_on
        .as_deref()
        .is_some_and(lower1::naming::is_global_link_symbol_class);
    if !has_global_linkage && let Some(assoc_item) = tcx.opt_associated_item(instance.def_id()) {
        let attachable_to_receiver_class = assoc_item.trait_container(tcx).is_none()
            && (assoc_item.trait_item_def_id().is_none() || assoc_item.is_method());
        if attachable_to_receiver_class {
            let fallback_function = oomir_function.clone();
            let has_enum_reference_receiver = assoc_item.is_method()
                && tcx
                    .fn_sig(instance.def_id())
                    .instantiate(tcx, instance.args)
                    .skip_binder()
                    .inputs()
                    .first()
                    .is_some_and(|receiver| {
                        matches!(
                            receiver.kind(),
                            TyKind::Ref(_, pointee, _)
                                if matches!(pointee.kind(), TyKind::Adt(adt_def, _)
                                    if adt_def.is_enum())
                        )
                    });
            let container_id = assoc_item.container_id(tcx);
            let container_ty = tcx
                .type_of(container_id)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let self_oomir_ty = lower1::types::ty_to_oomir_type(
                container_ty,
                tcx,
                &mut oomir_module.data_types,
                instance,
            );

            if !has_enum_reference_receiver && let Type::Class(class_name) = self_oomir_ty {
                let can_extend_compiled_core_class = lower1::jvm_names::uses_compiled_core(tcx)
                    && (instance.def_id().is_local()
                        || lower1::jvm_names::compiles_external_core_instances(tcx));
                let is_runtime_owned_class =
                    class_name.starts_with("org/rustlang/") && !can_extend_compiled_core_class;
                if !class_name.starts_with("java/") && !is_runtime_owned_class {
                    oomir_function.name =
                        lower1::naming::associated_method_name_from_instance(tcx, instance);
                    oomir_function.owner_class = None;

                    if assoc_item.is_method() {
                        oomir_function.signature.is_static = false;
                    }

                    let implemented_trait = assoc_item
                        .impl_container(tcx)
                        .and_then(|impl_def_id| tcx.impl_opt_trait_ref(impl_def_id))
                        .map(|trait_ref| {
                            let trait_ref =
                                trait_ref.instantiate(tcx, instance.args).skip_norm_wip();
                            let trait_name =
                                lower1::jvm_names::class_for_def_id(tcx, trait_ref.def_id);
                            ensure_trait_interface(
                                tcx,
                                trait_ref.def_id,
                                &mut oomir_module.data_types,
                            );
                            oomir_function
                                .signature
                                .replace_class_in_signature(&trait_name, &class_name);
                            trait_name
                        });

                    if assoc_item.is_method() {
                        materialize_instance_receiver_pointer(
                            tcx,
                            instance,
                            container_ty,
                            &class_name,
                            &mut oomir_function,
                            &mut oomir_module.data_types,
                        );
                    }

                    let mut has_instance_method = false;
                    if let Some(oomir::DataType::Class {
                        methods,
                        interfaces,
                        ..
                    }) = oomir_module.data_types.get_mut(&class_name)
                    {
                        let trait_method_matches_existing = implemented_trait.is_some()
                            && methods.get(&oomir_function.name).is_some_and(|method| {
                                matches!(method,
                                    oomir::DataTypeMethod::Function(existing)
                                        if existing.signature.to_string()
                                            == oomir_function.signature.to_string())
                            });
                        if let Some(trait_name) = implemented_trait {
                            if !interfaces.contains(&trait_name) {
                                interfaces.push(trait_name);
                            }
                        }
                        if trait_method_matches_existing {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mono-lowering",
                                format!(
                                    "Kept existing {}.{} for matching trait method; emitted {} as a static fallback",
                                    class_name, oomir_function.name, name.method_name
                                )
                            );
                            has_instance_method = true;
                        } else {
                            methods.insert(
                                oomir_function.name.clone(),
                                oomir::DataTypeMethod::Function(oomir_function.clone()),
                            );

                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mono-lowering",
                                format!(
                                    "Placed mono item {} into class {}",
                                    name.method_name, class_name
                                )
                            );
                            // Rust can statically resolve an associated method
                            // and name its monomorphized owner directly. Keep a
                            // static copy under that canonical owner in addition
                            // to the instance method used for JVM dispatch on the
                            // concrete class.
                            has_instance_method = true;
                        }
                    }

                    if !has_instance_method {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mono-lowering",
                            format!(
                                "Class {} not declared for mono method {}; keeping it as an owned static function",
                                class_name, name.method_name
                            )
                        );
                    }
                    oomir_function = fallback_function;
                }
            }
        }
    }

    // Emit the canonical owner-module form used by statically resolved Rust
    // calls. JVM module methods are static, so a Rust method's receiver must
    // remain an explicit descriptor parameter in this form.
    oomir_function.signature.is_static = true;
    oomir_module.insert_function(oomir_function);
}

fn emit_allocator_shim_guard(tcx: TyCtxt<'_>, oomir_module: &mut oomir::Module) {
    if rustc_codegen_ssa::base::allocator_kind_for_codegen(tcx).is_none() {
        return;
    }

    let entry = "entry".to_string();
    let symbol_name =
        lower1::jvm_names::member_name(rustc_ast::expand::allocator::NO_ALLOC_SHIM_IS_UNSTABLE);
    oomir_module.insert_function(oomir::Function {
        owner_class: Some(lower1::naming::global_link_symbol_class(&symbol_name)),
        name: symbol_name,
        signature: oomir::Signature {
            params: Vec::new(),
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        debug_variables: Vec::new(),
        body: oomir::CodeBlock {
            entry: entry.clone(),
            basic_blocks: HashMap::from([(
                entry.clone(),
                oomir::BasicBlock {
                    label: entry,
                    instructions: vec![oomir::Instruction::Return { operand: None }],
                },
            )]),
        },
    });
}

fn lower_mono_function<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    oomir_module: &mut oomir::Module,
    lowered_instances: &mut HashSet<Instance<'tcx>>,
) {
    let is_external_runtime_item = !instance.def_id().is_local()
        && lower1::jvm_names::is_runtime_crate(tcx, instance.def_id().krate);
    let uses_runtime_implementation =
        is_external_runtime_item && !lower1::jvm_names::compiles_external_core_instances(tcx);
    let needs_compiled_primitive_operator = uses_runtime_implementation
        && matches!(
            lower1::jvm_names::owner_class_for_function(tcx, instance.def_id()).as_str(),
            "org/rustlang/core/ops/arith" | "org/rustlang/core/ops/bit"
        );
    if uses_runtime_implementation && !needs_compiled_primitive_operator {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "mono-lowering",
            format!("Using runtime implementation for mono function: {instance:?}")
        );
        return;
    }

    if !lowered_instances.insert(instance) {
        return;
    }

    if matches!(
        instance.def,
        InstanceKind::Intrinsic(..) | InstanceKind::LlvmIntrinsic(..) | InstanceKind::Virtual(..)
    ) {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Warn,
            "mono-lowering",
            format!(
                "Skipping mono function without a concrete MIR body: {:?}",
                instance
            )
        );
        return;
    }

    let name = mono_item_name(tcx, instance);
    let mut mir = tcx.instance_mir(instance.def).clone();
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "mono-lowering",
        format!(
            "Lowering mono function {} from {:?}",
            name.method_name, instance
        )
    );

    let mut oomir_function = lower1::mir_to_oomir(
        tcx,
        instance,
        &mut mir,
        Some(name.clone()),
        true,
        &mut oomir_module.data_types,
    );
    if tcx.is_intrinsic(instance.def_id(), rustc_span::sym::const_allocate) {
        let result_ty = oomir_function.signature.ret.as_ref().clone();
        let result = "__const_allocate_result".to_string();
        let entry = "entry".to_string();
        oomir_function.body = oomir::CodeBlock {
            entry: entry.clone(),
            basic_blocks: HashMap::from([(
                entry.clone(),
                oomir::BasicBlock {
                    label: entry,
                    instructions: vec![
                        oomir::Instruction::InvokeStatic {
                            dest: Some(result.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "nullPointer".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![("view_size".to_string(), oomir::Type::U64)],
                                ret: Box::new(result_ty.clone()),
                                is_static: true,
                            },
                            args: vec![oomir::Operand::Constant(oomir::Constant::U64(1))],
                        },
                        oomir::Instruction::Return {
                            operand: Some(oomir::Operand::Variable {
                                name: result,
                                ty: result_ty,
                            }),
                        },
                    ],
                },
            )]),
        };
    }
    place_or_insert_mono_function(tcx, instance, &name, oomir_function, oomir_module);
}

fn lower_mono_items<'tcx>(
    tcx: TyCtxt<'tcx>,
    oomir_module: &mut oomir::Module,
    lowered_instances: &mut HashSet<Instance<'tcx>>,
) {
    let cgus = tcx.collect_and_partition_mono_items(());
    let mut seen = HashSet::new();
    let mut functions = VecDeque::new();

    for cgu in cgus.codegen_units {
        for (mono_item, _data) in cgu.items_in_deterministic_order(tcx) {
            if !seen.insert(mono_item) {
                continue;
            }

            match mono_item {
                MonoItem::Fn(instance) => {
                    functions.push_back(instance);
                }
                MonoItem::Static(def_id) => {
                    lower1::statics::lower_static(tcx, def_id, oomir_module).unwrap_or_else(
                        |error| panic!("failed to lower static {def_id:?}: {error}"),
                    );
                }
                MonoItem::GlobalAsm(item_id) => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mono-lowering",
                        format!("Skipping global asm mono item: {:?}", item_id)
                    );
                }
            }
        }
    }

    let mut queued = functions.iter().copied().collect::<HashSet<_>>();
    while let Some(instance) = functions.pop_front() {
        for callee in direct_mir_callees(tcx, instance) {
            if !matches!(
                callee.def,
                InstanceKind::Intrinsic(_)
                    | InstanceKind::LlvmIntrinsic(_)
                    | InstanceKind::Virtual(..)
            ) && queued.insert(callee)
            {
                functions.push_back(callee);
            }
        }
        lower_mono_function(tcx, instance, oomir_module, lowered_instances);
    }
}

fn direct_mir_callees<'tcx>(tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> Vec<Instance<'tcx>> {
    let has_callable_mir = match instance.def {
        InstanceKind::Item(_) => tcx.is_mir_available(instance.def_id()),
        InstanceKind::Shim(_) => true,
        InstanceKind::Intrinsic(_) | InstanceKind::LlvmIntrinsic(_) | InstanceKind::Virtual(..) => {
            false
        }
    };
    if !has_callable_mir {
        return Vec::new();
    }

    let mir = tcx.instance_mir(instance.def);
    let typing_env = TypingEnv::post_analysis(tcx, mir.source.def_id());
    mir.basic_blocks
        .iter()
        .filter_map(|block| {
            let terminator = block.terminator();
            let rustc_middle::mir::TerminatorKind::Call { func, .. } = &terminator.kind else {
                return None;
            };
            let func_ty = EarlyBinder::bind(tcx, func.ty(mir, tcx))
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let TyKind::FnDef(def_id, args) = func_ty.kind() else {
                return None;
            };
            let args = args.no_bound_vars()?;
            let callee = Instance::expect_resolve(
                tcx,
                typing_env,
                *def_id,
                args,
                terminator.source_info.span,
            );
            match callee.def {
                InstanceKind::Item(_) => tcx.is_mir_available(callee.def_id()).then_some(callee),
                InstanceKind::Shim(_) => Some(callee),
                InstanceKind::Intrinsic(_)
                | InstanceKind::LlvmIntrinsic(_)
                | InstanceKind::Virtual(..) => None,
            }
        })
        .collect()
}

fn ensure_trait_interface<'tcx>(
    tcx: TyCtxt<'tcx>,
    trait_def_id: DefId,
    data_types: &mut HashMap<String, oomir::DataType>,
) {
    let interface_name = lower1::jvm_names::class_for_def_id(tcx, trait_def_id);
    let methods = trait_interface_methods(tcx, trait_def_id, &interface_name, data_types);

    match data_types.get_mut(&interface_name) {
        Some(oomir::DataType::Interface {
            methods: existing_methods,
        }) => {
            existing_methods.extend(methods);
        }
        Some(oomir::DataType::Class { .. }) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "mono-lowering",
                format!(
                    "Trait interface '{}' already exists as a class; leaving it unchanged",
                    interface_name
                )
            );
        }
        None => {
            data_types.insert(interface_name, oomir::DataType::Interface { methods });
        }
    }
}

fn trait_interface_methods<'tcx>(
    tcx: TyCtxt<'tcx>,
    trait_def_id: DefId,
    interface_name: &str,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> HashMap<String, oomir::Signature> {
    let mut methods = HashMap::new();

    for assoc_item in tcx.associated_items(trait_def_id).in_definition_order() {
        let def_id = assoc_item.def_id;
        // Trait functions without a receiver are statically dispatched. JVM
        // interfaces cannot declare an abstract static method, so only methods
        // which participate in interface dispatch belong in this table.
        if !assoc_item.is_method() {
            continue;
        }

        let mir_sig = tcx.type_of(def_id).skip_binder().fn_sig(tcx);
        let params_ty = mir_sig.inputs();
        let return_ty = mir_sig.output();
        let explicit_inputs = params_ty.skip_binder();
        let output = return_ty.skip_binder();
        let instance = Instance::new_raw(
            def_id,
            rustc_middle::ty::GenericArgs::identity_for_item(tcx, def_id),
        );
        let has_open_abi_type = |ty: rustc_middle::ty::Ty<'tcx>| {
            lower1::types::has_open_jvm_abi_type(ty, tcx, instance)
        };
        if explicit_inputs
            .iter()
            .skip(1)
            .copied()
            .any(has_open_abi_type)
            || has_open_abi_type(output)
        {
            continue;
        }
        let params_oomir: Vec<(String, oomir::Type)> = explicit_inputs
            .iter()
            .enumerate()
            .filter_map(|(i, ty)| {
                if assoc_item.is_method() && i == 0 {
                    None
                } else {
                    let param_name = format!("arg{}", i);
                    let oomir_type =
                        lower1::types::ty_to_erased_oomir_type(*ty, tcx, data_types, instance);
                    Some((param_name, oomir_type))
                }
            })
            .collect();
        let return_oomir_ty =
            lower1::types::ty_to_erased_oomir_type(output, tcx, data_types, instance);

        let mut signature = oomir::Signature {
            params: params_oomir,
            ret: Box::new(return_oomir_ty),
            is_static: false,
        };
        let (params_changed, _) = signature.replace_class_in_signature("Self", interface_name);

        if params_changed {
            signature.is_static = false;
        }

        methods.insert(assoc_item.name().as_str().to_string(), signature);
    }

    methods
}

fn crate_emits_library_artifact(tcx: TyCtxt<'_>) -> bool {
    tcx.crate_types()
        .iter()
        .any(|crate_type| !matches!(crate_type, CrateType::Executable))
}

fn is_lowerable_java_public_function(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    if !matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn) {
        return false;
    }

    if let Some(assoc_item) = tcx.opt_associated_item(def_id) {
        if assoc_item.trait_container(tcx).is_some() {
            return false;
        }
        if tcx.crate_name(LOCAL_CRATE) == rustc_span::sym::core
            && assoc_item.impl_container(tcx).is_some()
        {
            return false;
        }
    }

    def_id.is_local()
        && !tcx.generics_of(def_id).requires_monomorphization(tcx)
        && tcx.is_mir_available(def_id)
}

enum JavaPublicSurface {
    Exported,
    Reachable,
}

fn java_public_surface_def_ids(tcx: TyCtxt<'_>, surface: JavaPublicSurface) -> Vec<DefId> {
    let effective_visibilities = tcx.effective_visibilities(());
    let mut def_ids: Vec<_> = effective_visibilities
        .iter()
        .filter_map(|(&local_def_id, _)| {
            let is_public_enough = match surface {
                JavaPublicSurface::Exported => effective_visibilities.is_exported(local_def_id),
                JavaPublicSurface::Reachable => effective_visibilities.is_reachable(local_def_id),
            };
            is_public_enough.then_some(local_def_id.to_def_id())
        })
        .collect();

    def_ids.sort_by_cached_key(|def_id| tcx.def_path_str(*def_id));
    def_ids
}

fn materialize_java_public_data_type<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    oomir_module: &mut oomir::Module,
) {
    match tcx.def_kind(def_id) {
        DefKind::Struct | DefKind::Enum | DefKind::Union => {
            if !tcx.generics_of(def_id).own_params.is_empty() {
                return;
            }
            let item_ty = tcx.type_of(def_id).instantiate_identity().skip_norm_wip();
            let instance_context =
                Instance::new_raw(def_id, GenericArgs::identity_for_item(tcx, def_id));
            lower1::types::ty_to_oomir_type(
                item_ty,
                tcx,
                &mut oomir_module.data_types,
                instance_context,
            );
        }
        DefKind::Trait => ensure_trait_interface(tcx, def_id, &mut oomir_module.data_types),
        _ => {}
    }
}

fn lower_public_library_exports<'tcx>(
    tcx: TyCtxt<'tcx>,
    oomir_module: &mut oomir::Module,
    lowered_instances: &mut HashSet<Instance<'tcx>>,
) {
    if !crate_emits_library_artifact(tcx) {
        return;
    }

    let function_defs = java_public_surface_def_ids(tcx, JavaPublicSurface::Exported);

    for def_id in function_defs {
        if is_lowerable_java_public_function(tcx, def_id) {
            lower_mono_function(
                tcx,
                Instance::mono(tcx, def_id),
                oomir_module,
                lowered_instances,
            );
        }
    }

    let data_type_defs = java_public_surface_def_ids(tcx, JavaPublicSurface::Reachable);

    for def_id in data_type_defs {
        materialize_java_public_data_type(tcx, def_id, oomir_module);
    }
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
        rustc_middle::ty::print::with_no_trimmed_paths!({
            let rust_crate = LOCAL_CRATE;
            let crate_name = tcx.crate_name(rust_crate).to_string();
            let crate_module_class = lower1::jvm_names::crate_module_class(tcx, rust_crate);

            let mut oomir_module = oomir::Module {
                name: crate_module_class.clone(),
                source_file: tcx
                    .sess
                    .local_crate_source_file()
                    .map(|file_name| rustc_span::FileName::Real(file_name).short().to_string()),
                functions: std::collections::HashMap::new(),
                data_types: std::collections::HashMap::new(),
                statics: std::collections::HashMap::new(),
            };

            let lower1_timer = instrumentation::Timer::phase("lower1", Some(&crate_name));

            let mut lowered_instances = HashSet::new();
            lower_public_library_exports(tcx, &mut oomir_module, &mut lowered_instances);
            lower_mono_items(tcx, &mut oomir_module, &mut lowered_instances);
            emit_allocator_shim_guard(tcx, &mut oomir_module);

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
            drop(lower1_timer);

            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "optimisation",
                format!(
                    "--- Starting OOMIR Optimisation for module: {} ---",
                    crate_name
                )
            );

            let optimise1_timer = instrumentation::Timer::phase("optimise1", Some(&crate_name));
            let oomir_module = optimise1::optimise_module(oomir_module);
            drop(optimise1_timer);

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
            let lower2_timer = instrumentation::Timer::phase("lower2", Some(&crate_name));
            let bytecode = lower2::oomir_to_jvm_bytecode(&oomir_module, tcx).unwrap();
            drop(lower2_timer);
            //let bytecode = vec![0; 1024];
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "bytecode-gen",
                format!(
                    "--- Finished OOMIR to JVM Bytecode Lowering for module: {} ---",
                    crate_name
                )
            );

            Box::new((bytecode, crate_name))
        })
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
            let (bytecode_map, _) = *ongoing_codegen
                .downcast::<(std::collections::HashMap<String, Vec<u8>>, String)>()
                .expect("in join_codegen: ongoing_codegen is not a bytecode map");

            let mut compiled_modules = Vec::new();

            // Iterate over each (file_name, bytecode) pair in the map.
            for (name, bytecode) in bytecode_map.into_iter() {
                let cgu_name = name.replace('/', "_");
                // The filesystem object name must remain crate-specific. Multiple
                // crates may emit complementary definitions for the same JVM class,
                // and Cargo can compile and archive those crates concurrently. The
                // canonical JVM name is stored inside the classfile and recovered by
                // the JVM linker; it does not need to be repeated in the path.
                let file_path = outputs.temp_path_ext_for_cgu("class", &cgu_name);
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
        emit_library_sidecar_jar(sess, outputs, &crate_info, &compiled_modules.modules);
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
