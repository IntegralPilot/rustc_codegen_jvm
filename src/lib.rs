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

use oomir::Type;
use rustc_codegen_ssa::back::archive::{ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder};
use rustc_codegen_ssa::{
    CompiledModule, CompiledModules, CrateInfo, ModuleKind, traits::CodegenBackend,
};
use std::collections::{HashMap, HashSet};

use rustc_data_structures::unord::UnordMap;
use rustc_hir::{QPath, TyKind as HirTyKind, def_id::LocalDefId};
use rustc_metadata::EncodedMetadata;
use rustc_middle::{
    dep_graph::{WorkProduct, WorkProductId},
    mono::MonoItem,
    ty::{Instance, InstanceKind, TyCtxt, TyKind},
};
use rustc_session::{
    Session,
    config::{CrateType, OutputFilenames},
};
use std::{any::Any, io::Write, path::Path, process::Command};

use misc::ToIdent;

mod instrumentation;
mod lower1;
mod lower2;
mod misc;
mod oomir;
mod optimise1;

/// An instance of our Java bytecode codegen backend.
struct MyBackend;

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

    let Some(linker) = sess.opts.cg.linker.as_ref() else {
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

    let mut command = Command::new(linker);
    for class_file in class_files {
        command.arg(class_file);
    }
    for link_arg in &sess.opts.cg.link_args {
        command.arg(link_arg);
    }
    command.arg("-o").arg(&jar_path);

    let output = command.output().unwrap_or_else(|e| {
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

fn mono_item_name<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    oomir_module: &oomir::Module,
) -> lower1::naming::FnNameData {
    let instance_ty = tcx
        .type_of(instance.def_id())
        .instantiate(tcx, instance.args)
        .skip_norm_wip();

    if matches!(instance_ty.kind(), TyKind::Closure(..)) {
        return lower1::naming::FnNameData {
            class_to_call_on: Some(oomir_module.name.clone()),
            method_name: lower1::generate_closure_function_name(tcx, instance.def_id()),
        };
    }

    lower1::naming::mono_fn_name_from_instance(tcx, instance)
}

fn place_or_insert_mono_function<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    name: &lower1::naming::FnNameData,
    mut oomir_function: oomir::Function,
    oomir_module: &mut oomir::Module,
) {
    if let Some(assoc_item) = tcx.opt_associated_item(instance.def_id()) {
        let container_id = assoc_item.container_id(tcx);
        let is_trait_container =
            matches!(tcx.def_kind(container_id), rustc_hir::def::DefKind::Trait);

        if !is_trait_container {
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

            if let Type::Class(class_name) = self_oomir_ty {
                if !class_name.starts_with("java/") && !class_name.starts_with("org/rustlang/") {
                    oomir_function.name =
                        lower1::naming::associated_method_name_from_instance(tcx, instance);
                    oomir_function.owner_class = None;

                    if assoc_item.is_method() {
                        oomir_function.signature.is_static = false;
                    }

                    if matches!(
                        tcx.def_kind(container_id),
                        rustc_hir::def::DefKind::Impl { of_trait: true }
                    ) {
                        let trait_ref = tcx
                            .impl_trait_ref(container_id)
                            .instantiate(tcx, instance.args)
                            .skip_norm_wip();
                        let trait_name = lower1::jvm_names::class_for_def_id(tcx, trait_ref.def_id);
                        oomir_function
                            .signature
                            .replace_class_in_signature(&trait_name, &class_name);
                    }

                    if let Some(oomir::DataType::Class { methods, .. }) =
                        oomir_module.data_types.get_mut(&class_name)
                    {
                        methods.insert(
                            oomir_function.name.clone(),
                            oomir::DataTypeMethod::Function(oomir_function),
                        );

                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mono-lowering",
                            format!(
                                "Placed mono item {} into class {}",
                                name.method_name, class_name
                            )
                        );
                        return;
                    }

                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mono-lowering",
                        format!(
                            "Class {} not declared for mono method {}; keeping it as an owned static function",
                            class_name, name.method_name
                        )
                    );
                }
            }
        }
    }

    oomir_module.insert_function(oomir_function);
}

fn lower_mono_function<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    oomir_module: &mut oomir::Module,
) {
    if !instance.def_id().is_local() {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "mono-lowering",
            format!("Skipping non-local mono function: {:?}", instance)
        );
        return;
    }

    if matches!(
        instance.def,
        InstanceKind::Intrinsic(..) | InstanceKind::Virtual(..)
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

    let name = mono_item_name(tcx, instance, oomir_module);
    let key = name.key(&oomir_module.name);
    if oomir_module.functions.contains_key(&key) {
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "mono-lowering",
            format!("Mono function {} already lowered, skipping", key)
        );
        return;
    }

    let mut mir = tcx.instance_mir(instance.def).clone();
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "mono-lowering",
        format!(
            "Lowering mono function {} from {:?}",
            name.method_name, instance
        )
    );

    let oomir_function = lower1::mir_to_oomir(
        tcx,
        instance,
        &mut mir,
        Some(name.clone()),
        true,
        &mut oomir_module.data_types,
    );

    place_or_insert_mono_function(tcx, instance, &name, oomir_function, oomir_module);
}

fn lower_mono_items<'tcx>(tcx: TyCtxt<'tcx>, oomir_module: &mut oomir::Module) {
    let cgus = tcx.collect_and_partition_mono_items(());
    let mut seen = HashSet::new();

    for cgu in cgus.codegen_units {
        for (mono_item, _data) in cgu.items_in_deterministic_order(tcx) {
            if !seen.insert(mono_item) {
                continue;
            }

            match mono_item {
                MonoItem::Fn(instance) => lower_mono_function(tcx, instance, oomir_module),
                MonoItem::Static(def_id) => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mono-lowering",
                        format!(
                            "Skipping mono static item without static lowering: {:?}",
                            def_id
                        )
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
}

fn crate_emits_library_artifact(tcx: TyCtxt<'_>) -> bool {
    tcx.crate_types()
        .iter()
        .any(|crate_type| !matches!(crate_type, CrateType::Executable))
}

fn is_lowerable_reachable_body(tcx: TyCtxt<'_>, def_id: LocalDefId) -> bool {
    matches!(
        tcx.def_kind(def_id),
        rustc_hir::def::DefKind::Fn | rustc_hir::def::DefKind::AssocFn
    ) && !tcx.generics_of(def_id).requires_monomorphization(tcx)
        && tcx.is_mir_available(def_id.to_def_id())
}

fn lower_public_library_exports<'tcx>(tcx: TyCtxt<'tcx>, oomir_module: &mut oomir::Module) {
    if !crate_emits_library_artifact(tcx) {
        return;
    }

    let reachable_defs: Vec<_> = tcx.with_stable_hashing_context(|mut hcx| {
        tcx.reachable_set(())
            .items()
            .copied()
            .filter(|&def_id| is_lowerable_reachable_body(tcx, def_id))
            .collect_sorted(&mut hcx, true)
    });

    for def_id in reachable_defs {
        lower_mono_function(tcx, Instance::mono(tcx, def_id.to_def_id()), oomir_module);
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
        let rust_crate = rustc_hir::def_id::CRATE_DEF_ID.to_def_id().krate;
        let crate_name = tcx.crate_name(rust_crate).to_string();
        let crate_module_class = lower1::jvm_names::crate_module_class(tcx, rust_crate);

        let mut oomir_module = oomir::Module {
            name: crate_module_class.clone(),
            functions: std::collections::HashMap::new(),
            data_types: std::collections::HashMap::new(),
        };

        let lower1_timer = instrumentation::Timer::phase("lower1", Some(&crate_name));

        // HIR still declares source-level JVM shapes. Function reachability and
        // monomorphized body lowering are handled by rustc's mono-item collector below.
        let module_items = tcx.hir_crate_items(());

        for item_id in module_items.free_items() {
            let item = tcx.hir_item(item_id);
            if let rustc_hir::ItemKind::Impl(impl_a) = item.kind {
                let impl_def_id = item_id.owner_id.to_def_id();
                let impl_generics = tcx.generics_of(impl_def_id);

                if !impl_generics.own_params.is_empty() {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "backend",
                        format!(
                            "Skipping source-level declaration pass for generic impl block (DefId: {:?}); concrete methods are handled by mono items",
                            impl_def_id
                        )
                    );
                    continue;
                }

                let legacy_ident = match impl_a.self_ty.kind {
                    HirTyKind::Path(qpath) => match qpath {
                        QPath::Resolved(_, p) => format!("{}", p.segments[0].ident),
                        QPath::TypeRelative(_, ps) => format!("{}", ps.ident),
                    },
                    _ => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "backend",
                            format!("{:?} has unknown kind", impl_a.self_ty)
                        );
                        "unknown_type_kind".into()
                    }
                };
                let legacy_ident = lower1::jvm_names::member_name(&legacy_ident);
                let ident = {
                    let impl_instance = Instance::new_raw(
                        impl_def_id,
                        rustc_middle::ty::GenericArgs::identity_for_item(tcx, impl_def_id),
                    );
                    let impl_self_ty = tcx
                        .type_of(impl_def_id)
                        .instantiate(
                            tcx,
                            rustc_middle::ty::GenericArgs::identity_for_item(tcx, impl_def_id),
                        )
                        .skip_norm_wip();
                    let impl_self_oomir_ty = lower1::types::ty_to_oomir_type(
                        impl_self_ty,
                        tcx,
                        &mut oomir_module.data_types,
                        impl_instance,
                    );
                    match impl_self_oomir_ty {
                        Type::Class(name) | Type::Interface(name) => name,
                        Type::MutableReference(inner) => match *inner {
                            Type::Class(name) | Type::Interface(name) => name,
                            _ => legacy_ident,
                        },
                        _ => legacy_ident,
                    }
                };

                let Some(of_trait) = impl_a.of_trait.and_then(|trait_impl_header| {
                    trait_impl_header
                        .trait_ref
                        .path
                        .res
                        .opt_def_id()
                        .map(|def_id| lower1::jvm_names::class_for_def_id(tcx, def_id))
                        .or_else(|| {
                            Some(lower1::jvm_names::member_name(
                                trait_impl_header
                                    .trait_ref
                                    .path
                                    .segments
                                    .last()
                                    .unwrap()
                                    .ident
                                    .as_str(),
                            ))
                        })
                }) else {
                    continue;
                };

                oomir_module
                    .data_types
                    .entry(of_trait.clone())
                    .or_insert_with(|| oomir::DataType::Interface {
                        methods: HashMap::new(),
                    });

                match oomir_module.data_types.get_mut(&ident) {
                    Some(oomir::DataType::Class { interfaces, .. }) => {
                        if !interfaces.contains(&of_trait) {
                            interfaces.push(of_trait);
                        }
                    }
                    Some(oomir::DataType::Interface { .. }) => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "backend",
                            format!(
                                "Skipping trait implementation declaration for Interface type '{}'.",
                                ident
                            )
                        );
                    }
                    None => {
                        oomir_module.data_types.insert(
                            ident,
                            oomir::DataType::Class {
                                methods: HashMap::new(),
                                is_abstract: false,
                                super_class: Some("java/lang/Object".to_string()),
                                fields: vec![],
                                interfaces: vec![of_trait],
                            },
                        );
                    }
                }
            } else if let rustc_hir::ItemKind::Trait {
                ident: _,
                items: item_refs,
                ..
            } = item.kind
            {
                let ident = lower1::jvm_names::class_for_def_id(tcx, item.owner_id.to_def_id());
                let mut fn_data = HashMap::new();
                for item in item_refs {
                    let name = item.to_ident(tcx).as_str().to_string();
                    let def_id = item.owner_id.to_def_id();
                    if tcx.def_kind(def_id) != rustc_hir::def::DefKind::AssocFn {
                        continue;
                    }
                    let mir_sig = tcx.type_of(def_id).skip_binder().fn_sig(tcx);

                    let params_ty = mir_sig.inputs();
                    let return_ty = mir_sig.output();

                    let instance = Instance::new_raw(
                        def_id,
                        rustc_middle::ty::GenericArgs::identity_for_item(tcx, def_id),
                    );

                    let params_inputs = params_ty.skip_binder();
                    let params_oomir: Vec<(String, oomir::Type)> = params_inputs
                        .iter()
                        .enumerate()
                        .filter_map(|(i, ty)| {
                            let is_self_param = matches!(
                                ty.peel_refs().kind(),
                                rustc_middle::ty::TyKind::Param(param) if param.name.as_str() == "Self"
                            );

                            if is_self_param {
                                None
                            } else {
                                let param_name = format!("arg{}", i);
                                let oomir_type = lower1::types::ty_to_oomir_type(
                                    *ty,
                                    tcx,
                                    &mut oomir_module.data_types,
                                    instance,
                                );
                                Some((param_name, oomir_type))
                            }
                        })
                        .collect();
                    let return_oomir_ty = lower1::types::ty_to_oomir_type(
                        return_ty.skip_binder(),
                        tcx,
                        &mut oomir_module.data_types,
                        instance,
                    );

                    let is_instance_method = params_inputs.len() > params_oomir.len();
                    let mut signature = oomir::Signature {
                        params: params_oomir,
                        ret: Box::new(return_oomir_ty),
                        is_static: !is_instance_method,
                    };
                    let (params_changed, _) = signature.replace_class_in_signature("Self", &ident);

                    if params_changed {
                        signature.is_static = false;
                    }

                    fn_data.insert(name, signature);
                }

                oomir_module
                    .data_types
                    .insert(ident, oomir::DataType::Interface { methods: fn_data });
            }
        }

        lower_public_library_exports(tcx, &mut oomir_module);
        lower_mono_items(tcx, &mut oomir_module);

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
