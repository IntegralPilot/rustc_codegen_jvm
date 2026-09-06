#![feature(alloc_error_hook)]
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
extern crate rustc_hashes;
extern crate rustc_hir;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_structures;
extern crate rustc_target;
extern crate self as breadcrumbs;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Verbose,
    Info,
    Warn,
    Error,
    Critical,
}

const LISTENING_CHANNELS: &[&str] = &[];

#[doc(hidden)]
pub fn backend_log_enabled(level: LogLevel, channel: &str) -> bool {
    level >= LogLevel::Error || LISTENING_CHANNELS.contains(&channel)
}

#[doc(hidden)]
pub fn write_backend_log(level: LogLevel, channel: &str, message: impl std::fmt::Display) {
    println!("[{channel}/{level:?}] {message}");
}

#[macro_export]
macro_rules! log {
    ($level:expr, $channel:expr, $message:expr) => {{
        let level = $level;
        let channel = $channel;
        if $crate::backend_log_enabled(level, channel) {
            $crate::write_backend_log(level, channel, $message);
        }
    }};
}

use crate::lower1::context::Definitions;
use oomir::Type;
use rustc_codegen_ssa::back::archive::{ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder};
use rustc_codegen_ssa::{
    CompiledModule, CompiledModules, CrateInfo, ModuleKind, traits::CodegenBackend,
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hir::def::DefKind;
use std::collections::{BTreeMap, VecDeque};

use rustc_data_structures::{
    sync::{IntoDynSyncSend, Lock, par_for_each_in},
    unord::UnordMap,
};
use rustc_metadata::EncodedMetadata;
use rustc_middle::{
    dep_graph::{WorkProduct, WorkProductId},
    mono::MonoItem,
    ty::{
        EarlyBinder, GenericArgs, Instance, InstanceKind, ShimKind, TyCtxt, TyKind,
        TypeVisitableExt, TypingEnv, Unnormalized, VtblEntry,
    },
};
use rustc_session::{IncrCompSession, Session, config::OutputFilenames};
use rustc_span::def_id::{DefId, LOCAL_CRATE};
use rustc_structures::CrateType;
use std::{
    any::Any,
    io::{BufReader, BufWriter, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

mod async_interop;
mod mono;
use mono::{mono_item_name, place_or_insert_mono_function};
mod lower1;
mod lower2;
mod metrics;
mod oomir;
mod pipeline;
mod stable_hash;

/// An instance of our Java bytecode codegen backend.
struct MyBackend;

// Four lower2 workers usefully saturate large crates without retaining an
// unbounded number of prepared OOMIR modules on many-core hosts.
const MAX_CODEGEN_WORKERS: usize = 4;
const OOMIR_SHARD_QUEUE_DEPTH: usize = 1;
use jvm_compiler_core::classfile::bundle::{self, MAGIC as CLASS_BUNDLE_MAGIC};

mod type_registry;
use type_registry::CanonicalDataTypeRegistry;

fn combine_class_bundles(path: &Path, bundles: &[(String, PathBuf)]) -> std::io::Result<()> {
    let mut output = BufWriter::new(std::fs::File::create(path)?);
    output.write_all(CLASS_BUNDLE_MAGIC)?;
    for (_, bundle_path) in bundles {
        let mut bundle = BufReader::new(std::fs::File::open(bundle_path)?);
        bundle::read_magic(&mut bundle)?;
        std::io::copy(&mut bundle, &mut output)?;
    }
    output.flush()
}

mod allocator_shims;
use allocator_shims::emit_allocator_shims;

fn lower_mono_function<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    oomir_module: &mut lower1::context::Module<'tcx>,
    lowered_instances: &Lock<HashSet<Instance<'tcx>>>,
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

    if !lowered_instances.borrow_mut().insert(instance) {
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

    let name = mono_item_name(tcx, instance, &oomir_module.data_types);
    let mir = tcx.instance_mir(instance.def);
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
        mir,
        Some(name.clone()),
        true,
        &mut oomir_module.data_types,
        &mut oomir_module.external_interfaces,
    );
    if tcx.is_intrinsic(instance.def_id(), rustc_span::sym::const_allocate) {
        let result_ty = oomir_function.signature.ret.as_ref().clone();
        let result = "__const_allocate_result".to_string();
        let entry = "entry".to_string();
        oomir_function.body = oomir::CodeBlock {
            entry: entry.clone(),
            basic_blocks: HashMap::from_iter([(
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
        }
        .into();
    }
    place_or_insert_mono_function(tcx, instance, &name, oomir_function, oomir_module);
}

fn lower_codegen_unit_items<'tcx>(
    tcx: TyCtxt<'tcx>,
    mono_items: impl IntoIterator<Item = MonoItem<'tcx>>,
    partitioned_functions: &HashSet<Instance<'tcx>>,
    oomir_module: &mut lower1::context::Module<'tcx>,
    claimed_mono_items: &Lock<HashSet<MonoItem<'tcx>>>,
    lowered_instances: &Lock<HashSet<Instance<'tcx>>>,
    scanned_instances: &Lock<HashSet<Instance<'tcx>>>,
) {
    let mut function_roots = Vec::new();
    for mono_item in mono_items {
        if !claimed_mono_items.borrow_mut().insert(mono_item) {
            continue;
        }
        match mono_item {
            MonoItem::Fn(instance) => {
                function_roots.push(instance);
            }
            MonoItem::Static(def_id) => {
                lower1::statics::lower_static(tcx, def_id, oomir_module)
                    .unwrap_or_else(|error| panic!("failed to lower static {def_id:?}: {error}"));
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
    lower_supplemental_instance_closure(
        tcx,
        function_roots,
        partitioned_functions,
        oomir_module,
        lowered_instances,
        scanned_instances,
    );
}

fn lower_supplemental_instance_closure<'tcx>(
    tcx: TyCtxt<'tcx>,
    roots: impl IntoIterator<Item = Instance<'tcx>>,
    partitioned_functions: &HashSet<Instance<'tcx>>,
    oomir_module: &mut lower1::context::Module<'tcx>,
    lowered_instances: &Lock<HashSet<Instance<'tcx>>>,
    scanned_instances: &Lock<HashSet<Instance<'tcx>>>,
) {
    let mut functions = roots.into_iter().collect::<VecDeque<_>>();
    let mut queued = functions.iter().copied().collect::<HashSet<_>>();
    while let Some(instance) = functions.pop_front() {
        lower_mono_function(tcx, instance, oomir_module, lowered_instances);
        if !scanned_instances.borrow_mut().insert(instance) {
            continue;
        }
        for callee in direct_mir_callees(tcx, instance) {
            if !matches!(
                callee.def,
                InstanceKind::Intrinsic(_)
                    | InstanceKind::LlvmIntrinsic(_)
                    | InstanceKind::Virtual(..)
            ) && !partitioned_functions.contains(&callee)
                && tcx.should_codegen_locally(callee)
                && queued.insert(callee)
            {
                // Supplement generated helpers using rustc's linkage policy.
                // Upstream exported bodies are already present in their rlibs.
                functions.push_back(callee);
            }
        }
    }
}

fn normalized_instance_ty<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    ty: rustc_middle::ty::Ty<'tcx>,
) -> rustc_middle::ty::Ty<'tcx> {
    let instantiated = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    tcx.try_normalize_erasing_regions(
        TypingEnv::fully_monomorphized(),
        Unnormalized::new_wip(instantiated),
    )
    .unwrap_or(instantiated)
}

fn unsize_vtable_callees<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    source_ty: rustc_middle::ty::Ty<'tcx>,
    target_ty: rustc_middle::ty::Ty<'tcx>,
) -> Vec<Instance<'tcx>> {
    let source_ty = normalized_instance_ty(tcx, instance, source_ty);
    let target_ty = normalized_instance_ty(tcx, instance, target_ty);
    let pointees = match (source_ty.kind(), target_ty.kind()) {
        (
            TyKind::Ref(_, source, _) | TyKind::RawPtr(source, _),
            TyKind::Ref(_, target, _) | TyKind::RawPtr(target, _),
        ) => Some((*source, *target)),
        _ => None,
    };
    let Some((source_pointee, target_pointee)) = pointees else {
        return Vec::new();
    };
    let typing_env = TypingEnv::fully_monomorphized();
    let source_tail = tcx.struct_tail_for_codegen(
        normalized_instance_ty(tcx, instance, source_pointee),
        typing_env,
    );
    let target_tail = tcx.struct_tail_for_codegen(
        normalized_instance_ty(tcx, instance, target_pointee),
        typing_env,
    );
    let TyKind::Dynamic(predicates, _) = target_tail.kind() else {
        return Vec::new();
    };
    let Some(principal) = predicates.principal() else {
        return Vec::new();
    };
    let trait_ref =
        tcx.instantiate_bound_regions_with_erased(principal.with_self_ty(tcx, source_tail));
    let mut callees = tcx
        .vtable_entries(trait_ref)
        .iter()
        .filter_map(|entry| match entry {
            VtblEntry::Method(target) if tcx.is_mir_available(target.def_id()) => Some(*target),
            _ => None,
        })
        .collect::<Vec<_>>();
    if !source_tail.has_escaping_bound_vars() && source_tail.needs_drop(tcx, typing_env) {
        callees.push(Instance::resolve_drop_glue(tcx, source_tail));
    }
    callees
}

fn synthetic_drop_callees_for_ty<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: rustc_middle::ty::Ty<'tcx>,
) -> Vec<Instance<'tcx>> {
    let typing_env = TypingEnv::fully_monomorphized();
    ty.walk()
        .filter_map(|arg| {
            let ty = arg.as_type()?;
            match ty.kind() {
                TyKind::FnDef(def_id, args) => {
                    Instance::resolve_for_fn_ptr(tcx, typing_env, *def_id, args.no_bound_vars()?)
                        .filter(|target| tcx.is_mir_available(target.def_id()))
                }
                TyKind::Slice(element)
                    if !element.has_escaping_bound_vars()
                        && element.needs_drop(tcx, typing_env) =>
                {
                    Some(Instance::resolve_drop_glue(tcx, *element))
                }
                TyKind::Adt(def, args) if def.is_box() => {
                    let pointee = args.type_at(0);
                    (!pointee.has_escaping_bound_vars() && pointee.needs_drop(tcx, typing_env))
                        .then(|| Instance::resolve_drop_glue(tcx, pointee))
                }
                TyKind::Coroutine(..)
                    if !ty.has_escaping_bound_vars() && ty.needs_drop(tcx, typing_env) =>
                {
                    Some(Instance::resolve_drop_glue(tcx, ty))
                }
                _ => None,
            }
        })
        .collect()
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
    let mut callees = mir
        .basic_blocks
        .iter()
        .filter_map(|block| {
            let terminator = block.terminator();
            let rustc_middle::mir::TerminatorKind::Call { func, .. } = &terminator.kind else {
                return None;
            };
            let instantiated_func_ty =
                EarlyBinder::bind(tcx, func.ty(mir, tcx)).instantiate(tcx, instance.args);
            let func_ty = tcx
                .try_normalize_erasing_regions(typing_env, instantiated_func_ty)
                .unwrap_or_else(|_| instantiated_func_ty.skip_norm_wip());
            let TyKind::FnDef(def_id, args) = func_ty.kind() else {
                return None;
            };
            let args = args.no_bound_vars()?;
            let callee = Instance::try_resolve(tcx, typing_env, *def_id, args)
                .ok()
                .flatten()?;
            match callee.def {
                InstanceKind::Item(_) => tcx.is_mir_available(callee.def_id()).then_some(callee),
                InstanceKind::Shim(_) => Some(callee),
                InstanceKind::Intrinsic(_)
                | InstanceKind::LlvmIntrinsic(_)
                | InstanceKind::Virtual(..) => None,
            }
        })
        .collect::<Vec<_>>();

    for local in &mir.local_decls {
        let ty = normalized_instance_ty(tcx, instance, local.ty);
        callees.extend(synthetic_drop_callees_for_ty(tcx, ty));
    }

    for block in mir.basic_blocks.iter() {
        if let rustc_middle::mir::TerminatorKind::Drop { place, .. } = &block.terminator().kind {
            let dropped_ty = normalized_instance_ty(tcx, instance, place.ty(mir, tcx).ty);
            if !dropped_ty.has_escaping_bound_vars() && dropped_ty.needs_drop(tcx, typing_env) {
                callees.push(Instance::resolve_drop_glue(tcx, dropped_ty));
            }
        }
        for statement in &block.statements {
            let rustc_middle::mir::StatementKind::Assign(assignment) = &statement.kind else {
                continue;
            };
            let (_, rvalue) = assignment.as_ref();
            let rustc_middle::mir::Rvalue::Cast(
                rustc_middle::mir::CastKind::PointerCoercion(
                    rustc_middle::ty::adjustment::PointerCoercion::Unsize,
                    _,
                ),
                source,
                target_ty,
            ) = rvalue
            else {
                continue;
            };
            callees.extend(unsize_vtable_callees(
                tcx,
                instance,
                source.ty(mir, tcx),
                *target_ty,
            ));
        }
    }
    callees
}

mod java_exports;
use java_exports::*;

fn empty_oomir_module<'tcx>(
    tcx: TyCtxt<'tcx>,
    name: &str,
    shared: lower1::context::Shared<'tcx>,
) -> lower1::context::Module<'tcx> {
    oomir::Module {
        name: name.to_string(),
        source_file: tcx
            .sess
            .local_crate_source_file()
            .map(|file_name| rustc_span::FileName::Real(file_name).short().to_string()),
        functions: HashMap::default(),
        data_types: lower1::context::Definitions::new(shared),
        suppressed_data_types: HashSet::default(),
        shared_data_types: None,
        relative_static_methods: Arc::new(HashSet::default()),
        external_interfaces: HashSet::default(),
        statics: HashMap::default(),
    }
}

fn prepare_oomir_shard(module: lower1::context::Module<'_>) -> oomir::Module {
    let mut oomir_module = module.map_definitions(lower1::context::Definitions::finish);
    for data_type in oomir_module.data_types.values_mut() {
        data_type.clean_duplicates();
    }

    oomir_module
}

fn emit_oomir_shard(
    shard_name: &str,
    oomir_module: oomir::Module,
    emit_runtime_views: bool,
    debug_info: lower2::DebugInfoOptions,
    emitted_class_registry: &lower2::EmittedClassRegistry,
) -> Vec<(String, PathBuf)> {
    let _metrics = metrics::begin_shard(shard_name, &oomir_module);
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "backend",
        format!(
            "OOMIR shard {shard_name} contains {} functions, {} data types, and {} statics",
            oomir_module.functions.len(),
            oomir_module.data_types.len(),
            oomir_module.statics.len()
        )
    );

    let generated_classes = lower2::oomir_to_jvm_bytecode(
        oomir_module,
        debug_info,
        emit_runtime_views,
        emitted_class_registry,
    )
    .unwrap_or_else(|error| {
        panic!("failed to lower OOMIR shard {shard_name} to JVM bytecode: {error}")
    });
    generated_classes
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
            let lowered_instances = Lock::new(HashSet::default());
            let claimed_mono_items = Lock::new(HashSet::default());
            let scanned_instances = Lock::new(HashSet::default());
            let emitted_class_registry = lower2::EmittedClassRegistry::default();
            let debug_info = lower2::debug_info_options(tcx);
            let mono_items = tcx.collect_and_partition_mono_items(());
            let partitioned_functions: HashSet<_> = mono_items
                .codegen_units
                .iter()
                .flat_map(|cgu| cgu.items_in_deterministic_order(tcx))
                .filter_map(|(item, _)| match item {
                    MonoItem::Fn(instance) => Some(instance),
                    MonoItem::Static(_) | MonoItem::GlobalAsm(_) => None,
                })
                .collect();

            let generated_classes = std::thread::scope(|scope| {
                let worker_count = std::thread::available_parallelism()
                    .map_or(1, std::num::NonZeroUsize::get)
                    .min(MAX_CODEGEN_WORKERS);
                let workers = pipeline::start(
                    scope,
                    worker_count,
                    OOMIR_SHARD_QUEUE_DEPTH,
                    |(ordinal, shard_name, module, emit_runtime_views): (
                        usize,
                        String,
                        oomir::Module,
                        bool,
                    )| {
                        (
                            ordinal,
                            emit_oomir_shard(
                                &shard_name,
                                module,
                                emit_runtime_views,
                                debug_info,
                                &emitted_class_registry,
                            ),
                        )
                    },
                );

                let mut canonical_data_types = CanonicalDataTypeRegistry::default();
                let shared_lowering = lower1::context::Shared::default();
                let mut submitted = 0usize;

                // Java exports are supplemental roots and are small enough to
                // stream as their own job while ordinary owners are lowered.
                let mut export_module =
                    empty_oomir_module(tcx, &crate_module_class, Arc::clone(&shared_lowering));
                lower_public_library_exports(
                    tcx,
                    &partitioned_functions,
                    &mut export_module,
                    &lowered_instances,
                    &scanned_instances,
                );
                emit_allocator_shims(tcx, &mut export_module);
                let mut export_module = prepare_oomir_shard(export_module);
                canonical_data_types.collect(&mut export_module);
                workers.submit((submitted, "java-exports".to_string(), export_module, true));
                submitted += 1;

                // Repartition native CGUs by final JVM owner. This prevents
                // duplicate holder construction while preserving fine-grained
                // streaming and dynamic worker load balancing.
                let mut items_by_owner = BTreeMap::<String, Vec<MonoItem<'_>>>::new();
                let naming = Definitions::new(Arc::clone(&shared_lowering));
                for cgu in mono_items.codegen_units {
                    for (item, _) in cgu.items_in_deterministic_order(tcx) {
                        let owner = match item {
                            MonoItem::Fn(instance) => mono_item_name(tcx, instance, &naming)
                                .class_to_call_on
                                .unwrap_or_else(|| crate_module_class.clone()),
                            MonoItem::Static(def_id) => format!(
                                "{}$Static",
                                lower1::jvm_names::class_for_def_id(tcx, def_id)
                            ),
                            MonoItem::GlobalAsm(_) => crate_module_class.clone(),
                        };
                        items_by_owner.entry(owner).or_default().push(item);
                    }
                }
                drop(naming);
                submitted += items_by_owner.len();
                let pending = Lock::new(
                    items_by_owner
                        .into_iter()
                        .enumerate()
                        .rev()
                        .collect::<Vec<_>>(),
                );
                let canonical_data_types = Lock::new(canonical_data_types);
                let producer = IntoDynSyncSend(workers.producer());
                // At most four MIR shards are alive, even when rustc's query
                // pool is larger. The emission queue applies backpressure.
                tcx.sess.time("jvm_lower_mir", || {
                    par_for_each_in(0..worker_count, |_| {
                        rustc_middle::ty::print::with_no_trimmed_paths!({
                            loop {
                                let next = pending.borrow_mut().pop();
                                let Some((index, (owner, items))) = next else {
                                    break;
                                };
                                let shard_name = format!(
                                    "jvm-class-{index}-{}",
                                    stable_hash::short_hash(&owner, 8)
                                );
                                let mut module = empty_oomir_module(
                                    tcx,
                                    &crate_module_class,
                                    Arc::clone(&shared_lowering),
                                );
                                lower_codegen_unit_items(
                                    tcx,
                                    items,
                                    &partitioned_functions,
                                    &mut module,
                                    &claimed_mono_items,
                                    &lowered_instances,
                                    &scanned_instances,
                                );
                                let mut module = prepare_oomir_shard(module);
                                canonical_data_types.borrow_mut().collect(&mut module);
                                producer.submit((index + 1, shard_name, module, false));
                            }
                        });
                    })
                });
                drop(producer);
                let canonical_data_types = canonical_data_types.into_inner();

                drop(shared_lowering);

                let canonical_timer = tcx.sess.timer("jvm_canonical_types");
                let canonical_source_file = tcx
                    .sess
                    .local_crate_source_file()
                    .map(|file_name| rustc_span::FileName::Real(file_name).short().to_string());
                for (index, module) in canonical_data_types
                    .into_modules(&crate_module_class, canonical_source_file)
                    .into_iter()
                    .enumerate()
                {
                    let shard_name = format!("canonical-types-{index}");
                    workers.submit((submitted, shard_name, module, false));
                    submitted += 1;
                }
                let mut results = tcx.sess.time("jvm_finish_emission", || workers.finish());
                drop(canonical_timer);
                results.sort_by_key(|(ordinal, _)| *ordinal);
                results
                    .into_iter()
                    .flat_map(|(_, generated)| generated)
                    .collect::<Vec<_>>()
            });

            if let Err(error) = metrics::finish_crate(&crate_name) {
                eprintln!("warning: failed to write rustc_codegen_jvm metrics: {error}");
            }

            Box::new((generated_classes, crate_name))
        })
    }

    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn Any>,
        _sess: &Session,
        _incr_comp_session: Option<&IncrCompSession>,
        outputs: &OutputFilenames,
        _crate_info: &CrateInfo,
    ) -> (CompiledModules, UnordMap<WorkProductId, WorkProduct>) {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let (generated_classes, _) = *ongoing_codegen
                .downcast::<(Vec<(String, PathBuf)>, String)>()
                .expect("in join_codegen: ongoing_codegen is not a generated-class list");

            let temporary_directories: HashSet<_> = generated_classes
                .iter()
                .filter_map(|(_, path)| path.parent().map(Path::to_path_buf))
                .collect();

            let mut compiled_modules = Vec::new();
            if !generated_classes.is_empty() {
                let cgu_name = "jvm_class_bundle".to_string();
                let file_path = outputs.temp_path_ext_for_cgu("jvmbundle", &cgu_name);
                if let Some(parent) = file_path.parent() {
                    std::fs::create_dir_all(parent).unwrap_or_else(|e| {
                        panic!(
                            "Could not create class output directory {}: {}",
                            parent.display(),
                            e
                        )
                    });
                }

                combine_class_bundles(&file_path, &generated_classes).unwrap_or_else(|e| {
                    panic!(
                        "Could not write generated class bundle {}: {}",
                        file_path.display(),
                        e
                    )
                });
                compiled_modules.push(CompiledModule {
                    name: cgu_name,
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
            for temporary_directory in temporary_directories {
                std::fs::remove_dir_all(&temporary_directory).unwrap_or_else(|error| {
                    panic!(
                        "Could not remove temporary class directory {}: {}",
                        temporary_directory.display(),
                        error
                    )
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

#[unsafe(no_mangle)]
pub extern "Rust" fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    std::alloc::set_alloc_error_hook(custom_alloc_error_hook);
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
