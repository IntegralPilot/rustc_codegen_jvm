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

use oomir::{CodeBlock, Function, Operand, Type};
use rustc_codegen_ssa::back::archive::{ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder};
use rustc_codegen_ssa::{
    CodegenResults, CompiledModule, CrateInfo, ModuleKind, traits::CodegenBackend,
};
use std::collections::HashMap;

use rustc_data_structures::fx::FxIndexMap;
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
    if oomir_module.functions.contains_key(&closure_name) {
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
    let (oomir_function, data_types) =
        lower1::mir_to_oomir(tcx, instance, &mut mir, Some(closure_name.clone()));

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "closure-lowering",
        format!("Successfully lowered closure: {}", closure_name)
    );

    // Add the closure function to the module
    oomir_module.functions.insert(closure_name, oomir_function);
    oomir_module.merge_data_types(&data_types);
}

impl CodegenBackend for MyBackend {
    fn locale_resource(&self) -> &'static str {
        ""
    }

    fn name(&self) -> &'static str {
        "rustc_codegen_jvm"
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

        // Track monomorphized function instances to lower and avoid duplicates by name
        let mut fn_instances_to_lower: Vec<(rustc_middle::ty::Instance<'_>, String)> = Vec::new();
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
                            if let rustc_middle::ty::TyKind::FnDef(fn_def_id, fn_args) = fn_ty.kind()
                            {
                                // Check the first argument to see if it's a closure type
                                let mut is_closure = false;
                                if let Some(first_arg) = fn_args.get(0) {
                                    if let Some(ty) = first_arg.as_type() {
                                        if let rustc_middle::ty::TyKind::Closure(closure_def_id, _,) = ty.kind() {
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
                                        if let rustc_middle::ty::InstanceKind::Virtual(_, _) = instance.def {
                                            breadcrumbs::log!(
                                                breadcrumbs::LogLevel::Info,
                                                "backend",
                                                format!("Skipping virtual instance: {:?}", instance)
                                            );
                                            continue;
                                        }
                                        // Skip trait method implementations (already lowered by impl block code with Type_method naming)
                                        if let Some(assoc_item) = tcx.opt_associated_item(*fn_def_id) {
                                            if assoc_item.trait_item_def_id().is_some() {
                                                breadcrumbs::log!(
                                                    breadcrumbs::LogLevel::Info,
                                                    "backend",
                                                    format!("Skipping trait impl method: {:?}", fn_def_id)
                                                );
                                                continue;
                                            }
                                        }
                                        let name = lower1::naming::mono_fn_name_from_instance(tcx, instance);
                                        if seen_fn_names.insert(name.clone()) {
                                            fn_instances_to_lower.push((instance, name));
                                        }
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
                let oomir_result = lower1::mir_to_oomir(tcx, instance, &mut mir, None);
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!("--- Finished MIR to OOMIR Lowering for function: {i} ---")
                );

                let oomir_function = oomir_result.0;

                oomir_module
                    .functions
                    .insert(oomir_function.name.clone(), oomir_function);

                oomir_module.merge_data_types(&oomir_result.1);
            } else if let rustc_hir::ItemKind::Impl(impl_a) = item.kind {
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
                                if let rustc_middle::ty::TyKind::FnDef(fn_def_id, fn_args) = fn_ty.kind() {
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
                                            let instance = rustc_middle::ty::Instance::expect_resolve(
                                                tcx,
                                                typing_env,
                                                *fn_def_id,
                                                *fn_args,
                                                rustc_span::DUMMY_SP,
                                            );
                                            // Skip virtual trait method calls (handled via trait objects at runtime)
                                            if let rustc_middle::ty::InstanceKind::Virtual(_, _) = instance.def {
                                                breadcrumbs::log!(
                                                    breadcrumbs::LogLevel::Info,
                                                    "backend",
                                                    format!("Skipping virtual instance: {:?}", instance)
                                                );
                                                continue;
                                            }
                                            // Skip trait method implementations (already lowered by impl block code with Type_method naming)
                                            if let Some(assoc_item) = tcx.opt_associated_item(*fn_def_id) {
                                                if assoc_item.trait_item_def_id().is_some() {
                                                    breadcrumbs::log!(
                                                        breadcrumbs::LogLevel::Info,
                                                        "backend",
                                                        format!("Skipping trait impl method: {:?}", fn_def_id)
                                                    );
                                                    continue;
                                                }
                                            }
                                            let name = lower1::naming::mono_fn_name_from_instance(tcx, instance);
                                            if seen_fn_names.insert(name.clone()) {
                                                fn_instances_to_lower.push((instance, name));
                                            }
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
                    let oomir_result = lower1::mir_to_oomir(tcx, instance, &mut mir, None);
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!("--- Finished MIR to OOMIR Lowering for function: {i2} ---")
                    );

                    let mut oomir_function = oomir_result.0;
                    oomir_function.name = i.to_string();

                    // Replace self references with the trait name as to match signatures with the one we've put on the interface
                    if of_trait.is_some() {
                        match oomir_function.signature.params.get(0) {
                            Some(Type::Class(name)) => {
                                if *name != ident {
                                    continue;
                                }
                                oomir_function.signature.params[0] =
                                    Type::Class(of_trait.clone().unwrap());

                                // insert a Cast instruction to cast the trait (interface) object to the specific type of this class
                                // this is needed because the method signature will be different than what MIR expects

                                let entry_bb_name = oomir_function.body.entry.clone();

                                let entry_bb =
                                    oomir_function.body.basic_blocks.get_mut(&entry_bb_name);

                                if let Some(entry_bb) = entry_bb {
                                    entry_bb.instructions.insert(
                                        0,
                                        oomir::Instruction::Cast {
                                            op: Operand::Variable {
                                                name: "_1".into(),
                                                ty: Type::Class(of_trait.clone().unwrap()),
                                            },
                                            ty: Type::Class(ident.clone()),
                                            dest: "_1".into(),
                                        },
                                    );
                                }
                            }
                            Some(Type::MutableReference(box Type::Class(name))) => {
                                if *name != ident {
                                    continue;
                                }
                                oomir_function.signature.params[0] = Type::MutableReference(
                                    Box::new(Type::Class(of_trait.clone().unwrap())),
                                );

                                // insert a Cast instruction to cast the trait (interface) object to the specific type of this class
                                // this is needed because the method signature will be different than what MIR expects
                                let entry_bb_name = oomir_function.body.entry.clone();
                                let entry_bb =
                                    oomir_function.body.basic_blocks.get_mut(&entry_bb_name);
                                if let Some(entry_bb) = entry_bb {
                                    entry_bb.instructions.insert(
                                        0,
                                        oomir::Instruction::Cast {
                                            op: Operand::Variable {
                                                name: "_1".into(),
                                                ty: Type::MutableReference(Box::new(Type::Class(
                                                    of_trait.clone().unwrap(),
                                                ))),
                                            },
                                            ty: Type::MutableReference(Box::new(Type::Class(
                                                ident.clone(),
                                            ))),
                                            dest: "_1".into(),
                                        },
                                    );
                                }
                            }
                            _ => {}
                        }
                    }

                    let has_return = oomir_function.signature.ret.as_ref() != &oomir::Type::Void;
                    let mut args = vec![];

                    let mut helper_sig = oomir_function.signature.clone();

                    if of_trait.is_some() {
                        let mut new_params = vec![];
                        // replace any MutableReference(Class(of_trait)) or Class(of_trait) with MutableReference(Class(ident))/ Class(ident)
                        // this is just for the helper function, the actual method will use oomir_function.signature and we're only modifying helper_sig
                        for arg in &oomir_function.signature.params {
                            if let Type::MutableReference(box Type::Class(name)) = arg {
                                if *name == of_trait.clone().unwrap() {
                                    new_params.push(Type::MutableReference(Box::new(Type::Class(
                                        ident.clone(),
                                    ))));
                                } else {
                                    new_params.push(arg.clone());
                                }
                            } else if let Type::Class(name) = arg {
                                if *name == of_trait.clone().unwrap() {
                                    new_params.push(Type::Class(ident.clone()));
                                } else {
                                    new_params.push(arg.clone());
                                }
                            } else {
                                new_params.push(arg.clone());
                            }
                        }
                        helper_sig.params = new_params;
                    }

                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "backend",
                        format!("Function signature: {:?}", oomir_function.signature)
                    );

                    let mut instructions = vec![];

                    let mut idx = 1;
                    for arg in &oomir_function.signature.params {
                        let arg_name = format!("_{idx}");
                        let arg_ty = arg.clone();
                        let arg = Operand::Variable {
                            name: arg_name.clone(),
                            ty: arg_ty,
                        };
                        args.push(arg);
                        idx += 1;
                    }

                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "backend",
                        format!("Function args: {:?}", args)
                    );

                    let mut bbs = HashMap::new();

                    let mut to_call_invokevirtual_on = "_1".to_string();

                    if let oomir::Operand::Variable { ty, .. } = &args[0] {
                        // &mut self
                        if ty
                            == &oomir::Type::MutableReference(Box::new(oomir::Type::Class(
                                ident.clone(),
                            )))
                        {
                            instructions.push(oomir::Instruction::Cast {
                                op: Operand::Variable {
                                    name: "_1".into(),
                                    ty: Type::MutableReference(Box::new(Type::Class(
                                        ident.clone(),
                                    ))),
                                },
                                ty: Type::Class(ident.clone()),
                                dest: "_1_mutderef".into(),
                            });
                            to_call_invokevirtual_on = "_1_mutderef".to_string();
                        } else if *ty != oomir::Type::Class(ident.clone()) {
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest: "_1_instance".into(),
                                class_name: ident.clone(),
                            });
                            to_call_invokevirtual_on = "_1_instance".into();
                        }
                    }

                    instructions.extend(vec![
                        oomir::Instruction::InvokeVirtual {
                            operand: Operand::Variable {
                                name: to_call_invokevirtual_on,
                                ty: Type::Class(ident.clone()),
                            },
                            class_name: ident.to_string(),
                            method_name: i.to_string(),
                            method_ty: oomir_function.signature.clone(),
                            args,
                            dest: if has_return {
                                Some("dest".into())
                            } else {
                                None
                            },
                        },
                        oomir::Instruction::Return {
                            operand: if has_return {
                                Some(Operand::Variable {
                                    name: "dest".into(),
                                    ty: *oomir_function.signature.ret.clone(),
                                })
                            } else {
                                None
                            },
                        },
                    ]);

                    // insert a wrapper function for the trait method - calling InvokeVirtual on the class to call the actual method
                    bbs.insert(
                        "bb0".into(),
                        oomir::BasicBlock {
                            instructions,
                            label: "bb0".to_string(),
                        },
                    );
                    oomir_module.functions.insert(
                        i2.clone(),
                        Function {
                            name: i2,
                            signature: helper_sig,
                            body: CodeBlock {
                                entry: "bb0".into(),
                                basic_blocks: bbs,
                            },
                        },
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
                            panic!("Tried to implement trait for an interface?");
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
                                panic!("Tried to implement trait for an interface?");
                            }
                        }
                    }
                }
            } else if let rustc_hir::ItemKind::Trait(_, _, _, ident, _, _, item_refs) = item.kind {
                let ident = lower1::place::make_jvm_safe(ident.as_str());
                let mut fn_data = HashMap::new();
                let mut new_functions = HashMap::new();
                for item in item_refs {
                    let name = item.to_ident(tcx).as_str().to_string();
                    let def_id = item.owner_id.to_def_id(); // Get the DefId of the trait item (e.g., get_number)
                    let mir_sig = tcx.type_of(def_id).skip_binder().fn_sig(tcx);

                    let params_ty = mir_sig.inputs();
                    let return_ty = mir_sig.output();

                    let data_types = &mut HashMap::new(); // Consider if this should be shared across loop iterations or functions

                    // Use skip_binder here too, as inputs/outputs are bound by the same binder as the fn_sig
                    let params_oomir_ty: Vec<oomir::Type> = params_ty
                        .skip_binder()
                        .iter()
                        .map(|ty| lower1::types::ty_to_oomir_type(*ty, tcx, data_types))
                        .collect();
                    let return_oomir_ty: oomir::Type =
                        lower1::types::ty_to_oomir_type(return_ty.skip_binder(), tcx, data_types);

                    let mut signature = oomir::Signature {
                        params: params_oomir_ty,
                        ret: Box::new(return_oomir_ty.clone()),
                    };
                    signature.replace_class_in_signature("Self", &ident);

                    fn_data.insert(name.clone(), signature.clone());

                    let mut args = vec![];
                    let mut idx = 1;
                    for arg in signature.clone().params {
                        let arg_name = format!("_{idx}");
                        let arg_ty = arg.clone();
                        let arg = Operand::Variable {
                            name: arg_name.clone(),
                            ty: arg_ty,
                        };
                        args.push(arg);
                        idx += 1;
                    }

                    let mut bbs = HashMap::new();
                    bbs.insert(
                        "bb0".into(),
                        oomir::BasicBlock {
                            instructions: vec![
                                oomir::Instruction::InvokeInterface {
                                    class_name: ident.clone(),
                                    method_name: name.clone(),
                                    method_ty: signature.clone(),
                                    args,
                                    dest: if return_oomir_ty != oomir::Type::Void {
                                        Some("dest".into())
                                    } else {
                                        None
                                    },
                                    operand: Operand::Variable {
                                        name: "_1".into(),
                                        ty: oomir::Type::Class(ident.clone()),
                                    },
                                },
                                oomir::Instruction::Return {
                                    operand: if return_oomir_ty != oomir::Type::Void {
                                        Some(Operand::Variable {
                                            name: "dest".into(),
                                            ty: return_oomir_ty,
                                        })
                                    } else {
                                        None
                                    },
                                },
                            ],
                            label: "bb0".to_string(),
                        },
                    );

                    let function: Function = Function {
                        name: format!("dyn_{}_{}", ident, name),
                        signature: signature.clone(),
                        body: CodeBlock {
                            entry: "bb0".into(),
                            basic_blocks: bbs,
                        },
                    };

                    new_functions.insert(format!("dyn_{}_{}", ident, name), function);
                }

                oomir_module
                    .data_types
                    .insert(ident, oomir::DataType::Interface { methods: fn_data });
                for (name, function) in new_functions {
                    oomir_module.functions.insert(name.clone(), function);
                }
            }
        }

        // Lower all discovered monomorphized function instances
        for (instance, name) in fn_instances_to_lower {
            let mut mir = tcx.instance_mir(instance.def).clone();
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "mir-lowering",
                format!("--- Lowering monomorphized function instance: {} ---", name)
            );
            let (oomir_function, data_types) = lower1::mir_to_oomir(tcx, instance, &mut mir, Some(name.clone()));
            oomir_module.functions.insert(name, oomir_function);
            oomir_module.merge_data_types(&data_types);
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
            // Check if this closure captures any variables (has upvars)
            // Closures that don't capture anything can be lowered with Instance::mono
            let upvars = tcx.upvars_mentioned(closure_def_id);

            if upvars.is_some() && !upvars.unwrap().is_empty() {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "closure-lowering",
                    format!(
                        "Closure {} captures variables ({} upvars), skipping for now",
                        lower1::generate_closure_function_name(tcx, closure_def_id),
                        upvars.unwrap().len()
                    )
                );
                continue;
            }

            lower_closure_to_oomir(tcx, closure_def_id, &mut oomir_module);
        }

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "backend",
            format!("OOMIR module: {:?}", oomir_module)
        );

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
    ) -> (CodegenResults, FxIndexMap<WorkProductId, WorkProduct>) {
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
                let file_path = outputs.temp_path_ext_for_cgu(&name, ".class", None);

                // extract the directory from the file path
                let dir = file_path.parent().unwrap();

                // make the actual file path by adding {name}.class to the directory
                let file_path = dir.join(format!("{}.class", name));

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
                    bytecode: None,
                    dwarf_object: None,
                    llvm_ir: None,
                    links_from_incr_cache: Vec::new(),
                    assembly: None,
                });
            }

            let codegen_results = CodegenResults {
                modules: compiled_modules,
                allocator_module: None,
                // metadata_module: None,
                // metadata,
                crate_info,
            };
            (codegen_results, FxIndexMap::default())
        }))
        .expect("Could not join_codegen")
    }

    fn link(
        &self,
        sess: &Session,
        codegen_results: CodegenResults,
        metadata: EncodedMetadata,
        outputs: &OutputFilenames,
    ) {
        breadcrumbs::log!(breadcrumbs::LogLevel::Info, "backend", "linking!");
        use rustc_codegen_ssa::back::link::link_binary;
        link_binary(
            sess,
            &RlibArchiveBuilder,
            codegen_results,
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
