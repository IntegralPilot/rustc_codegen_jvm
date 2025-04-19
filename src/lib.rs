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
extern crate rustc_target;

use rustc_codegen_ssa::back::archive::{ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder};
use rustc_codegen_ssa::{
    CodegenResults, CompiledModule, CrateInfo, ModuleKind, traits::CodegenBackend,
};

use rustc_data_structures::fx::FxIndexMap;
use rustc_metadata::EncodedMetadata;
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_middle::ty::TyCtxt;
use rustc_session::{Session, config::OutputFilenames};
use std::{any::Any, io::Write, path::Path};

mod lower1;
mod lower2;
mod oomir;

/// An instance of our Java bytecode codegen backend.
struct MyBackend;

impl CodegenBackend for MyBackend {
    fn locale_resource(&self) -> &'static str {
        ""
    }

    fn codegen_crate<'a>(
        &self,
        tcx: TyCtxt<'_>,
        metadata: EncodedMetadata,
        _need_metadata_module: bool,
    ) -> Box<dyn Any> {
        let crate_name = tcx
            .crate_name(rustc_hir::def_id::CRATE_DEF_ID.to_def_id().krate)
            .to_string();

        let mut oomir_module = oomir::Module {
            name: crate_name.clone(),
            functions: std::collections::HashMap::new(),
            data_types: std::collections::HashMap::new(),
        };

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
                if tcx.generics_of(def_id).count() != 0 {
                    println!("Skipping generic function: {i}");
                    continue; // Skip generic functions for now
                }
                let instance = rustc_middle::ty::Instance::mono(tcx, def_id);
                let mut mir = tcx.optimized_mir(instance.def_id()).clone(); // Clone the MIR

                println!("MIR for function {i}: {:?}", mir);

                println!("--- Starting MIR to OOMIR Lowering for function: {i} ---");
                let oomir_result = lower1::mir_to_oomir(tcx, instance, &mut mir);
                println!("--- Finished MIR to OOMIR Lowering for function: {i} ---");

                let oomir_function = oomir_result.0;

                oomir_module
                    .functions
                    .insert(oomir_function.name.clone(), oomir_function);

                oomir_module.merge_data_types(&oomir_result.1);
            }
        }

        println!("OOMIR module: {:?}", oomir_module);

        println!(
            "--- Starting OOMIR to JVM Bytecode Lowering for module: {} ---",
            crate_name
        );
        let bytecode = lower2::oomir_to_jvm_bytecode(&oomir_module, tcx).unwrap();
        //let bytecode = vec![0; 1024];
        println!(
            "--- Finished OOMIR to JVM Bytecode Lowering for module: {} ---",
            crate_name
        );

        Box::new((
            bytecode,
            crate_name,
            metadata,
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
            let (bytecode_map, _, metadata, crate_info) = *ongoing_codegen
                .downcast::<(
                    std::collections::HashMap<String, Vec<u8>>,
                    String,
                    EncodedMetadata,
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
                metadata_module: None,
                metadata,
                crate_info,
            };
            (codegen_results, FxIndexMap::default())
        }))
        .expect("Could not join_codegen")
    }

    fn link(&self, sess: &Session, codegen_results: CodegenResults, outputs: &OutputFilenames) {
        println!("linking!");
        use rustc_codegen_ssa::back::link::link_binary;
        link_binary(sess, &RlibArchiveBuilder, codegen_results, outputs);
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
