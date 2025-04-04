#![feature(alloc_error_hook)]
#![feature(box_patterns)]
#![feature(rustc_private)]
#![warn(clippy::pedantic)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]

//! Rustc Codegen JVM
//!
//! Compiler backend for rustc that generates JVM bytecode, given Rust HIR and MIR.
//! Supports both Rust static libraries and binaries, generating a file - [cratename].class as it's output.
//! The class file supports Java 8 or later.

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
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, BinOp, Body, Location, Rvalue, Statement, StatementKind,
    Terminator, TerminatorKind, visit::Visitor,
};
use rustc_middle::ty::{Instance, Ty, TyCtxt};
use rustc_session::{Session, config::OutputFilenames};
use std::{any::Any, io::Write, path::Path, vec};

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
        let mut function_bytecodes = FxIndexMap::default();
        let crate_name = tcx
            .crate_name(rustc_hir::def_id::CRATE_DEF_ID.to_def_id().krate)
            .to_string();

        // Iterate through all items in the crate and find functions
        let module_items = tcx.hir_crate_items(()); // Get ModuleItems
        for item_id in module_items.free_items() {
            // Use free_items() iterator
            let item = tcx.hir_item(item_id);
            if let rustc_hir::ItemKind::Fn {
                ident: i,
                sig: _,
                generics: _,
                body: _,
                has_body: _,
            } = item.kind
            {
                // Corrected destructuring
                let def_id = item_id.owner_id.to_def_id();
                let instance = rustc_middle::ty::Instance::mono(tcx, def_id);
                let mir = tcx.optimized_mir(instance.def_id());

                println!("--- Starting MIR Visitor for function: {i} ---");
                let method_bytecode_instructions: Vec<Instruction> = Vec::new();
                let function_name = i.to_string(); // Capture function name
                let mut visitor = MirToBytecodeVisitor::new(
                    method_bytecode_instructions,
                    &function_name,
                    tcx,
                    instance,
                ); // Pass tcx and instance
                visitor.visit_body(mir);
                let generated_bytecode = visitor.method_bytecode_instructions;
                println!("--- MIR Visitor Finished for function: {i} ---");

                function_bytecodes.insert(function_name, generated_bytecode); // Store bytecode
            }
        }

        // Generate basic Java bytecode for a class with static methods,
        // passing function_bytecodes which now contains bytecodes for each function
        let bytecode = generate_class_with_static_methods_bytecode(
            crate_name.as_str(),
            &function_bytecodes,
            tcx,
        )
        .unwrap_or_default(); // Modified function to pass tcx

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
            let (bytecode, crate_name, metadata, crate_info) = *ongoing_codegen
                .downcast::<(Vec<u8>, String, EncodedMetadata, CrateInfo)>()
                .expect("in join_codegen: ongoing_codegen is not bytecode vector");

            let class_path = outputs.temp_path_ext("class", None);

            let mut class_file =
                std::fs::File::create(&class_path).expect("Could not create the Java .class file!");
            class_file
                .write_all(&bytecode)
                .expect("Could not write Java bytecode to file!");

            let modules = vec![CompiledModule {
                name: crate_name,
                kind: ModuleKind::Regular,
                object: Some(class_path),
                bytecode: None,
                dwarf_object: None,
                llvm_ir: None,
                links_from_incr_cache: Vec::new(), // Corrected to Vec::new()
                assembly: None,
            }];
            let codegen_results = CodegenResults {
                modules,
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

use ristretto_classfile::attributes::{Attribute, Instruction};
use ristretto_classfile::{
    BaseType, ClassAccessFlags, ClassFile, ConstantPool, Method, MethodAccessFlags, Version,
};
use std::alloc::Layout;

/// # Panics
///
/// Panics when called, every time, with a message statating the memory allocation of the bytes
/// corresponding to the provided layout failed.
pub fn custom_alloc_error_hook(layout: Layout) {
    panic!("Memory allocation failed: {} bytes", layout.size());
}

// --- Improved helper function to convert Rust Ty to JVM descriptor ---
fn rust_ty_to_jvm_descriptor(rust_ty: Ty<'_>, _tcx: TyCtxt<'_>) -> String {
    use rustc_middle::ty::{FloatTy, IntTy, TyKind, UintTy};

    match rust_ty.kind() {
        // Primitive types
        TyKind::Bool => BaseType::Boolean.code().to_string(),
        TyKind::Char => BaseType::Char.code().to_string(),

        // Signed integers mapped to JVM types or fallback to BigInteger for i128
        TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => BaseType::Byte.code().to_string(),
            IntTy::I16 => BaseType::Short.code().to_string(),
            IntTy::I32 => BaseType::Int.code().to_string(),
            IntTy::I64 => BaseType::Long.code().to_string(),
            IntTy::Isize => BaseType::Int.code().to_string(), // Fallback for isize
            IntTy::I128 => "Ljava/math/BigInteger;".to_string(), // No primitive for i128
        },

        // Unsigned integers mapped to JVM types or fallback to BigInteger for u128
        TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => BaseType::Byte.code().to_string(),
            UintTy::U16 => BaseType::Short.code().to_string(),
            UintTy::U32 => BaseType::Int.code().to_string(),
            UintTy::U64 => BaseType::Long.code().to_string(),
            UintTy::Usize => BaseType::Int.code().to_string(), // Fallback for usize
            UintTy::U128 => "Ljava/math/BigInteger;".to_string(), // No primitive for u128
        },

        // Floating-point numbers mapped to appropriate JVM primitives.
        TyKind::Float(float_ty) => match float_ty {
            FloatTy::F32 => BaseType::Float.code().to_string(),
            FloatTy::F64 => BaseType::Double.code().to_string(),
            FloatTy::F16 => BaseType::Float.code().to_string(), // Fallback for half-precision float
            FloatTy::F128 => BaseType::Double.code().to_string(), // Fallback for extended precision
        },

        // Handle references: if it’s a string slice, map it to java.lang.String;
        // otherwise, use a generic object reference.
        TyKind::Ref(_, inner_ty, _) => {
            if let TyKind::Str = inner_ty.kind() {
                "Ljava/lang/String;".to_string()
            } else {
                "Ljava/lang/Object;".to_string()
            }
        }

        // For raw pointers, allow string pointers but panic otherwise.
        TyKind::RawPtr(ptr_ty, _) => {
            if let TyKind::Str = ptr_ty.kind() {
                "Ljava/lang/String;".to_string()
            } else {
                panic!("Pointers are not supported in Java.")
            }
        }

        // Map Rust string slices directly to java.lang.String
        TyKind::Str => "Ljava/lang/String;".to_string(),

        // Handle tuples: map the unit type () to void,
        // and for non-empty tuples, use a generic object (or consider a more specific mapping)
        TyKind::Tuple(tuple_fields) => {
            if tuple_fields.is_empty() {
                "V".to_string()
            } else {
                "Ljava/lang/Object;".to_string()
            }
        }

        // Map Rust's never type to void (even though it is conceptually different)
        TyKind::Never => "V".to_string(),

        // Fallback for any unhandled types
        _ => "Ljava/lang/Object;".to_string(),
    }
}

// --- MIR Visitor ---

struct MirToBytecodeVisitor<'tcx> {
    method_bytecode_instructions: Vec<Instruction>,
    function_name: String,    // Store function name
    tcx: TyCtxt<'tcx>,        // Store TyCtxt
    instance: Instance<'tcx>, // Store Instance
}

impl<'tcx> MirToBytecodeVisitor<'tcx> {
    fn new(
        method_bytecode_instructions: Vec<Instruction>,
        function_name: &str,
        tcx: TyCtxt<'tcx>,
        instance: Instance<'tcx>,
    ) -> Self {
        MirToBytecodeVisitor {
            method_bytecode_instructions,
            function_name: function_name.to_string(), // Store function name
            tcx,                                      // Store TyCtxt
            instance,                                 // Store Instance
        }
    }
}

impl Visitor<'_> for MirToBytecodeVisitor<'_> {
    fn visit_body(&mut self, body: &Body<'_>) {
        println!(
            "Visiting function body for function: {}...",
            self.function_name
        );
        self.super_body(body);
        println!(
            "...Finished visiting function body for function: {}.",
            self.function_name
        );
    }

    fn visit_basic_block_data(&mut self, block: BasicBlock, data: &BasicBlockData<'_>) {
        println!("  Visiting basic block: {block:?}");
        self.super_basic_block_data(block, data);
    }

    fn visit_statement(&mut self, statement: &Statement<'_>, location: Location) {
        println!(
            "    Visiting statement in block {:?}: {:?}",
            location.block, statement
        );
        if let StatementKind::Assign(box (_place, Rvalue::BinaryOp(bin_op, operands))) =
            &statement.kind
        {
            match bin_op {
                BinOp::Add | BinOp::AddWithOverflow => {
                    println!(
                        "      Found addition operation: {:?} + {:?}",
                        operands.0, operands.1
                    );

                    // --- Generate Java bytecode for iadd ---
                    // Load the first operand (argument 0)
                    self.method_bytecode_instructions.push(Instruction::Iload_0);
                    // Load the second operand (argument 1)
                    self.method_bytecode_instructions.push(Instruction::Iload_1);
                    // Perform integer addition
                    self.method_bytecode_instructions.push(Instruction::Iadd);
                    println!("      Generated bytecode: iload_0, iload_1, iadd");
                    // --- End bytecode generation ---
                }
                BinOp::Sub | BinOp::SubWithOverflow => {
                    println!(
                        "      Found subtraction operation: {:?} - {:?}",
                        operands.0, operands.1
                    );

                    // --- Generate Java bytecode for isub ---
                    // Load the first operand (argument 0)
                    self.method_bytecode_instructions.push(Instruction::Iload_0);
                    // Load the second operand (argument 1)
                    self.method_bytecode_instructions.push(Instruction::Iload_1);
                    // Perform integer subtraction
                    self.method_bytecode_instructions.push(Instruction::Isub);
                    println!("      Generated bytecode: iload_0, iload_1, isub");
                    // --- End bytecode generation ---
                }
                _ => {
                    println!("      Unsupported binary operation: {bin_op:?}");
                }
            }
        }
        self.super_statement(statement, location);
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'_>, location: Location) {
        println!(
            "    Visiting terminator in block {:?}: {:?}",
            location.block, terminator
        );
        if terminator.kind == TerminatorKind::Return {
            println!(
                "      Found return terminator in function: {}",
                self.function_name
            );

            // Determine return type and generate appropriate bytecode
            let fn_sig = self.tcx.fn_sig(self.instance.def_id());
            let return_ty = fn_sig.skip_binder().output().skip_binder(); // Skip binder twice!
            let jvm_return_descriptor = rust_ty_to_jvm_descriptor(return_ty, self.tcx);

            match jvm_return_descriptor.as_str() {
                "V" => {
                    self.method_bytecode_instructions.push(Instruction::Return); // _return for void
                    println!("      Generated bytecode: return (_return)");
                }
                "I" | "F" | "Z" | "B" | "C" | "S" => {
                    // Integer, Float, Boolean, Byte, Char, Short returns
                    self.method_bytecode_instructions.push(Instruction::Ireturn); // ireturn (return integer value) - Correct return for i32, and others mapped to 'I'
                    println!("      Generated bytecode: ireturn");
                }
                "Ljava/lang/String;" | "Ljava/lang/Object;" => {
                    // Object returns (String, etc. for now)
                    self.method_bytecode_instructions.push(Instruction::Areturn); // areturn (return object reference)
                    println!("      Generated bytecode: areturn");
                }
                _ => {
                    self.method_bytecode_instructions.push(Instruction::Return); // default to void return if type is unknown or unsupported for now
                    println!(
                        "      Generated bytecode: return (_return) - default void return for unknown type"
                    );
                }
            }
        }
        self.super_terminator(terminator, location);
    }
}

fn generate_class_with_static_methods_bytecode(
    crate_name: &str,
    function_bytecodes: &FxIndexMap<String, Vec<Instruction>>,
    tcx: TyCtxt<'_>, // Take TyCtxt as argument
) -> ristretto_classfile::Result<Vec<u8>> {
    let mut constant_pool = ConstantPool::default();
    let super_class = constant_pool.add_class("java/lang/Object")?;
    let this_class = constant_pool.add_class(crate_name)?;
    let code_index = constant_pool.add_utf8("Code")?;

    let mut methods = Vec::new();

    for (function_name, method_bytecode_instructions) in function_bytecodes {
        let method_name_index = constant_pool.add_utf8(function_name)?;
        // Method descriptor - determine based on function signature, special case for "main"
        let instance =
            find_instance_by_name(tcx, function_name).expect("Instance not found for function");
        let fn_sig = tcx.fn_sig(instance.def_id());
        let mut method_descriptor = String::new();

        if function_name == "main" && fn_sig.skip_binder().inputs().skip_binder().is_empty() {
            // Check for main and no args
            method_descriptor = "([Ljava/lang/String;)V".to_string(); // Special main descriptor, needed as rust main = 0 args but java main expects an array of strings
        } else {
            // Regular descriptor generation
            method_descriptor.push('(');
            // Add argument descriptors
            for arg_ty in fn_sig.skip_binder().inputs().skip_binder() {
                method_descriptor.push_str(&rust_ty_to_jvm_descriptor(*arg_ty, tcx));
            }
            method_descriptor.push(')');

            // Add return descriptor
            let output_ty = fn_sig.skip_binder().output();
            method_descriptor.push_str(&rust_ty_to_jvm_descriptor(output_ty.skip_binder(), tcx));
        }
        let method_descriptor_index = constant_pool.add_utf8(method_descriptor)?;

        let mut method = Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index: method_name_index,
            descriptor_index: method_descriptor_index,
            attributes: Vec::new(),
        };

        let max_stack = method_bytecode_instructions
            .iter()
            .map(Instruction::stack_utilization)
            .sum();
        let max_locals = fn_sig.skip_binder().inputs().skip_binder().len() as u16 + 1; // +1 for 'this' (even for static methods in JVM, slot 0 exists) // Skip binder twice!

        method.attributes.push(Attribute::Code {
            name_index: code_index,
            max_stack,
            max_locals,
            code: method_bytecode_instructions.clone(),
            exception_table: Vec::new(),
            attributes: Vec::new(),
        });
        methods.push(method);
    }

    let class_file = ClassFile {
        version: Version::Java8 { minor: 0 },
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        constant_pool,
        this_class,
        super_class,
        methods,
        ..Default::default()
    };
    class_file.verify()?;

    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes)?;
    Ok(bytes)
}

// Helper function to find Instance by function name (for descriptor generation)
fn find_instance_by_name<'tcx>(tcx: TyCtxt<'tcx>, function_name: &str) -> Option<Instance<'tcx>> {
    let module_items = tcx.hir_crate_items(());
    for item_id in module_items.free_items() {
        let item = tcx.hir_item(item_id);
        if let rustc_hir::ItemKind::Fn { ident, .. } = &item.kind {
            if ident.to_string() == function_name {
                let def_id = item_id.owner_id.to_def_id();
                return Some(Instance::mono(tcx, def_id));
            }
        }
    }
    None // Instance not found
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
