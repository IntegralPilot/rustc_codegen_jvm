use std::env;
use std::fs;
use std::fs::rename;
use std::hash::{Hash, Hasher};
use std::io::{self, BufReader, BufWriter, Cursor, Read, Seek, Write};
use std::path::{Path, PathBuf};

use rayon::prelude::*;
use ristretto_classfile::attributes::{
    Attribute, BootstrapMethod, Instruction, StackFrame, VerificationType,
};
use ristretto_classfile::{ClassAccessFlags, ClassFile, Constant, ConstantPool, JavaString};
use rustc_hash::FxHasher;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use zip::write::{SimpleFileOptions, ZipWriter};
use zip::{CompressionMethod, ZipArchive};

use jvm_compiler_core::classfile::bundle::{self, MAGIC as CLASS_BUNDLE_MAGIC};

mod remap;
use remap::*;
mod merge;
use merge::*;
mod metrics;
use metrics::*;
mod args;
use args::*;
mod jar;
#[cfg(test)]
use jar::*;
mod inputs;
mod pipeline;
#[cfg(test)]
mod tests;

#[derive(Debug)]
struct ClassInfo {
    jar_entry_name: String,
    data: Vec<u8>,
}

use jvm_compiler_core::classfile::key::ConstantKey;

fn main() -> Result<(), i32> {
    let args = linker_args().map_err(|error| {
        eprintln!("Error: Failed to read linker response file: {error}");
        1
    })?;
    if args.len() < 3 {
        eprintln!("Usage: java-linker <input_files...> -o <output_jar_file>");
        return Err(1);
    }
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

    pipeline::link(
        &input_class_files,
        &input_class_bundles,
        &input_rlib_files,
        &input_jar_files,
        &output_file_path,
    )
    .map_err(|error| {
        eprintln!("Error linking JVM classes: {error}");
        1
    })?;

    // Don't print success message if used as a linker, rustc handles that.
    // println!("JAR file created successfully: {}", output_file_path);
    Ok(())
}
