use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;
use std::process::exit;

use ristretto_classfile::{
    ClassFile, MethodAccessFlags,
};
use serde::Serialize;
use zip::ZipArchive;

#[derive(Serialize, Debug, Clone)]
struct ShimInfo {
    descriptor: String,
    is_static: bool,
}

// Use BTreeMap to keep the shims sorted by function (key) name
type ShimMap = BTreeMap<String, ShimInfo>;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} <path/to/kotlin-library.jar> <path/to/output.json>", args[0]);
        exit(1);
    }

    let jar_path = PathBuf::from(&args[1]);
    let output_path = PathBuf::from(&args[2]);

    println!("Input JAR: {:?}", jar_path);
    println!("Output JSON: {:?}", output_path);

    match generate_metadata(&jar_path, &output_path) {
        Ok(count) => {
            println!("Successfully generated metadata for {} shim methods.", count);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            exit(1);
        }
    }
}

/// Reads the specified class from the JAR, parses it, and generates the JSON metadata file.
fn generate_metadata(jar_path: &PathBuf, output_path: &PathBuf) -> Result<usize, String> {
    let target_class_name_internal = "org/rustlang/core/Core";
    let target_class_path = format!("{}.class", target_class_name_internal);

    // 1. Open the JAR (Zip Archive)
    let file = fs::File::open(jar_path)
        .map_err(|e| format!("Failed to open JAR file {:?}: {}", jar_path, e))?;
    let mut archive = ZipArchive::new(file)
        .map_err(|e| format!("Failed to read JAR archive {:?}: {}", jar_path, e))?;

    // 2. Find and Read the target .class file
    let mut class_file_entry = archive
        .by_name(&target_class_path)
        .map_err(|e| format!("Class '{}' not found in JAR {:?}: {}", target_class_path, jar_path, e))?;

    let mut class_data = Vec::new();
    class_file_entry
        .read_to_end(&mut class_data)
        .map_err(|e| format!("Failed to read '{}' from JAR: {}", target_class_path, e))?;

    println!("Read {} bytes for {}", class_data.len(), target_class_path);

    // 3. Parse the .class file using ristretto_classfile
    let parsed_class = ClassFile::from_bytes(&mut Cursor::new(class_data))
        .map_err(|e| format!("Failed to parse '{}': {:?}", target_class_path, e))?;
    println!("Successfully parsed class file.");

    // 4. Build the ShimMap (Method Name -> ShimInfo)
    let mut shim_map: ShimMap = BTreeMap::new();
    let cp = &parsed_class.constant_pool; // Borrow constant pool for lookups

    for method in &parsed_class.methods {
        let method_name = cp
            .try_get_utf8(method.name_index)
            .map_err(|e| format!("Failed to get method name at index {}: {:?}", method.name_index, e))?
            .to_string();

        // Skip constructors and static initializers
        if method_name == "<init>" || method_name == "<clinit>" {
            continue;
        }

        // --- Filter for shims ---
        // Convention: Assuming all public static methods in this class are potential shims
        let is_public = method.access_flags.contains(MethodAccessFlags::PUBLIC);
        let is_static = method.access_flags.contains(MethodAccessFlags::STATIC);

        if is_public && is_static {
            let descriptor = cp
                .try_get_utf8(method.descriptor_index)
                .map_err(|e| {
                    format!(
                        "Failed to get descriptor for method '{}' at index {}: {:?}",
                        method_name, method.descriptor_index, e
                    )
                })?
                .to_string();

            println!(
                "  Found potential shim: '{}', Descriptor: '{}'",
                method_name, descriptor
            );

            shim_map.insert(
                method_name, // Assumes this matches make_jvm_safe output
                ShimInfo {
                    descriptor,
                    is_static: true, // We already filtered for static
                },
            );
        } else {
             println!("  Skipping non-public-static method: '{}'", method_name);
        }
    }

    // 5. Serialize the map to JSON (keys will be in sorted order because of BTreeMap)
    let json_output = serde_json::to_string_pretty(&shim_map)
        .map_err(|e| format!("Failed to serialize metadata to JSON: {}", e))?;

    // 6. Write JSON to the output file
    let mut output_file = fs::File::create(output_path)
        .map_err(|e| format!("Failed to create output file {:?}: {}", output_path, e))?;

    output_file
        .write_all(json_output.as_bytes())
        .map_err(|e| format!("Failed to write JSON to {:?}: {}", output_path, e))?;

    Ok(shim_map.len())
}