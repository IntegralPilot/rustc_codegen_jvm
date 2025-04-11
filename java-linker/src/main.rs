use std::env;
use std::fs;
use regex::Regex;
use ristretto_classfile::{
    ClassFile, MethodAccessFlags
};
use std::io::{self, BufReader, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::NamedTempFile;
use zip::write::{SimpleFileOptions, ZipWriter};
use zip::{CompressionMethod, ZipArchive};

fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!(
            "Usage: java-linker <input_files...> -o <output_jar_file> [--asm-processor <processor_jar>] [--known-good <identifier>]"
        );
        return Err(1);
    }

    let mut input_files: Vec<String> = Vec::new();
    let mut output_file: Option<String> = None;
    let mut processor_jar_path: Option<PathBuf> = None;
    let mut known_good_identifier: Option<String> = None;

    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        if arg == "-o" {
            if i + 1 < args.len() {
                output_file = Some(args[i + 1].clone());
                i += 2;
            } else {
                eprintln!("Error: -o flag requires an output file path");
                return Err(1);
            }
        } else if arg == "--asm-processor" {
            if i + 1 < args.len() {
                processor_jar_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            } else {
                eprintln!("Error: --asm-processor flag requires a path");
                return Err(1);
            }
        } else if arg == "--known-good" {
            if i + 1 < args.len() {
                known_good_identifier = Some(args[i + 1].clone());
                i += 2;
            } else {
                eprintln!("Error: --known-good flag requires an identifier string");
                return Err(1);
            }
        } else if !arg.starts_with("-") && (arg.ends_with(".class") || arg.ends_with(".jar")) {
            input_files.push(arg.clone());
            i += 1;
        } else if !arg.starts_with("-") {
            input_files.push(arg.clone());
            i += 1;
        } else {
            eprintln!("Warning: Ignoring unknown argument or flag: {}", arg);
            i += 1;
        }
    }

    if input_files.is_empty() {
        eprintln!("Error: No input files (.class or .jar) provided.");
        return Err(1);
    }

    if let Some(ref path) = output_file {
        if !path.ends_with(".jar") {
            eprintln!("Warning: Output file should end with .jar. Adding .jar extension.");
            output_file = Some(format!("{}.jar", path));
        }
    }

    let output_file_path = match output_file {
        Some(path) => path,
        None => {
            eprintln!("Error: Output file (-o) not specified.");
            return Err(1);
        }
    };

    let main_classes = match find_main_classes_with_ristretto(&input_files) {
        Ok(classes) => classes,
        Err(e) => {
            eprintln!("Error scanning files for main method: {}", e);
            return Err(1);
        }
    };

    if main_classes.len() > 1 {
        eprintln!("Error: Multiple classes with 'public static void main(String[])' method found:");
        for class_name in main_classes {
            eprintln!("  - {}", class_name);
        }
        eprintln!("Please ensure only one entry point is present in the input files.");
        return Err(1);
    }

    // Get the first (and hopefully only) main class name
    let main_class_name = main_classes.first().cloned();

    let processor_jar_path = processor_jar_path.as_deref();
    if let Some(path) = processor_jar_path {
        if !path.exists() || !path.is_file() {
            eprintln!(
                "Error: ASM processor JAR does not exist or is not a file: {}",
                path.display()
            );
            return Err(1);
        }
    }

    if let Err(err) = create_jar(
        &input_files,
        &output_file_path,
        main_class_name.as_deref(),
        processor_jar_path,
        known_good_identifier.as_deref(),
    ) {
        eprintln!("Error creating JAR: {}", err);
        return Err(1);
    }

    println!("JAR file created successfully: {}", output_file_path);
    Ok(())
}

/// Finds classes with a `public static void main(String[])` method using ristretto_classfile.
/// Returns a list of fully qualified class names.
fn find_main_classes_with_ristretto(input_files: &[String]) -> io::Result<Vec<String>> {
    let mut main_classes = Vec::new();
    let main_method_name = "main";
    let main_method_descriptor = "([Ljava/lang/String;)V";

    for file_path_str in input_files {
        let path = Path::new(file_path_str);
        if path.is_file() {
            if file_path_str.ends_with(".class") {
                // Process individual .class file
                match fs::read(path) {
                    Ok(data) => {
                        match check_class_data_for_main(
                            &data,
                            main_method_name,
                            main_method_descriptor,
                        ) {
                            Ok(Some(class_name)) => {
                                println!("Found main method in class file: {}", class_name);
                                main_classes.push(class_name);
                            }
                            Ok(None) => {} // No main method here
                            Err(e) => {
                                eprintln!(
                                    "Warning: Could not parse class file '{}': {}. Skipping.",
                                    file_path_str, e
                                );
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!(
                            "Warning: Failed to read file '{}': {}. Skipping.",
                            file_path_str, e
                        );
                    }
                }
            } else if file_path_str.ends_with(".jar") {
                // Process .jar file
                println!("Scanning JAR for main method: {}", file_path_str);
                match fs::File::open(path) {
                    Ok(jar_file) => {
                        let reader = BufReader::new(jar_file); // Use BufReader
                        match ZipArchive::new(reader) {
                            Ok(mut archive) => {
                                for i in 0..archive.len() {
                                    match archive.by_index(i) {
                                        Ok(mut file) => {
                                            if file.is_file() && file.name().ends_with(".class") {
                                                let entry_name = file.name().to_string(); // For logging
                                                let mut data =
                                                    Vec::with_capacity(file.size() as usize);
                                                if let Err(e) = file.read_to_end(&mut data) {
                                                    eprintln!(
                                                        "Warning: Failed to read entry '{}' in JAR '{}': {}. Skipping entry.",
                                                        entry_name, file_path_str, e
                                                    );
                                                    continue;
                                                }

                                                match check_class_data_for_main(
                                                    &data,
                                                    main_method_name,
                                                    main_method_descriptor,
                                                ) {
                                                    Ok(Some(class_name)) => {
                                                        println!(
                                                            "  Found main method in: {} (within {})",
                                                            class_name, entry_name
                                                        );
                                                        main_classes.push(class_name);
                                                    }
                                                    Ok(None) => {} // No main method in this class entry
                                                    Err(e) => {
                                                        eprintln!(
                                                            "Warning: Could not parse class entry '{}' within JAR '{}': {}. Skipping entry.",
                                                            entry_name, file_path_str, e
                                                        );
                                                    }
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            eprintln!(
                                                "Warning: Error reading entry {} in JAR '{}': {}. Skipping entry.",
                                                i, file_path_str, e
                                            );
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                eprintln!(
                                    "Warning: Could not open or read JAR file '{}' as zip archive: {}. Skipping.",
                                    file_path_str, e
                                );
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!(
                            "Warning: Failed to open file '{}': {}. Skipping.",
                            file_path_str, e
                        );
                    }
                }
            }
        } else if path.exists() {
            eprintln!(
                "Warning: Input path is not a file: {}. Skipping.",
                file_path_str
            );
        } else {
            eprintln!(
                "Warning: Input path does not exist: {}. Skipping.",
                file_path_str
            );
        }
    }
    Ok(main_classes)
}

/// Helper function to check if class data contains the main method expected by JVM.
/// Returns Ok(Some(class_name)) if found, Ok(None) if not, Err on parse error or lookup failure.
fn check_class_data_for_main(
    data: &[u8],
    main_method_name: &str,
    main_method_descriptor: &str,
) -> io::Result<Option<String>> {
    // Use Cursor<Vec<u8>> as per the from_bytes signature in the docs
    // This involves a clone of the data, which is less ideal than Cursor<&[u8]>
    // but necessary to match the documented function signature.
    let mut cursor = Cursor::new(data.to_vec());

    let class_file = match ClassFile::from_bytes(&mut cursor) {
        Ok(cf) => cf,
        Err(e) => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Class parsing failed: {}", e),
            ));
        }
    };

    for method in &class_file.methods {
        let flags = &method.access_flags;
        if flags.contains(MethodAccessFlags::PUBLIC) && flags.contains(MethodAccessFlags::STATIC) {
            let name_result = class_file.constant_pool.try_get_utf8(method.name_index);
            let descriptor_result = class_file.constant_pool.try_get_utf8(method.descriptor_index);

            match (name_result, descriptor_result) {
                (Ok(name), Ok(descriptor)) => {
                    if name == main_method_name && descriptor == main_method_descriptor {
                        return match class_file.class_name() {
                            Ok(class_name_ref) => Ok(Some(class_name_ref.to_string())), // Clone the &String
                            Err(e) => Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!(
                                    "Failed to get class name after finding main method: {}",
                                    e
                                ),
                            )),
                        };
                    }
                }
                (Err(e), _) | (_, Err(e)) => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "Failed to resolve method name or descriptor from constant pool: {}",
                            e
                        ),
                    ));
                }
            }
        }
    }

    // No main method found after checking all methods
    Ok(None)
}

fn process_jar_file(jar_path: &str) -> io::Result<Vec<(String, Vec<u8>)>> {
    let jar_file = fs::File::open(jar_path)?;
    let reader = io::BufReader::new(jar_file);
    let mut archive = ZipArchive::new(reader)?;
    let mut class_files = Vec::new();

    for i in 0..archive.len() {
        let mut file = match archive.by_index(i) {
            Ok(f) => f,
            Err(e) => {
                eprintln!(
                    "Warning: Error reading entry {} in JAR '{}' during processing: {}. Skipping entry.",
                    i, jar_path, e
                );
                continue;
            }
        };
        if file.is_file() && file.name().ends_with(".class") {
            let mut contents = Vec::with_capacity(file.size() as usize);
            file.read_to_end(&mut contents)?;
            class_files.push((file.name().to_string(), contents));
        }
    }
    Ok(class_files)
}

fn create_jar(
    input_files: &[String],
    output_jar_path: &str,
    main_class_name: Option<&str>,
    processor_jar_path: Option<&Path>,
    known_good_identifier: Option<&str>,
) -> io::Result<()> {
    let output_file = fs::File::create(output_jar_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let options = SimpleFileOptions::default()
        .compression_method(CompressionMethod::DEFLATE)
        .unix_permissions(0o644);

    let manifest_content = create_manifest_content(main_class_name);
    zip_writer.start_file("META-INF/MANIFEST.MF", options)?;
    zip_writer.write_all(manifest_content.as_bytes())?;

    let re = Regex::new(r"^(.*?)-[0-9a-fA-F]+(\.class)$").unwrap();
    let mut added_class_files = std::collections::HashSet::new();

    for input_file_path_str in input_files {
        let input_path = Path::new(input_file_path_str);
        if !input_path.exists() {
            eprintln!(
                "Warning: Input file does not exist: {}. Skipping.",
                input_file_path_str
            );
            continue;
        }

        if input_file_path_str.ends_with(".jar") {
            println!("Processing JAR file: {}", input_file_path_str);
            let class_files = match process_jar_file(input_file_path_str) {
                Ok(cf) => cf,
                Err(e) => {
                    eprintln!(
                        "Warning: Failed to process JAR file '{}': {}. Skipping.",
                        input_file_path_str, e
                    );
                    continue;
                }
            };

            let is_known_good = known_good_identifier
                .map(|id| input_file_path_str.contains(id))
                .unwrap_or(false);

            if is_known_good {
                println!(
                    "Skipping ASM processing for known-good JAR file: {}",
                    input_file_path_str
                );
            }

            for (class_path, class_data) in class_files {
                let jar_entry_name = class_path;

                if !added_class_files.contains(&jar_entry_name) {
                    let processed_data = if !is_known_good && processor_jar_path.is_some() {
                        process_with_asm(
                            &class_data,
                            processor_jar_path.unwrap(),
                            &jar_entry_name,
                            true,
                        )?
                    } else {
                        class_data
                    };

                    zip_writer.start_file(&jar_entry_name, options)?;
                    zip_writer.write_all(&processed_data)?;
                    added_class_files.insert(jar_entry_name.clone());
                    if !is_known_good && processor_jar_path.is_some() {
                        println!("Processed and added class from JAR: {}", jar_entry_name);
                    } else {
                        println!("Added class from JAR: {}", jar_entry_name);
                    }
                } else {
                    println!("Skipping duplicate class from JAR: {}", jar_entry_name);
                }
            }
        } else if input_file_path_str.ends_with(".class") {
            let original_file_name = input_path
                .file_name()
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!("Invalid input path: {}", input_file_path_str),
                    )
                })?
                .to_str()
                .ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Input filename is not valid UTF-8",
                    )
                })?;

            let jar_entry_name = if let Some(caps) = re.captures(original_file_name) {
                format!("{}{}", &caps[1], &caps[2])
            } else {
                // If not hashed, assume the filename corresponds to the path within the JAR.
                // This might need adjustment depending on how class files are laid out relative
                // to their package structure on the filesystem before being passed to the linker.
                // For now, we just use the filename. A better approach might involve stripping
                // a known source root path.
                original_file_name.to_string()
            };

            if !added_class_files.contains(&jar_entry_name) {
                let is_known_good = known_good_identifier
                    .map(|id| input_file_path_str.contains(id))
                    .unwrap_or(false);

                if is_known_good {
                    println!(
                        "Skipping ASM processing for known-good class file: {}",
                        input_file_path_str
                    );
                }

                let file_data = fs::read(input_path)?;

                let processed_data = if !is_known_good && processor_jar_path.is_some() {
                    process_with_asm(
                        &file_data,
                        processor_jar_path.unwrap(),
                        input_file_path_str,
                        false,
                    )?
                } else {
                    file_data
                };

                zip_writer.start_file(&jar_entry_name, options)?;
                zip_writer.write_all(&processed_data)?;
                added_class_files.insert(jar_entry_name.clone());
                if !is_known_good && processor_jar_path.is_some() {
                    println!(
                        "Processed and added class file: {} as {}",
                        input_file_path_str, jar_entry_name
                    );
                } else {
                    println!(
                        "Added class file: {} as {}",
                        input_file_path_str, jar_entry_name
                    );
                }
            } else {
                println!(
                    "Skipping duplicate class file: {} (as {})",
                    input_file_path_str, jar_entry_name
                );
            }
        } else {
            println!(
                "Warning: Skipping non-class/jar file: {}",
                input_file_path_str
            );
        }
    }

    zip_writer.finish()?;
    Ok(())
}

fn process_with_asm(
    class_data: &[u8],
    processor_path: &Path,
    original_path_for_logging: &str,
    is_from_jar: bool,
) -> io::Result<Vec<u8>> {
    let mut temp_in_file = NamedTempFile::new()?;
    temp_in_file.write_all(class_data)?;
    let temp_in_path = temp_in_file.path().to_path_buf();

    let temp_out_file = NamedTempFile::new()?;
    let temp_out_path = temp_out_file.path().to_path_buf();

    let mut cmd = Command::new("java");
    cmd.arg("-jar")
        .arg(processor_path)
        .arg(&temp_in_path)
        .arg(&temp_out_path);

    let source_desc = if is_from_jar {
        "class from JAR"
    } else {
        "class file"
    };
    println!(
        "Running processor on {} ({}): {:?}",
        original_path_for_logging, source_desc, cmd
    );

    let output = cmd.output()?;

    if !output.status.success() {
        eprintln!(
            "Error processing {} ({}): {}",
            original_path_for_logging, source_desc, original_path_for_logging
        );
        eprintln!(
            "Processor STDOUT:\n{}",
            String::from_utf8_lossy(&output.stdout)
        );
        eprintln!(
            "Processor STDERR:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        return Err(io::Error::new(
            io::ErrorKind::Other,
            format!("ASM processor failed for {}", original_path_for_logging),
        ));
    }

    fs::read(&temp_out_path)
}

fn create_manifest_content(main_class_name: Option<&str>) -> String {
    let mut manifest = String::new();
    manifest.push_str("Manifest-Version: 1.0\r\n");
    manifest.push_str("Created-By: java-linker-rs (rust)\r\n");

    if let Some(main_class) = main_class_name {
        manifest.push_str(&format!("Main-Class: {}\r\n", main_class));
    }
    manifest.push_str("\r\n");
    manifest
}
