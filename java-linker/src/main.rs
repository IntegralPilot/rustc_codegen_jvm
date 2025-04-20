use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io::{self, BufReader, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use regex::Regex;
use ristretto_classfile::{ClassFile, MethodAccessFlags};
use tempfile::NamedTempFile;
use zip::write::{SimpleFileOptions, ZipWriter};
use zip::{CompressionMethod, ZipArchive};

// Define a structure to hold class info during processing
#[derive(Debug)]
struct ClassInfo {
    original_path: String,  // For logging/reference
    jar_entry_name: String, // e.g., "option.class" or "com/example/My.class"
    data: Vec<u8>,          // Raw class data
    is_known_good: bool,
}

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

    // --- Argument Parsing ---
    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        // Basic argument parsing (could be improved with a crate like clap)
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
        // Tentatively accept potential input files or ignored flags
        } else if !arg.starts_with("-") {
             // Only add if it looks like a class or jar, or isn't clearly a linker flag
             // This filtering is basic and might need refinement
             if arg.ends_with(".class") || arg.ends_with(".jar") ||
                !["-lSystem", "-lc", "-lm", "-arch", "-mmacosx-version-min", "-Wl,-dead_strip", "-nodefaultlibs"].contains(&arg.as_str()) && !arg.contains('=') // Avoid flags like -mmacosx-version-min=11.0
             {
                 input_files.push(arg.clone());
             } else {
                 eprintln!("Warning: Ignoring likely native linker argument: {}", arg);
             }
            i += 1;
        }
        else {
            eprintln!("Warning: Ignoring unknown argument or flag: {}", arg);
            i += 1;
        }
    }

    // --- Validate Arguments ---
    if input_files.is_empty() {
        eprintln!("Error: No input files (.class or .jar) provided after filtering.");
        eprintln!("Please ensure class/jar files are passed correctly.");
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

    let processor_jar_path_opt = processor_jar_path.as_deref(); // Use Option<&Path>
    if let Some(path) = processor_jar_path_opt {
        if !path.exists() || !path.is_file() {
            eprintln!(
                "Error: ASM processor JAR does not exist or is not a file: {}",
                path.display()
            );
            return Err(1);
        }
    }

    // --- Find Main Class (before actual processing) ---
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
    let main_class_name = main_classes.first().cloned(); // Get the single main class name

    // --- Execute JAR Creation Logic ---
    if let Err(err) = create_jar(
        &input_files,
        &output_file_path,
        main_class_name.as_deref(), // Pass Option<&str>
        processor_jar_path_opt,     // Pass Option<&Path>
        known_good_identifier.as_deref(),
    ) {
        eprintln!("Error creating JAR: {}", err);
        // Attempt to clean up temp dir explicitly on error if possible (best effort)
        // Note: TempDir automatically cleans up on drop if `create_jar` returns Ok
        return Err(1);
    }

    println!("JAR file created successfully: {}", output_file_path);
    Ok(())
}

// --- Helper Functions ---

/// Finds classes with a `public static void main(String[])` method using ristretto_classfile.
/// Returns a list of fully qualified class names.
fn find_main_classes_with_ristretto(input_files: &[String]) -> io::Result<Vec<String>> {
    let mut main_classes = Vec::new();
    let main_method_name = "main";
    let main_method_descriptor = "([Ljava/lang/String;)V";

    for file_path_str in input_files {
        let path = Path::new(file_path_str);
        if !path.exists() {
             eprintln!(
                "Warning (main scan): Input path does not exist: {}. Skipping.",
                file_path_str
            );
            continue;
        }
        if !path.is_file() {
             eprintln!(
                "Warning (main scan): Input path is not a file: {}. Skipping.",
                file_path_str
            );
            continue;
        }

        if file_path_str.ends_with(".class") {
            match fs::read(path) {
                Ok(data) => {
                    match check_class_data_for_main(&data, main_method_name, main_method_descriptor)
                    {
                        Ok(Some(class_name)) => {
                            println!("Found main method in class file: {}", class_name);
                            main_classes.push(class_name);
                        }
                        Ok(None) => {} // No main method here
                        Err(e) => {
                            eprintln!(
                                "Warning (main scan): Could not parse class file '{}': {}. Skipping.",
                                file_path_str, e
                            );
                        }
                    }
                }
                Err(e) => {
                    eprintln!(
                        "Warning (main scan): Failed to read file '{}': {}. Skipping.",
                        file_path_str, e
                    );
                }
            }
        } else if file_path_str.ends_with(".jar") {
            println!("Scanning JAR for main method: {}", file_path_str);
            match fs::File::open(path) {
                Ok(jar_file) => {
                    let reader = BufReader::new(jar_file);
                    match ZipArchive::new(reader) {
                        Ok(mut archive) => {
                            for i in 0..archive.len() {
                                match archive.by_index(i) {
                                    Ok(mut file) => {
                                        if file.is_file() && file.name().ends_with(".class") {
                                            let entry_name = file.name().to_string();
                                            let mut data =
                                                Vec::with_capacity(file.size() as usize);
                                            if let Err(e) = file.read_to_end(&mut data) {
                                                eprintln!(
                                                    "Warning (main scan): Failed to read entry '{}' in JAR '{}': {}. Skipping entry.",
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
                                                Ok(None) => {}
                                                Err(e) => {
                                                    eprintln!(
                                                        "Warning (main scan): Could not parse class entry '{}' within JAR '{}': {}. Skipping entry.",
                                                        entry_name, file_path_str, e
                                                    );
                                                }
                                            }
                                        }
                                    }
                                    Err(e) => {
                                        eprintln!(
                                            "Warning (main scan): Error reading entry {} in JAR '{}': {}. Skipping entry.",
                                            i, file_path_str, e
                                        );
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!(
                                "Warning (main scan): Could not open or read JAR file '{}' as zip archive: {}. Skipping.",
                                file_path_str, e
                            );
                        }
                    }
                }
                Err(e) => {
                    eprintln!(
                        "Warning (main scan): Failed to open file '{}': {}. Skipping.",
                        file_path_str, e
                    );
                }
            }
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
    let cursor = Cursor::new(data); 

    let class_file = match ClassFile::from_bytes(&mut Cursor::new(data.to_vec())) {
        Ok(cf) => cf,
        Err(e) => {
            // Don't return Err for parsing errors here, just indicate no main found
            // Let the main processing handle deeper errors if needed
             eprintln!("Debug (check_class_data): Ristretto parse failed: {}", e);
             return Ok(None);
        }
    };

    for method in &class_file.methods {
        let flags = &method.access_flags;
        if flags.contains(MethodAccessFlags::PUBLIC) && flags.contains(MethodAccessFlags::STATIC) {
            // Using fallible pool lookups
            let name_result = class_file.constant_pool.try_get_utf8(method.name_index);
            let descriptor_result = class_file.constant_pool.try_get_utf8(method.descriptor_index);

            match (name_result, descriptor_result) {
                (Ok(name), Ok(descriptor)) => {
                    if name == main_method_name && descriptor == main_method_descriptor {
                        return match class_file.class_name() {
                             // Ristretto's class_name() returns Result<&String, _>
                            Ok(class_name_ref) => Ok(Some(class_name_ref.replace('/', "."))), // Convert internal name to FQN
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
                 // Ignore errors during lookup for finding main, just means this method isn't it
                (Err(_), _) | (_, Err(_)) => {}
            }
        }
    }
    Ok(None) // No main method found
}

/// Extracts class files from a JAR.
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
                    "Warning (jar extract): Error reading entry {} in JAR '{}': {}. Skipping entry.",
                    i, jar_path, e
                );
                continue;
            }
        };
        // Ensure it's a file and ends with .class, ignore directories like META-INF/
        if file.is_file() && file.name().ends_with(".class") {
            let mut contents = Vec::with_capacity(file.size() as usize);
            file.read_to_end(&mut contents)?;
            // Use the file.name() directly as it should contain the full path within the JAR
            class_files.push((file.name().to_string(), contents));
        }
    }
    Ok(class_files)
}


/// Creates the final JAR, staging files and running the ASM processor if specified.
fn create_jar(
    input_files: &[String],
    output_jar_path: &str,
    main_class_name: Option<&str>,
    processor_jar_path: Option<&Path>,
    known_good_identifier: Option<&str>,
) -> io::Result<()> {

    // Regex to potentially clean up mangled class names from rustc_codegen_jvm
    // Assumes format like `prefix-hash.ClassName.class` -> `ClassName.class`
    let re = Regex::new(r"^[^-]+-[0-9a-fA-F]+\.(?P<name>[^/\\]+\.class)$").unwrap();

    let mut all_classes: Vec<ClassInfo> = Vec::new();
    let mut processed_data_map: HashMap<String, Vec<u8>> = HashMap::new();
    let mut added_jar_entries = HashSet::new(); // Tracks JAR entry names to avoid duplicates

    // --- Stage 1: Collect all class info ---
    println!("Stage 1: Collecting class file information...");
    for input_file_path_str in input_files {
        let input_path = Path::new(input_file_path_str);
        if !input_path.exists() {
            eprintln!("Warning (collect): Input file does not exist: {}. Skipping.", input_file_path_str);
            continue;
        }
        if !input_path.is_file() {
            eprintln!("Warning (collect): Input path is not a file: {}. Skipping.", input_file_path_str);
            continue;
        }

        // Determine if the source file/JAR is considered "known good"
        let is_known_good_source = known_good_identifier
                .map(|id| input_file_path_str.contains(id))
                .unwrap_or(false);

        if input_file_path_str.ends_with(".jar") {
            println!("Collecting from JAR: {}", input_file_path_str);
            let class_files_in_jar = match process_jar_file(input_file_path_str) {
                Ok(cf) => cf,
                Err(e) => {
                    eprintln!("Warning (collect): Failed to process JAR file '{}': {}. Skipping.", input_file_path_str, e);
                    continue;
                }
            };
            for (jar_entry_name, data) in class_files_in_jar {
                // Basic duplicate check based on JAR entry name during collection
                 if added_jar_entries.insert(jar_entry_name.clone()) {
                     all_classes.push(ClassInfo {
                         original_path: format!("{}!/{}", input_file_path_str, jar_entry_name),
                         jar_entry_name, // Already has correct path like "kotlin/collections/List.class"
                         data,
                         is_known_good: is_known_good_source, // Mark based on JAR source
                     });
                 } else {
                    println!("  Skipping duplicate entry during collection: {}", jar_entry_name);
                 }
            }
        } else if input_file_path_str.ends_with(".class") {
            println!("Collecting class file: {}", input_file_path_str);
             let original_file_name = input_path.file_name().and_then(|n| n.to_str()).unwrap_or("");

            // Attempt to clean up the name for the JAR entry
            let jar_entry_name = if let Some(caps) = re.captures(original_file_name) {
                caps["name"].to_string() // e.g., "option.class"
            } else {
                 // Assume simple name if no mangling pattern matches.
                 // Might need adjustment if classes have packages.
                 original_file_name.to_string()
            };

            if jar_entry_name.is_empty() || !jar_entry_name.ends_with(".class") {
                eprintln!("Warning (collect): Could not determine valid JAR entry name for {}. Skipping.", input_file_path_str);
                continue;
            }

            // Check for duplicates before reading file
            if added_jar_entries.insert(jar_entry_name.clone()) {
                 match fs::read(input_path) {
                    Ok(data) => {
                        all_classes.push(ClassInfo {
                            original_path: input_file_path_str.to_string(),
                            jar_entry_name,
                            data,
                            is_known_good: is_known_good_source, // Mark based on file source
                        });
                    }
                    Err(e) => {
                        eprintln!("Warning (collect): Failed to read class file '{}': {}. Skipping.", input_file_path_str, e);
                         // Remove from added set if read fails
                         added_jar_entries.remove(&jar_entry_name);
                    }
                }
            } else {
                 println!("  Skipping duplicate entry during collection: {} (from {})", jar_entry_name, input_file_path_str);
            }
        } else {
             // Ignore other file types silently during collection
        }
    }
    println!("Collected {} unique class entries.", all_classes.len());
    if all_classes.is_empty() {
        eprintln!("Error: No valid class files found in the input.");
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No class files found"));
    }

    // --- Stage 2: Create temp dir and stage all collected files ---
    println!("Stage 2: Staging class files into temporary directory...");
    let temp_dir = tempfile::tempdir()?; // RAII temporary directory
    println!("Temporary directory: {}", temp_dir.path().display());
    for class_info in &all_classes {
        // Construct the target path inside the temp directory, respecting package structure
        let target_path_in_temp = temp_dir.path().join(&class_info.jar_entry_name);

        // Create parent directories if the jar_entry_name includes slashes
        if let Some(parent_dir) = target_path_in_temp.parent() {
            fs::create_dir_all(parent_dir)?;
        }
        fs::write(&target_path_in_temp, &class_info.data)?;
         // Minimal logging here, could add verbose flag
         // println!("  Staged: {} -> {}", class_info.original_path, target_path_in_temp.display());
    }
    println!("Staging complete.");

    // --- Stage 3: Setup classpath and process necessary files ---
    let classpath_string;

    if let Some(processor_path) = processor_jar_path {
        println!("Stage 3: Processing class files with ASM processor...");
        let processor_main_class = "asmprocessor.ProcessorKt"; // Assumed main class

        let mut classpath_items = Vec::new();
        // 1. Add the populated temporary directory FIRST
        classpath_items.push(temp_dir.path().to_string_lossy().to_string());
        // 2. Add the processor JAR itself
        classpath_items.push(processor_path.to_string_lossy().to_string());

        // 3. *** IMPORTANT: Add Kotlin Stdlib to classpath ***
        // Find the kotlin-stdlib JAR path from the original input files
        let kotlin_stdlib_path = input_files.iter().find(|f| f.contains("kotlin-stdlib") && f.ends_with(".jar"));
        if let Some(kt_stdlib) = kotlin_stdlib_path {
            println!("Adding Kotlin stdlib to processor classpath: {}", kt_stdlib);
            classpath_items.push(kt_stdlib.clone()); // Add the full path to kotlin-stdlib JAR
        } else {
            // Check if the processor is a shadow JAR (might contain stdlib)
            let is_shadow_jar = processor_path.file_name()
                .map_or(false, |name| name.to_string_lossy().contains("-all"));
            if !is_shadow_jar {
                 eprintln!("Warning: Kotlin standard library JAR not found in inputs, and processor doesn't appear to be a shadow JAR. Processing might fail if the processor needs it.");
            } else {
                 println!("Processor appears to be a shadow JAR, assuming Kotlin stdlib is included.");
            }
        }

        // Join classpath items with the system-specific separator
        classpath_string = classpath_items.join(":");
        println!("Processor Classpath: {}", classpath_string);

        println!("Debug: Verifying contents of temp dir ({}) before processing loop:", temp_dir.path().display());
        match fs::read_dir(temp_dir.path()) {
            Ok(entries) => {
                let mut found_option = false;
                for entry in entries {
                    if let Ok(entry) = entry {
                        let fname = entry.file_name().to_string_lossy().to_string();
                        println!("  - {}", fname);
                        if fname == "option.class" {
                            found_option = true;
                        }
                    }
                }
                if !found_option {
                     eprintln!("  ERROR: option.class NOT FOUND in temp dir!");
                } else {
                     println!("  OK: option.class found in temp dir.");
                }
            }
            Err(e) => {
                eprintln!("  Error reading temp dir: {}", e);
            }
        }

        // Process each class that isn't known-good
        for class_info in &all_classes {
            if !class_info.is_known_good {
                let path_in_temp = temp_dir.path().join(&class_info.jar_entry_name);

                // Call the processing function using the staged path
                match process_with_asm(
                    processor_main_class,
                    &classpath_string,
                    &path_in_temp, // Path to the input class *in the staged directory*
                    &class_info.original_path, // Original source path for logging
                ) {
                    Ok(processed_data) => {
                         // Store the processed data, keyed by the JAR entry name
                         processed_data_map.insert(class_info.jar_entry_name.clone(), processed_data);
                         println!("  Successfully processed: {}", class_info.original_path);
                    }
                    Err(e) => {
                        // Error is already logged in process_with_asm
                        // Return the error to stop the build immediately
                        // temp_dir is cleaned up automatically by RAII when function returns
                        return Err(e);
                    }
                }
            } else {
                 println!("  Skipping processing (known good): {}", class_info.original_path);
            }
        }
        println!("Processing finished.");
    } else {
        println!("Stage 3: Skipping ASM processing (no --asm-processor specified).");
    }

    // --- Stage 4: Create the final JAR ---
    println!("Stage 4: Creating final JAR file: {}", output_jar_path);
    let output_file = fs::File::create(output_jar_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let options = SimpleFileOptions::default()
        .compression_method(CompressionMethod::DEFLATE) // Use DEFLATE for compression
        .unix_permissions(0o644); // Standard file permissions

    // Add Manifest file first
    let manifest_content = create_manifest_content(main_class_name);
    zip_writer.start_file("META-INF/MANIFEST.MF", options)?;
    zip_writer.write_all(manifest_content.as_bytes())?;
    println!("  Added META-INF/MANIFEST.MF");

    // Use a separate set here just for the final JAR writing phase to be explicit
    let mut final_jar_entries = HashSet::new();
    final_jar_entries.insert("META-INF/MANIFEST.MF".to_string()); // Track manifest

    // Add class files (processed or original)
    for class_info in &all_classes {
        // Ensure we only add each unique JAR entry name once to the final JAR
        if final_jar_entries.insert(class_info.jar_entry_name.clone()) {
            // Get data: check the processed map first, fallback to original data
            let data_to_write = processed_data_map
                .get(&class_info.jar_entry_name)
                .unwrap_or(&class_info.data); // Fallback to original data if not processed

            zip_writer.start_file(&class_info.jar_entry_name, options)?;
            zip_writer.write_all(data_to_write)?;

            if processed_data_map.contains_key(&class_info.jar_entry_name) {
                println!("  Adding processed: {}", class_info.jar_entry_name);
            } else {
                println!("  Adding original:  {}", class_info.jar_entry_name);
            }
        } else {
             // This check should ideally be redundant due to collection-phase checks, but belt-and-suspenders
             println!("  Skipping duplicate entry during final write: {}", class_info.jar_entry_name);
        }
    }

    zip_writer.finish()?; // Finalize the ZIP archive
    println!("JAR creation finished.");
    // temp_dir is automatically removed when `temp_dir` goes out of scope here (RAII)
    Ok(())
}


/// Executes the external Java ASM processor.
fn process_with_asm(
    processor_main_class: &str,
    classpath: &str,
    input_class_path_in_temp: &Path, // Path to class file *inside temp_dir*
    original_path_for_logging: &str, // Original source file path for messages
) -> io::Result<Vec<u8>> {

    // Create a *new* temporary file just for the output of this specific run
    // This file will be automatically deleted when `temp_out_file` goes out of scope
    let temp_out_file = NamedTempFile::new()?;
    let temp_out_path = temp_out_file.path().to_path_buf(); // Get the path before it's potentially dropped

    let mut cmd = Command::new("java");
    cmd.arg("-cp")
       .arg(classpath) // The full classpath (temp_dir:processor.jar:kotlin-stdlib.jar)
       .arg(processor_main_class) // e.g., asmprocessor.ProcessorKt
       // Pass the path *within the temp dir* as the input file argument
       .arg(input_class_path_in_temp)
       // Pass the path to the temporary output file as the second argument
       .arg(&temp_out_path);

    println!(
        "Running processor for {} (staged at {}):",
        original_path_for_logging, input_class_path_in_temp.display()
    );
     // For debugging, print the exact command
     // eprintln!("Executing: {:?}", cmd);

    let output = cmd.output().map_err(|e| {
        // Add context if the command couldn't even be executed
        io::Error::new(e.kind(), format!("Failed to execute java command: {}", e))
    })?;

    if !output.status.success() {
        // Provide detailed error context if the processor fails
        eprintln!("--- ASM Processor Failed ---");
        eprintln!("Class Source: {}", original_path_for_logging);
        eprintln!("Staged Path: {}", input_class_path_in_temp.display());
        eprintln!("Exit Code: {}", output.status);
        eprintln!("Command: {:?}", cmd); // Show the exact command run
        eprintln!("Processor STDOUT:\n{}", String::from_utf8_lossy(&output.stdout));
        eprintln!("Processor STDERR:\n{}", String::from_utf8_lossy(&output.stderr));
        eprintln!("--- End ASM Processor Failure ---");
        return Err(io::Error::new(
            io::ErrorKind::Other, // Use Other for external process failure
            format!("ASM processor failed for class originally from {}", original_path_for_logging),
        ));
    }

    // Read the processed data from the temporary output file
    fs::read(&temp_out_path)
}


/// Creates the content for the META-INF/MANIFEST.MF file.
fn create_manifest_content(main_class_name: Option<&str>) -> String {
    let mut manifest = String::new();
    manifest.push_str("Manifest-Version: 1.0\r\n");
    // Identify the creator tool
    manifest.push_str("Created-By: java-linker-rs (rust)\r\n");

    // Add the Main-Class entry if provided
    if let Some(main_class) = main_class_name {
        // Ensure FQN uses dots, not slashes
        let main_class_fqn = main_class.replace('/', ".");
        manifest.push_str(&format!("Main-Class: {}\r\n", main_class_fqn));
    }
    // Important: Manifest entries must end with CRLF, and the file must end with a newline
    manifest.push_str("\r\n"); // Ensure final newline
    manifest
}