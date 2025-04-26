use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io::{self, BufReader, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::fs::rename;

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
            "Usage: java-linker <input_files...> -o <output_jar_file> [--stackmapadder <processor_jar>] [--known-good <identifier>] [--optimizer <optimizer_jar> --proguard-config <config_file>]"
        );
        return Err(1);
    }

    let mut input_files: Vec<String> = Vec::new();
    let mut output_file: Option<String> = None;
    let mut processor_jar_path: Option<PathBuf> = None;
    let mut optimizer_jar_path: Option<PathBuf> = None;
    let mut proguard_config_path: Option<PathBuf> = None;
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
        } else if arg == "--stackmapadder" {
            if i + 1 < args.len() {
                processor_jar_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            } else {
                eprintln!("Error: --stackmapadder flag requires a path");
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
        } else if arg == "--optimizer" {
            if i + 1 < args.len() {
                optimizer_jar_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            } else {
                eprintln!("Error: --optimizer flag requires a path to the optimizer JAR");
                return Err(1);
            }
        } else if arg == "--proguard-config" {
            if i + 1 < args.len() {
                proguard_config_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            } else {
                eprintln!(
                    "Error: --proguard-config flag requires a path to the ProGuard config file"
                );
                return Err(1);
            }
        // Tentatively accept potential input files or ignored flags
        } else if !arg.starts_with("-") {
            // Only add if it looks like a class or jar, or isn't clearly a linker flag
            // This filtering is basic and might need refinement
            if arg.ends_with(".class")
                || arg.ends_with(".jar")
                || ![
                    "-lSystem",
                    "-lc",
                    "-lm",
                    "-arch",
                    "-mmacosx-version-min",
                    "-Wl,-dead_strip",
                    "-nodefaultlibs",
                ]
                .contains(&arg.as_str())
                    && !arg.contains('=')
            // Avoid flags like -mmacosx-version-min=11.0
            {
                input_files.push(arg.clone());
            } else {
                eprintln!("Warning: Ignoring likely native linker argument: {}", arg);
            }
            i += 1;
        } else {
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

    if optimizer_jar_path.is_some() != proguard_config_path.is_some() {
        eprintln!("Error: --optimizer and --proguard-config must be used together.");
        return Err(1);
    }

    if let Some(path) = &optimizer_jar_path {
        if !path.exists() || !path.is_file() {
            eprintln!(
                "Error: Optimizer JAR does not exist or is not a file: {}",
                path.display()
            );
            return Err(1);
        }
    }
    if let Some(path) = &proguard_config_path {
        if !path.exists() || !path.is_file() {
            eprintln!(
                "Error: ProGuard config file does not exist or is not a file: {}",
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
        main_class_name.as_deref(),
        processor_jar_path_opt,
        optimizer_jar_path.as_deref(),
        proguard_config_path.as_deref(),
        known_good_identifier.as_deref(),
        &input_files, // Pass original list again for classpath lookups
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
                                            let mut data = Vec::with_capacity(file.size() as usize);
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
    let class_file = match ClassFile::from_bytes(&mut Cursor::new(data.to_vec())) {
        Ok(cf) => cf,
        Err(e) => {
            // Don't return Err for parsing errors here, just indicate no main found
            eprintln!("Debug (check_class_data): Ristretto parse failed: {}", e);
            return Ok(None);
        }
    };

    for method in &class_file.methods {
        let flags = &method.access_flags;
        if flags.contains(MethodAccessFlags::PUBLIC) && flags.contains(MethodAccessFlags::STATIC) {
            let name_result = class_file.constant_pool.try_get_utf8(method.name_index);
            let descriptor_result = class_file
                .constant_pool
                .try_get_utf8(method.descriptor_index);

            match (name_result, descriptor_result) {
                (Ok(name), Ok(descriptor)) => {
                    if name == main_method_name && descriptor == main_method_descriptor {
                        return match class_file.class_name() {
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
                (Err(_), _) | (_, Err(_)) => {} // Ignore lookup errors here
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
        if file.is_file() && file.name().ends_with(".class") {
            let mut contents = Vec::with_capacity(file.size() as usize);
            file.read_to_end(&mut contents)?;
            class_files.push((file.name().to_string(), contents));
        }
    }
    Ok(class_files)
}

/// Creates the final JAR, staging files and running the ASM processor if specified.
fn create_jar(
    input_files_for_content: &[String],
    final_output_jar_path: &str,
    main_class_name: Option<&str>,
    stackmap_adder_jar_path: Option<&Path>,
    optimizer_jar_path: Option<&Path>,
    proguard_config_path: Option<&Path>,
    known_good_identifier: Option<&str>,
    original_input_files: &[String], // Used for classpath searches
) -> io::Result<()> {
    // --- Stage 1: Collect all class info ---
    println!("Stage 1: Collecting class file information...");
    let re = Regex::new(r"^[^-]+-[0-9a-fA-F]+\.(?P<name>[^/\\]+\.class)$").unwrap();
    let mut all_classes: Vec<ClassInfo> = Vec::new();
    let mut added_jar_entries = HashSet::new();

    for input_file_path_str in input_files_for_content {
        let input_path = Path::new(input_file_path_str);
        if !input_path.exists() {
            eprintln!(
                "Warning (collect): Input file does not exist: {}. Skipping.",
                input_file_path_str
            );
            continue;
        }
        if !input_path.is_file() {
             eprintln!(
                "Warning (collect): Input path is not a file: {}. Skipping.",
                 input_file_path_str
            );
            continue;
        }

        let is_known_good_source = known_good_identifier
            .map(|id| input_file_path_str.contains(id))
            .unwrap_or(false);

        if input_file_path_str.ends_with(".jar") {
            let class_files_in_jar = match process_jar_file(input_file_path_str) {
                Ok(cf) => cf,
                Err(e) => {
                    eprintln!(
                        "Warning (collect): Failed to process JAR file '{}': {}. Skipping.",
                        input_file_path_str, e
                    );
                    continue;
                }
            };
            for (jar_entry_name, data) in class_files_in_jar {
                if added_jar_entries.insert(jar_entry_name.clone()) {
                    all_classes.push(ClassInfo {
                        original_path: format!("{}!/{}", input_file_path_str, jar_entry_name),
                        jar_entry_name,
                        data,
                        is_known_good: is_known_good_source,
                    });
                }
            }
        } else if input_file_path_str.ends_with(".class") {
            let original_file_name = input_path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("");

            let jar_entry_name = if let Some(caps) = re.captures(original_file_name) {
                caps["name"].to_string()
            } else {
                original_file_name.to_string()
            };

            if jar_entry_name.is_empty() || !jar_entry_name.ends_with(".class") {
                eprintln!(
                    "Warning (collect): Could not determine valid JAR entry name for {}. Skipping.",
                    input_file_path_str
                );
                continue;
            }

            if added_jar_entries.insert(jar_entry_name.clone()) {
                match fs::read(input_path) {
                    Ok(data) => {
                        all_classes.push(ClassInfo {
                            original_path: input_file_path_str.to_string(),
                            jar_entry_name,
                            data,
                            is_known_good: is_known_good_source,
                        });
                    }
                    Err(e) => {
                        eprintln!(
                            "Warning (collect): Failed to read class file '{}': {}. Skipping.",
                            input_file_path_str, e
                        );
                        added_jar_entries.remove(&jar_entry_name);
                    }
                }
            }
        }
    }
    println!("Collected {} unique class entries.", all_classes.len());
    if all_classes.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "No class files found",
        ));
    }

    // --- Stage 2: Create temp dir and stage all collected files ---
    println!("Stage 2: Staging class files into temporary directory...");
    let temp_dir = tempfile::tempdir()?; // RAII temporary directory
    println!("Temporary directory: {}", temp_dir.path().display());
    for class_info in &all_classes {
        let target_path_in_temp = temp_dir.path().join(&class_info.jar_entry_name);
        if let Some(parent_dir) = target_path_in_temp.parent() {
            fs::create_dir_all(parent_dir)?;
        }
        fs::write(&target_path_in_temp, &class_info.data)?;
    }
    println!("Staging complete.");

    // --- Stage 3: Process with Stack Map Adder (if specified) ---
    let mut processed_data_map: HashMap<String, Vec<u8>> = HashMap::new();
    if let Some(processor_path) = stackmap_adder_jar_path {
        println!("Stage 3: Processing class files with Stack map adder...");
        let processor_main_class = "stackmapadder.ProcessorKt";
        let mut classpath_items = Vec::new();
        classpath_items.push(temp_dir.path().to_string_lossy().to_string());
        classpath_items.push(processor_path.to_string_lossy().to_string());

        let kotlin_stdlib_path = original_input_files
            .iter()
            .find(|f| f.contains("kotlin-stdlib") && f.ends_with(".jar"));

        if let Some(kt_stdlib) = kotlin_stdlib_path {
            classpath_items.push(kt_stdlib.clone());
        } else {
            let is_shadow_jar = processor_path.file_name().map_or(false, |name| name.to_string_lossy().contains("-all"));
            if !is_shadow_jar {
                 eprintln!("Warning (Stackmap Adder): Kotlin standard library JAR not found in inputs and processor doesn't appear to be a shadow JAR. Processing might fail.");
            }
        }
        let classpath_string = classpath_items.join(if cfg!(windows) { ";" } else { ":" });

        for class_info in &all_classes {
            if !class_info.is_known_good {
                let path_in_temp = temp_dir.path().join(&class_info.jar_entry_name);
                match process_with_asm(
                    processor_main_class,
                    &classpath_string,
                    &path_in_temp,
                    &class_info.original_path,
                ) {
                    Ok(processed_data) => {
                        processed_data_map.insert(class_info.jar_entry_name.clone(), processed_data);
                         // Overwrite in temp dir as well, optimizer might read from dir or JAR
                        fs::write(&path_in_temp, processed_data_map.get(&class_info.jar_entry_name).unwrap())?;
                    }
                    Err(e) => {
                        eprintln!("Error during stack map adding: {}", e);
                        return Err(e);
                    }
                }
            }
        }
        println!("Stack map adding finished.");
    } else {
        println!("Stage 3: Skipping stack map adding.");
    }

    // --- Stage 4: Create Intermediate JAR (Content Only - NO MANIFEST) ---
    println!("Stage 4: Creating intermediate JAR file (content only)...");
    let intermediate_jar_path = temp_dir.path().join("intermediate_output.jar");
    // Pass the processed data map to use the stackmap-added data where available
    create_jar_content_only(
        &intermediate_jar_path,
        &all_classes,
        &processed_data_map,
    )?;
    println!("Intermediate JAR (content only) created: {}", intermediate_jar_path.display());


    // --- Stage 5: Run ProGuard Optimizer (if specified) ---
    let mut jar_before_manifest_path: PathBuf; // This will point to the JAR needing the manifest added

    if let (Some(opt_jar_path), Some(pg_config_path)) = (optimizer_jar_path, proguard_config_path) {
        println!("Stage 5: Running ProGuard Optimizer...");
        let optimized_output_jar_path = temp_dir.path().join("optimized_output.jar");

        match run_proguard_optimizer(
            opt_jar_path,
            pg_config_path,
            &intermediate_jar_path, // Input is the content-only intermediate JAR
            &optimized_output_jar_path, // Output is a new temp file
            original_input_files,
        ) {
            Ok(_) => {
                println!("ProGuard optimization successful.");
                // The JAR needing the manifest is the one produced by the optimizer
                jar_before_manifest_path = optimized_output_jar_path;
            }
            Err(e) => {
                 eprintln!("Error during ProGuard optimization: {}", e);
                 return Err(e);
            }
        }
        println!("ProGuard optimization finished.");
    } else {
        println!("Stage 5: Skipping ProGuard optimization.");
        // If optimizer wasn't run, the JAR needing the manifest is the intermediate JAR
        jar_before_manifest_path = intermediate_jar_path;
    }

    // --- Stage 5.5: Add Manifest to the selected JAR ---
    println!("Stage 5.5: Adding manifest to final JAR content...");
    let final_jar_with_manifest_path = temp_dir.path().join("final_with_manifest.jar");
    add_manifest_to_jar(
        &jar_before_manifest_path, // Input is the result of Stage 5 (optimized or intermediate)
        &final_jar_with_manifest_path, // Output is a new temp JAR containing manifest + content
        main_class_name, // Pass the main class name determined earlier
    )?;
    println!("Manifest added. Temporary path: {}", final_jar_with_manifest_path.display());


    // --- Stage 6: Finalize Output ---
    println!("Stage 6: Finalizing output JAR...");
    // Move or rename the JAR *that now includes the manifest* to the final destination path
    if let Some(parent) = Path::new(final_output_jar_path).parent() {
        fs::create_dir_all(parent)?;
    }
    let _ = fs::remove_file(final_output_jar_path); // Attempt removal first

    match rename(&final_jar_with_manifest_path, final_output_jar_path) { // Source is the JAR with manifest
        Ok(_) => {
             println!(
                "Final JAR placed at: {}",
                final_output_jar_path
            );
        }
        Err(e) => {
            eprintln!(
                "Error moving temporary JAR '{}' to final destination '{}': {}",
                final_jar_with_manifest_path.display(), final_output_jar_path, e
            );
             eprintln!("Attempting to copy instead...");
             match fs::copy(&final_jar_with_manifest_path, final_output_jar_path) {
                Ok(_) => println!("Copy successful."),
                Err(copy_err) => {
                     eprintln!("Fallback copy failed: {}", copy_err);
                     return Err(e);
                }
            }
        }
    }

    // TempDir cleans up automatically on drop.
    println!("JAR creation process complete.");
    Ok(())
}

/// Executes the external Java ASM processor.
fn process_with_asm(
    processor_main_class: &str,
    classpath: &str,
    input_class_path_in_temp: &Path,
    original_path_for_logging: &str,
) -> io::Result<Vec<u8>> {
    let temp_out_file = NamedTempFile::new()?;
    let temp_out_path = temp_out_file.path().to_path_buf();

    let mut cmd = Command::new("java");
    cmd.arg("-cp")
        .arg(classpath)
        .arg(processor_main_class)
        .arg(input_class_path_in_temp)
        .arg(&temp_out_path);

    let output = cmd.output().map_err(|e| {
        io::Error::new(e.kind(), format!("Failed to execute java command: {}", e))
    })?;

    if !output.status.success() {
        eprintln!("--- ASM Processor Failed ---");
        eprintln!("Class Source: {}", original_path_for_logging);
        eprintln!("Staged Path: {}", input_class_path_in_temp.display());
        eprintln!("Exit Code: {}", output.status);
        eprintln!("Command: {:?}", cmd);
        eprintln!("Processor STDOUT:\n{}", String::from_utf8_lossy(&output.stdout));
        eprintln!("Processor STDERR:\n{}", String::from_utf8_lossy(&output.stderr));
        eprintln!("--- End ASM Processor Failure ---");
        return Err(io::Error::new(
            io::ErrorKind::Other,
            format!("ASM processor failed for class originally from {}", original_path_for_logging),
        ));
    }

    fs::read(&temp_out_path)
}

/// Creates the content for the META-INF/MANIFEST.MF file.
fn create_manifest_content(main_class_name: Option<&str>) -> String {
    let mut manifest = String::new();
    manifest.push_str("Manifest-Version: 1.0\r\n");
    manifest.push_str("Created-By: java-linker-rs (rust)\r\n");
    if let Some(main_class) = main_class_name {
        let main_class_fqn = main_class.replace('/', ".");
        manifest.push_str(&format!("Main-Class: {}\r\n", main_class_fqn));
    }
    manifest.push_str("\r\n"); // Final newline needed
    manifest
}

/// Creates an intermediate JAR file containing ONLY the provided class content.
/// Does NOT add a manifest file.
fn create_jar_content_only(
    jar_path: &Path,
    all_classes: &[ClassInfo],
    processed_data_map: &HashMap<String, Vec<u8>>, // From stackmap adder
) -> io::Result<()> {
    let output_file = fs::File::create(jar_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let options = SimpleFileOptions::default()
        .compression_method(CompressionMethod::DEFLATE)
        .unix_permissions(0o644);

    let mut final_jar_entries = HashSet::new(); // Track added entries

    // Add class files (use processed if available, else original)
    for class_info in all_classes {
        if final_jar_entries.insert(class_info.jar_entry_name.clone()) {
            let data_to_write = processed_data_map
                .get(&class_info.jar_entry_name)
                .unwrap_or(&class_info.data); // Fallback to original if not processed

            zip_writer.start_file(&class_info.jar_entry_name, options)?;
            zip_writer.write_all(data_to_write)?;
        }
    }

    zip_writer.finish()?;
    Ok(())
}

/// Adds a standard META-INF/MANIFEST.MF to an existing JAR file.
/// Reads the input JAR, writes a new output JAR including the manifest and all original entries (except any pre-existing manifest).
fn add_manifest_to_jar(
    input_jar_path: &Path,
    output_jar_path: &Path,
    main_class_name: Option<&str>,
) -> io::Result<()> {
    let input_file = fs::File::open(input_jar_path)?;
    let reader = BufReader::new(input_file);
    let mut input_archive = ZipArchive::new(reader)?;

    let output_file = fs::File::create(output_jar_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let options = SimpleFileOptions::default()
        .compression_method(CompressionMethod::DEFLATE)
        .unix_permissions(0o644);

    // 1. Add the new Manifest file first
    let manifest_content = create_manifest_content(main_class_name);
    zip_writer.start_file("META-INF/MANIFEST.MF", options)?;
    zip_writer.write_all(manifest_content.as_bytes())?;

    // 2. Copy all entries from the input JAR, *except* for any existing manifest
    for i in 0..input_archive.len() {
        let entry = input_archive.by_index(i)?; // Read entry info
        let entry_name = entry.name();

        // Skip copying if it's the manifest file we potentially just wrote
        if entry_name == "META-INF/MANIFEST.MF" {
            continue;
        }

        // Use raw_copy_file for efficiency (preserves compression, metadata)
        zip_writer.raw_copy_file(entry)?;
    }

    zip_writer.finish()?;
    Ok(())
}


/// Executes the external ProGuard optimizer Kotlin script.
fn run_proguard_optimizer(
    optimizer_jar_path: &Path,
    proguard_config_path: &Path,
    input_jar_path: &Path,          // The intermediate JAR (content only)
    output_jar_path: &Path,         // Where the optimizer should write its result
    original_input_files: &[String], // To find Kotlin stdlib
) -> io::Result<()> {
    let optimizer_main_class = "optimise2.ProcessorKt";

    let mut classpath_items = Vec::new();
    classpath_items.push(optimizer_jar_path.to_string_lossy().to_string());

    let kotlin_stdlib_path = original_input_files
        .iter()
        .find(|f| f.contains("kotlin-stdlib") && f.ends_with(".jar"));

    if let Some(kt_stdlib) = kotlin_stdlib_path {
        classpath_items.push(kt_stdlib.clone());
    } else {
         let is_shadow_jar = optimizer_jar_path.file_name().map_or(false, |name| name.to_string_lossy().contains("-all"));
        if !is_shadow_jar {
            eprintln!("Warning (Optimizer): Kotlin standard library JAR not found in inputs, and optimizer JAR doesn't appear to be a shadow JAR. Optimization might fail.");
        }
    }
    let classpath_string = classpath_items.join(if cfg!(windows) { ";" } else { ":" });


    let mut cmd = Command::new("java");
    cmd.arg("-cp")
       .arg(&classpath_string)
       .arg(optimizer_main_class)
       .arg(input_jar_path)
       .arg(output_jar_path)
       .arg(proguard_config_path);

    let output = cmd.output().map_err(|e| {
        io::Error::new(e.kind(), format!("Failed to execute optimizer java command: {}", e))
    })?;

    if !output.status.success() {
        eprintln!("--- ProGuard Optimizer Failed ---");
        eprintln!("Input JAR: {}", input_jar_path.display());
        eprintln!("Output JAR Path: {}", output_jar_path.display());
        eprintln!("Config File: {}", proguard_config_path.display());
        eprintln!("Exit Code: {}", output.status);
        eprintln!("Command: {:?}", cmd);
        eprintln!("Optimizer STDOUT:\n{}", String::from_utf8_lossy(&output.stdout));
        eprintln!("Optimizer STDERR:\n{}", String::from_utf8_lossy(&output.stderr));
        eprintln!("--- End ProGuard Optimizer Failure ---");
        return Err(io::Error::new(io::ErrorKind::Other, "ProGuard optimizer process failed"));
    }

     if !output_jar_path.exists() {
         eprintln!("--- ProGuard Optimizer Error ---");
         eprintln!("Optimizer process completed successfully, but the output JAR file was not found at: {}", output_jar_path.display());
         eprintln!("Optimizer STDOUT:\n{}", String::from_utf8_lossy(&output.stdout));
         eprintln!("Optimizer STDERR:\n{}", String::from_utf8_lossy(&output.stderr));
         eprintln!("--- End ProGuard Optimizer Error ---");
          return Err(io::Error::new(io::ErrorKind::NotFound, "ProGuard optimizer did not create the expected output JAR file"));
     }

    Ok(())
}