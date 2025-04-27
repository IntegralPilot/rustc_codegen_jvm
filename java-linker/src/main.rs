use std::env;
use std::fs;
use std::io::{self, BufReader, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::fs::rename;

use regex::Regex;
use ristretto_classfile::{ClassFile, MethodAccessFlags};
use tempfile::tempdir;
use zip::write::{SimpleFileOptions, ZipWriter};
use zip::{CompressionMethod, ZipArchive};

// ClassInfo struct remains the same
#[derive(Debug)]
struct ClassInfo {
    jar_entry_name: String, // Now specifically for loose .class files
    data: Vec<u8>,
}

// --- main function remains largely the same ---
fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!(
            "Usage: java-linker <input_files...> -o <output_jar_file> \
            [--r8-jar <r8.jar_path> --proguard-config <config_file>]" // Note: --java-lib is implicitly derived from JAVA_HOME now
        );
        return Err(1);
    }

    let mut input_class_files: Vec<String> = Vec::new();
    let mut input_jar_files: Vec<String> = Vec::new(); // Separate JARs
    let mut output_file: Option<String> = None;
    let mut r8_jar_path: Option<PathBuf> = None;
    let mut proguard_config_path: Option<PathBuf> = None;
    let java_lib_path: Option<PathBuf> = env::var("JAVA_HOME").ok().map(PathBuf::from);


    // --- Argument Parsing (Modified) ---
    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        if arg == "-o" {
            if i + 1 < args.len() {
                let mut output_name = args[i + 1].clone();
                if !output_name.ends_with(".jar") {
                    output_name.push_str(".jar");
                }
                output_file = Some(output_name);
                i += 2;
            } else {
                eprintln!("Error: -o flag requires an output file path");
                return Err(1);
            }
        } else if arg == "--r8-jar" {
            if i + 1 < args.len() {
                r8_jar_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            } else {
                eprintln!("Error: --r8-jar flag requires a path to r8.jar");
                return Err(1);
            }
        } else if arg == "--proguard-config" {
            if i + 1 < args.len() {
                proguard_config_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            } else {
                eprintln!(
                    "Error: --proguard-config flag requires a path to the ProGuard/R8 config file"
                );
                return Err(1);
            }
        } else if !arg.starts_with('-') {
             // Collect potential input files, differentiating classes and JARs
             if arg.ends_with(".class") {
                 input_class_files.push(arg.clone());
                 i += 1;
             } else if arg.ends_with(".jar") {
                 input_jar_files.push(arg.clone());
                 i += 1;
             } else {
                 // If it's not a flag and not a recognized input type, warn or error
                 eprintln!("Warning: Ignoring unrecognized argument: {}", arg);
                 i += 1; // Move to the next argument
             }
        } else {
             eprintln!("Warning: Ignoring unknown or unused flag: {}", arg);
             i += 1;
        }
    }

    // Combine inputs for scanning, but keep them separate for create_jar
    let all_input_paths: Vec<String> = input_class_files.iter().cloned().chain(input_jar_files.iter().cloned()).collect();

    if all_input_paths.is_empty() {
        eprintln!("Error: No input files (.class or .jar) provided.");
        return Err(1);
    }

    let output_file_path = match output_file {
        Some(path) => {
            path
        }
        None => {
            eprintln!("Error: Output file (-o) not specified.");
            return Err(1);
        }
    };

    // --- Validation for R8 flags ---
    if r8_jar_path.is_some() != proguard_config_path.is_some() {
         eprintln!("Error: --r8-jar and --proguard-config must be used together.");
         return Err(1);
    }

    if r8_jar_path.is_some() && java_lib_path.is_none() {
         eprintln!("Error: JAVA_HOME environment variable must be set when using R8.");
         return Err(1);
    }


    // Validate R8 JAR, config file, and Java lib paths if provided
    if let Some(ref p) = r8_jar_path {
        if !p.exists() || !p.is_file() {
            eprintln!("Error: R8 JAR not found or not a file: {}", p.display());
            return Err(1);
        }
    }
    if let Some(ref c) = proguard_config_path {
        if !c.exists() || !c.is_file() {
            eprintln!("Error: ProGuard/R8 config not found or not a file: {}", c.display());
            return Err(1);
        }
    }
     if r8_jar_path.is_some() { // Only check java_lib_path if R8 is used
         if let Some(ref j) = java_lib_path {
            if !j.exists() {
                eprintln!("Error: Derived Java library path not found: {}. Check JAVA_HOME.", j.display());
                return Err(1);
            }
        } else {
             // This case was already handled above, but added for clarity
             eprintln!("Error: Could not derive Java library path from JAVA_HOME (required for R8).");
             return Err(1);
        }
     }


    // Find main class (scan both .class and .jar inputs)
    let main_classes = find_main_classes_with_ristretto(&all_input_paths)
        .map_err(|e| {
            eprintln!("Error during main class scan: {}", e);
            1
        })?;
    // --- Main class handling remains the same ---
    if main_classes.len() > 1 {
        eprintln!("Error: Multiple entry-point classes found:");
        for c in main_classes {
            eprintln!("  - {}", c);
        }
        eprintln!("Specify the main class explicitly or ensure only one exists.");
        return Err(1);
    }
    let main_class_name = main_classes.into_iter().next();


    // Create the JAR (pass separated inputs)
    create_jar(
        &input_class_files,
        &input_jar_files, // Pass JARs separately
        &output_file_path,
        main_class_name.as_deref(),
        r8_jar_path.as_deref(),
        proguard_config_path.as_deref(),
        java_lib_path.as_deref(),
    ).map_err(|e| {
        eprintln!("Error creating JAR file: {}", e);
        1 // Propagate error code
    })?;

    // Don't print success message if used as a linker, rustc handles that.
    // println!("JAR file created successfully: {}", output_file_path);
    Ok(())
}

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
                            //println!("Found main method in class file: {}", class_name);
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
                                                   // println!(
                                                   //     "  Found main method in: {} (within {})",
                                                   //     class_name, entry_name
                                                   // );
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

// --- check_class_data_for_main remains the same ---
fn check_class_data_for_main(
    data: &[u8],
    main_method_name: &str,
    main_method_descriptor: &str,
) -> io::Result<Option<String>> {
    let class_file = match ClassFile::from_bytes(&mut Cursor::new(data.to_vec())) {
        Ok(cf) => cf,
        Err(_e) => { // Ignore parse error details for this check
            // Treat parse failure as "no main method found in this file"
            // eprintln!("Debug (check_class_data): Parse error: {}", e); // Optional debug
            return Ok(None);
        }
    };

    for method in &class_file.methods {
        let flags = &method.access_flags;
        if flags.contains(MethodAccessFlags::PUBLIC) && flags.contains(MethodAccessFlags::STATIC) {
            // Avoid panics if constant pool is malformed, treat as not found
            let name = class_file.constant_pool.try_get_utf8(method.name_index).ok().cloned();
            let descriptor = class_file.constant_pool.try_get_utf8(method.descriptor_index).ok().cloned();


            if let (Some(n), Some(d)) = (name, descriptor) {
                 if n == main_method_name && d == main_method_descriptor {
                    return match class_file.class_name() {
                        Ok(class_name_ref) => Ok(Some(class_name_ref.replace('/', "."))),
                        Err(e) => {
                             eprintln!("Warning (check_class_data): Found main method but failed to get class name: {}", e);
                             Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!("Failed to get class name after finding main method: {}", e),
                            ))
                        }
                    };
                }
            }
        }
    }
    Ok(None)
}


// --- create_jar ---
fn create_jar(
    input_class_files: &[String], // Separate .class files
    input_jar_files: &[String],   // Separate .jar files (treated as libraries)
    final_output_jar_path: &str,
    main_class_name: Option<&str>,
    r8_jar_path: Option<&Path>,
    proguard_config_path: Option<&Path>,
    java_lib_path: Option<&Path>, // Base Java runtime lib path
) -> io::Result<()> {
    // Regex for stripping cargo hashes from .class filenames
    let re_strip_hash = Regex::new(r"^(?P<name>[^-]+)(?:-[0-9a-fA-F]+)?\.class$").unwrap();

    // Stage 1: Collect only loose class files
    let mut app_classes = Vec::new();
    // let mut seen_classes = HashSet::new(); // Less critical now we don't merge JARs

    for path_str in input_class_files {
        let path = Path::new(path_str);
        if !path.exists() {
             eprintln!("Warning (create_jar): Input class path does not exist: {}. Skipping.", path_str);
             continue;
        }
         if !path.is_file() {
             eprintln!("Warning (create_jar): Input class path is not a file: {}. Skipping.", path_str);
             continue;
         }

         let file_name_os = path.file_name().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, format!("Invalid class file path: {}", path_str)))?;
         let file_name = file_name_os.to_string_lossy();

         // Use the regex to get the base name, default to full name if no match
         let base_name = re_strip_hash.captures(&file_name)
             .and_then(|caps| caps.name("name").map(|m| format!("{}.class", m.as_str())))
             .unwrap_or_else(|| file_name.to_string());

         let jar_entry_name = base_name;

         let data = fs::read(path)?;
         app_classes.push(ClassInfo {
             jar_entry_name,
             data,
         });
    }

    // Convert input JAR file strings to PathBufs for R8
    let library_jar_paths: Vec<PathBuf> = input_jar_files.iter().map(PathBuf::from).collect();

    // Check if we have any application code to process
    if app_classes.is_empty() && r8_jar_path.is_none() {
        // If no loose classes and no R8, maybe we just need to copy/add manifest to a single input JAR?
        // This case is less likely when used by rustc. For now, error if no app classes.
        // Consider handling the "just add manifest to single input jar" case if needed.
         if library_jar_paths.len() == 1 && main_class_name.is_some() {
             println!("Warning: No loose .class files found. Adding manifest to the single input JAR.");
             let input_jar = &library_jar_paths[0];
             add_manifest_to_jar(input_jar, Path::new(final_output_jar_path), main_class_name)?;
             return Ok(()); // Successfully added manifest
         } else {
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input .class files found to process."));
         }
    }


    // --- Use a temporary directory ---
    let temp_dir = tempdir()?;
    let temp_dir_path = temp_dir.path();

    // --- Stage 2: Create Intermediate JAR (only with loose app classes) ---
    let intermediate_jar_path = temp_dir_path.join("intermediate_app.jar");
    if !app_classes.is_empty() {
        println!("Creating intermediate JAR for app classes...");
        let output_file = fs::File::create(&intermediate_jar_path)?;
        let mut zip_writer = ZipWriter::new(output_file);
        let options = SimpleFileOptions::default()
            .compression_method(CompressionMethod::DEFLATE)
            .unix_permissions(0o644);

        for class_info in &app_classes {
            zip_writer.start_file(&class_info.jar_entry_name, options)?;
            zip_writer.write_all(&class_info.data)?;
        }
        zip_writer.finish()?;
        println!("Intermediate JAR created at: {}", intermediate_jar_path.display());
    } else {
        println!("No loose application .class files found; intermediate JAR will be empty or skipped.");
        // If app_classes is empty, intermediate_jar_path won't exist. Handle this later.
    }


    // --- Stage 3: Optional R8 Optimization ---
    // This variable will hold the path to the JAR that needs the manifest added.
    let mut jar_to_add_manifest_to: Option<PathBuf> = None;

    if let (Some(r8_jar), Some(config), Some(lib)) = (r8_jar_path, proguard_config_path, java_lib_path) {
        println!("Running R8 optimizer...");
        let optimized_jar_path = temp_dir_path.join("optimized.jar");

        // R8 needs at least one program input. If intermediate_jar exists, use it.
        // If not (no loose .class files), R8 might still process libraries if the config requires it,
        // but usually, you need program input. This scenario needs clarification.
        // For now, we require the intermediate JAR to exist if R8 is run.
        if !intermediate_jar_path.exists() && !app_classes.is_empty() {
             // This shouldn't happen if app_classes wasn't empty, indicates an issue above.
             return Err(io::Error::new(io::ErrorKind::Other, "Intermediate JAR creation failed unexpectedly."));
        } else if app_classes.is_empty() {
            // What should happen if R8 is requested but there are no application classes?
            // Maybe R8 is just used to process/shrink the libraries themselves based on config?
            // R8 usually requires program input. Let's error for now.
            eprintln!("Error: R8 requested, but no input .class files were provided to process.");
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "R8 requires program input (.class files)."));
        }


        match run_r8_optimizer(
            r8_jar,
            config,
            lib, // Java runtime lib
            &intermediate_jar_path, // Program input
            &library_jar_paths, // Additional libs
            &optimized_jar_path, // Output
        ) {
            Ok(_) => {
                println!("R8 optimization successful. Output: {}", optimized_jar_path.display());
                jar_to_add_manifest_to = Some(optimized_jar_path); // Use the optimized JAR
            }
            Err(e) => {
                // R8 failed! Preserve the temp directory.
                let preserved_path = temp_dir.into_path(); // Consumes temp_dir, preventing cleanup
                eprintln!("\n--- R8 FAILED ---");
                eprintln!("R8 optimization failed. Intermediate files preserved for inspection.");
                if intermediate_jar_path.exists() {
                    eprintln!("Intermediate Input JAR: {}", intermediate_jar_path.display());
                }
                eprintln!("Input Libraries:");
                for lib_jar in &library_jar_paths {
                    eprintln!("  - {}", lib_jar.display());
                }
                eprintln!("Preserved Directory:    {}", preserved_path.display());
                eprintln!("See R8 output above for failure details.");
                eprintln!("-------------------\n");
                return Err(io::Error::new(
                    e.kind(),
                    format!("R8 optimization failed (intermediate files preserved at {}): {}", preserved_path.display(), e),
                ));
            }
        }
    } else {
        // No R8. Use the intermediate JAR if it has content, otherwise, handle the library-only case.
        println!("Skipping R8 optimization.");
        if intermediate_jar_path.exists() {
            jar_to_add_manifest_to = Some(intermediate_jar_path); // Use the unoptimized app JAR
        } else if library_jar_paths.len() == 1 {
             // No app classes, no R8, one input JAR -> just add manifest to it (handled earlier, but double-check)
             println!("Warning: No loose classes and no R8. Will add manifest to the single input JAR: {}", library_jar_paths[0].display());
             // The actual addition happens below using jar_to_add_manifest_to = None logic
             jar_to_add_manifest_to = None; // Signal that we need to use the input lib jar directly
        } else if library_jar_paths.is_empty() {
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input .class or .jar files specified."));
        } else {
             // Multiple library JARs, no app classes, no R8. What should happen? Merge them? Error?
             // Let's error for now, as the intended output is unclear.
             return Err(io::Error::new(io::ErrorKind::InvalidInput, "Multiple input JARs provided without .class files or R8 processing. Cannot determine output structure."));
        }
    };

    // --- Stage 4: Add Manifest ---
    let final_jar_temp_path = temp_dir_path.join("final_with_manifest.jar");

    let source_jar_for_manifest = match jar_to_add_manifest_to {
        Some(path) => path, // Use the R8 output or the intermediate JAR
        None => {
            // This path is taken if no R8 and no app classes, implying we use the single input library JAR
             if library_jar_paths.len() == 1 {
                 library_jar_paths[0].clone()
             } else {
                 // This state should have been caught earlier
                 return Err(io::Error::new(io::ErrorKind::Other, "Internal error: Ambiguous source JAR for manifest."));
             }
        }
    };

    println!("Adding manifest to: {}", source_jar_for_manifest.display());
    add_manifest_to_jar(
        &source_jar_for_manifest,
        &final_jar_temp_path,
        main_class_name,
    )?;
    println!("Manifest added. Temporary final JAR at: {}", final_jar_temp_path.display());


    // --- Stage 5: Move final JAR to destination ---
    if let Some(parent_dir) = Path::new(final_output_jar_path).parent() {
        fs::create_dir_all(parent_dir)?;
    }
    match rename(&final_jar_temp_path, final_output_jar_path) {
        Ok(_) => {
            //println!("Moved temporary JAR to final destination.");
        },
        Err(e) => {
            // Error cross-device link might occur, fall back to copy
            if e.kind() == io::ErrorKind::CrossesDevices {
                 eprintln!("Warning: Failed to rename temporary JAR across devices ({}). Attempting copy.", e);
                 fs::copy(&final_jar_temp_path, final_output_jar_path)?;
                 println!("Copied temporary JAR to final destination.");
                 // We might want to manually clean up the source temp file after copy, but tempdir should handle it on drop.
            } else {
                 eprintln!("Error: Failed to move temporary JAR to final destination: {}", e);
                 // Preserve the temp dir for inspection
                 let preserved_path = temp_dir.into_path();
                 eprintln!("Temporary JAR preserved at: {}", final_jar_temp_path.display());
                 eprintln!("Preserved Directory: {}", preserved_path.display());
                 return Err(e);
            }
        }
    }

    Ok(())
}


// --- add_manifest_to_jar remains the same ---
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
        .compression_method(CompressionMethod::DEFLATE) // Use DEFLATE for better compatibility
        .unix_permissions(0o644);

    // 1. Write the new Manifest file first
    let manifest_content = create_manifest_content(main_class_name);
    // Ensure parent directory exists for the manifest
    if let Some(parent) = Path::new("META-INF/MANIFEST.MF").parent() {
        // In-memory zip doesn't need directory creation, but for consistency:
        // zip_writer.add_directory(parent.to_str().unwrap(), options)?; // Not strictly needed for simple files
    }
    zip_writer.start_file("META-INF/MANIFEST.MF", options)?;
    zip_writer.write_all(manifest_content.as_bytes())?;

    // 2. Copy all entries from the input JAR, *except* for any existing manifest
    let mut copied_count = 0;
    for i in 0..input_archive.len() {
        let entry = input_archive.by_index_raw(i)?;

        let entry_name = entry.name();

        // Skip the existing manifest directory entry and file entry
        if entry_name == "META-INF/" || entry_name == "META-INF/MANIFEST.MF" {
             //println!("Debug: Skipping existing manifest entry: {}", entry_name);
            continue;
        }

        //println!("Debug: Copying entry: {}", entry_name);
        // raw_copy_file_rename might be useful if names need changing
        zip_writer.raw_copy_file(entry)?;
        copied_count += 1;
    }
    //println!("Debug: Copied {} entries from input JAR.", copied_count);

    zip_writer.finish()?;
    Ok(())
}

// --- create_manifest_content remains the same ---
fn create_manifest_content(main_class_name: Option<&str>) -> String {
    let mut manifest = String::new();
    manifest.push_str("Manifest-Version: 1.0\r\n");
    // Common practice to include Created-By
    manifest.push_str("Created-By: java-linker-rs (rust)\r\n");
    if let Some(main_class) = main_class_name {
        // Ensure FQN uses dots
        let main_class_fqn = main_class.replace('/', ".");
        manifest.push_str(&format!("Main-Class: {}\r\n", main_class_fqn));
    }
    // Crucial: Ensure the manifest ends with a blank line (CRLF CRLF)
    manifest.push_str("\r\n");
    manifest
}


// --- run_r8_optimizer (Modified) ---
/// Executes the R8 optimizer using `java -cp r8.jar com.android.tools.r8.R8 ...`.
fn run_r8_optimizer(
    r8_jar_path: &Path,             // Path to r8.jar
    proguard_config_path: &Path,    // Path to R8/ProGuard config file
    java_runtime_lib_path: &Path,   // Path to base Java runtime library (e.g., rt.jar or jmods)
    program_input_jar_path: &Path,  // The intermediate JAR with app classes
    library_jar_paths: &[PathBuf], // List of other input JARs (dependencies)
    output_jar_path: &Path,         // Where R8 should write the optimized JAR
) -> io::Result<()> {
    println!("--- Running R8 ---");
    println!("  R8 JAR: {}", r8_jar_path.display());
    println!("  Config: {}", proguard_config_path.display());
    println!("  Java Runtime Lib: {}", java_runtime_lib_path.display());
    println!("  Program Input: {}", program_input_jar_path.display());
    println!("  Library Inputs:");
    for lib_path in library_jar_paths {
        println!("    - {}", lib_path.display());
    }
    println!("  Output: {}", output_jar_path.display());

    // --- Construct the command: java -cp <r8.jar> com.android.tools.r8.R8 [args...] ---
    let r8_main_class = "com.android.tools.r8.R8";

    let mut cmd = Command::new("java");
    cmd.arg("-cp")                     // Use classpath option
       .arg(r8_jar_path)               // Provide path to r8.jar
       .arg(r8_main_class)             // Specify the main class to run
       // --- R8 specific arguments ---
       .arg("--release")               // Enable release mode (optimizations, shrinking)
       .arg("--output")                // Specify output path
       .arg(output_jar_path)
       .arg("--pg-conf")               // Specify ProGuard/R8 config file
       .arg(proguard_config_path)
       // Add the base Java runtime library
       .arg("--lib")
       .arg(java_runtime_lib_path)
       .arg("--classfile");

    // Add all the *other* input JARs as libraries
    for lib_path in library_jar_paths {
        cmd.arg("--classpath").arg(lib_path);
    }

    // Add the program input JAR (containing app classes) last
    cmd.arg(program_input_jar_path);


    println!("Executing command: {:?}", cmd);

    let output = cmd.output().map_err(|e| {
        io::Error::new(e.kind(), format!("Failed to execute R8 java command: {}", e))
    })?;

    // --- Process R8 output ---
    if !output.status.success() {
        // R8 failure details are crucial
        eprintln!("--- R8 Execution Failed ---");
        eprintln!("Exit Status: {}", output.status); // More specific than just code
        eprintln!("Command: {:?}", cmd);
        if !output.stdout.is_empty() {
            eprintln!("R8 STDOUT:\n---\n{}\n---", String::from_utf8_lossy(&output.stdout));
        } else {
            eprintln!("R8 STDOUT: (empty)");
        }
        if !output.stderr.is_empty() {
            eprintln!("R8 STDERR:\n---\n{}\n---", String::from_utf8_lossy(&output.stderr));
        } else {
            eprintln!("R8 STDERR: (empty)");
        }
        eprintln!("--- End R8 Failure ---");
        // Return an error that signals R8 failure, the calling function handles preservation
        return Err(io::Error::new(io::ErrorKind::Other, "R8 process failed"));
    } else {
        // Print stdout/stderr even on success for info/warnings
        if !output.stdout.is_empty() {
             println!("R8 STDOUT:\n---\n{}\n---", String::from_utf8_lossy(&output.stdout));
        }
        if !output.stderr.is_empty() {
             // R8 often prints informational messages to stderr
             println!("R8 STDERR (Info/Warnings):\n---\n{}\n---", String::from_utf8_lossy(&output.stderr));
        }
    }

    // Verify the output file was created (important sanity check)
    if !output_jar_path.exists() {
        eprintln!("--- R8 Execution Error ---");
        eprintln!("R8 process completed successfully (exit code 0), but the output JAR file was not found at: {}", output_jar_path.display());
        eprintln!("Check R8 output above for potential issues (e.g., empty output due to overly aggressive shrinking).");
        eprintln!("Command was: {:?}", cmd);
        eprintln!("--- End R8 Error ---");
        // Return an error that signals R8 failure, the calling function handles preservation
        return Err(io::Error::new(io::ErrorKind::NotFound, "R8 did not create the expected output JAR file"));
    }

    Ok(())
}