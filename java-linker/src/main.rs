use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::path::Path;
use regex::Regex;
use zip::write::{SimpleFileOptions, ZipWriter};
use zip::CompressionMethod;

fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: java-linker <input_class_files...> -o <output_jar_file>");
        return Err(1);
    }

    let mut input_files: Vec<String> = Vec::new();
    let mut output_file: Option<String> = None;

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
        } else if !arg.starts_with("-") && (arg.ends_with(".class") || arg.ends_with(".jar")) {
            input_files.push(arg.clone());
            i += 1;
        } else {
            i += 1; // Ignore flags
        }
    }

    if input_files.is_empty() {
        eprintln!("Error: No input class files provided.");
        return Err(1);
    }

    // if output_file doesn't end in .jar, add .jar
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

    let main_classes = find_main_classes(&input_files);

    if main_classes.len() > 1 {
        eprintln!("Error: Multiple classes with 'main' method found: {:?}", main_classes);
        return Err(1);
    }

    // Prepare the regex for sanitizing the main class file name.
    let re = Regex::new(r"^(.*?)-[0-9a-f]+(\.class)$").unwrap();
    let main_class_name = main_classes.first().map(|class_path| {
        let file_name = Path::new(class_path)
            .file_name()
            .unwrap()
            .to_str()
            .unwrap();
        // Sanitize the file name if it matches the pattern.
        let cleaned_name = if let Some(caps) = re.captures(file_name) {
            format!("{}{}", &caps[1], &caps[2])
        } else {
            file_name.to_string()
        };
        // Remove the ".class" extension and replace "/" with "." to get the fully qualified name.
        cleaned_name.trim_end_matches(".class").replace("/", ".")
    });

    if let Err(err) = create_jar(&input_files, &output_file_path, main_class_name.as_deref()) {
        eprintln!("Error creating JAR: {}", err);
        return Err(1);
    }

    println!("JAR file created successfully: {}", output_file_path);
    Ok(())
}

fn find_main_classes(class_files: &[String]) -> Vec<String> {
    // currently very simplified, will implement proper parsing later


    let mut main_classes = Vec::new();
    // Byte sequences to look for.
    let main_name = b"main";
    let main_descriptor = b"([Ljava/lang/String;)V";

    for file in class_files {
        if let Ok(data) = fs::read(file) {
            let has_main_name = data.windows(main_name.len()).any(|w| w == main_name);
            let has_main_descriptor = data.windows(main_descriptor.len()).any(|w| w == main_descriptor);
            if has_main_name && has_main_descriptor {
                main_classes.push(file.clone());
            }
        }
    }
    main_classes
}

fn create_jar(
    input_files: &[String],
    output_jar_path: &str,
    main_class_name: Option<&str>,
) -> io::Result<()> {
    let output_file = fs::File::create(output_jar_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let options = SimpleFileOptions::default()
        .compression_method(CompressionMethod::DEFLATE)
        .unix_permissions(0o644);

    // Create META-INF/MANIFEST.MF with the appropriate Main-Class.
    let manifest_content = create_manifest_content(main_class_name);
    zip_writer.start_file("META-INF/MANIFEST.MF", options)?;
    zip_writer.write_all(manifest_content.as_bytes())?;

    // Regex to match file names with a -randomnumbers suffix.
    let re = Regex::new(r"^(.*?)-[0-9a-f]+(\.class)$").unwrap();

    for input_file in input_files {
        let path = Path::new(input_file);
        let original_file_name = path.file_name().unwrap().to_str().unwrap();
        // Remove the random numbers suffix if it exists.
        let file_name = if let Some(caps) = re.captures(original_file_name) {
            format!("{}{}", &caps[1], &caps[2])
        } else {
            original_file_name.to_string()
        };

        let data = fs::read(input_file)?;
        zip_writer.start_file(file_name, options)?;
        zip_writer.write_all(&data)?;
    }

    zip_writer.finish()?;
    Ok(())
}

fn create_manifest_content(main_class_name: Option<&str>) -> String {
    let mut manifest = String::new();
    manifest.push_str("Manifest-Version: 1.0\r\n");
    manifest.push_str("Created-By: java-linker-rs\r\n");

    if let Some(main_class) = main_class_name {
        manifest.push_str(&format!("Main-Class: {}\r\n", main_class));
    }
    manifest.push_str("\r\n");
    manifest
}
