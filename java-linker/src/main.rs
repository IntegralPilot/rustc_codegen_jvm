use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::fs::rename;
use std::io::{self, BufReader, Cursor, Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use regex::Regex;
use ristretto_classfile::attributes::{Attribute, Instruction, StackFrame, VerificationType};
use ristretto_classfile::{ClassFile, Constant, MethodAccessFlags};
use tempfile::tempdir;
use zip::write::{SimpleFileOptions, ZipWriter};
use zip::{CompressionMethod, ZipArchive};

#[derive(Debug)]
struct ClassInfo {
    jar_entry_name: String,
    data: Vec<u8>,
}

fn shifted_constant_index(index: u16, offset: u16) -> io::Result<u16> {
    index.checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "merged JVM class exceeds the constant-pool index limit",
        )
    })
}

fn remap_constant(mut constant: Constant, offset: u16) -> io::Result<Constant> {
    match &mut constant {
        Constant::Class(index)
        | Constant::String(index)
        | Constant::MethodType(index)
        | Constant::Module(index)
        | Constant::Package(index) => *index = shifted_constant_index(*index, offset)?,
        Constant::FieldRef {
            class_index,
            name_and_type_index,
        }
        | Constant::MethodRef {
            class_index,
            name_and_type_index,
        }
        | Constant::InterfaceMethodRef {
            class_index,
            name_and_type_index,
        } => {
            *class_index = shifted_constant_index(*class_index, offset)?;
            *name_and_type_index = shifted_constant_index(*name_and_type_index, offset)?;
        }
        Constant::NameAndType {
            name_index,
            descriptor_index,
        } => {
            *name_index = shifted_constant_index(*name_index, offset)?;
            *descriptor_index = shifted_constant_index(*descriptor_index, offset)?;
        }
        Constant::MethodHandle {
            reference_index, ..
        } => *reference_index = shifted_constant_index(*reference_index, offset)?,
        Constant::Dynamic {
            name_and_type_index,
            ..
        }
        | Constant::InvokeDynamic {
            name_and_type_index,
            ..
        } => *name_and_type_index = shifted_constant_index(*name_and_type_index, offset)?,
        Constant::Utf8(_)
        | Constant::Integer(_)
        | Constant::Float(_)
        | Constant::Long(_)
        | Constant::Double(_) => {}
    }
    Ok(constant)
}

fn remap_verification_type(
    verification_type: &mut VerificationType,
    offset: u16,
) -> io::Result<()> {
    if let VerificationType::Object { cpool_index } = verification_type {
        *cpool_index = shifted_constant_index(*cpool_index, offset)?;
    }
    Ok(())
}

fn remap_stack_frame(frame: &mut StackFrame, offset: u16) -> io::Result<()> {
    match frame {
        StackFrame::SameLocals1StackItemFrame { stack, .. }
        | StackFrame::SameLocals1StackItemFrameExtended { stack, .. } => {
            for item in stack {
                remap_verification_type(item, offset)?;
            }
        }
        StackFrame::AppendFrame { locals, .. } => {
            for item in locals {
                remap_verification_type(item, offset)?;
            }
        }
        StackFrame::FullFrame { locals, stack, .. } => {
            for item in locals.iter_mut().chain(stack.iter_mut()) {
                remap_verification_type(item, offset)?;
            }
        }
        StackFrame::SameFrame { .. }
        | StackFrame::ChopFrame { .. }
        | StackFrame::SameFrameExtended { .. } => {}
    }
    Ok(())
}

fn remap_instruction(instruction: &mut Instruction, offset: u16) -> io::Result<()> {
    match instruction {
        Instruction::Ldc(index) => {
            let shifted = shifted_constant_index(u16::from(*index), offset)?;
            if let Ok(short_index) = u8::try_from(shifted) {
                *index = short_index;
            } else {
                *instruction = Instruction::Ldc_w(shifted);
            }
        }
        Instruction::Ldc_w(index)
        | Instruction::Ldc2_w(index)
        | Instruction::Getstatic(index)
        | Instruction::Putstatic(index)
        | Instruction::Getfield(index)
        | Instruction::Putfield(index)
        | Instruction::Invokevirtual(index)
        | Instruction::Invokespecial(index)
        | Instruction::Invokestatic(index)
        | Instruction::Invokedynamic(index)
        | Instruction::New(index)
        | Instruction::Anewarray(index)
        | Instruction::Checkcast(index)
        | Instruction::Instanceof(index) => *index = shifted_constant_index(*index, offset)?,
        Instruction::Invokeinterface(index, _) | Instruction::Multianewarray(index, _) => {
            *index = shifted_constant_index(*index, offset)?;
        }
        _ => {}
    }
    Ok(())
}

fn remap_attribute(attribute: &mut Attribute, offset: u16) -> io::Result<()> {
    match attribute {
        Attribute::Code {
            name_index,
            code,
            exception_table,
            attributes,
            ..
        } => {
            *name_index = shifted_constant_index(*name_index, offset)?;
            for instruction in code {
                remap_instruction(instruction, offset)?;
            }
            for exception in exception_table {
                if exception.catch_type != 0 {
                    exception.catch_type = shifted_constant_index(exception.catch_type, offset)?;
                }
            }
            for nested in attributes {
                remap_attribute(nested, offset)?;
            }
        }
        Attribute::StackMapTable { name_index, frames } => {
            *name_index = shifted_constant_index(*name_index, offset)?;
            for frame in frames {
                remap_stack_frame(frame, offset)?;
            }
        }
        Attribute::Exceptions {
            name_index,
            exception_indexes,
        } => {
            *name_index = shifted_constant_index(*name_index, offset)?;
            for index in exception_indexes {
                *index = shifted_constant_index(*index, offset)?;
            }
        }
        Attribute::Signature {
            name_index,
            signature_index,
        } => {
            *name_index = shifted_constant_index(*name_index, offset)?;
            *signature_index = shifted_constant_index(*signature_index, offset)?;
        }
        Attribute::LineNumberTable { name_index, .. }
        | Attribute::Synthetic { name_index }
        | Attribute::Deprecated { name_index } => {
            *name_index = shifted_constant_index(*name_index, offset)?;
        }
        Attribute::MethodParameters {
            name_index,
            parameters,
        } => {
            *name_index = shifted_constant_index(*name_index, offset)?;
            for parameter in parameters {
                if parameter.name_index != 0 {
                    parameter.name_index = shifted_constant_index(parameter.name_index, offset)?;
                }
            }
        }
        other => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "cannot merge generated JVM method with unsupported {} attribute",
                    other.name()
                ),
            ));
        }
    }
    Ok(())
}

fn class_file_from_data(data: &[u8]) -> io::Result<ClassFile> {
    ClassFile::from_bytes(&mut Cursor::new(data.to_vec())).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("invalid JVM class while merging generic specializations: {error}"),
        )
    })
}

fn method_identity(class_file: &ClassFile, method_index: usize) -> io::Result<(String, String)> {
    let method = &class_file.methods[method_index];
    let name = class_file
        .constant_pool
        .try_get_utf8(method.name_index)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.to_string()))?;
    let descriptor = class_file
        .constant_pool
        .try_get_utf8(method.descriptor_index)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.to_string()))?;
    Ok((name.clone(), descriptor.clone()))
}

fn merge_class_data(base_data: &[u8], incoming_data: &[u8]) -> io::Result<Vec<u8>> {
    let mut base = class_file_from_data(base_data)?;
    let incoming = class_file_from_data(incoming_data)?;
    if base.class_name().ok() != incoming.class_name().ok() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "cannot merge class files with different JVM names",
        ));
    }

    let mut existing_methods = HashSet::new();
    for index in 0..base.methods.len() {
        existing_methods.insert(method_identity(&base, index)?);
    }
    let mut missing_method_indexes = Vec::new();
    for index in 0..incoming.methods.len() {
        if !existing_methods.contains(&method_identity(&incoming, index)?) {
            missing_method_indexes.push(index);
        }
    }
    if missing_method_indexes.is_empty() {
        return Ok(base_data.to_vec());
    }

    let offset = u16::try_from(base.constant_pool.len()).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "base JVM class constant pool exceeds the index limit",
        )
    })?;
    for constant in &incoming.constant_pool {
        base.constant_pool
            .push(remap_constant(constant.clone(), offset)?);
    }

    for index in missing_method_indexes {
        let mut method = incoming.methods[index].clone();
        method.name_index = shifted_constant_index(method.name_index, offset)?;
        method.descriptor_index = shifted_constant_index(method.descriptor_index, offset)?;
        for attribute in &mut method.attributes {
            remap_attribute(attribute, offset)?;
        }
        base.methods.push(method);
    }

    let mut merged = Vec::new();
    base.to_bytes(&mut merged).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("failed to serialize merged JVM class: {error}"),
        )
    })?;
    Ok(merged)
}

fn merge_duplicate_classes(classes: Vec<ClassInfo>) -> io::Result<Vec<ClassInfo>> {
    let mut positions: HashMap<String, usize> = HashMap::new();
    let mut merged: Vec<ClassInfo> = Vec::new();
    for class_info in classes {
        if let Some(&index) = positions.get(&class_info.jar_entry_name) {
            merged[index].data = merge_class_data(&merged[index].data, &class_info.data)?;
        } else {
            positions.insert(class_info.jar_entry_name.clone(), merged.len());
            merged.push(class_info);
        }
    }
    Ok(merged)
}

struct InstrumentationTimer {
    stage: &'static str,
    start: Instant,
}

impl InstrumentationTimer {
    fn new(stage: &'static str) -> Self {
        Self {
            stage,
            start: Instant::now(),
        }
    }
}

impl Drop for InstrumentationTimer {
    fn drop(&mut self) {
        record_instrumentation_duration(self.stage, self.start.elapsed());
    }
}

fn json_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn json_optional_string(value: Option<&str>) -> String {
    value.map(json_string).unwrap_or_else(|| "null".to_string())
}

fn record_instrumentation_duration(stage: &'static str, duration: Duration) {
    let Some(path) = env::var_os("RCGJ_INSTRUMENT_PATH") else {
        return;
    };
    if path.as_os_str().is_empty() {
        return;
    }
    let path = PathBuf::from(path);
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }

    let test = env::var("RCGJ_INSTRUMENT_TEST").ok();
    let mode = env::var("RCGJ_INSTRUMENT_MODE").ok();
    let seconds = duration.as_secs_f64();
    let line = format!(
        "{{\"schema_version\":1,\"kind\":\"phase\",\"stage\":{},\"crate_name\":null,\"item\":null,\"seconds\":{:.9},\"millis\":{:.6},\"pid\":{},\"test\":{},\"mode\":{}}}\n",
        json_string(stage),
        seconds,
        seconds * 1000.0,
        std::process::id(),
        json_optional_string(test.as_deref()),
        json_optional_string(mode.as_deref()),
    );

    if let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) {
        let _ = file.write_all(line.as_bytes());
    }
}

fn msvc_output_path(arg: &str) -> Option<&str> {
    let prefix = arg.get(..5)?;
    prefix.eq_ignore_ascii_case("/out:").then(|| &arg[5..])
}

fn jar_output_path(mut output_name: String) -> String {
    if output_name.to_ascii_lowercase().ends_with(".exe") {
        output_name.truncate(output_name.len() - ".exe".len());
    }
    if !output_name.to_ascii_lowercase().ends_with(".jar") {
        output_name.push_str(".jar");
    }
    output_name
}

fn parse_response_lines(content: &str, msvc_quoting: bool) -> Vec<String> {
    content
        .lines()
        .filter_map(|line| {
            let line = line.trim_end_matches('\r');
            if line.is_empty() {
                return None;
            }

            let line = if msvc_quoting {
                line.strip_prefix('"')
                    .and_then(|line| line.strip_suffix('"'))
                    .unwrap_or(line)
            } else {
                line
            };

            let mut parsed = String::with_capacity(line.len());
            let mut chars = line.chars().peekable();
            while let Some(character) = chars.next() {
                if character == '\\' {
                    if msvc_quoting {
                        if chars.peek() == Some(&'"') {
                            parsed.push(chars.next().unwrap());
                        } else {
                            parsed.push(character);
                        }
                    } else if let Some(escaped) = chars.next() {
                        parsed.push(escaped);
                    } else {
                        parsed.push(character);
                    }
                } else {
                    parsed.push(character);
                }
            }
            Some(parsed)
        })
        .collect()
}

fn read_response_file(path: &Path) -> io::Result<Vec<String>> {
    let bytes = fs::read(path)?;
    if bytes.starts_with(&[0xff, 0xfe]) {
        let body = &bytes[2..];
        if body.len() % 2 != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "UTF-16 response file has an odd byte length",
            ));
        }
        let units: Vec<u16> = body
            .chunks_exact(2)
            .map(|bytes| u16::from_le_bytes([bytes[0], bytes[1]]))
            .collect();
        let content = String::from_utf16(&units)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
        Ok(parse_response_lines(&content, true))
    } else {
        let content =
            std::str::from_utf8(bytes.strip_prefix(&[0xef, 0xbb, 0xbf]).unwrap_or(&bytes))
                .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
        Ok(parse_response_lines(content, false))
    }
}

fn linker_args() -> io::Result<Vec<String>> {
    let mut command_line = env::args();
    let mut expanded = vec![command_line.next().unwrap_or_else(|| "java-linker".into())];
    for arg in command_line {
        if let Some(path) = arg.strip_prefix('@') {
            expanded.extend(read_response_file(Path::new(path))?);
        } else {
            expanded.push(arg);
        }
    }
    Ok(expanded)
}

fn main() -> Result<(), i32> {
    let args = linker_args().map_err(|error| {
        eprintln!("Error: Failed to read linker response file: {error}");
        1
    })?;
    if args.len() < 3 {
        eprintln!("Usage: java-linker <input_files...> -o <output_jar_file>");
        return Err(1);
    }
    let _linker_timer = InstrumentationTimer::new("java-linker");

    let mut input_class_files: Vec<String> = Vec::new();
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

    // Combine inputs for scanning, but keep them separate for create_jar
    let all_input_paths: Vec<String> = input_class_files
        .iter()
        .cloned()
        .chain(input_jar_files.iter().cloned())
        .chain(input_rlib_files.iter().cloned())
        .collect();

    if all_input_paths.is_empty() {
        eprintln!("Error: No input files (.class or .jar) provided.");
        return Err(1);
    }

    let output_file_path = match output_file {
        Some(path) => path,
        None => {
            eprintln!("Error: Output file (-o or /OUT:) not specified.");
            return Err(1);
        }
    };

    // Find main class (scan both .class and .jar inputs)
    let main_classes = find_main_classes_with_ristretto(&all_input_paths).map_err(|e| {
        eprintln!("Error during main class scan: {}", e);
        1
    })?;
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
        &input_rlib_files,
        &output_file_path,
        main_class_name.as_deref(),
    )
    .map_err(|e| {
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
        } else if file_path_str.ends_with(".rlib") {
            match collect_rlib_classes(path) {
                Ok(classes) => {
                    for class_info in classes {
                        match check_class_data_for_main(
                            &class_info.data,
                            main_method_name,
                            main_method_descriptor,
                        ) {
                            Ok(Some(class_name)) => main_classes.push(class_name),
                            Ok(None) => {}
                            Err(e) => {
                                eprintln!(
                                    "Warning (main scan): Could not parse class entry '{}' within rlib '{}': {}. Skipping entry.",
                                    class_info.jar_entry_name, file_path_str, e
                                );
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!(
                        "Warning (main scan): Could not read rlib '{}': {}. Skipping.",
                        file_path_str, e
                    );
                }
            }
        }
    }
    Ok(main_classes)
}

fn check_class_data_for_main(
    data: &[u8],
    main_method_name: &str,
    main_method_descriptor: &str,
) -> io::Result<Option<String>> {
    let class_file = match ClassFile::from_bytes(&mut Cursor::new(data.to_vec())) {
        Ok(cf) => cf,
        Err(_e) => {
            // Ignore parse error details for this check
            // Treat parse failure as "no main method found in this file"
            // eprintln!("Debug (check_class_data): Parse error: {}", e); // Optional debug
            return Ok(None);
        }
    };

    for method in &class_file.methods {
        let flags = &method.access_flags;
        if flags.contains(MethodAccessFlags::PUBLIC) && flags.contains(MethodAccessFlags::STATIC) {
            // Avoid panics if constant pool is malformed, treat as not found
            let name = class_file
                .constant_pool
                .try_get_utf8(method.name_index)
                .ok()
                .cloned();
            let descriptor = class_file
                .constant_pool
                .try_get_utf8(method.descriptor_index)
                .ok()
                .cloned();

            if let (Some(n), Some(d)) = (name, descriptor) {
                if n == main_method_name && d == main_method_descriptor {
                    return match class_file.class_name() {
                        Ok(class_name_ref) => Ok(Some(class_name_ref.replace('/', "."))),
                        Err(e) => {
                            eprintln!(
                                "Warning (check_class_data): Found main method but failed to get class name: {}",
                                e
                            );
                            Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!(
                                    "Failed to get class name after finding main method: {}",
                                    e
                                ),
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
    input_class_files: &[String],
    input_jar_files: &[String],
    input_rlib_files: &[String],
    final_output_jar_path: &str,
    main_class_name: Option<&str>,
) -> io::Result<()> {
    // Regex for stripping cargo hashes from .class filenames
    let re_strip_hash = Regex::new(r"^(?P<name>[^-]+)(?:-[0-9a-fA-F]+)?\.class$").unwrap();

    // Stage 1: Collect only loose class files
    let mut app_classes = Vec::new();
    // let mut seen_classes = HashSet::new(); // Less critical now we don't merge JARs

    for path_str in input_class_files {
        let path = Path::new(path_str);
        if !path.exists() {
            eprintln!(
                "Warning (create_jar): Input class path does not exist: {}. Skipping.",
                path_str
            );
            continue;
        }
        if !path.is_file() {
            eprintln!(
                "Warning (create_jar): Input class path is not a file: {}. Skipping.",
                path_str
            );
            continue;
        }

        let file_name_os = path.file_name().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Invalid class file path: {}", path_str),
            )
        })?;
        let file_name = file_name_os.to_string_lossy();

        // Use the regex to get the base name, default to full name if no match
        let base_name = re_strip_hash
            .captures(&file_name)
            .and_then(|caps| caps.name("name").map(|m| format!("{}.class", m.as_str())))
            .unwrap_or_else(|| file_name.to_string());

        app_classes.push(class_info_from_class_data(fs::read(path)?, base_name));
    }

    for path_str in input_rlib_files {
        let path = Path::new(path_str);
        if !path.exists() {
            eprintln!(
                "Warning (create_jar): Input rlib path does not exist: {}. Skipping.",
                path_str
            );
            continue;
        }
        if !path.is_file() {
            eprintln!(
                "Warning (create_jar): Input rlib path is not a file: {}. Skipping.",
                path_str
            );
            continue;
        }
        app_classes.extend(collect_rlib_classes(path)?);
    }

    let app_classes = merge_duplicate_classes(app_classes)?;

    // Input JARs are bundled into the final artifact alongside generated classes.
    let library_jar_paths: Vec<PathBuf> = input_jar_files.iter().map(PathBuf::from).collect();

    if app_classes.is_empty() && library_jar_paths.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "No input .class or .jar files specified.",
        ));
    }

    // --- Use a temporary directory ---
    let temp_dir = tempdir()?;
    let temp_dir_path = temp_dir.path();

    // --- Stage 2: Create Intermediate JAR (only with loose app classes) ---
    let intermediate_jar_path = temp_dir_path.join("intermediate_app.jar");
    if !app_classes.is_empty() {
        let output_file = fs::File::create(&intermediate_jar_path)?;
        let mut zip_writer = ZipWriter::new(output_file);
        let options = SimpleFileOptions::default()
            .compression_method(CompressionMethod::DEFLATE)
            .unix_permissions(0o644);

        let mut seen_class_entries = HashSet::new();
        for class_info in &app_classes {
            if !seen_class_entries.insert(class_info.jar_entry_name.as_str()) {
                continue;
            }
            zip_writer.start_file(&class_info.jar_entry_name, options)?;
            zip_writer.write_all(&class_info.data)?;
        }
        zip_writer.finish()?;
    }

    // --- Stage 3: Bundle generated classes and input JARs ---
    let source_jar_for_manifest = if intermediate_jar_path.exists() && library_jar_paths.is_empty()
    {
        intermediate_jar_path
    } else {
        let bundled_jar_path = temp_dir_path.join("bundled.jar");
        let app_jar_path = if intermediate_jar_path.exists() {
            Some(intermediate_jar_path.as_path())
        } else {
            None
        };
        merge_input_jars(app_jar_path, &library_jar_paths, &bundled_jar_path)?;
        bundled_jar_path
    };

    // --- Stage 4: Add Manifest ---
    let final_jar_temp_path = temp_dir_path.join("final_with_manifest.jar");

    add_manifest_to_jar(
        &source_jar_for_manifest,
        &final_jar_temp_path,
        main_class_name,
    )?;

    // --- Stage 5: Move final JAR to destination ---
    if let Some(parent_dir) = Path::new(final_output_jar_path).parent() {
        fs::create_dir_all(parent_dir)?;
    }
    match rename(&final_jar_temp_path, final_output_jar_path) {
        Ok(_) => {
            //println!("Moved temporary JAR to final destination.");
        }
        Err(e) => {
            // Error cross-device link might occur, fall back to copy
            if e.kind() == io::ErrorKind::CrossesDevices {
                eprintln!(
                    "Warning: Failed to rename temporary JAR across devices ({}). Attempting copy.",
                    e
                );
                fs::copy(&final_jar_temp_path, final_output_jar_path)?;
                // We might want to manually clean up the source temp file after copy, but tempdir should handle it on drop.
            } else {
                eprintln!(
                    "Error: Failed to move temporary JAR to final destination: {}",
                    e
                );
                // Preserve the temp dir for inspection
                let preserved_path = temp_dir.into_path();
                eprintln!(
                    "Temporary JAR preserved at: {}",
                    final_jar_temp_path.display()
                );
                eprintln!("Preserved Directory: {}", preserved_path.display());
                return Err(e);
            }
        }
    }

    Ok(())
}

fn class_info_from_class_data(mut data: Vec<u8>, fallback_name: String) -> ClassInfo {
    let mut cursor = Cursor::new(data.clone());
    let class_file = ClassFile::from_bytes(&mut cursor).ok();
    if class_file.is_some() {
        data.truncate(cursor.position() as usize);
    }

    let jar_entry_name = class_file
        .and_then(|class_file| {
            class_file
                .class_name()
                .ok()
                .map(|class_name| format!("{class_name}.class"))
        })
        .unwrap_or(fallback_name);

    ClassInfo {
        jar_entry_name,
        data,
    }
}

fn try_class_info_from_class_data(mut data: Vec<u8>, fallback_name: String) -> Option<ClassInfo> {
    let mut cursor = Cursor::new(data.clone());
    let class_file = ClassFile::from_bytes(&mut cursor).ok()?;
    data.truncate(cursor.position() as usize);
    let jar_entry_name = class_file
        .class_name()
        .ok()
        .map(|class_name| format!("{class_name}.class"))
        .unwrap_or(fallback_name);

    Some(ClassInfo {
        jar_entry_name,
        data,
    })
}

fn gnu_long_name(table: &[u8], offset_text: &str, path: &Path) -> io::Result<String> {
    let offset = offset_text.trim().parse::<usize>().map_err(|e| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "invalid GNU ar long-name offset '{}' in {}: {}",
                offset_text,
                path.display(),
                e
            ),
        )
    })?;

    if offset >= table.len() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "GNU ar long-name offset {} is outside table in {}",
                offset,
                path.display()
            ),
        ));
    }

    let tail = &table[offset..];
    let end = tail
        .windows(2)
        .position(|window| window == b"/\n")
        .unwrap_or(tail.len());
    Ok(String::from_utf8_lossy(&tail[..end]).to_string())
}

fn collect_rlib_classes(path: &Path) -> io::Result<Vec<ClassInfo>> {
    let archive = fs::read(path)?;
    if !archive.starts_with(b"!<arch>\n") {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("{} is not an ar archive", path.display()),
        ));
    }

    let mut classes = Vec::new();
    let mut offset = 8usize;
    let mut gnu_long_names = Vec::new();
    while offset + 60 <= archive.len() {
        let header = &archive[offset..offset + 60];
        let raw_name = String::from_utf8_lossy(&header[0..16]).trim().to_string();
        let size_text = String::from_utf8_lossy(&header[48..58]).trim().to_string();
        let size = size_text.parse::<usize>().map_err(|e| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "invalid ar member size '{}' in {}: {}",
                    size_text,
                    path.display(),
                    e
                ),
            )
        })?;

        if &header[58..60] != b"`\n" {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid ar member header in {}", path.display()),
            ));
        }

        let mut data_start = offset + 60;
        let mut data_len = size;
        let member_name = if raw_name == "/" {
            "__gnu_symbol_table__".to_string()
        } else if raw_name == "//" {
            "__gnu_long_names__".to_string()
        } else if let Some(name_offset_text) = raw_name.strip_prefix('/') {
            gnu_long_name(&gnu_long_names, name_offset_text, path)?
        } else if let Some(name_len_text) = raw_name.strip_prefix("#1/") {
            let name_len = name_len_text.trim().parse::<usize>().map_err(|e| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "invalid BSD ar name length '{}' in {}: {}",
                        name_len_text,
                        path.display(),
                        e
                    ),
                )
            })?;
            if data_start + name_len > archive.len() || name_len > data_len {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("invalid BSD ar name in {}", path.display()),
                ));
            }
            let name = String::from_utf8_lossy(&archive[data_start..data_start + name_len])
                .trim_end_matches('\0')
                .to_string();
            data_start += name_len;
            data_len -= name_len;
            name
        } else {
            raw_name.trim_end_matches('/').to_string()
        };

        if data_start + data_len > archive.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "ar member '{}' extends past end of {}",
                    member_name,
                    path.display()
                ),
            ));
        }

        let data = &archive[data_start..data_start + data_len];
        if member_name == "__gnu_long_names__" {
            gnu_long_names = data.to_vec();
        } else if member_name.ends_with(".class") || data.starts_with(b"\xca\xfe\xba\xbe") {
            if let Some(class_info) =
                try_class_info_from_class_data(data.to_vec(), member_name.clone())
            {
                classes.push(class_info);
            }
        }

        offset += 60 + size;
        if offset % 2 == 1 {
            offset += 1;
        }
    }

    Ok(classes)
}

fn merge_input_jars(
    app_jar_path: Option<&Path>,
    library_jar_paths: &[PathBuf],
    output_jar_path: &Path,
) -> io::Result<()> {
    let output_file = fs::File::create(output_jar_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let mut seen_entries = HashSet::new();

    for library_jar_path in library_jar_paths {
        copy_jar_entries(library_jar_path, &mut zip_writer, &mut seen_entries)?;
    }

    // Runtime shim classes are authoritative for Java-owned core APIs such as
    // fmt::Arguments. Generated app classes with the same names are skipped.
    if let Some(app_jar) = app_jar_path {
        copy_jar_entries(app_jar, &mut zip_writer, &mut seen_entries)?;
    }

    zip_writer.finish()?;
    Ok(())
}

fn copy_jar_entries<W: Write + Seek>(
    input_jar_path: &Path,
    zip_writer: &mut ZipWriter<W>,
    seen_entries: &mut HashSet<String>,
) -> io::Result<()> {
    let input_file = fs::File::open(input_jar_path)?;
    let reader = BufReader::new(input_file);
    let mut input_archive = ZipArchive::new(reader)?;

    for i in 0..input_archive.len() {
        let entry = input_archive.by_index_raw(i)?;
        let entry_name = entry.name().to_string();

        if entry_name == "META-INF/" || entry_name == "META-INF/MANIFEST.MF" {
            continue;
        }

        if seen_entries.insert(entry_name) {
            zip_writer.raw_copy_file(entry)?;
        }
    }

    Ok(())
}

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

    let manifest_content = create_manifest_content(main_class_name);
    zip_writer.start_file("META-INF/MANIFEST.MF", options)?;
    zip_writer.write_all(manifest_content.as_bytes())?;

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
    }

    zip_writer.finish()?;
    Ok(())
}

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

#[cfg(test)]
mod tests {
    use super::{
        class_file_from_data, jar_output_path, merge_class_data, method_identity, msvc_output_path,
        parse_response_lines,
    };
    use ristretto_classfile::{
        ClassAccessFlags, ClassFile, ConstantPool, Method, MethodAccessFlags, Version,
    };

    fn abstract_class_with_method(method_name: &str) -> Vec<u8> {
        let mut constant_pool = ConstantPool::default();
        let this_class = constant_pool.add_class("test/Generic").unwrap();
        let super_class = constant_pool.add_class("java/lang/Object").unwrap();
        let name_index = constant_pool.add_utf8(method_name).unwrap();
        let descriptor_index = constant_pool.add_utf8("()V").unwrap();
        let class_file = ClassFile {
            version: Version::Java8 { minor: 0 },
            constant_pool,
            access_flags: ClassAccessFlags::PUBLIC
                | ClassAccessFlags::ABSTRACT
                | ClassAccessFlags::SUPER,
            this_class,
            super_class,
            methods: vec![Method {
                access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT,
                name_index,
                descriptor_index,
                attributes: Vec::new(),
            }],
            ..Default::default()
        };
        let mut bytes = Vec::new();
        class_file.to_bytes(&mut bytes).unwrap();
        bytes
    }

    #[test]
    fn recognizes_msvc_output_argument_case_insensitively() {
        assert_eq!(
            msvc_output_path("/OUT:C:\\tmp\\app.exe"),
            Some("C:\\tmp\\app.exe")
        );
        assert_eq!(
            msvc_output_path("/out:C:\\tmp\\app.exe"),
            Some("C:\\tmp\\app.exe")
        );
        assert_eq!(msvc_output_path("/DEBUG"), None);
    }

    #[test]
    fn converts_native_output_names_to_jar_names() {
        assert_eq!(jar_output_path("target/app".into()), "target/app.jar");
        assert_eq!(jar_output_path("target/app.exe".into()), "target/app.jar");
        assert_eq!(jar_output_path("target/app.EXE".into()), "target/app.jar");
        assert_eq!(jar_output_path("target/app.jar".into()), "target/app.jar");
    }

    #[test]
    fn parses_rustc_msvc_response_file_lines() {
        let content = concat!(
            "\"C:\\project with spaces\\Main.class\"\n",
            "\"/OUT:C:\\project with spaces\\app.exe\"\n",
            "\"/PDBALTPATH:%_PDB%\"\n",
        );
        assert_eq!(
            parse_response_lines(content, true),
            [
                "C:\\project with spaces\\Main.class",
                "/OUT:C:\\project with spaces\\app.exe",
                "/PDBALTPATH:%_PDB%",
            ]
        );
    }

    #[test]
    fn parses_rustc_gnu_response_file_lines() {
        assert_eq!(
            parse_response_lines("path\\ with\\ spaces/Main.class\n-o\noutput\n", false),
            ["path with spaces/Main.class", "-o", "output"]
        );
    }

    #[test]
    fn merges_complementary_generic_class_methods() {
        let first = abstract_class_with_method("first");
        let second = abstract_class_with_method("second");
        let merged = merge_class_data(&first, &second).unwrap();
        let class_file = class_file_from_data(&merged).unwrap();
        let methods: Vec<_> = (0..class_file.methods.len())
            .map(|index| method_identity(&class_file, index).unwrap())
            .collect();

        assert_eq!(
            methods,
            [
                ("first".into(), "()V".into()),
                ("second".into(), "()V".into())
            ]
        );
    }
}
