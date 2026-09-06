use crate::*;

pub(crate) struct Writer {
    zip: ZipWriter<BufWriter<fs::File>>,
    seen: HashSet<String>,
}

fn options() -> SimpleFileOptions {
    SimpleFileOptions::default()
        .compression_method(CompressionMethod::DEFLATE)
        .compression_level(Some(1))
        .unix_permissions(0o644)
}

impl Writer {
    pub(crate) fn create(path: &Path, main: Option<&str>) -> io::Result<Self> {
        let mut zip = ZipWriter::new(BufWriter::with_capacity(
            256 * 1024,
            fs::File::create(path)?,
        ));
        zip.start_file("META-INF/MANIFEST.MF", options())?;
        zip.write_all(create_manifest_content(main).as_bytes())?;
        Ok(Self {
            zip,
            seen: HashSet::from_iter(["META-INF/MANIFEST.MF".into(), "META-INF/".into()]),
        })
    }
    pub(crate) fn class(&mut self, class: &ClassInfo) -> io::Result<()> {
        if self.seen.insert(class.jar_entry_name.clone()) {
            self.zip.start_file(&class.jar_entry_name, options())?;
            self.zip.write_all(&class.data)?;
        }
        Ok(())
    }
    pub(crate) fn finish(mut self, libraries: &[PathBuf]) -> io::Result<()> {
        for path in libraries {
            copy_jar_entries(path, &mut self.zip, &mut self.seen)?;
        }
        self.zip.finish()?.flush()
    }
}

#[cfg(test)]
pub(crate) fn write_final_jar(
    classes: &[ClassInfo],
    libraries: &[PathBuf],
    output: &Path,
    main: Option<&str>,
) -> io::Result<()> {
    let mut writer = Writer::create(output, main)?;
    for class in classes {
        writer.class(class)?;
    }
    writer.finish(libraries)
}

#[cfg(test)]
pub(crate) fn merge_input_jars(
    app_jar_path: Option<&Path>,
    library_jar_paths: &[PathBuf],
    output_jar_path: &Path,
) -> io::Result<()> {
    let output_file = fs::File::create(output_jar_path)?;
    let mut zip_writer = ZipWriter::new(output_file);
    let mut seen_entries = HashSet::default();

    // Compiled Rust classes are authoritative. In particular, a stale runtime
    // JAR must never shadow classes produced by compiling the real core crate.
    if let Some(app_jar) = app_jar_path {
        copy_jar_entries(app_jar, &mut zip_writer, &mut seen_entries)?;
    }

    for library_jar_path in library_jar_paths {
        copy_jar_entries(library_jar_path, &mut zip_writer, &mut seen_entries)?;
    }

    zip_writer.finish()?;
    Ok(())
}

pub(crate) fn copy_jar_entries<W: Write + Seek>(
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

pub(crate) fn create_manifest_content(main_class_name: Option<&str>) -> String {
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
