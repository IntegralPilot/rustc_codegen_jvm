//! Bounded indexed-input → class merge → JAR pipeline.
use crate::*;
use inputs::{Index, Readers};

const BATCH_BYTES: usize = 16 * 1024 * 1024;
const BATCH_CLASSES: usize = 128;

pub(crate) fn link(
    classes: &[String],
    bundles: &[String],
    archives: &[String],
    libraries: &[String],
    output: &str,
) -> io::Result<()> {
    let index = Index::collect(classes, bundles, archives)?;
    if index.groups.is_empty() && libraries.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "no JVM classes or library JARs",
        ));
    }
    if index.mains.len() > 1 {
        let mut mains = index.mains.iter().collect::<Vec<_>>();
        mains.sort_unstable();
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("multiple entry-point classes: {mains:?}"),
        ));
    }
    let mut metrics =
        LinkerMetrics::enabled().then(|| LinkerMetrics::from_index(&index, libraries));
    let namespaces = namespaces::Namespaces::collect(index.groups.iter().map(|g| g.name.as_str()));
    let output = Path::new(output);
    let parent = output
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    fs::create_dir_all(parent)?;
    // Staging beside the output permits one atomic rename, including when the
    // system temporary directory is on another filesystem.
    let temporary = tempfile::Builder::new()
        .prefix(".jvm-link-")
        .tempdir_in(parent)?;
    let staged = temporary.path().join("output.jar");
    let main = index.mains.iter().next().map(|name| namespaces.name(name));
    let mut jar = jar::Writer::create(&staged, main.as_deref())?;
    let readers = Readers::open(&index)?;
    let relocations = std::sync::Mutex::new(split::Relocations::default());
    let mut start = 0;
    while start < index.groups.len() {
        let mut end = start;
        let mut bytes = 0usize;
        while end < index.groups.len() && end - start < BATCH_CLASSES {
            let next = index.groups[end].bytes;
            if end > start && next > BATCH_BYTES.saturating_sub(bytes) {
                break;
            }
            bytes = bytes.saturating_add(next);
            end += 1;
        }
        // A single oversized class group is indivisible. All other live input
        // bytes are bounded by BATCH_BYTES, independent of the whole program.
        let chunk = (end - start).div_ceil(rayon::current_num_threads()).max(1);
        let merged = index.groups[start..end]
            .par_chunks(chunk)
            .map(|groups| {
                jar::Batch::encode(groups.iter().flat_map(|group| {
                    let result = readers.load(&index.paths, group).and_then(|fragments| {
                        merge_group_with_relocations(fragments, Some(&relocations))
                    });
                    match result {
                        Ok(classes) => classes
                            .into_iter()
                            .map(|class| namespaces.class(class))
                            .collect::<Vec<_>>(),
                        Err(error) => vec![Err(error)],
                    }
                }))
            })
            .collect::<io::Result<Vec<_>>>()?;
        for batch in merged {
            if let Some(metrics) = &mut metrics {
                metrics.merged_classes += batch.classes;
                metrics.merged_class_bytes += batch.bytes;
            }
            jar.batch(batch)?;
        }
        start = end;
    }
    jar.finish(&libraries.iter().map(PathBuf::from).collect::<Vec<_>>())?;
    let relocations = namespaces.relocations(relocations.into_inner().unwrap());
    if relocations.is_empty() {
        rename(staged, output)?;
    } else {
        let relocated = temporary.path().join("relocated.jar");
        jar::redirect(&staged, &relocated, &relocations)?;
        rename(relocated, output)?;
    }
    if let Some(metrics) = &mut metrics {
        metrics.output_jar_bytes = fs::metadata(output)?.len();
        if let Err(error) = metrics.write(&output.to_string_lossy()) {
            eprintln!("Warning: Failed to write java-linker metrics: {error}");
        }
    }
    Ok(())
}
