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
    let mut index = Index::collect(classes, bundles, archives)?;
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
    let mut namespaces =
        namespaces::Namespaces::collect(index.groups.iter().map(|g| g.name.as_str()));
    let pruned = index.retain_demanded(libraries)?;
    if let Some(metrics) = &mut metrics {
        metrics.pruned_classes = pruned;
        metrics.forwarding_methods = index.aliases.values().map(HashMap::len).sum();
        metrics.pruned_resources = index.pruned_resources;
    }
    let plan = packing::Plan::build(&mut index);
    // Release proof recipes before loading class bodies.
    for group in &mut index.groups {
        group.carrier = None;
    }
    if let Some(metrics) = &mut metrics {
        metrics.packed_owners = plan.units.iter().map(|u| u.groups.len() - 1).sum();
        metrics.shared_carriers = plan.shared_carriers;
        metrics.pruned_methods = plan
            .units
            .iter()
            .flat_map(|unit| &unit.groups)
            .filter_map(|&group| {
                index
                    .dead_methods
                    .get(index.groups[group].name.trim_end_matches(".class"))
            })
            .map(HashSet::len)
            .sum();
    }
    namespaces.pack(plan.names);
    namespaces.aliases = std::mem::take(&mut index.aliases);
    namespaces.short_methods = std::mem::take(&mut index.short_methods);
    namespaces.strip_proofs = index.share_carriers;
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
    while start < plan.units.len() {
        let mut end = start;
        let mut bytes = 0usize;
        while end < plan.units.len() && end - start < BATCH_CLASSES {
            let next = plan.units[end].bytes;
            if end > start && next > BATCH_BYTES.saturating_sub(bytes) {
                break;
            }
            bytes = bytes.saturating_add(next);
            end += 1;
        }
        // A single oversized class group is indivisible. All other live input
        // bytes are bounded by BATCH_BYTES, independent of the whole program.
        let chunk = (end - start).div_ceil(rayon::current_num_threads()).max(1);
        let merged = plan.units[start..end]
            .par_chunks(chunk)
            .map(|units| {
                jar::Batch::encode(units.iter().flat_map(|unit| {
                    match merge_unit(unit, &index, &readers, &namespaces, &relocations) {
                        Ok(classes) => classes.into_iter().map(Ok).collect::<Vec<_>>(),
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

fn merge_unit(
    unit: &packing::Unit,
    index: &Index,
    readers: &Readers,
    namespaces: &namespaces::Namespaces,
    relocations: &std::sync::Mutex<split::Relocations>,
) -> io::Result<Vec<ClassInfo>> {
    let mut output = Vec::new();
    for &position in &unit.groups {
        let group = &index.groups[position];
        let fragments = readers.load(&index.paths, group)?;
        if group.resource {
            let mut fragments = fragments.into_iter();
            let data = fragments.next().expect("resource has a fragment");
            if fragments.any(|fragment| fragment.data != data.data) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("conflicting binary constant {}", group.name),
                ));
            }
            return Ok(vec![data]);
        }
        let classes = merge_group_with_demands(
            fragments,
            Some(relocations),
            index
                .dead_methods
                .get(group.name.trim_end_matches(".class")),
        )?;
        for class in classes {
            output.push(namespaces.class(class)?);
        }
    }
    if unit.groups.len() > 1 {
        merge_group_with_demands(output, Some(relocations), None)
    } else {
        Ok(output)
    }
}
