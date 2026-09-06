use crate::*;

pub(crate) fn class_file_from_data(data: &[u8]) -> io::Result<ClassFile<'static>> {
    ClassFile::from_bytes(data).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("invalid JVM class while merging generic specializations: {error}"),
        )
    })
}

pub(crate) fn method_identity(
    class_file: &ClassFile<'_>,
    method_index: usize,
) -> io::Result<(JavaString, JavaString)> {
    let (name, descriptor) = method_key(class_file, method_index)?;
    Ok((name.to_owned(), descriptor.to_owned()))
}

fn method_key<'a>(
    class_file: &'a ClassFile<'_>,
    method_index: usize,
) -> io::Result<(
    &'a ristretto_classfile::JavaStr,
    &'a ristretto_classfile::JavaStr,
)> {
    let method = &class_file.methods[method_index];
    let name = class_file
        .constant_pool
        .try_get_utf8(method.name_index)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.to_string()))?;
    let descriptor = class_file
        .constant_pool
        .try_get_utf8(method.descriptor_index)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.to_string()))?;
    Ok((name, descriptor))
}

struct MergeIndex {
    constants: HashMap<ConstantKey, u16>,
    methods: HashSet<(JavaString, JavaString)>,
    interfaces: HashSet<JavaString>,
}

impl MergeIndex {
    fn new(base: &ClassFile<'_>) -> io::Result<Self> {
        Ok(Self {
            constants: constant_pool_index(&base.constant_pool),
            methods: (0..base.methods.len())
                .map(|i| method_identity(base, i))
                .collect::<io::Result<_>>()?,
            interfaces: (0..base.interfaces.len())
                .map(|i| interface_name(base, i))
                .collect::<io::Result<_>>()?,
        })
    }
}

pub(crate) fn interface_name(
    class_file: &ClassFile<'_>,
    interface_index: usize,
) -> io::Result<JavaString> {
    let constant_index = *class_file.interfaces.get(interface_index).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("missing interface at index {interface_index}"),
        )
    })?;
    class_file
        .constant_pool
        .try_get_class(constant_index)
        .map(ToOwned::to_owned)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.to_string()))
}

fn merge_class_files(
    base: &mut ClassFile<'static>,
    incoming: &ClassFile<'static>,
    index: &mut MergeIndex,
) -> io::Result<bool> {
    if base.class_name().ok() != incoming.class_name().ok() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "cannot merge class files with different JVM names",
        ));
    }

    // A trait interface can receive static helper methods from an ordinary
    // holder fragment. Preserve the interface identity and never retain the
    // holder's synthetic constructor.
    let merged_is_interface = base.access_flags.contains(ClassAccessFlags::INTERFACE)
        || incoming.access_flags.contains(ClassAccessFlags::INTERFACE);
    let mut base_changed = false;
    if merged_is_interface {
        if incoming.access_flags.contains(ClassAccessFlags::INTERFACE)
            && base.access_flags != incoming.access_flags
        {
            base.access_flags = incoming.access_flags;
            base_changed = true;
        }
        let original_method_count = base.methods.len();
        base.methods.retain(|method| {
            base.constant_pool
                .try_get_utf8(method.name_index)
                .is_ok_and(|name| name != "<init>")
        });
        index.methods.retain(|(name, _)| name != "<init>");
        base_changed |= base.methods.len() != original_method_count;
    }

    let mut missing_method_indexes = Vec::new();
    for method in 0..incoming.methods.len() {
        let identity = method_identity(&incoming, method)?;
        if !(merged_is_interface && identity.0 == "<init>") && index.methods.insert(identity) {
            missing_method_indexes.push(method);
        }
    }

    let mut missing_interface_indexes = Vec::new();
    for interface in 0..incoming.interfaces.len() {
        if index
            .interfaces
            .insert(interface_name(&incoming, interface)?)
        {
            missing_interface_indexes.push(interface);
        }
    }

    if missing_method_indexes.is_empty() && missing_interface_indexes.is_empty() {
        return Ok(base_changed);
    }

    let base_bootstrap_count = base
        .attributes
        .iter()
        .find_map(|attribute| match attribute {
            Attribute::BootstrapMethods { methods, .. } => Some(methods.len()),
            _ => None,
        })
        .unwrap_or(0);
    let bootstrap_method_offset = u16::try_from(base_bootstrap_count).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "base class has too many bootstrap methods",
        )
    })?;
    let incoming_bootstrap_methods = incoming.attributes.iter().find_map(|attribute| {
        let Attribute::BootstrapMethods {
            name_index,
            methods,
        } = attribute
        else {
            return None;
        };
        Some((*name_index, methods.clone()))
    });

    let constant_indexes = import_constant_pool(
        &incoming.constant_pool,
        &mut base.constant_pool,
        &mut index.constants,
        bootstrap_method_offset,
    )?;

    for index in missing_interface_indexes {
        base.interfaces.push(remapped_constant_index(
            incoming.interfaces[index],
            &constant_indexes,
        )?);
    }

    if let Some((name_index, mut methods)) = incoming_bootstrap_methods {
        for BootstrapMethod {
            bootstrap_method_ref,
            arguments,
        } in &mut methods
        {
            *bootstrap_method_ref =
                remapped_constant_index(*bootstrap_method_ref, &constant_indexes)?;
            for argument in arguments {
                *argument = remapped_constant_index(*argument, &constant_indexes)?;
            }
        }
        if let Some(Attribute::BootstrapMethods {
            methods: base_methods,
            ..
        }) = base
            .attributes
            .iter_mut()
            .find(|attribute| matches!(attribute, Attribute::BootstrapMethods { .. }))
        {
            base_methods.extend(methods);
        } else {
            base.attributes.push(Attribute::BootstrapMethods {
                name_index: remapped_constant_index(name_index, &constant_indexes)?,
                methods,
            });
        }
    }

    for index in missing_method_indexes {
        let mut method = incoming.methods[index].clone();
        method.name_index = remapped_constant_index(method.name_index, &constant_indexes)?;
        method.descriptor_index =
            remapped_constant_index(method.descriptor_index, &constant_indexes)?;
        for attribute in &mut method.attributes {
            remap_attribute(attribute, &constant_indexes)?;
        }
        base.methods.push(method);
    }

    Ok(true)
}

pub(crate) fn serialize_class_file(class_file: &ClassFile<'static>) -> io::Result<Vec<u8>> {
    let mut data = Vec::new();
    jvm_compiler_core::classfile::encode::class_file(class_file, &mut data).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("failed to serialize merged JVM class: {error}"),
        )
    })?;
    Ok(data)
}

#[cfg(test)]
pub(crate) fn merge_class_data(base_data: &[u8], incoming_data: &[u8]) -> io::Result<Vec<u8>> {
    let mut base = class_file_from_data(base_data).map_err(|error| {
        io::Error::new(
            error.kind(),
            format!("could not parse the accumulated base fragment: {error}"),
        )
    })?;
    let incoming = class_file_from_data(incoming_data).map_err(|error| {
        io::Error::new(
            error.kind(),
            format!("could not parse the incoming fragment: {error}"),
        )
    })?;
    let mut index = MergeIndex::new(&base)?;
    if merge_class_files(&mut base, &incoming, &mut index)? {
        serialize_class_file(&base)
    } else {
        Ok(base_data.to_vec())
    }
}

#[cfg(test)]
pub(crate) fn merge_duplicate_classes(classes: Vec<ClassInfo>) -> io::Result<Vec<ClassInfo>> {
    merge_duplicate_classes_with_metrics(classes, None)
}

#[cfg(test)]
pub(crate) fn merge_duplicate_classes_with_metrics(
    classes: Vec<ClassInfo>,
    metrics: Option<&mut LinkerMetrics>,
) -> io::Result<Vec<ClassInfo>> {
    let mut positions: HashMap<String, usize> = HashMap::default();
    let mut groups: Vec<Vec<ClassInfo>> = Vec::new();
    for class_info in classes {
        if let Some(&index) = positions.get(&class_info.jar_entry_name) {
            groups[index].push(class_info);
        } else {
            positions.insert(class_info.jar_entry_name.clone(), groups.len());
            groups.push(vec![class_info]);
        }
    }
    if let Some(metrics) = metrics {
        metrics.record_fragment_groups(&groups);
    }

    groups.into_iter().map(merge_group).collect()
}

pub(crate) fn merge_group(mut fragments: Vec<ClassInfo>) -> io::Result<ClassInfo> {
    if fragments.len() == 1 {
        return Ok(fragments.pop().unwrap());
    }

    // Downstream crates can contribute a byte-for-byte identical
    // specialization to the same upstream holder. Discard those
    // before parsing: merge_class_files would make the same decision
    // after allocating a ClassFile and indexing its constant pool.
    let mut content_hashes: HashMap<u64, Vec<usize>> = HashMap::default();
    let mut unique_fragments = Vec::<ClassInfo>::with_capacity(fragments.len());
    for fragment in fragments {
        let mut hasher = FxHasher::default();
        fragment.data.hash(&mut hasher);
        let hash = hasher.finish();
        if content_hashes.get(&hash).is_some_and(|indexes| {
            indexes
                .iter()
                .any(|&index| unique_fragments[index].data == fragment.data)
        }) {
            continue;
        }
        let index = unique_fragments.len();
        unique_fragments.push(fragment);
        content_hashes.entry(hash).or_default().push(index);
    }
    if unique_fragments.len() == 1 {
        return Ok(unique_fragments.pop().unwrap());
    }

    // Parse every surviving fragment once. Previously the reorder
    // check parsed all fragments and the merge loop parsed them all a
    // second time.
    let mut parsed = unique_fragments
        .into_iter()
        .map(|fragment| {
            let class_file = class_file_from_data(&fragment.data).map_err(|error| {
                io::Error::new(
                    error.kind(),
                    format!(
                        "failed to parse duplicate JVM class {}: {error}",
                        fragment.jar_entry_name
                    ),
                )
            })?;
            Ok((fragment, class_file))
        })
        .collect::<io::Result<Vec<_>>>()?;

    // Preserve the largest fragment's compact constant indexes. This
    // avoids growing its near-limit methods through ldc-to-ldc_w remaps.
    if class_fragments_can_be_reordered(&parsed)? {
        parsed.sort_by(|(left, _), (right, _)| right.data.len().cmp(&left.data.len()));
    }

    let mut fragments = parsed.into_iter();
    let (mut merged, mut base) = fragments.next().unwrap();
    let mut index = MergeIndex::new(&base)?;
    let mut changed = false;
    for (_, incoming) in fragments {
        changed |= merge_class_files(&mut base, &incoming, &mut index).map_err(|error| {
            io::Error::new(
                error.kind(),
                format!(
                    "failed to merge duplicate JVM class {}: {error}",
                    merged.jar_entry_name
                ),
            )
        })?;
    }
    if changed {
        merged.data = serialize_class_file(&base).map_err(|error| {
            io::Error::new(
                error.kind(),
                format!(
                    "failed to merge duplicate JVM class {}: {error}",
                    merged.jar_entry_name
                ),
            )
        })?;
    }
    Ok(merged)
}

pub(crate) fn class_fragments_can_be_reordered(
    fragments: &[(ClassInfo, ClassFile<'static>)],
) -> io::Result<bool> {
    let mut methods = HashSet::default();
    for (_, class_file) in fragments {
        if !class_file.fields.is_empty() {
            return Ok(false);
        }
        for index in 0..class_file.methods.len() {
            let identity = method_key(&class_file, index)?;
            if identity.0 != "<init>" && !methods.insert(identity) {
                return Ok(false);
            }
        }
    }
    Ok(true)
}
