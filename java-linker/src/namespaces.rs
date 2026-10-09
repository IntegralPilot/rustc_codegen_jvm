//! Keep Rust crate identities through merging, then expose readable Java names
//! wherever the complete link contains only one identity for a crate name.
use crate::*;
use jvm_compiler_core::classfile::names::{
    CRATE_MARKER, CRATE_MARKER_LEN, LITERAL_STRING, NAME_STRING, is_crate_marker,
};
use ristretto_classfile::byte_reader::ByteReader;

#[derive(Default)]
pub(crate) struct Namespaces {
    removable: HashSet<Vec<u8>>,
    packed: HashMap<String, String>,
    pub(crate) strip_proofs: bool,
    pub(crate) aliases: crate::aliases::Aliases,
    pub(crate) short_methods: HashMap<Vec<u8>, Vec<u8>>,
}

impl Namespaces {
    pub(crate) fn collect<'a>(names: impl Iterator<Item = &'a str>) -> Self {
        let mut roots: HashMap<&str, HashSet<&str>> = HashMap::default();
        for name in names {
            let root = name.split('/').next().unwrap_or(name);
            let (plain, marker) = root
                .split_once(CRATE_MARKER)
                .filter(|(plain, _)| is_crate_marker(&root.as_bytes()[plain.len()..]))
                .map_or((root, ""), |(plain, _)| (plain, &root[plain.len()..]));
            roots.entry(plain).or_default().insert(marker);
        }
        Self {
            packed: HashMap::default(),
            strip_proofs: false,
            aliases: crate::aliases::Aliases::default(),
            short_methods: HashMap::default(),
            removable: roots
                .into_values()
                .filter(|identities| identities.len() == 1)
                .flatten()
                .filter(|marker| !marker.is_empty())
                .map(|marker| marker.as_bytes().to_vec())
                .collect(),
        }
    }

    pub(crate) fn pack(&mut self, names: HashMap<String, String>) {
        self.packed = names;
    }

    fn packed(&self, bytes: &[u8]) -> Option<Vec<u8>> {
        if self.packed.is_empty() {
            return None;
        }
        let mut output = Vec::new();
        let mut copied = 0;
        let mut start = 0;
        for end in 0..=bytes.len() {
            if end < bytes.len()
                && !matches!(bytes[end], b'(' | b')' | b';' | b'[' | b'#' | b':' | b'\n')
            {
                continue;
            }
            let token = &bytes[start..end];
            let text = std::str::from_utf8(token).ok();
            if let Some(text) = text {
                let binary = text.contains('.');
                let normalized = if binary {
                    text.replace('.', "/")
                } else {
                    text.into()
                };
                let mut prefix = 0;
                let mut target = self.packed.get(&normalized);
                if target.is_none() {
                    let descriptor = normalized
                        .trim_start_matches(['B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z', 'V']);
                    if let Some(name) = descriptor.strip_prefix('L') {
                        prefix = normalized.len() - name.len();
                        target = self.packed.get(name);
                    }
                }
                if let Some(target) = target {
                    output.extend_from_slice(&bytes[copied..start + prefix]);
                    if binary {
                        output.extend_from_slice(target.replace('/', ".").as_bytes());
                    } else {
                        output.extend_from_slice(target.as_bytes());
                    }
                    copied = end;
                }
            }
            start = end + 1;
        }
        if copied == 0 {
            return None;
        }
        output.extend_from_slice(&bytes[copied..]);
        Some(output)
    }

    fn rewrite(&self, original: &[u8]) -> Option<Vec<u8>> {
        if let Some(short) = self.short_methods.get(original) {
            return Some(short.clone());
        }
        let packed = self.packed(original);
        let bytes = packed.as_deref().unwrap_or(original);
        let mut result = Vec::new();
        let mut copied = 0;
        let mut scanned = 0;
        while let Some(offset) = bytes[scanned..]
            .windows(CRATE_MARKER.len())
            .position(|window| window == CRATE_MARKER.as_bytes())
        {
            let start = scanned + offset;
            let end = start + CRATE_MARKER_LEN;
            if bytes
                .get(start..end)
                .is_some_and(|marker| self.removable.contains(marker))
            {
                result.extend_from_slice(&bytes[copied..start]);
                copied = end;
                scanned = end;
            } else {
                scanned = start + CRATE_MARKER.len();
            }
        }
        if copied == 0 {
            return packed;
        }
        result.extend_from_slice(&bytes[copied..]);
        Some(result)
    }

    pub(crate) fn name(&self, name: &str) -> String {
        self.rewrite(name.as_bytes())
            .map(|bytes| String::from_utf8(bytes).expect("removing ASCII preserves UTF-8"))
            .unwrap_or_else(|| name.to_owned())
    }

    pub(crate) fn class(&self, mut class: ClassInfo) -> io::Result<ClassInfo> {
        let error = |error| constant_pool_error("crate namespace relocation", error);
        let mut reader = ByteReader::new(&class.data);
        reader.set_position(8); // magic and classfile version
        let mut pool = ConstantPool::from_bytes(&mut reader).map_err(error)?;
        let end = reader.position();
        let mut changed = crate::aliases::redirect(&mut pool, &self.aliases)?;
        let (names, class_attributes) = name_constants(&pool, &mut reader).map_err(error)?;
        let tail = if self.strip_proofs {
            strip_proofs(&pool, &class.data[class_attributes..]).map_err(error)?
        } else {
            None
        };
        let count = pool.len() as u16;
        let mut literals = HashMap::default();
        for index in 1..=count {
            if let Some(Constant::String(value)) = pool.get(index) {
                literals.entry(*value).or_insert_with(Vec::new).push(index);
            }
        }
        for index in 1..=count {
            let Some(Constant::Utf8(value)) = pool.get(index) else {
                continue;
            };
            if literals.contains_key(&index) {
                let relocated =
                    if let Some(name) = value.as_bytes().strip_prefix(NAME_STRING.as_bytes()) {
                        Some(self.rewrite(name).unwrap_or_else(|| name.to_vec()))
                    } else {
                        value
                            .as_bytes()
                            .strip_prefix(LITERAL_STRING.as_bytes())
                            .map(<[u8]>::to_vec)
                            .or_else(|| {
                                // Older static-owner recipes were untagged.
                                value
                                    .as_bytes()
                                    .windows(6)
                                    .any(|s| s == b"/mono/" || s == b".mono.")
                                    .then(|| self.packed(value.as_bytes()))
                                    .flatten()
                            })
                    };
                if let Some(bytes) = relocated {
                    let value = JavaString::from_mutf8(bytes).map_err(error)?;
                    pool.set(index, Constant::Utf8(value.into()))
                        .map_err(error)?;
                    changed = true;
                    continue;
                }
            }
            if !names.contains(&index) {
                continue;
            }
            let Some(bytes) = self.rewrite(value.as_bytes()) else {
                continue;
            };
            // A string literal may share its UTF8 entry with a class name or
            // descriptor. Preserve its contents and its ldc operand index.
            if let Some(strings) = literals.get(&index) {
                let original = pool.add(Constant::Utf8(value.clone())).map_err(error)?;
                for &string in strings {
                    pool.set(string, Constant::String(original))
                        .map_err(error)?;
                }
            }
            let value = JavaString::from_mutf8(bytes).map_err(error)?;
            pool.set(index, Constant::Utf8(value.into()))
                .map_err(error)?;
            changed = true;
        }
        if changed || tail.is_some() {
            // Method bodies, stack maps and their constant indexes stay intact;
            // there is no second instruction decode/encode or whole-JAR pass.
            let mut bytes = Vec::with_capacity(class.data.len());
            bytes.extend_from_slice(&class.data[..8]);
            pool.to_bytes(&mut bytes).map_err(error)?;
            bytes.extend_from_slice(&class.data[end..class_attributes]);
            bytes.extend_from_slice(tail.as_deref().unwrap_or(&class.data[class_attributes..]));
            class.data = bytes;
            let name = class.jar_entry_name.trim_end_matches(".class");
            class.jar_entry_name = format!("{}.class", self.name(name));
        }
        Ok(class)
    }

    pub(crate) fn relocations(&self, relocations: split::Relocations) -> split::Relocations {
        relocations
            .into_iter()
            .map(|(owner, methods)| {
                let methods = methods
                    .into_iter()
                    .map(|((name, descriptor), target)| {
                        (
                            (
                                self.name(&name.to_rust_string()).into(),
                                self.name(&descriptor.to_rust_string()).into(),
                            ),
                            self.name(&target),
                        )
                    })
                    .collect();
                (self.name(&owner.to_rust_string()).into(), methods)
            })
            .collect()
    }
}

/// Read only name-bearing metadata. Debug paths, annotation values and other
/// arbitrary UTF8 constants are not JVM identities and must remain untouched.
fn name_constants(
    pool: &ConstantPool<'_>,
    reader: &mut ByteReader<'_>,
) -> ristretto_classfile::Result<(HashSet<u16>, usize)> {
    let mut names = HashSet::default();
    for constant in pool.iter() {
        match constant {
            Constant::Class(name) | Constant::MethodType(name) => {
                names.insert(*name);
            }
            Constant::NameAndType {
                name_index,
                descriptor_index,
            } => {
                names.insert(*name_index);
                names.insert(*descriptor_index);
            }
            _ => {}
        }
    }
    reader.skip(6)?; // access flags, this class, superclass
    let interfaces = reader.read_u16()?;
    reader.skip(usize::from(interfaces) * 2)?;
    for _ in 0..2 {
        // fields and methods
        for _ in 0..reader.read_u16()? {
            reader.skip(2)?;
            names.insert(reader.read_u16()?);
            names.insert(reader.read_u16()?);
            name_attributes(pool, reader, &mut names)?;
        }
    }
    let attributes = reader.position();
    name_attributes(pool, reader, &mut names)?;
    Ok((names, attributes))
}

/// Keep proof attributes for later library linking. Remove them from the final executable.
fn strip_proofs(
    pool: &ConstantPool<'_>,
    bytes: &[u8],
) -> ristretto_classfile::Result<Option<Vec<u8>>> {
    let mut reader = ByteReader::new(bytes);
    let count = reader.read_u16()?;
    let mut retained = count;
    let mut output = Vec::new();
    let mut copied = 2;
    for _ in 0..count {
        let start = reader.position();
        let name = pool.try_get_utf8(reader.read_u16()?)?;
        let length = reader.read_u32()? as usize;
        reader.skip(length)?;
        if matches!(name.as_bytes(), b"RustJvmPrivate" | b"RustJvmCarrier") {
            if output.is_empty() {
                output.extend_from_slice(&[0, 0]);
            }
            output.extend_from_slice(&bytes[copied..start]);
            copied = reader.position();
            retained -= 1;
        }
    }
    if retained == count {
        return Ok(None);
    }
    output.extend_from_slice(&bytes[copied..]);
    output[..2].copy_from_slice(&retained.to_be_bytes());
    Ok(Some(output))
}

fn name_attributes(
    pool: &ConstantPool<'_>,
    reader: &mut ByteReader<'_>,
    names: &mut HashSet<u16>,
) -> ristretto_classfile::Result<()> {
    for _ in 0..reader.read_u16()? {
        let name = pool.try_get_utf8(reader.read_u16()?)?;
        let len = reader.read_u32()? as usize;
        let mut attribute = ByteReader::new(reader.read_bytes(len)?);
        match name.as_bytes() {
            b"Signature" => {
                names.insert(attribute.read_u16()?);
            }
            b"InnerClasses" => {
                for _ in 0..attribute.read_u16()? {
                    attribute.skip(4)?;
                    names.insert(attribute.read_u16()?);
                    attribute.skip(2)?;
                }
            }
            b"Code" => {
                attribute.skip(4)?;
                let len = attribute.read_u32()? as usize;
                attribute.skip(len)?;
                let exceptions = attribute.read_u16()?;
                attribute.skip(usize::from(exceptions) * 8)?;
                name_attributes(pool, &mut attribute, names)?;
            }
            b"LocalVariableTable" | b"LocalVariableTypeTable" => {
                for _ in 0..attribute.read_u16()? {
                    attribute.skip(6)?;
                    names.insert(attribute.read_u16()?);
                    attribute.skip(2)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}
