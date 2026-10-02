//! Overflow recovery for compiler-private, stateless method holders.
use crate::*;
use ristretto_classfile::MethodAccessFlags;

pub(crate) type Relocations = HashMap<JavaString, HashMap<(JavaString, JavaString), String>>;

pub(crate) fn eligible(class: &ClassFile<'_>) -> bool {
    class.class_name().is_ok_and(|name| {
        let name = name.to_string();
        name.contains("/mono/Mono_")
            || name.contains("/mono/MonoBucket_")
            || jvm_compiler_core::classfile::names::codec_owner(&name)
    }) && class.fields.is_empty()
        && class.interfaces.is_empty()
        && !class.access_flags.contains(ClassAccessFlags::INTERFACE)
        && class
            .constant_pool
            .try_get_class(class.super_class)
            .is_ok_and(|s| s == "java/lang/Object")
        && class.methods.iter().all(|m| {
            class
                .constant_pool
                .try_get_utf8(m.name_index)
                .is_ok_and(|name| {
                    name == "<init>"
                        || (name != "<clinit>"
                            && m.access_flags.contains(MethodAccessFlags::STATIC)
                            && !m.access_flags.intersects(
                                MethodAccessFlags::SYNCHRONIZED | MethodAccessFlags::NATIVE,
                            ))
                })
        })
}

pub(crate) use crate::compact::class as compact;

pub(crate) fn holders<'a>(
    sources: impl IntoIterator<Item = &'a ClassFile<'static>>,
    relocations: &mut Relocations,
) -> io::Result<Vec<ClassInfo>> {
    let mut output = Vec::new();
    let mut seen = HashSet::default();
    let mut moves = HashMap::default();
    let mut owner = None;
    for source in sources {
        if !eligible(source) {
            return Err(constant_pool_error(
                "cannot split stateful or public JVM class",
                "constant pool overflow",
            ));
        }
        let name = source
            .class_name()
            .map_err(|e| constant_pool_error("split owner", e))?
            .to_owned();
        if owner.is_none() {
            let constructors = (0..source.methods.len())
                .filter(|&i| method_identity(source, i).is_ok_and(|(n, _)| n == "<init>"))
                .collect::<Vec<_>>();
            output.push(compact(source, &constructors, &name.to_string())?);
            owner = Some(name.clone());
        }
        let mut methods = Vec::new();
        for i in 0..source.methods.len() {
            let key = method_identity(source, i)?;
            if key.0 != "<init>" && seen.insert(key) {
                methods.push(i);
            }
        }
        let mut pending = vec![methods];
        while let Some(methods) = pending.pop() {
            if methods.is_empty() {
                continue;
            }
            let piece = format!("{name}$Split_{}", output.len());
            match compact(source, &methods, &piece) {
                Ok(class) => {
                    for i in methods {
                        moves.insert(method_identity(source, i)?, piece.clone());
                    }
                    output.push(class);
                }
                Err(e) if methods.len() > 1 && e.to_string().contains("constant pool") => {
                    let middle = methods.len() / 2;
                    pending.push(methods[middle..].to_vec());
                    pending.push(methods[..middle].to_vec());
                }
                Err(e) => return Err(e),
            }
        }
    }
    if let Some(owner) = owner {
        if jvm_compiler_core::classfile::names::codec_owner(&owner.to_string()) {
            // Keep reflective codec lookup on the original owner after moving method bodies.
            let mut root = class_file_from_data(&output[0].data)?;
            crate::split_bridges::retain_codec_surface(&mut root, &moves)?;
            output[0].data = serialize_class_file(&root)?;
        }
        relocations.insert(owner, moves);
    }
    Ok(output)
}

/// Change MethodRef entries in place, so instructions, stack maps and method
/// handles retain their indexes. Only overflow recovery needs this second pass.
pub(crate) fn redirect(
    class: &mut ClassFile<'static>,
    relocations: &Relocations,
) -> io::Result<bool> {
    let mut replacements = Vec::new();
    for i in 1..=class.constant_pool.len() {
        let i = u16::try_from(i).map_err(|e| constant_pool_error("method reference index", e))?;
        let Some(Constant::MethodRef {
            class_index,
            name_and_type_index,
        }) = class.constant_pool.get(i)
        else {
            continue;
        };
        let pool = &class.constant_pool;
        let owner = pool
            .try_get_class(*class_index)
            .map_err(|e| constant_pool_error("method owner", e))?;
        let Some(moves) = relocations.get(owner) else {
            continue;
        };
        let (name, descriptor) = pool
            .try_get_name_and_type(*name_and_type_index)
            .map_err(|e| constant_pool_error("method identity", e))?;
        let name = pool
            .try_get_utf8(*name)
            .map_err(|e| constant_pool_error("method name", e))?;
        let descriptor = pool
            .try_get_utf8(*descriptor)
            .map_err(|e| constant_pool_error("method descriptor", e))?;
        if let Some(target) = moves.get(&(name.to_owned(), descriptor.to_owned())) {
            replacements.push((i, *name_and_type_index, target.clone()));
        }
    }
    if replacements.is_empty() {
        return Ok(false);
    }
    let mut constants = constant_pool_index(&class.constant_pool);
    for (i, name_and_type_index, owner) in replacements {
        let mut intern = |constant: Constant<'static>| -> io::Result<u16> {
            let key = ConstantKey::from(&constant);
            if let Some(&i) = constants.get(&key) {
                return Ok(i);
            }
            let i = class
                .constant_pool
                .add(constant)
                .map_err(|e| constant_pool_error("relocated constant pool is full", e))?;
            constants.insert(key, i);
            Ok(i)
        };
        let name = intern(Constant::Utf8(JavaString::from(owner).into()))?;
        let class_index = intern(Constant::Class(name))?;
        class
            .constant_pool
            .set(
                i,
                Constant::MethodRef {
                    class_index,
                    name_and_type_index,
                },
            )
            .map_err(|e| constant_pool_error("relocated method", e))?;
    }
    Ok(true)
}
