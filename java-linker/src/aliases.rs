//! Redirect direct calls and method handles through their shared constant-pool entries.
use crate::*;

pub(crate) type Aliases =
    HashMap<JavaString, HashMap<(JavaString, JavaString), (JavaString, JavaString)>>;

pub(crate) fn redirect<'a>(pool: &mut ConstantPool<'a>, aliases: &Aliases) -> io::Result<bool> {
    if aliases.is_empty() {
        return Ok(false);
    }
    let error = |e| constant_pool_error("forwarding method relocation", e);
    let mut replacements = Vec::new();
    for index in 1..=pool.len() as u16 {
        let Some(Constant::MethodRef {
            class_index,
            name_and_type_index,
        }) = pool.get(index)
        else {
            continue;
        };
        let owner = pool.try_get_class(*class_index).map_err(error)?;
        let Some(methods) = aliases.get(owner) else {
            continue;
        };
        let (name, descriptor) = pool
            .try_get_name_and_type(*name_and_type_index)
            .map_err(error)?;
        let key = (
            pool.try_get_utf8(*name).map_err(error)?.to_owned(),
            pool.try_get_utf8(*descriptor).map_err(error)?.to_owned(),
        );
        if let Some((owner, name)) = methods.get(&key) {
            replacements.push((index, *descriptor, owner.clone(), name.clone()));
        }
    }
    if replacements.is_empty() {
        return Ok(false);
    }
    let mut constants = constant_pool_index(pool);
    for (index, descriptor_index, owner, name) in replacements {
        let mut intern = |constant: Constant<'a>| -> io::Result<u16> {
            let key = ConstantKey::from(&constant);
            if let Some(&id) = constants.get(&key) {
                return Ok(id);
            }
            let id = pool.add(constant).map_err(error)?;
            constants.insert(key, id);
            Ok(id)
        };
        let owner = intern(Constant::Utf8(owner.into()))?;
        let class_index = intern(Constant::Class(owner))?;
        let name_index = intern(Constant::Utf8(name.into()))?;
        let name_and_type_index = intern(Constant::NameAndType {
            name_index,
            descriptor_index,
        })?;
        pool.set(
            index,
            Constant::MethodRef {
                class_index,
                name_and_type_index,
            },
        )
        .map_err(error)?;
    }
    Ok(true)
}
