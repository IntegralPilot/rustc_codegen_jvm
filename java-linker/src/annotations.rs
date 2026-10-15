//! Preserve declaration annotations when class fragments share an owner.
use crate::*;
use ristretto_classfile::attributes::{Annotation, AnnotationElement};

pub(crate) fn entries(attribute: &Attribute) -> Option<(bool, &[Annotation])> {
    match attribute {
        Attribute::RuntimeVisibleAnnotations { annotations, .. } => Some((true, annotations)),
        Attribute::RuntimeInvisibleAnnotations { annotations, .. } => Some((false, annotations)),
        _ => None,
    }
}

pub(crate) fn remap(annotation: &mut Annotation, indexes: &impl ConstantIndexes) -> io::Result<()> {
    annotation.type_index = indexes.remap(annotation.type_index)?;
    for pair in &mut annotation.elements {
        pair.name_index = indexes.remap(pair.name_index)?;
        remap_value(&mut pair.value, indexes)?;
    }
    Ok(())
}

fn remap_value(value: &mut AnnotationElement, indexes: &impl ConstantIndexes) -> io::Result<()> {
    use AnnotationElement as V;
    match value {
        V::Byte { const_value_index }
        | V::Short { const_value_index }
        | V::Int { const_value_index }
        | V::Long { const_value_index }
        | V::Float { const_value_index }
        | V::Double { const_value_index }
        | V::Char { const_value_index }
        | V::Boolean { const_value_index }
        | V::String { const_value_index } => {
            *const_value_index = indexes.remap(*const_value_index)?
        }
        V::Class { class_info_index } => *class_info_index = indexes.remap(*class_info_index)?,
        V::Enum {
            type_name_index,
            const_name_index,
        } => {
            *type_name_index = indexes.remap(*type_name_index)?;
            *const_name_index = indexes.remap(*const_name_index)?;
        }
        V::Annotation { annotation } => remap(annotation, indexes)?,
        V::Array { values } => {
            for value in values {
                remap_value(value, indexes)?;
            }
        }
    }
    Ok(())
}

pub(crate) fn merge(
    target: &mut Vec<Attribute>,
    source: &[Attribute],
    indexes: &impl ConstantIndexes,
) -> io::Result<bool> {
    let mut changed = false;
    for attribute in source {
        let Some((visible, _)) = entries(attribute) else {
            continue;
        };
        let mut attribute = attribute.clone();
        remap_attribute(&mut attribute, indexes)?;
        let (Attribute::RuntimeVisibleAnnotations { annotations, .. }
        | Attribute::RuntimeInvisibleAnnotations { annotations, .. }) = &mut attribute
        else {
            unreachable!()
        };
        let mut additions = Vec::new();
        for annotation in annotations.drain(..) {
            let existing = target
                .iter()
                .filter_map(entries)
                .find_map(|(visibility, entries)| {
                    entries
                        .iter()
                        .find(|a| a.type_index == annotation.type_index)
                        .map(|a| (visibility, a))
                });
            if let Some((visibility, existing)) = existing {
                if visibility != visible || existing != &annotation {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "conflicting Java annotations on class fragments",
                    ));
                }
            } else {
                additions.push(annotation);
            }
        }
        if additions.is_empty() {
            continue;
        }
        changed = true;
        if let Some(existing) = target
            .iter_mut()
            .find(|a| entries(a).is_some_and(|(v, _)| v == visible))
        {
            let (Attribute::RuntimeVisibleAnnotations { annotations, .. }
            | Attribute::RuntimeInvisibleAnnotations { annotations, .. }) = existing
            else {
                unreachable!()
            };
            annotations.extend(additions);
        } else {
            *annotations = additions;
            target.push(attribute);
        }
    }
    Ok(changed)
}

#[cfg(test)]
mod tests;
