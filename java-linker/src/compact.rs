//! Rebuild supported class metadata and the pool reachable from retained members.
use crate::*;
use ristretto_classfile::MethodAccessFlags;

pub(crate) fn class(
    source: &ClassFile<'static>,
    methods: &[usize],
    owner: &str,
) -> io::Result<ClassInfo> {
    let mut pool = ConstantPool::default();
    let this_class = pool
        .add_class(owner)
        .map_err(|e| constant_pool_error("split class name", e))?;
    let superclass = source
        .constant_pool
        .try_get_class(source.super_class)
        .map_err(|e| constant_pool_error("compact superclass", e))?;
    let super_class = pool
        .add_class(superclass.to_string())
        .map_err(|e| constant_pool_error("split superclass", e))?;
    let mut constants = constant_pool_index(&pool);
    let mut bootstrap = Vec::new();
    let indexes = ConstantImporter::new(source, &mut pool, &mut constants, &mut bootstrap);
    let renamed = source
        .class_name()
        .map_err(|e| constant_pool_error("compact owner", e))?
        != owner;
    if renamed && !source.fields.is_empty() {
        return Err(io::Error::other(
            "field-bearing classes cannot move to another method holder",
        ));
    }
    let fields = source
        .fields
        .iter()
        .map(|field| {
            let mut field = field.clone();
            field.name_index = indexes.remap(field.name_index)?;
            field.descriptor_index = indexes.remap(field.descriptor_index)?;
            for attribute in &mut field.attributes {
                remap_attribute(attribute, &indexes)?;
            }
            Ok(field)
        })
        .collect::<io::Result<Vec<_>>>()?;
    let interfaces = source
        .interfaces
        .iter()
        .map(|&i| indexes.remap(i))
        .collect::<io::Result<Vec<_>>>()?;
    let methods = methods
        .iter()
        .map(|&i| {
            let mut method = source.methods[i].clone();
            method.name_index = indexes.remap(method.name_index)?;
            method.descriptor_index = indexes.remap(method.descriptor_index)?;
            // Only split holders need wider access for sibling pieces.
            if renamed {
                method
                    .access_flags
                    .remove(MethodAccessFlags::PRIVATE | MethodAccessFlags::PROTECTED);
                method.access_flags.insert(MethodAccessFlags::PUBLIC);
            }
            for attribute in &mut method.attributes {
                remap_attribute(attribute, &indexes)?;
            }
            Ok(method)
        })
        .collect::<io::Result<Vec<_>>>()?;
    let mut attributes = Vec::new();
    for attribute in &source.attributes {
        if let Attribute::Unknown { name_index, info } = attribute {
            // Compiler demand markers have no embedded constant indexes.
            if info.is_empty() {
                attributes.push(Attribute::Unknown {
                    name_index: indexes.remap(*name_index)?,
                    info: Vec::new(),
                });
            }
        } else if let Attribute::SourceFile {
            name_index,
            source_file_index,
        } = attribute
        {
            attributes.push(Attribute::SourceFile {
                name_index: indexes.remap(*name_index)?,
                source_file_index: indexes.remap(*source_file_index)?,
            });
        } else if matches!(attribute, Attribute::InnerClasses { .. }) {
            let mut attribute = attribute.clone();
            remap_attribute(&mut attribute, &indexes)?;
            attributes.push(attribute);
        }
    }
    drop(indexes);
    if !bootstrap.is_empty() {
        let name_index = pool
            .add_utf8("BootstrapMethods")
            .map_err(|e| constant_pool_error("split bootstrap name", e))?;
        attributes.push(Attribute::BootstrapMethods {
            name_index,
            methods: bootstrap,
        });
    }
    let class = ClassFile {
        version: source.version.clone(),
        constant_pool: pool,
        access_flags: source.access_flags,
        this_class,
        super_class,
        methods,
        fields,
        interfaces,
        attributes,
        ..Default::default()
    };
    Ok(ClassInfo {
        jar_entry_name: format!("{owner}.class"),
        data: serialize_class_file(&class)?,
    })
}
