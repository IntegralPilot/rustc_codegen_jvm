use crate::*;
use jvm_compiler_core::classfile::summary;

pub(crate) fn holder(name: &str, private: bool) -> ClassFile<'static> {
    let mut class = ClassFile {
        version: ristretto_classfile::Version::Java8 { minor: 0 },
        ..Default::default()
    };
    class.this_class = class.constant_pool.add_class(name).unwrap();
    class.super_class = class.constant_pool.add_class("java/lang/Object").unwrap();
    class.access_flags = ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER;
    class.attributes.push(Attribute::SourceFile {
        name_index: class.constant_pool.add_utf8("SourceFile").unwrap(),
        source_file_index: class.constant_pool.add_utf8("main.rs").unwrap(),
    });
    if private {
        class.attributes.push(Attribute::Unknown {
            name_index: class
                .constant_pool
                .add_utf8(summary::PRIVATE_ATTRIBUTE)
                .unwrap(),
            info: Vec::new(),
        });
    }
    class
}

pub(crate) fn body(
    class: &mut ClassFile<'static>,
    name: &str,
    descriptor: &str,
    code: Vec<Instruction>,
) {
    class.methods.push(ristretto_classfile::Method {
        access_flags: ristretto_classfile::MethodAccessFlags::PUBLIC
            | ristretto_classfile::MethodAccessFlags::STATIC,
        name_index: class.constant_pool.add_utf8(name).unwrap(),
        descriptor_index: class.constant_pool.add_utf8(descriptor).unwrap(),
        attributes: vec![Attribute::Code {
            name_index: class.constant_pool.add_utf8("Code").unwrap(),
            max_stack: 2,
            max_locals: 1,
            code,
            exception_table: Vec::new(),
            attributes: Vec::new(),
        }],
    });
}

pub(crate) fn write_classes(
    directory: &Path,
    classes: impl IntoIterator<Item = ClassFile<'static>>,
) -> Vec<String> {
    classes
        .into_iter()
        .enumerate()
        .map(|(index, class)| {
            let path = directory.join(format!("{index}.class"));
            fs::write(&path, serialize_class_file(&class).unwrap()).unwrap();
            path.to_string_lossy().into_owned()
        })
        .collect()
}

pub(crate) fn run_jar(path: &Path) -> std::process::Output {
    let output = std::process::Command::new("java")
        .args(["-Xverify:all", "-jar"])
        .arg(path)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    output
}
